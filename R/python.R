#' @rdname BiocBook-python
#' @name BiocBook-python
#' @title Executing `python` code in a BiocBook
#'
#' @description
#'
#' `BiocBook` pages can execute `python` code as well as `R` code. The code is
#' executed on every render, including with GHA or when the *Bioconductor Build System*
#' (BBS) rebuilds the book: `R CMD build` runs `vignettes/Makefile`, which runs
#' `quarto render`. 
#'
#' The consequence is that the `python` packages a book uses must be installed
#' at build time, wherever the book is built. `setup_python()` does that from
#' within the book itself, building the `conda` environment declared in
#' `inst/requirements.yml`, so the book carries its own `python` environment
#' rather than relying on one being present.
#'
#' - `setup_python()`: provision and activate the book's `conda` environment
#'   from `inst/requirements.yml`. Call it from the first page that needs
#'   `python`; later pages re-activate it with `reticulate::use_condaenv()`.
#' - `micromamba()`: path to the `micromamba` binary the book provisions with,
#'   downloading a pinned, checksummed copy on first use if none is present.
#'
#' `micromamba` is the only provisioning dependency, and it is a single
#' self-contained binary: no `conda` installation, no base environment, no
#' `python`. `micromamba()` looks for one in `RETICULATE_CONDA`, then on the
#' `PATH`, then in `BiocBook`'s cache, and only then downloads it. 
#' On a build machine that has no `conda` at all (which includes the
#' `r-universe` build image Bioconductor is migrating to), it is what makes
#' the book buildable. On GitHub Actions, the book's `Docker` image installs 
#' it up front
#'
#' @section Engines:
#'
#' `quarto` binds an execution engine per file. A page holding at least one `R`
#' chunk uses `knitr`, and its `python` chunks then run through `reticulate` in
#' a single session shared with `R`. A page with no `R` chunk at all uses
#' `jupyter` instead.
#'
#' Prefer `knitr`. It is the only way to share objects between `R` and `python`,
#' it keeps the book's house style (`collapse`, `comment`, `fig.align` from
#' `inst/assets/_knitr.yml`) and `code-link`, which apply to `knitr` pages only
#' -- and, decisively, the Bioconductor builders provide `python3` but not
#' `jupyter`, so a `jupyter` page cannot be rendered there at all.
#'
#' @param book A `BiocBook` object.
#' @param title Title of the new chapter.
#' @param file Name of the new `.qmd` file. If `NA`, derived from `title`.
#' @param position Position of the new chapter in the book.
#' @param setup Whether the new chapter should open with a `setup_python()`
#'   chunk. Use `TRUE` for the first `python` chapter of a book, and `FALSE`
#'   for later ones, which only need to re-activate the environment.
#' @param open Whether to open the file for editing.
#' @param requirements Path to a `conda` environment file. If `NULL`,
#'   `inst/requirements.yml` is located by walking up from the working directory
#'   to the folder holding `_quarto.yml`, which works both while `quarto`
#'   renders a page and from a normal R session.
#' @param envname Name of the `conda` environment to create or re-use. If
#'   `NULL`, the `name:` declared in the environment file is used.
#' @param prefix Full path of the `conda` environment. If `NULL`, a path under
#'   `BiocBook`'s cache is derived from `envname`. Environments are always
#'   addressed by path rather than by name, since several `conda` root
#'   prefixes may each hold an environment of the same name.
#' @param conda Path to the `conda`, `mamba` or `micromamba` binary to
#'   provision with. Defaults to `micromamba()`, which finds or fetches one.
#' @param version Pinned `micromamba` release to use, as
#'   `"<version>-<build>"`. Bump it here rather than tracking `latest`.
#' @param quiet Whether to suppress progress messages.
#'
#' @return
#'
#' `setup_python()` invisibly returns the path of the environment it
#' activated, or of the interpreter that was already configured.
#' `add_python_chapter()` invisibly returns `book`.
#'
#' @examples
#' \dontrun{
#' book <- BiocBook("path/to/book")
#' add_python_chapter(book, "Working with anndata")
#' }
NULL

#' @rdname BiocBook-python
#' @export

setup_python <- function(
    requirements = NULL,
    envname = NULL,
    prefix = NULL,
    conda = micromamba()
) {

    rlang::check_installed("reticulate", "to execute `python` code in a BiocBook.")

    ## An interpreter may already have been chosen for us. The book's Docker
    ## image sets `RETICULATE_PYTHON` in `Renviron.site`, and `reticulate`
    ## honours it on its own, so there is nothing to provision or activate.
    chosen <- Sys.getenv("RETICULATE_PYTHON", unset = NA)
    if (!is.na(chosen) && nzchar(chosen) && file.exists(chosen)) {
        cli::cli_alert_success(cli::col_grey(
            "Using the `python` already configured for this session: \\
            {.file {chosen}}"
        ))
        return(invisible(chosen))
    }

    ## Locate `inst/requirements.yml`. This has to work in two very different
    ## situations: while `quarto` renders a page (the package is typically not
    ## installed yet, and the working directory is the page's own folder), and
    ## from a normal R session.
    if (is.null(requirements)) requirements <- .find_requirements()
    if (is.null(requirements) || !file.exists(requirements)) cli::cli_abort(c(
        "Could not find a `requirements.yml` file for this book.",
        "i" = "Create {.file inst/requirements.yml} declaring the `conda` \\
               environment this book needs, or edit it with \\
               {.code edit_requirements_yml(book)}."
    ))

    spec <- yaml::read_yaml(requirements)
    if (is.null(envname)) {
        envname <- if (is.null(spec[["name"]])) "BiocBook" else trimws(spec[["name"]])
    }

    ## `conda` environment files may carry a `pip:` block. This book stack is
    ## deliberately conda-only, so say so rather than silently dropping packages.
    deps <- spec[["dependencies"]]
    is_pip <- vapply(deps, is.list, logical(1))
    if (any(is_pip)) cli::cli_abort(c(
        "{.file {basename(requirements)}} contains a `pip:` section.",
        "x" = "`BiocBook` provisions `conda` environments only.",
        "i" = "Declare these packages as `conda` packages instead, from the \\
               `conda-forge` or `bioconda` channels."
    ))
    packages <- unlist(deps)

    ## Address the environment by its full path, never by name. Environments
    ## of the same name can exist under several `conda` root prefixes at once,
    ## and `reticulate` then warns and picks the first it happens to list --
    ## which may not be this book's.
    if (is.null(prefix)) prefix <- .book_env_prefix(envname)

    if (dir.exists(prefix)) {
        cli::cli_alert_success(cli::col_grey(
            "Re-using `conda` environment {.file {prefix}}"
        ))
    } else {
        cli::cli_alert_info(cli::col_grey(
            "Creating `conda` environment {.file {prefix}}"
        ))
        .setup_conda_env(prefix, packages, spec[["channels"]], conda)
    }

    ## `reticulate` rediscovers an environment's `conda` binary from the
    ## `# cmd:` line of its `conda-meta/history`. If the binary that built the
    ## environment has since moved or gone, activation fails with an opaque
    ## `normalizePath` error, so rebuild rather than surface that.
    activated <- tryCatch({
        reticulate::use_condaenv(prefix, required = TRUE, conda = conda)
        TRUE
    }, error = function(e) FALSE)

    if (!activated) {
        cli::cli_alert_warning(cli::col_grey(
            "The environment at {.file {prefix}} is not usable, most likely \\
            because the `conda` binary that created it has moved. Rebuilding it."
        ))
        unlink(prefix, recursive = TRUE, force = TRUE)
        .setup_conda_env(prefix, packages, spec[["channels"]], conda)
        reticulate::use_condaenv(prefix, required = TRUE, conda = conda)
    }

    invisible(prefix)
}

## Book environments live under BiocBook's own cache, so that a book always
## resolves its own environment rather than a same-named one belonging to
## something else.
.book_env_prefix <- function(envname) {
    file.path(tools::R_user_dir("BiocBook", which = "cache"), "envs", envname)
}

## Deliberately `conda create` rather than `conda env create -f <file>`.
## reticulate rediscovers an environment's `conda` binary by parsing the
## `# cmd:` line of its `conda-meta/history`, and the extra word in
## `env create` makes that parse yield a path that does not exist. The
## environment is then unusable in a way that does not fail: reticulate falls
## back to the system `python`, so the book would render against the wrong
## interpreter.
.setup_conda_env <- function(prefix, packages, channels, conda) {
    dir.create(dirname(prefix), recursive = TRUE, showWarnings = FALSE)
    reticulate::conda_create(
        envname = prefix,
        packages = packages,
        channel = channels,
        forge = FALSE,
        conda = conda
    )
}

## Walk up from the working directory looking for the book root, i.e. the
## folder holding `_quarto.yml`. During a `quarto` render the working directory
## is the page's own folder, so the book root is normally one level up.
.find_requirements <- function(start = getwd()) {
    dir <- normalizePath(start, mustWork = FALSE)
    for (i in seq_len(10L)) {
        if (file.exists(file.path(dir, "_quarto.yml"))) {
            req <- file.path(dir, "requirements.yml")
            return(if (file.exists(req)) req else NULL)
        }
        parent <- dirname(dir)
        if (identical(parent, dir)) break
        dir <- parent
    }
    ## Fallback: the book package is installed, so ask R where its files went
    desc <- tryCatch(
        read.dcf(file.path(rprojroot::find_root("DESCRIPTION"), "DESCRIPTION")),
        error = function(e) NULL
    )
    if (!is.null(desc) && "Package" %in% colnames(desc)) {
        req <- system.file("requirements.yml", package = desc[1, "Package"])
        if (nzchar(req)) return(req)
    }
    NULL
}

#' @rdname BiocBook-editing
#' @export

add_python_chapter <- function(
    book,
    title,
    file = NA,
    position = NULL,
    setup = TRUE,
    open = TRUE
) {

    if (is.na(file)) file <- .sanitize_filename(title)

    ## The `R` chunk is not decoration: it is what binds the page to the knitr
    ## engine, and therefore to reticulate rather than to a Jupyter kernel.
    body <- glue::glue(
        "# {title}\n",
        "\n",
        "```{{r}}\n",
        "#| include: false\n",
        "library(reticulate)\n",
        "BiocBook::setup_python()\n",
        "```\n",
        "\n",
        "```{{python}}\n",
        "print(\"Hello from python\")\n",
        "```\n"
    )

    full_path <- .add_page(book, title, file, position, open = FALSE, body = body)

    cli::cli_alert_info(cli::col_grey(
        "This page executes `python` on every render, including on the \\
        Bioconductor Build System. Declare the packages it needs in \\
        {.file inst/requirements.yml}."
    ))
    cli::cli_alert_info(cli::col_grey(
        "Keep at least one `R` chunk on the page: it is what keeps `quarto` on \\
        the `knitr` engine. A page with only `python` chunks uses `jupyter` \\
        instead, which the Bioconductor builders do not provide."
    ))
    if (rlang::is_interactive() && open) usethis::edit_file(full_path)

    invisible(book)
}
