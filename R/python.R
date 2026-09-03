#' @rdname BiocBook-python
#' @name BiocBook-python
#' @title Executing `python` code in a BiocBook
#'
#' @description
#'
#' `BiocBook` pages can execute `python` code as well as `R` code. The code is
#' executed on every render, including when the *Bioconductor Build System*
#' (BBS) rebuilds the book: `R CMD build` runs `vignettes/Makefile`, which runs
#' `quarto render`. That is deliberate. A `BiocBook` is versioned against a
#' Bioconductor release so that its code is re-proven against that release, and
#' output that is stored rather than recomputed would quietly stop being true.
#'
#' The consequence is that the `python` packages a book uses must be installed
#' at build time, wherever the book is built. `setup_python()` does that from
#' within the book itself, building the `conda` environment declared in
#' `inst/requirements.yml`, so the book carries its own `python` environment
#' rather than relying on one being present. A `conda` implementation
#' (`conda`, `mamba` or `micromamba`) must exist on the build machine.
#'
#' - `setup_python()`: provision and activate the book's `conda` environment
#'   from `inst/requirements.yml`. Call it from the first page that needs
#'   `python`; later pages re-activate it with `reticulate::use_condaenv()`.
#' - `add_python_chapter()`: add a new chapter wired up to execute `python`.
#'
#' Note that `python.reticulate` must be `true` in `inst/assets/_knitr.yml`
#' (its default, and what the `BiocBook` template ships). When it is `false`,
#' `knitr` runs every `python` chunk as a separate `python -c ...` subprocess:
#' no variable survives from one chunk to the next, and `matplotlib` figures are
#' silently dropped.
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
#' @param conda Path to the `conda`, `mamba` or `micromamba` binary, or
#'   `"auto"` to let `reticulate` find one. `reticulate` looks at the
#'   `RETICULATE_CONDA` environment variable, which is how the book's `Docker`
#'   image points it at `micromamba`.
#'
#' @return
#'
#' `setup_python()` invisibly returns the environment name.
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
    conda = "auto"
) {

    rlang::check_installed("reticulate", "to execute `python` code in a BiocBook.")

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

    ## The environment may already exist, typically because the book's Docker
    ## image pre-built it. Re-use it rather than resolving it all over again.
    existing <- tryCatch(
        reticulate::conda_list(conda = conda), error = function(e) NULL
    )
    if (is.null(existing)) cli::cli_abort(c(
        "No `conda` binary could be found.",
        "i" = "`BiocBook` provisions `conda` environments, so one of `conda`, \\
               `mamba` or `micromamba` must be installed where the book is built.",
        "i" = "Point `reticulate` at it with the {.envvar RETICULATE_CONDA} \\
               environment variable, e.g. \\
               {.code RETICULATE_CONDA=/usr/local/bin/micromamba}."
    ))

    if (envname %in% existing$name) {
        cli::cli_alert_success(cli::col_grey(
            "Re-using existing `conda` environment {.val {envname}}"
        ))
    } else {
        cli::cli_alert_info(cli::col_grey(
            "Creating `conda` environment {.val {envname}}"
        ))
        ## Deliberately `conda create` rather than `conda env create -f <file>`.
        ## reticulate rediscovers an environment's `conda` binary by parsing the
        ## `# cmd:` line of its `conda-meta/history`, and the extra word in
        ## `env create` makes that parse yield a path that does not exist. The
        ## environment is then unusable: reticulate falls back to the system
        ## `python` without failing, so the book would render against the wrong
        ## interpreter.
        reticulate::conda_create(
            envname = envname,
            packages = packages,
            channel = spec[["channels"]],
            forge = FALSE,
            conda = conda
        )
    }

    reticulate::use_condaenv(envname, required = TRUE, conda = conda)
    invisible(envname)
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

#' @rdname BiocBook-python
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
    activate <- if (setup) {
        "BiocBook::setup_python()"
    } else {
        "reticulate::use_condaenv(\"BiocBook\", required = TRUE)"
    }

    body <- glue::glue(
        "# {title}\n",
        "\n",
        "```{{r}}\n",
        "#| include: false\n",
        "library(reticulate)\n",
        "{activate}\n",
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
