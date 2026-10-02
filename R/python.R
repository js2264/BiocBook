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
#'   from `inst/requirements.yml`. Call it from any page that needs
#'   `python`.
#' - `python_envs()`: list the `conda` environments `setup_python()` has
#'   cached on this machine, and remove the ones that are no longer needed.
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
#' @section Cached environments:
#'
#' Environments live under `BiocBook`'s cache
#' (`tools::R_user_dir("BiocBook", "cache")`), in a folder named after the
#' `name:` declared in `requirements.yml` followed by a short hash of the file
#' itself, e.g. `envs/BiocBook-3f2a9c1b7d4e`. An environment is therefore
#' re-used for as long as `requirements.yml` is unchanged: editing the file
#' (adding, removing or re-pinning a package) gives a new environment on the
#' next render, and two books only ever share an environment when they declare
#' exactly the same thing.
#'
#' Older environments are not deleted automatically, since another book on the
#' same machine may still use them. `python_envs()` lists them with their size,
#' and `python_envs(remove = ...)` deletes them.
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
#' @param requirements Path to a `conda` environment file. If `NULL`,
#'   `inst/requirements.yml` is located by walking up from the working directory
#'   to the folder holding `_quarto.yml`, which works both while `quarto`
#'   renders a page and from a normal R session.
#' @param envname Name of the `conda` environment to create or re-use. If
#'   `NULL`, the `name:` declared in the environment file is used.
#' @param prefix Full path of the `conda` environment. If `NULL`, a path under
#'   `BiocBook`'s cache is derived from `envname` and from a hash of the
#'   `requirements` file (see "Cached environments"). Environments are always
#'   addressed by path rather than by name, since several `conda` root
#'   prefixes may each hold an environment of the same name.
#' @param conda Path to the `conda`, `mamba` or `micromamba` binary to
#'   provision with. Defaults to `micromamba()`, which finds or fetches one.
#' @param version Pinned `micromamba` release to use, as
#'   `"<version>-<build>"`. Bump it here rather than tracking `latest`.
#' @param quiet Whether to suppress progress messages.
#' @param remove Names of cached environments to delete, as listed by
#'   `python_envs()`. A name without its hash suffix (e.g. `"BiocBook"`)
#'   matches every environment created from a `requirements.yml` declaring
#'   that `name:`, and `"all"` deletes every cached environment.
#'
#' @return
#'
#' `setup_python()` invisibly returns the path of the environment it
#' activated, or of the interpreter that was already configured.
#' `python_envs()` returns a tibble with one row per cached environment
#' (`name`, `path`, `size_mb`, `modified`), or, when `remove` is set, the rows
#' of the environments it deleted, invisibly.
#' `add_python_chapter()` invisibly returns `book`.
#'
#' @examples
#' ## In practice, you should not use `.local` argument. 
#' unique_id <- as.numeric(Sys.time())
#' bookname <- paste(Sys.info()[['sysname']], unique_id, sep = '.')
#' bb <- init(bookname, .local = TRUE)
#' add_preamble(bb, open = FALSE)
#' add_chapter(bb, title = "Chapitre Un", open = FALSE)
#' add_python_chapter(bb, title = "Chapitre Deux", open = FALSE)
#' unlink(bookname, recursive = TRUE)
#'
#' ## `conda` environments cached on this machine
#' python_envs()
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
    if (is.null(prefix)) prefix <- .book_env_prefix(envname, requirements)

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
##
## Key the environment on the requirements file itself: editing the file
## must produce a new environment, and two books only share one when they
## declare exactly the same thing. Machines that build every day (e.g. the
## Bioconductor builders) would otherwise keep re-using whatever environment
## the first build created, whatever `requirements.yml` now says.
.book_env_prefix <- function(envname, requirements = NULL) {
    if (!is.null(requirements)) {
        envname <- paste(envname, substr(.sha256(requirements), 1, 12), sep = "-")
    }
    file.path(.book_envs_root(), envname)
}

.book_envs_root <- function() {
    file.path(tools::R_user_dir("BiocBook", which = "cache"), "envs")
}

#' @rdname BiocBook-python
#' @export

python_envs <- function(remove = NULL) {

    envs <- .list_book_envs()
    if (is.null(remove)) return(envs)

    ## A bare `name:` (no hash suffix) matches every environment built from a
    ## requirements file declaring it, i.e. every past version of a book's
    ## environment.
    hit <- if (identical(remove, "all")) {
        rep(TRUE, nrow(envs))
    } else {
        envs$name %in% remove | sub("-[0-9a-f]{12}$", "", envs$name) %in% remove
    }
    if (!any(hit)) {
        cli::cli_alert_info(cli::col_grey(
            "No cached `conda` environment matches {.val {remove}}."
        ))
        return(invisible(envs[hit, ]))
    }
    unlink(envs$path[hit], recursive = TRUE, force = TRUE)
    cli::cli_alert_success(cli::col_grey(
        "Removed {sum(hit)} cached `conda` environment{?s}: {.file {envs$name[hit]}}"
    ))
    invisible(envs[hit, ])
}

.list_book_envs <- function(root = .book_envs_root()) {
    paths <- if (dir.exists(root)) {
        list.dirs(root, full.names = TRUE, recursive = FALSE)
    } else {
        character(0)
    }
    ## `conda-meta/history` is rewritten by every install into the environment,
    ## so it dates the environment's content better than its folder does
    history <- file.path(paths, "conda-meta", "history")
    stamps <- as.character(ifelse(file.exists(history), history, paths))
    modified <- file.info(stamps)$mtime
    tibble::tibble(
        name = basename(paths),
        path = paths,
        size_mb = vapply(paths, .dir_size_mb, numeric(1), USE.NAMES = FALSE),
        modified = modified
    )
}

.dir_size_mb <- function(path) {
    files <- list.files(
        path, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE
    )
    round(sum(file.size(files), na.rm = TRUE) / 1024^2, 1)
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
    open = TRUE
) {

    if (is.na(file)) file <- .sanitize_filename(title)

    ## The `R` chunk is not decoration: it is what binds the page to the knitr
    ## engine, and therefore to reticulate rather than to a Jupyter kernel.
    ## `engine: knitr` says the same thing explicitly, so the page stays on
    ## knitr even if that chunk is later deleted.
    body <- glue::glue(
        "---\n",
        "engine: knitr\n",
        "---\n",
        "\n",
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

    ## The page now calls `BiocBook::setup_python()` and runs through
    ## `reticulate` while the book builds, so the book package must declare
    ## both, or the build machines will not install them.
    usethis::with_project(path(book), {
        usethis::use_package("BiocBook", "Suggests")
        usethis::use_package("reticulate", "Suggests")
    }, quiet = TRUE)

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
