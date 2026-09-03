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
#' within the book itself, so the book carries its own `python` environment
#' rather than relying on one being present.
#'
#' - `setup_python()`: provision and activate the book's `python` environment
#'   from `inst/requirements.txt`. Call it from the first page that needs
#'   `python`; later pages re-activate it with `reticulate::use_virtualenv()`.
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
#' @param packages Character vector of `python` package specifications, ideally
#'   pinned (e.g. `c("numpy==1.26.4", "scanpy==1.9.6")`). Takes precedence over
#'   `requirements`.
#' @param requirements Path to a `requirements.txt` file. When neither
#'   `packages` nor `requirements` is given, `inst/requirements.txt` is located
#'   by walking up from the working directory to the folder holding
#'   `_quarto.yml`, which works both while `quarto` renders a page and from a
#'   normal R session.
#' @param envname Name of the virtual environment to create or re-use.
#' @param python_version `python` version to install and build the environment
#'   against, e.g. `"3.12"`. If `NULL`, the `python` already on the machine is
#'   used, which is faster and is usually what you want.
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
    packages = NULL,
    requirements = NULL,
    envname = "BiocBook",
    python_version = NULL
) {

    rlang::check_installed("reticulate", "to execute `python` code in a BiocBook.")

    ## Resolve the requirements file. This has to work in two very different
    ## situations: while `quarto` renders a page (the package is typically not
    ## installed yet, and the working directory is the page's own folder), and
    ## from a normal R session.
    if (is.null(packages)) {
        if (is.null(requirements)) requirements <- .find_requirements()
        if (is.null(requirements) || !file.exists(requirements)) cli::cli_abort(c(
            "Could not find a `requirements.txt` file for this book.",
            "i" = "Create {.file inst/requirements.txt} listing the `python` packages \\
                   this book needs, one pinned specification per line.",
            "i" = "Or pass them directly, e.g. \\
                   {.code setup_python(packages = c('numpy==1.26.4'))}."
        ))
    }

    ## An environment may already have been provisioned, typically by the book's
    ## Docker image. Re-use it rather than rebuilding it.
    if (isTRUE(tryCatch(reticulate::virtualenv_exists(envname), error = function(e) FALSE))) {
        cli::cli_alert_success(cli::col_grey(
            "Re-using existing `python` environment {.val {envname}}"
        ))
        reticulate::use_virtualenv(envname, required = TRUE)
        return(invisible(envname))
    }

    if (!is.null(python_version)) {
        cli::cli_alert_info(cli::col_grey("Installing `python` {python_version}"))
        reticulate::install_python(version = python_version)
    }

    cli::cli_alert_info(cli::col_grey(
        "Creating `python` environment {.val {envname}}"
    ))
    args <- list(envname = envname)
    if (!is.null(python_version)) args$version <- python_version
    if (is.null(packages)) args$requirements <- requirements else args$packages <- packages
    do.call(reticulate::virtualenv_create, args)

    reticulate::use_virtualenv(envname, required = TRUE)
    invisible(envname)
}

## Walk up from the working directory looking for the book root, i.e. the
## folder holding `_quarto.yml`. During a `quarto` render the working directory
## is the page's own folder, so the book root is normally one level up.
.find_requirements <- function(start = getwd()) {
    dir <- normalizePath(start, mustWork = FALSE)
    for (i in seq_len(10L)) {
        if (file.exists(file.path(dir, "_quarto.yml"))) {
            req <- file.path(dir, "requirements.txt")
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
        req <- system.file("requirements.txt", package = desc[1, "Package"])
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
        "reticulate::use_virtualenv(\"BiocBook\", required = TRUE)"
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
        {.file inst/requirements.txt}."
    ))
    cli::cli_alert_info(cli::col_grey(
        "Keep at least one `R` chunk on the page: it is what keeps `quarto` on \\
        the `knitr` engine. A page with only `python` chunks uses `jupyter` \\
        instead, which the Bioconductor builders do not provide."
    ))
    if (rlang::is_interactive() && open) usethis::edit_file(full_path)

    invisible(book)
}
