#' @rdname BiocBook-python
#' @name BiocBook-python
#' @title Executing `python` code in a BiocBook
#'
#' @description
#'
#' `BiocBook` pages can execute `python` code as well as `R` code. The
#' difficulty is not making `python` run: `quarto` does that out of the box.
#' The difficulty is making sure the book still renders on machines that have
#' no `python`, the *Bioconductor Build System* (BBS) chief among them.
#'
#' Note that the `python.reticulate` option in `inst/assets/_knitr.yml` must be
#' `true` (its default) for this to work. When it is `false`, `knitr` runs every
#' `python` chunk as a separate `python -c ...` subprocess: no variable survives
#' from one chunk to the next, and `matplotlib` figures are silently dropped.
#'
#' The strategy implemented here is to **freeze** `python`-using pages. A frozen
#' page is executed only when its author asks for it; its computed output is
#' stored in `inst/_freeze/` and committed to the repository. Every subsequent
#' render (by the BBS, by a co-author, by a reader) replays that stored output
#' instead of re-executing the code, and therefore needs no `python` at all.
#'
#' - `add_python_chapter()`: add a new chapter set up to execute `python` code.
#' - `refresh_freeze()`: re-execute frozen pages and update `inst/_freeze/`.
#' - `check_freeze()`: check that every `python`-using page has an up-to-date
#'   frozen result, i.e. that the book will render without `python`.
#' - `setup_python()`: provision a `python` environment from `inst/requirements.txt`,
#'   for books that would rather execute their `python` at build time.
#'
#' `setup_python()` covers the other strategy: rather than freezing, provision
#' the `python` environment from within the book itself, so that the code really
#' does run on every build. Call it from a setup chunk of the first page that
#' needs `python`; later pages re-use the environment with
#' `reticulate::use_virtualenv()`. This is the approach taken by the
#' [OSTA](https://github.com/lmweber/OSTA) book. It keeps the output always
#' current, at the cost of making every build depend on `PyPI` being reachable
#' and on the environment resolving identically each time.
#'
#' @param book A `BiocBook` object.
#' @param title Title of the new chapter.
#' @param file Name of the new `.qmd` file. If `NA`, derived from `title`.
#' @param position Position of the new chapter in the book.
#' @param engine Which `quarto` engine the new page should use.
#'
#'   `"reticulate"` (the default) keeps the page on the `knitr` engine: it may
#'   mix `R` and `python` chunks sharing a single `python` session, and it
#'   keeps the book's house style, since `knitr` chunk options (`collapse`,
#'   `comment`, `fig.align`, set in `inst/assets/_knitr.yml`) and `code-link`
#'   only apply to `knitr` pages.
#'
#'   `"jupyter"` produces a `python`-only page executed by a Jupyter kernel.
#'   `quarto` binds the engine per file: a page holding at least one `R` chunk
#'   is always `knitr`, and a page with no `R` chunk at all is always
#'   `jupyter`. A `jupyter` page therefore cannot contain `R` code, silently
#'   loses `code-link`, and ignores the book's `knitr` chunk options.
#' @param open Whether to open the file for editing.
#' @param pages Character vector of pages to refresh, relative to `inst/`
#'   (e.g. `"pages/Chapter-4.qmd"`). If `NULL`, every frozen page is refreshed.
#' @param quiet Whether to suppress `quarto` output.
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
#'   against, e.g. `"3.12"`. If `NULL`, the `python` already available is used.
#'
#' @return
#'
#' `add_python_chapter()` and `refresh_freeze()` invisibly return `book`.
#' `setup_python()` invisibly returns the environment name.
#' `check_freeze()` invisibly returns a `tibble` describing each page that
#' executes `python`.
#'
#' @examples
#' \dontrun{
#' book <- BiocBook("path/to/book")
#' add_python_chapter(book, "Working with anndata")
#' refresh_freeze(book, "pages/working-with-anndata.qmd")
#' check_freeze(book)
#' }
NULL

.python_chunk_regex <- "^\\s*```+\\s*\\{python"

## Strip `<!-- ... -->` blocks so that python chunks shown *as documentation*
## are not mistaken for chunks that will actually be executed.
.drop_html_comments <- function(lines) {
    txt <- paste(lines, collapse = "\n")
    txt <- gsub("<!--.*?-->", "", txt, perl = TRUE)
    strsplit(txt, "\n", fixed = TRUE)[[1]]
}

.page_uses_python <- function(path) {
    if (!file.exists(path)) return(FALSE)
    lines <- .drop_html_comments(readLines(path, warn = FALSE))
    starts <- which(grepl(.python_chunk_regex, lines))
    if (!length(starts)) return(FALSE)
    fences <- which(grepl("^\\s*```", lines))
    for (start in starts) {
        ends <- fences[fences > start]
        end <- if (length(ends)) ends[1] else length(lines)
        opts <- lines[seq(start, end)]
        ## a chunk that is never evaluated needs neither python nor a freeze
        if (any(grepl("^\\s*#\\|\\s*eval:\\s*false\\s*$", opts))) next
        return(TRUE)
    }
    FALSE
}

.page_is_frozen <- function(path) {
    if (!file.exists(path)) return(FALSE)
    lines <- readLines(path, warn = FALSE)
    ## Only inspect the YAML front matter, i.e. up to the second `---`
    delim <- which(grepl("^---\\s*$", lines))
    if (length(delim) < 2 || delim[1] != 1) return(FALSE)
    yml <- lines[seq(delim[1] + 1, delim[2] - 1)]
    parsed <- tryCatch(yaml::yaml.load(paste(yml, collapse = "\n")), error = function(e) NULL)
    freeze <- parsed[["execute"]][["freeze"]]
    !is.null(freeze) && !identical(freeze, FALSE)
}

.freeze_result <- function(book, page) {
    stem <- tools::file_path_sans_ext(page)
    file.path(path(book), "inst", "_freeze", stem, "execute-results")
}

.book_pages <- function(book) {
    pages_dir <- file.path(path(book), "inst", "pages")
    files <- list.files(pages_dir, pattern = "\\.qmd$", full.names = TRUE, recursive = TRUE)
    index <- file.path(path(book), "inst", "index.qmd")
    if (file.exists(index)) files <- c(index, files)
    files
}

.page_id <- function(book, file) {
    inst <- normalizePath(file.path(path(book), "inst"), mustWork = FALSE)
    file <- normalizePath(file, mustWork = FALSE)
    gsub("\\\\", "/", substring(file, nchar(inst) + 2L))
}

#' @rdname BiocBook-python
#' @export

add_python_chapter <- function(
    book,
    title,
    file = NA,
    position = NULL,
    engine = c("reticulate", "jupyter"),
    open = TRUE
) {

    engine <- match.arg(engine)
    if (is.na(file)) file <- .sanitize_filename(title)

    body <- if (engine == "reticulate") {
        glue::glue(
            "---\n",
            "execute:\n",
            "  freeze: true\n",
            "---\n",
            "\n",
            "# {title}\n",
            "\n",
            "```{{r}}\n",
            "#| include: false\n",
            "library(reticulate)\n",
            "```\n",
            "\n",
            "```{{python}}\n",
            "print(\"Hello from python\")\n",
            "```\n"
        )
    } else {
        glue::glue(
            "---\n",
            "engine: jupyter\n",
            "execute:\n",
            "  freeze: true\n",
            "---\n",
            "\n",
            "# {title}\n",
            "\n",
            "```{{python}}\n",
            "print(\"Hello from python\")\n",
            "```\n"
        )
    }

    full_path <- .add_page(book, title, file, position, open = FALSE, body = body)

    cli::cli_alert_info(cli::col_grey(
        "This page is {.strong frozen}: its `python` code runs only when you call \\
        `refresh_freeze()`, and the results committed in `inst/_freeze/` are replayed \\
        everywhere else. Declare the `python` packages it needs in `inst/requirements.yml`."
    ))
    if (rlang::is_interactive() && open) usethis::edit_file(full_path)

    invisible(book)
}

#' @rdname BiocBook-python
#' @export

refresh_freeze <- function(book, pages = NULL, quiet = FALSE) {

    inst <- file.path(path(book), "inst")
    all_pages <- .book_pages(book)
    frozen <- all_pages[vapply(all_pages, .page_is_frozen, logical(1))]

    if (is.null(pages)) {
        targets <- frozen
    } else {
        targets <- file.path(inst, gsub("^inst[/\\\\]", "", pages))
        missing <- targets[!file.exists(targets)]
        if (length(missing)) cli::cli_abort(
            "Cannot find page{?s}: {.file {missing}}"
        )
    }

    if (!length(targets)) {
        cli::cli_alert_info("No frozen page found in this book. Nothing to refresh.")
        return(invisible(book))
    }

    cli::cli_alert_info(cli::col_grey(
        "Re-executing {length(targets)} frozen page{?s}. This requires `python` \\
        (and the packages listed in {.file inst/requirements.yml}) to be available."
    ))

    for (target in targets) {
        id <- .page_id(book, target)
        cli::cli_progress_message(cli::col_grey("{cli::pb_spin} Executing {id}"))
        ## A *single-file* render deliberately bypasses the freeze, executes the
        ## page, and rewrites its frozen result. A project-wide render would do
        ## the opposite: replay the existing freeze.
        quarto::quarto_render(input = target, quiet = quiet)
        cli::cli_alert_success(cli::col_grey("Refreshed {id}"))
    }

    cli::cli_alert_warning(cli::col_grey(
        "Remember to commit {.file inst/_freeze/} along with the page{?s} you \\
        just refreshed, otherwise the book will not render without `python`."
    ))

    invisible(book)
}

#' @rdname BiocBook-python
#' @export

check_freeze <- function(book) {

    all_pages <- .book_pages(book)
    py_pages <- all_pages[vapply(all_pages, .page_uses_python, logical(1))]

    if (!length(py_pages)) {
        cli::cli_alert_success("No page executes `python` code in this book.")
        return(invisible(tibble::tibble(
            page = character(0), frozen = logical(0),
            has_result = logical(0), up_to_date = logical(0)
        )))
    }

    res <- purrr::map_dfr(py_pages, function(page) {
        frozen <- .page_is_frozen(page)
        results_dir <- .freeze_result(book, .page_id(book, page))
        results <- list.files(results_dir, pattern = "\\.json$", full.names = TRUE)
        has_result <- length(results) > 0
        up_to_date <- has_result &&
            all(file.mtime(results) >= file.mtime(page))
        tibble::tibble(
            page = .page_id(book, page),
            frozen = frozen,
            has_result = has_result,
            up_to_date = up_to_date
        )
    })

    not_frozen <- res$page[!res$frozen]
    if (length(not_frozen)) {
        cli::cli_alert_danger(
            "{length(not_frozen)} page{?s} execute{?s/} `python` but {?is/are} not frozen:"
        )
        d <- cli::cli_div(theme = list(ul = list(`margin-left` = 2, before = "")))
        cli::cli_ul(not_frozen)
        cli::cli_end(d)
        cli::cli_alert_info(cli::col_grey(
            "Add `execute: freeze: true` to their YAML front matter, or the book will \\
            fail to render anywhere `python` is missing, including on the \\
            Bioconductor Build System."
        ))
    }

    no_result <- res$page[res$frozen & !res$has_result]
    if (length(no_result)) {
        cli::cli_alert_danger(
            "{length(no_result)} frozen page{?s} {?has/have} no committed result in `inst/_freeze/`:"
        )
        d <- cli::cli_div(theme = list(ul = list(`margin-left` = 2, before = "")))
        cli::cli_ul(no_result)
        cli::cli_end(d)
        cli::cli_alert_info(cli::col_grey("Run `refresh_freeze(book)` and commit `inst/_freeze/`."))
    }

    stale <- res$page[res$frozen & res$has_result & !res$up_to_date]
    if (length(stale)) {
        cli::cli_alert_warning(
            "{length(stale)} frozen page{?s} {?is/are} possibly stale (edited after last execution):"
        )
        d <- cli::cli_div(theme = list(ul = list(`margin-left` = 2, before = "")))
        cli::cli_ul(stale)
        cli::cli_end(d)
        cli::cli_alert_info(cli::col_grey(
            "A frozen page replays its stored output verbatim, so edits (including \\
            edits to prose) will not appear until you `refresh_freeze()` it."
        ))
    }

    if (!length(not_frozen) && !length(no_result) && !length(stale)) {
        cli::cli_alert_success(
            "All {nrow(res)} `python` page{?s} {?is/are} frozen and up to date."
        )
    }

    invisible(res)
}

#' @rdname BiocBook-python
#' @export

setup_python <- function(
    packages = NULL,
    requirements = NULL,
    envname = "BiocBook",
    python_version = NULL,
    quiet = FALSE
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

    if (!is.null(python_version)) {
        if (!quiet) cli::cli_alert_info(cli::col_grey(
            "Installing `python` {python_version}"
        ))
        reticulate::install_python(version = python_version)
    }

    existing <- tryCatch(
        reticulate::virtualenv_exists(envname), error = function(e) FALSE
    )
    if (!existing) {
        if (!quiet) cli::cli_alert_info(cli::col_grey(
            "Creating virtual environment {.val {envname}}"
        ))
        args <- list(envname = envname)
        if (!is.null(python_version)) args$python <- python_version
        if (is.null(packages)) args$requirements <- requirements
        else args$packages <- packages
        do.call(reticulate::virtualenv_create, args)
    } else if (!quiet) {
        cli::cli_alert_success(cli::col_grey(
            "Re-using existing virtual environment {.val {envname}}"
        ))
    }

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
