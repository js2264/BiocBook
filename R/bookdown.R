#' @rdname BiocBook-bookdown
#' @name BiocBook-bookdown
#' @title Converting a `bookdown` book to a BiocBook
#'
#' @description
#'
#' `from_bookdown()` converts a `bookdown` project into a `BiocBook` in place:
#' in its own folder and, when that folder is a git repository, on its current
#' branch, one commit per step. Run it on a branch of its own: the conversion
#' can then be reviewed step by step, and squashed or rebased onto the main
#' branch like any other change.
#'
#' 1. **Add the BiocBook template**: `DESCRIPTION`, the `Dockerfile`, the GitHub
#'    workflows, the `quarto` configuration (`inst/_quarto.yml`, `inst/assets/`)
#'    and `vignettes/Makefile`, filled in as `init()` does. The book keeps its
#'    `README.md`, which gets the BiocBook badges, and its `.gitignore` and
#'    `.Rbuildignore`, which get the entries of the template.
#' 2. **Move the pages to `inst/`**: `index.Rmd` becomes `inst/index.qmd`, and
#'    every chapter a page of `inst/pages/` that keeps its file name (e.g.
#'    `10-raw.Rmd` becomes `inst/pages/10-raw.qmd`). Asset folders (e.g.
#'    `img/`) move to `inst/pages/` too, so that the relative paths of the
#'    chapters keep working. Nothing else changes, so that git follows every
#'    file.
#' 3. **Move the book settings to `_book.yml` and `_format.yml`**: the title,
#'    authors and list of chapters, with their parts and appendices, go to
#'    `_book.yml`; the bibliographies, the CSS and the output options `quarto`
#'    has an equivalent for go to `_format.yml`. Bibliography files and CSS move
#'    to `inst/assets/`.
#' 4. **Remove the bookdown build**: `_bookdown.yml`, `_output.yml`, the
#'    rendered book (`output_dir`), the intermediate files of `bookdown`, and
#'    the `Makefile`, shell scripts or GitHub workflows that render the book
#'    with `bookdown`.
#' 5. **Use the BiocBook landing page**: the preamble of `index.Rmd` becomes the
#'    welcome part of the landing page of the template.
#' 6. **Rewrite cross-references** for `quarto` (see "Rewriting rules").
#' 7. **Turn questions and solutions into callouts**, in `msmbstyle` books.
#' 8. **Translate figure layout options**.
#' 9. **Set up every chapter as `index.Rmd` did**: `bookdown` runs every
#'    chapter in a single R session (unless `new_session: yes`), while `quarto`
#'    renders each chapter in its own. The `library()`, `require()` and
#'    `options()` calls of `index.Rmd` (and the `before_chapter_script`) are
#'    therefore repeated in a hidden chunk at the top of each chapter.
#' 10. **Describe the book in `DESCRIPTION`**: title, description and authors
#'    from the `index.Rmd` header, the packages the pages use in `Imports`, and
#'    the licence of the book: a Creative Commons licence its pages link to,
#'    its `LICENSE` file, or else the MIT licence of the template.
#'
#' A step with nothing to change is skipped. Rewrites leave code chunks, inline
#' code and the blank lines of the book alone.
#'
#' `bookdown`'s `\@ref()` renders a number only, while `quarto`'s `@`
#' references render their own "Figure", "Table" or "Chapter": a type word right
#' before a reference is therefore dropped, and other references render their
#' number only, as in `bookdown`.
#'
#' What still needs a human is listed in `MIGRATION.md`, at the root of the
#' book, which is not committed: downloads during the build, cached chunks,
#' `knitr::write_bib()` calls, unresolved cross-references, dependencies that
#' are not on CRAN or Bioconductor, output options with no `quarto`
#' equivalent, and files that are not part of a BiocBook. It also records how
#' often each rewriting rule was applied. Delete it once done.
#'
#' @section Rewriting rules:
#'
#' | `bookdown` / `msmbstyle` | `quarto` / `BiocBook` |
#' |---|---|
#' | `Figure \@ref(fig:x)` / `\@ref(fig:x)` | `@fig-x` / `[-@fig-x]`, chunk `x` labelled `fig-x` |
#' | `Table \@ref(tab:x)` / `\@ref(tab:x)` | `@tbl-x` / `[-@tbl-x]`, chunk `x` labelled `tbl-x` |
#' | `(\#eq:x)` / `Equation \@ref(eq:x)` / `\@ref(eq:x)` | `{#eq-x}` / `@eq-x` / `[-@eq-x]` |
#' | `Chapter \@ref(x)` / `\@ref(x)` | `@sec-x` / `[-@sec-x]`, `{#x}` renamed `{#sec-x}` |
#' | `# (PART) X {-}` | `part: "X"` in `_book.yml` |
#' | `# (APPENDIX) X {-}` | `appendices:` in `_book.yml` |
#' | `` `r msmbstyle::question_begin()` `` ... `question_end()` | a `.callout-note` titled "Question", in a `.callout-question` div |
#' | `` `r msmbstyle::solution_begin()` `` ... `solution_end()` | a collapsed `.callout-note` titled "Solution", in a `.callout-answer` div |
#' | chunk option `fig.margin=TRUE` | `#| column: margin` |
#' | chunk option `fig.fullwidth=TRUE` | `#| column: page` |
#' | output option `margin_references: TRUE` | `citation-location: margin` |
#'
#' @param path Path to the `bookdown` project, i.e. the folder holding
#'   `index.Rmd` (and optionally `_bookdown.yml`). When it is in a git
#'   repository, it must be the root of the repository.
#' @param package Name of the book package. Defaults to the `book_filename` of
#'   `_bookdown.yml`, else to the name of the folder.
#' @param user GitHub user or organization of the book, used in the links of
#'   the template and in the name of its `Docker` image. Defaults to the owner
#'   of the `origin` remote of the repository, when it is on GitHub, else to the
#'   user of the GitHub token.
#' @param style Syntax to convert: `"bookdown"`, `"msmbstyle"` (`bookdown`
#'   with the `msmbstyle` question and solution blocks), or `"auto"` to detect
#'   it from the output format and the pages.
#' @param appendices Optional. Chapters to list as appendices of the new book
#'   (file names, with or without extension), in addition to those following a
#'   `# (APPENDIX)` header.
#' @param commit Whether to commit each step, when `path` is a git repository.
#'   Its working tree must then have no uncommitted changes.
#' @param skip_availability Whether to skip checking that the dependencies of
#'   the book are on CRAN or Bioconductor, which requires network access.
#'
#' @return A `BiocBook` object, invisibly.
#'
#' @seealso \code{\link{BiocBook}}
#'
#' @examples
#' ## A minimal bookdown project, in a git repository
#' bd <- file.path(tempdir(), "bookdown-example")
#' dir.create(bd)
#' writeLines(c(
#'     "---", "title: My bookdown book", "author: Jane Doe", "---", "",
#'     "# Preface {-}", "", "See chapter \\@ref(intro)."
#' ), file.path(bd, "index.Rmd"))
#' writeLines(
#'     c("# Introduction {#intro}", "", "Some text."),
#'     file.path(bd, "01-intro.Rmd")
#' )
#' gert::git_init(bd)
#' gert::git_config_set("user.name", "Jane Doe", repo = bd)
#' gert::git_config_set("user.email", "jane@example.com", repo = bd)
#' gert::git_add(".", repo = bd)
#' gert::git_commit("A bookdown book", repo = bd)
#'
#' ## Converted in place, one commit per step
#' bb <- from_bookdown(bd, package = "myBook", user = "jane", skip_availability = TRUE)
#' chapters(bb)
#' gert::git_log(repo = bd)$message
#' unlink(bd, recursive = TRUE)
NULL

#' @rdname BiocBook-bookdown
#' @export

from_bookdown <- function(
    path = ".",
    package = NULL,
    user = NULL,
    style = c("auto", "bookdown", "msmbstyle"),
    appendices = NULL,
    commit = TRUE,
    skip_availability = FALSE
) {

    style <- match.arg(style)

    ## Read the bookdown project, and check everything, before changing anything
    if (file.exists(file.path(path, "inst", "_quarto.yml"))) cli::cli_abort(
        "{.file {normalizePath(path)}} is already a {.pkg BiocBook}."
    )
    bd <- .read_bookdown(path)
    if (identical(style, "auto")) style <- .bookdown_style(bd)
    repo <- .bookdown_repo(bd$path, commit)
    mig <- .bookdown_migration(
        bd, style, appendices,
        package = .bookdown_package(package, bd),
        user = .bookdown_user(user, bd$path),
        repo = repo,
        skip_availability = skip_availability
    )
    on.exit(unlink(dirname(mig$template), recursive = TRUE), add = TRUE)

    cli::cli_text("")
    cli::cat_rule("Converting the `bookdown` book", col = "cyan", line = 2)
    steps <- list(
        .bookdown_template_step, .bookdown_move_step, .bookdown_settings_step,
        .bookdown_cleanup_step, .bookdown_landing_step, .bookdown_crossref_step,
        .bookdown_callout_step, .bookdown_figure_step, .bookdown_setup_step,
        .bookdown_description_step
    )
    for (step in steps) .bookdown_step(mig, step)
    .write_migration_report(mig)

    cli::cli_text("")
    cli::cat_rule("Results", col = "cyan", line = 2)
    if (is.null(repo)) {
        cli::cli_alert_success("Converted {.file {bd$path}} to a BiocBook")
    } else {
        n <- length(mig$commits)
        branch <- gert::git_branch(repo = repo)
        cli::cli_alert_success(
            "Converted {.file {bd$path}} to a BiocBook, in {n} commit{?s} on {.val {branch}}"
        )
    }
    cli::cli_alert_info(cli::col_grey(
        "Review {.file MIGRATION.md}: it lists what still needs to be done by hand, and is not committed."
    ))
    cli::cli_text("")

    invisible(BiocBook(bd$path))
}

## ---------------------------------------------------------------------------
## Reading the bookdown project
## ---------------------------------------------------------------------------

.read_bookdown <- function(path) {

    path <- normalizePath(path, mustWork = FALSE)
    if (!dir.exists(path)) cli::cli_abort("{.file {path}} is not a folder.")
    config_f <- file.path(path, "_bookdown.yml")
    config <- if (file.exists(config_f)) .read_yaml_file(config_f) else list()

    ## Same order as bookdown: `rmd_files` if given, otherwise every .Rmd file
    ## not starting with `_`, alphabetically, `index.Rmd` first
    files <- config[["rmd_files"]]
    if (is.list(files)) files <- if (is.null(files[["html"]])) unlist(files) else files[["html"]]
    if (is.null(files)) {
        files <- list.files(path, pattern = "\\.Rmd$", ignore.case = TRUE)
        files <- sort(files[!startsWith(files, "_")])
    }
    files <- unlist(files)
    is_index <- tolower(basename(files)) == "index.rmd"
    if (!any(is_index)) cli::cli_abort(c(
        "{.file {path}} is not a {.pkg bookdown} project.",
        "i" = "A {.pkg bookdown} project has an {.file index.Rmd} file."
    ))
    files <- c(files[is_index][1], files[!is_index])
    missing <- files[!file.exists(file.path(path, files))]
    if (length(missing)) cli::cli_abort(
        "{.file _bookdown.yml} lists files that do not exist: {.file {missing}}"
    )

    pages <- lapply(files, function(f) readLines(file.path(path, f), warn = FALSE, encoding = "UTF-8"))
    names(pages) <- files
    index <- .split_front_matter(pages[[1]])
    pages[[1]] <- index$body

    ## Output options: from the `index.Rmd` header, else from `_output.yml`
    output <- index$yaml[["output"]]
    output_f <- file.path(path, "_output.yml")
    if (is.null(output) && file.exists(output_f)) output <- .read_yaml_file(output_f)
    if (is.character(output)) output <- structure(rep(list(list()), length(output)), names = output)
    html <- grep("html|gitbook|bs4_book|tufte", names(output), value = TRUE)[1]

    list(
        path = path,
        config = config,
        meta = index$yaml,
        format = if (is.na(html)) NA_character_ else html,
        options = if (is.na(html) || !is.list(output[[html]])) list() else output[[html]],
        pages = pages
    )
}

.read_yaml_file <- function(f) {
    res <- yaml::read_yaml(f, readLines.warn = FALSE)
    if (is.null(res)) list() else res
}

.split_front_matter <- function(lines) {
    if (length(lines) && grepl("^---\\s*$", lines[1])) {
        end <- which(grepl("^(---|\\.\\.\\.)\\s*$", lines))
        end <- end[end > 1][1]
        if (!is.na(end)) {
            yml <- if (end > 2) yaml::yaml.load(paste(lines[2:(end - 1)], collapse = "\n")) else NULL
            return(list(yaml = if (is.null(yml)) list() else yml, body = lines[-seq_len(end)]))
        }
    }
    list(yaml = list(), body = lines)
}

.bookdown_style <- function(bd) {
    if (isTRUE(startsWith(bd$format, "msmbstyle::"))) return("msmbstyle")
    qa <- vapply(bd$pages, function(p) any(grepl("(question|solution)_(begin|end)\\(", p)), logical(1))
    if (any(qa)) "msmbstyle" else "bookdown"
}

## The name bookdown gives the book: its `book_filename`, without extension
.bookdown_main_name <- function(bd) {
    name <- bd$config[["book_filename"]]
    if (is.null(name)) "_main" else sub("\\.[Rr]?md$", "", name)
}

.bookdown_output_dir <- function(bd) {
    out <- bd$config[["output_dir"]]
    if (is.null(out)) "_book" else sub("/+$", "", sub("^\\./", "", out))
}

## ---------------------------------------------------------------------------
## Rewriting rules, kept as data so that adding a style is adding a list.
## Each rule is applied outside code chunks, to whole lines (`scope = "line"`)
## or else to the text outside inline code; `name` is how the migration report
## refers to it.
## ---------------------------------------------------------------------------

.bookdown_rules <- list(
    ## bookdown's `\@ref()` renders a number, quarto's `@ref` renders its own
    ## "Figure", "Table", "Chapter"... A type word before the reference is
    ## therefore dropped, and a reference without one renders its number only
    ## (`[-@ref]`), as it did in bookdown.
    bookdown = list(
        list(name = "`Figure \\@ref(fig:x)` -> `@fig-x`", perl = TRUE,
            pattern = "\\b[Ff]ig(?:ure)?s?\\.?(?:\\s|~|\u00a0)+\\\\@ref\\(fig:([^)]+)\\)",
            replacement = "@fig-\\1"),
        list(name = "`\\@ref(fig:x)` -> `[-@fig-x]`",
            pattern = "\\\\@ref\\(fig:([^)]+)\\)", replacement = "[-@fig-\\1]"),
        list(name = "`Table \\@ref(tab:x)` -> `@tbl-x`", perl = TRUE,
            pattern = "\\b[Tt]ab(?:le)?s?\\.?(?:\\s|~|\u00a0)+\\\\@ref\\(tab:([^)]+)\\)",
            replacement = "@tbl-\\1"),
        list(name = "`\\@ref(tab:x)` -> `[-@tbl-x]`",
            pattern = "\\\\@ref\\(tab:([^)]+)\\)", replacement = "[-@tbl-\\1]"),
        list(name = "`Equation \\@ref(eq:x)` -> `@eq-x`", perl = TRUE,
            pattern = "\\b[Ee]q(?:uation)?s?\\.?(?:\\s|~|\u00a0)+\\(?\\\\@ref\\(eq:([^)]+)\\)\\)?",
            replacement = "@eq-\\1"),
        list(name = "`\\@ref(eq:x)` -> `[-@eq-x]`",
            pattern = "\\\\@ref\\(eq:([^)]+)\\)", replacement = "[-@eq-\\1]"),
        list(name = "`Chapter \\@ref(x)` -> `@sec-x`", perl = TRUE,
            pattern = "\\b(?:[Cc]hapters?|[Ss]ections?|[Ss]ecs?\\.|[Aa]ppendix|[Aa]ppendices)(?:\\s|~|\u00a0)+\\\\@ref\\((?:sec-)?([^):]+)\\)",
            replacement = "@sec-\\1"),
        list(name = "`\\@ref(x)` -> `[-@sec-x]`",
            pattern = "\\\\@ref\\((?:sec-)?([^):]+)\\)", replacement = "[-@sec-\\1]", perl = TRUE)
    ),
    ## Quarto drops extra classes from callouts, so the BiocBook styling of
    ## questions and answers targets a div wrapped around them
    msmbstyle = list(
        list(name = "`question_begin()` -> question callout", scope = "line",
            pattern = "^\\s*`r\\s+(msmbstyle::)?question_begin\\([^`]*\\)`\\s*$",
            replacement = "\n:::: {.callout-question}\n::: {.callout-note}\n\n#### Question\n"),
        list(name = "`question_end()` -> end of callout", scope = "line",
            pattern = "^\\s*`r\\s+(msmbstyle::)?question_end\\([^`]*\\)`\\s*$",
            replacement = "\n:::\n::::\n"),
        list(name = "`solution_begin()` -> collapsed answer callout", scope = "line",
            pattern = "^\\s*`r\\s+(msmbstyle::)?solution_begin\\([^`]*\\)`\\s*$",
            replacement = "\n:::: {.callout-answer}\n::: {.callout-note collapse=\"true\"}\n\n#### Solution\n"),
        list(name = "`solution_end()` -> end of callout", scope = "line",
            pattern = "^\\s*`r\\s+(msmbstyle::)?solution_end\\([^`]*\\)`\\s*$",
            replacement = "\n:::\n::::\n")
    )
)

## Chunk options with a quarto equivalent: removed from the chunk header, and
## replaced by `#|` lines at the top of the chunk
.bookdown_chunk_rules <- list(
    list(name = "`fig.margin=TRUE` -> `#| column: margin`",
        option = "fig\\.margin\\s*=\\s*(TRUE|T)", add = "#| column: margin"),
    list(name = "`fig.margin=FALSE` removed",
        option = "fig\\.margin\\s*=\\s*(FALSE|F)", add = NULL),
    list(name = "`fig.fullwidth=TRUE` -> `#| column: page`",
        option = "fig\\.fullwidth\\s*=\\s*(TRUE|T)", add = "#| column: page"),
    list(name = "`fig.fullwidth=FALSE` removed",
        option = "fig\\.fullwidth\\s*=\\s*(FALSE|F)", add = NULL)
)

## Packages a converted book does not depend on
.bookdown_not_deps <- c(
    "bookdown", "msmbstyle", "rmarkdown", "knitr", "BiocBook",
    "base", "compiler", "datasets", "graphics", "grDevices", "grid", "methods",
    "parallel", "splines", "stats", "stats4", "tcltk", "tools", "utils"
)

## The licences of R's licence database a Creative Commons link can name
.bookdown_cc_licences <- c(
    "CC BY 4.0", "CC BY-SA 4.0", "CC BY-NC 4.0", "CC BY-NC-SA 4.0", "CC BY-NC-ND 4.0"
)

## ---------------------------------------------------------------------------
## Before converting: the repository, the package, the user
## ---------------------------------------------------------------------------

## The git repository whose current branch receives the conversion, or NULL
## when nothing is committed
.bookdown_repo <- function(path, commit) {
    ## The root of the working tree (`git_find()` gives the git folder of a
    ## linked worktree instead)
    root <- tryCatch(normalizePath(gert::git_info(repo = path)$path), error = function(e) NULL)
    if (is.null(root)) {
        if (commit) cli::cli_alert_info(cli::col_grey(
            "{.file {path}} is not a git repository: the conversion is not committed."
        ))
        return(NULL)
    }
    if (!identical(root, path)) cli::cli_abort(c(
        "{.file {path}} is a subfolder of the git repository {.file {root}}.",
        "i" = "A BiocBook is the root of its repository: convert a bookdown project that is."
    ))
    if (!commit) return(NULL)
    status <- gert::git_status(repo = root)
    dirty <- status$file[!(status$status == "new" & !status$staged)]
    if (length(dirty)) cli::cli_abort(c(
        "{.file {path}} has uncommitted changes: {.file {dirty}}.",
        "i" = "Commit or stash them first: each step of the conversion is committed."
    ))
    tryCatch(gert::git_signature_default(repo = root), error = function(e) cli::cli_abort(c(
        "Git has no user name or email to commit with.",
        "i" = "Set them with {.code gert::git_config_global_set('user.name', '...')} and {.code gert::git_config_global_set('user.email', '...')}."
    )))
    root
}

.bookdown_package <- function(package, bd) {
    if (is.null(package)) {
        package <- .bookdown_main_name(bd)
        if (!.is_package_name(package)) package <- basename(bd$path)
    }
    if (!.is_package_name(package)) cli::cli_abort(c(
        "{.val {package}} is not a valid package name.",
        "i" = "Name the book package with {.arg package}."
    ))
    package
}

.is_package_name <- function(x) {
    is.character(x) && length(x) == 1L && grepl("^[[:alpha:]][[:alnum:].]*[[:alnum:]]$", x)
}

## The owner of the `origin` remote on GitHub, else the user of the token
.bookdown_user <- function(user, path) {
    if (!is.null(user)) return(user)
    remotes <- tryCatch(gert::git_remote_list(repo = path), error = function(e) NULL)
    url <- if (is.null(remotes)) NA_character_ else remotes$url[match("origin", remotes$name)]
    if (!is.na(url)) {
        url <- sub("^(https?://)[^/@]*@", "\\1", url)
        pattern <- "^(https?://|ssh://)?(git@)?(www\\.)?github\\.com[:/]+([^/]+)/.*$"
        if (grepl(pattern, url)) return(sub(pattern, "\\4", url))
    }
    login <- tryCatch(gh::gh_whoami()$login, error = function(e) NULL)
    if (!is.null(login)) return(login)
    cli::cli_abort(c(
        "Cannot tell the GitHub user or organization of the book.",
        "i" = "Give it with {.arg user}."
    ))
}

.bookdown_template <- function() {
    tmp <- tempfile("BiocBook.template")
    dir.create(tmp)
    utils::untar(
        system.file("template", "BiocBook.template.tar.gz", package = "BiocBook"),
        exdir = tmp
    )
    file.path(tmp, "BiocBook.template")
}

## Everything the steps need to know about the project, read before any of
## them changes it
.bookdown_migration <- function(bd, style, appendices, package, user, repo, skip_availability) {

    mig <- new.env(parent = emptyenv())
    mig$bd <- bd
    mig$path <- root <- bd$path
    mig$style <- style
    mig$package <- package
    mig$user <- user
    mig$repo <- repo
    mig$skip_availability <- skip_availability
    mig$template <- .bookdown_template()
    mig$counts <- structure(integer(0), names = character(0))
    mig$todo <- list()
    mig$commits <- character(0)
    mig$changed <- character(0)
    mig$deps <- character(0)
    mig$setup <- character(0)

    ## Pages: `index.Rmd` goes to `inst/`, the chapters to `inst/pages/`
    files <- names(bd$pages)
    stems <- tools::file_path_sans_ext(basename(files))
    mig$files <- files
    mig$targets <- c("index.qmd", file.path("pages", paste0(stems[-1], ".qmd")))
    mig$pages <- file.path("inst", mig$targets)

    ## Asset folders, which go next to the chapters, bibliographies and CSS
    skip <- "^(docs|lib|_book|_bookdown_files|_freeze|renv|packrat|rsconnect|libs|site_libs|inst|vignettes)$|_(cache|files)$|^\\."
    dirs <- list.dirs(root, full.names = FALSE, recursive = FALSE)
    mig$dirs <- dirs[!grepl(skip, dirs) & dirs != .bookdown_output_dir(bd)]
    mig$bibs <- list.files(root, pattern = "\\.bib$")
    css <- as.character(unlist(bd$options[["css"]]))
    mig$css <- css[file.exists(file.path(root, css))]

    ## What the project already has: .gitignore, DESCRIPTION, LICENSE, the
    ## files that build it with bookdown and its `before_chapter_script`
    gitignore <- file.path(root, ".gitignore")
    mig$gitignore <- if (file.exists(gitignore)) .read_page(gitignore) else character(0)
    mig$old_deps <- NULL
    mig$old_remotes <- character(0)
    if (file.exists(file.path(root, "DESCRIPTION"))) {
        d <- desc::desc(file = file.path(root, "DESCRIPTION"))
        deps <- d$get_deps()
        mig$old_deps <- deps[deps$package != "R", , drop = FALSE]
        mig$old_remotes <- d$get_remotes()
    }
    mig$licence_file <- file.exists(file.path(root, "LICENSE"))
    mig$build <- .bookdown_build_files(root)
    scripts <- unlist(bd$config[["before_chapter_script"]])
    mig$scripts <- scripts[file.exists(file.path(root, scripts))]
    mig$script_lines <- unlist(lapply(mig$scripts, function(s) {
        readLines(file.path(root, s), warn = FALSE, encoding = "UTF-8")
    }))

    ## Book structure: `(PART)` and `(APPENDIX)` headers
    part_of <- rep(NA_character_, length(files))
    appendix <- rep(FALSE, length(files))
    current_part <- NA_character_
    in_appendix <- FALSE
    for (i in seq_along(bd$pages)) {
        lines <- bd$pages[[i]]
        code <- .code_lines(lines)
        is_part <- !code & grepl("^#\\s+\\(PART\\*?\\)", lines)
        is_appx <- !code & grepl("^#\\s+\\(APPENDIX\\)", lines)
        if (any(is_part)) {
            current_part <- .strip_header(sub("^#\\s+\\(PART\\*?\\)\\s*", "", lines[which(is_part)[1]]))
        }
        if (any(is_appx)) in_appendix <- TRUE
        if (i > 1) {
            part_of[i] <- if (in_appendix) NA_character_ else current_part
            appendix[i] <- in_appendix
        }
    }
    if (length(appendices)) {
        asked <- tools::file_path_sans_ext(basename(appendices))
        unknown <- setdiff(asked, stems[-1])
        if (length(unknown)) cli::cli_warn("Unknown {.arg appendices}: {.file {unknown}}")
        appendix[stems %in% asked & seq_along(stems) > 1] <- TRUE
        part_of[appendix] <- NA_character_
    }
    mig$part_of <- part_of
    mig$appendix <- appendix

    ## Labels referenced across the book
    text <- unlist(lapply(bd$pages, function(l) l[!.code_lines(l)]))
    text <- gsub("`+[^`]*`+", "", text)
    refs <- function(prefix) {
        m <- regmatches(text, gregexpr(sprintf("\\\\@ref\\(%s:([^)]+)\\)", prefix), text))
        unique(sub(sprintf("^\\\\@ref\\(%s:(.*)\\)$", prefix), "\\1", unlist(m)))
    }
    mig$fig_refs <- refs("fig")
    mig$tab_refs <- refs("tab")
    sec_refs <- unlist(regmatches(text, gregexpr("\\\\@ref\\(([^):]+)\\)", text)))
    mig$sec_refs <- unique(sub("^\\\\@ref\\((.*)\\)$", "\\1", sec_refs))
    mig$sec_rename <- mig$sec_refs[!startsWith(mig$sec_refs, "sec-")]

    mig
}

## A `Makefile`, shell scripts and GitHub workflows that render the book with
## bookdown
.bookdown_build_files <- function(root) {
    workflows <- list.files(file.path(root, ".github", "workflows"), pattern = "\\.ya?ml$")
    files <- c(
        list.files(root, pattern = "^(Makefile|.*\\.sh)$"),
        if (length(workflows)) file.path(".github", "workflows", workflows)
    )
    files[vapply(files, function(f) {
        any(grepl("render_book|bookdown::", readLines(file.path(root, f), warn = FALSE)))
    }, logical(1))]
}

## ---------------------------------------------------------------------------
## Steps: each changes the book in place, records the files it changed, and
## returns its commit message (or NULL when it has nothing to do)
## ---------------------------------------------------------------------------

.bookdown_step <- function(mig, step) {
    mig$changed <- character(0)
    msg <- step(mig)
    if (is.null(msg) || !length(mig$changed)) return(invisible(FALSE))
    cli::cli_alert_success(cli::col_grey(msg[1]))
    if (!is.null(mig$repo)) .bookdown_commit(mig, c(msg, "", sprintf(
        "Done by BiocBook::from_bookdown() (BiocBook %s).", utils::packageVersion("BiocBook")
    )))
    invisible(TRUE)
}

.bookdown_commit <- function(mig, message) {
    paths <- unique(mig$changed)
    here <- file.exists(file.path(mig$path, paths))
    tracked <- gert::git_ls(repo = mig$repo)$path
    gone <- paths[!here & paths %in% tracked]
    if (length(gone)) gert::git_rm(gone, repo = mig$repo)
    if (any(here)) gert::git_add(paths[here], force = TRUE, repo = mig$repo)
    if (!nrow(gert::git_status(staged = TRUE, repo = mig$repo))) return(invisible(NULL))
    sig <- gert::git_signature_default(repo = mig$repo)
    sha <- gert::git_commit(
        paste(message, collapse = "\n"), author = sig, committer = sig, repo = mig$repo
    )
    mig$commits <- c(mig$commits, sha)
    invisible(sha)
}

## 1. The files every BiocBook has, filled in as `init()` does
.bookdown_template_step <- function(mig) {

    tpl <- mig$template
    root <- mig$path
    files <- list.files(tpl, recursive = TRUE, all.files = TRUE, no.. = TRUE)
    own <- setdiff(files, c(
        "README.md", ".gitignore", ".Rbuildignore", "LICENSE", "LICENSE.md", "inst/index.qmd"
    ))
    replaced <- own[file.exists(file.path(root, own))]
    for (f in own) .bd_copy(mig, file.path(tpl, f), f)
    for (f in intersect(c("inst/assets/_book.yml", "DESCRIPTION", "inst/requirements.yml"), own)) {
        .fix_placeholders(file.path(root, f), pkg = mig$package, usr = mig$user)
    }

    ## README.md: the BiocBook badges, above the README of the book
    badges <- .bookdown_fill(readLines(file.path(tpl, "README.md"), warn = FALSE, encoding = "UTF-8"), mig)
    if (!file.exists(file.path(root, "README.md"))) {
        .bd_write(mig, badges, "README.md")
    } else {
        lines <- .read_page(file.path(root, "README.md"))
        at <- grep("^<!-- badges: start -->", lines)[1]
        lines <- if (is.na(at)) c(badges, "", lines) else append(lines, badges[-c(1, length(badges))], after = at)
        .bd_write(mig, lines, "README.md")
    }

    ## .gitignore and .Rbuildignore: the entries of the template the book misses
    for (f in c(".gitignore", ".Rbuildignore")) {
        new <- readLines(file.path(tpl, f), warn = FALSE, encoding = "UTF-8")
        old <- if (file.exists(file.path(root, f))) .read_page(file.path(root, f)) else character(0)
        if (length(setdiff(new, old))) .bd_write(mig, c(old, setdiff(new, old)), f)
    }

    for (f in replaced) .bd_todo(mig, "Replaced files", sprintf(
        "`%s` was replaced by the one of the template: check whether it held something the book needs", f
    ))
    version <- desc::desc_get_field("BiocBookTemplate", default = "", file = file.path(tpl, "DESCRIPTION"))
    .commit_message(
        "Add the BiocBook template",
        sprintf(paste(
            "The package files every BiocBook has, from the BiocBook template%s, filled in",
            "as BiocBook::init() does: DESCRIPTION, the Dockerfile, the GitHub workflows,",
            "the quarto configuration (inst/_quarto.yml and inst/assets/) and",
            "vignettes/Makefile, which renders the book when the package is built."
        ), if (nzchar(version)) sprintf(" (%s)", version) else ""),
        "README.md gets the BiocBook badges, .gitignore and .Rbuildignore the entries of the template."
    )
}

## 2. The pages, and the folders next to them, move as they are
.bookdown_move_step <- function(mig) {
    for (i in seq_along(mig$files)) .bd_move(mig, mig$files[i], mig$pages[i])
    for (d in mig$dirs) .bd_move(mig, d, file.path("inst", "pages", d))
    .commit_message(
        "Move the pages to inst/",
        sprintf(
            "%s becomes inst/index.qmd, and each chapter a page of inst/pages/ that keeps its file name.",
            mig$files[1]
        ),
        if (length(mig$dirs)) sprintf(
            "%s %s next to the chapters, so that their relative paths keep working.",
            .and(paste0(mig$dirs, "/")), if (length(mig$dirs) == 1L) "moves" else "move"
        )
    )
}

## 3. The header of `index.Rmd`, and the `(PART)` and `(APPENDIX)` headers,
## become the configuration of the book
.bookdown_settings_step <- function(mig) {

    root <- mig$path
    bd <- mig$bd

    ## The header of `index.Rmd`
    index <- .read_page(file.path(root, mig$pages[1]))
    body <- .split_front_matter(index)$body
    if (length(body) < length(index)) {
        .bd_write(mig, .tidy_touched(body, seq_along(body) == 1L), mig$pages[1])
    }

    ## `(PART)` and `(APPENDIX)` headers
    for (p in mig$pages) {
        lines <- .read_page(file.path(root, p))
        code <- .code_lines(lines)
        is_part <- !code & grepl("^#\\s+\\(PART\\*?\\)", lines)
        is_appx <- !code & grepl("^#\\s+\\(APPENDIX\\)", lines)
        .bd_count(mig, "`# (PART) X {-}` -> `part:` in `_book.yml`", sum(is_part))
        .bd_count(mig, "`# (APPENDIX) X {-}` -> `appendices:` in `_book.yml`", sum(is_appx))
        if (any(is_part | is_appx)) .bd_write(mig, .drop_lines(lines, is_part | is_appx), p)
    }

    ## _book.yml, and _format.yml with the bibliographies and the CSS
    assets <- file.path("inst", "assets")
    .write_book_yml(
        file.path(root, assets, "_book.yml"), bd$meta, mig$targets, mig$part_of, mig$appendix
    )
    .bd_mark(mig, file.path(assets, "_book.yml"))
    for (b in mig$bibs) .bd_move(mig, b, file.path(assets, b))
    for (css in mig$css) {
        ## A CSS file in a folder has moved to inst/pages/ with it
        from <- if (dirname(css) %in% c(".", "")) css else file.path("inst", "pages", css)
        .bd_move(mig, from, file.path(assets, basename(css)))
    }
    dirs <- structure(mig$dirs, bibs = mig$bibs, css = basename(mig$css))
    for (item in .write_format_yml(file.path(root, assets, "_format.yml"), bd, dirs)) {
        .bd_todo(mig, "Unknown options", item)
    }
    .bd_mark(mig, file.path(assets, "_format.yml"))
    if (length(bd$config[["language"]])) .bd_todo(
        mig, "Unknown options",
        "`language` (`_bookdown.yml`): translate the labels with quarto's `lang`/`language` options"
    )

    options <- c(
        if (isTRUE(bd$options[["margin_references"]])) "margin references",
        if (!is.null(bd$options[["toc_depth"]])) "toc depth",
        if (isTRUE(bd$meta[["link-citations"]]) || identical(bd$meta[["link-citations"]], "yes")) "linked citations"
    )
    moved <- c(mig$bibs, basename(mig$css))
    .commit_message(
        "Move the book settings to _book.yml and _format.yml",
        paste0(
            "quarto reads them from the configuration of the book rather than from the header of ",
            mig$files[1], ": its title, authors and list of chapters",
            if (any(!is.na(mig$part_of)) || any(mig$appendix)) ", with its parts and appendices,",
            " go to _book.yml; its bibliographies, CSS and the output options quarto has an equivalent for",
            if (length(options)) sprintf(" (%s)", .and(options)),
            " go to _format.yml."
        ),
        if (length(moved)) sprintf(
            "%s %s to inst/assets/.", .and(moved), if (length(moved) == 1L) "moves" else "move"
        )
    )
}

## 4. What built the book with bookdown
.bookdown_cleanup_step <- function(mig) {

    root <- mig$path
    out <- .bookdown_output_dir(mig$bd)
    name <- unique(c("_main", .bookdown_main_name(mig$bd)))
    made <- unique(c(
        "_bookdown.yml", "_output.yml", out, "_bookdown_files",
        as.vector(outer(name, c(".Rmd", ".md", "_files", "_cache"), paste0))
    ))
    gone <- c(made[file.exists(file.path(root, made))], mig$build)
    folders <- gone[dir.exists(file.path(root, gone))]
    for (g in gone) .bd_remove(mig, g)

    ## Their entries in .gitignore: those of the template stay
    dropped <- FALSE
    if (length(mig$gitignore)) {
        lines <- .read_page(file.path(root, ".gitignore"))
        template <- readLines(file.path(mig$template, ".gitignore"), warn = FALSE, encoding = "UTF-8")
        entry <- gsub("^/+|/+$", "", trimws(lines))
        drop <- seq_along(lines) <= length(mig$gitignore) & entry %in% made & !lines %in% template
        if (any(drop)) {
            .bd_write(mig, lines[!drop], ".gitignore")
            dropped <- TRUE
        }
    }
    if (!length(mig$changed)) return(NULL)
    if (out %in% gone) .bd_todo(mig, "GitHub Pages", sprintf(paste(
        "`%s/` held the rendered book: once the `biocbook` workflow has deployed it,",
        "serve GitHub Pages from the `gh-pages` branch"
    ), out))

    removed <- paste0(gone, ifelse(gone %in% folders, "/", ""))
    .commit_message(
        "Remove the bookdown build",
        paste(
            "BiocBook builds the book from vignettes/Makefile, and deploys it to the gh-pages",
            "branch: the bookdown configuration, its build and the book it rendered go."
        ),
        sprintf(
            "Removed: %s%s.", .and(removed), if (dropped) ", and their entries in .gitignore" else ""
        )
    )
}

## 5. The landing page of the template, around the preamble of the book
.bookdown_landing_step <- function(mig) {
    index <- file.path(mig$path, mig$pages[1])
    body <- .index_body(.read_page(index), mig$dirs)
    n <- attr(body, "n")
    .bd_count(mig, "asset paths of `index.Rmd` -> `pages/...`", n)
    file.copy(file.path(mig$template, "inst", "index.qmd"), index, overwrite = TRUE)
    .fix_placeholders(index, pkg = mig$package, usr = mig$user)
    .write_index(index, body)
    .bd_mark(mig, mig$pages[1])
    .commit_message(
        "Use the BiocBook landing page",
        paste0(
            "The preamble of the book becomes the welcome part of the landing page of the ",
            "template, which shows the package, its version and its licence at the top, and ",
            "how to run the Docker image of the book at the bottom. Its headers are not numbered",
            if (n) ", and the paths of its images start with pages/, where their folders moved",
            "."
        )
    )
}

## 6. Cross-references, equation labels and the chunk labels they point to
.bookdown_crossref_step <- function(mig) {

    root <- mig$path
    before <- mig$counts
    for (p in mig$pages) {
        lines <- .read_page(file.path(root, p))
        code <- .code_lines(lines)
        new <- lines
        for (rule in .bookdown_rules[["bookdown"]]) {
            hits <- which(!code & grepl(rule$pattern, new, perl = isTRUE(rule$perl)))
            for (h in hits) {
                res <- .rewrite_text(new[h], rule)
                new[h] <- res$line
                .bd_count(mig, rule$name, res$n)
            }
        }
        ## Header ids referenced as sections, e.g. `{#intro}` -> `{#sec-intro}`
        for (id in mig$sec_rename) {
            pattern <- sprintf("(^#+ .*\\{[^}]*)#%s([ }])", .escape_regex(id))
            hits <- !code & grepl(pattern, new)
            new[hits] <- gsub(pattern, sprintf("\\1#sec-%s\\2", id), new[hits])
            .bd_count(mig, "header id `{#x}` -> `{#sec-x}`", sum(hits))
        }
        ## Equation labels go after the closing `$$` of their display math
        eq <- .convert_equations(new, code)
        new <- eq$lines
        .bd_count(mig, "`(\\#eq:x)` -> `{#eq-x}`", eq$n)
        for (label in eq$unresolved) .bd_todo(
            mig, "Equations to label by hand",
            sprintf("`%s`: no closing `$$` found after `(\\#eq:%s)`", p, label)
        )
        ## Chunks of the figures and tables referenced
        ch <- .convert_chunks(new, mig$fig_refs, mig$tab_refs, options = FALSE)
        new <- ch$lines
        for (name in names(ch$counts)) .bd_count(mig, name, ch$counts[[name]])
        if (!identical(new, lines)) .bd_write(mig, new, p)
    }

    ## Labels referenced but never found
    pages <- lapply(file.path(root, mig$pages), .read_page)
    found <- function(prefix) unique(unlist(lapply(pages, function(l) {
        m <- regmatches(l, regexpr(sprintf("^\\s*```+\\s*\\{r[ ,]+%s-[^ ,}]+", prefix), l))
        sub(sprintf("^.*%s-", prefix), "", m)
    })))
    for (x in setdiff(mig$fig_refs, found("fig"))) .bd_todo(
        mig, "Unresolved cross-references", sprintf("`@fig-%s`: no chunk labelled `%s` found", x, x)
    )
    for (x in setdiff(mig$tab_refs, found("tbl"))) .bd_todo(
        mig, "Unresolved cross-references", sprintf("`@tbl-%s`: no chunk labelled `%s` found", x, x)
    )
    ids <- unique(unlist(lapply(pages, function(l) {
        attrs <- sub("^#+ [^{]*", "", l[!.code_lines(l) & grepl("^#+ ", l)])
        sub("^#", "", unlist(regmatches(attrs, gregexpr("#[A-Za-z][A-Za-z0-9_.:-]*", attrs))))
    })))
    for (x in mig$sec_refs) {
        target <- if (startsWith(x, "sec-")) x else paste0("sec-", x)
        if (!target %in% ids) .bd_todo(
            mig, "Unresolved cross-references",
            sprintf("`@%s`: no header with the id `{#%s}` found", target, target)
        )
    }
    if (length(mig$tab_refs)) .bd_todo(
        mig, "Tables",
        "Check the captions of the tables now labelled `tbl-*`: quarto takes them from the `tbl-cap` chunk option"
    )
    if (!length(mig$changed)) return(NULL)

    done <- .bd_delta(mig$counts, before)
    count <- function(pattern) sum(done[grepl(pattern, names(done))])
    extra <- c(
        if (count("^header id")) paste(.n(count("^header id"), "header id"), "now starting with sec-"),
        if (count("eq:x\\)` -> `\\{#eq")) paste(.n(count("eq:x\\)` -> `\\{#eq"), "equation label"), "after its display math"),
        if (count("^chunk")) paste(.n(count("^chunk"), "chunk label"), "starting with fig- or tbl-")
    )
    refs <- count("\\\\@ref")
    .commit_message(
        "Rewrite cross-references for quarto",
        paste(
            "bookdown's \\@ref() renders a number only, while quarto's references render",
            "\"Figure 2\", \"Chapter 3\"... by themselves: a type word right before a reference",
            "is dropped, and the other references render their number only, as before."
        ),
        sprintf(
            "%s, in %s%s.", .n(refs, "reference"), .n(length(unique(mig$changed)), "page"),
            if (length(extra)) paste0("; ", .and(extra)) else ""
        )
    )
}

## 7. msmbstyle's questions and solutions
.bookdown_callout_step <- function(mig) {

    if (!identical(mig$style, "msmbstyle")) return(NULL)
    before <- mig$counts
    for (p in mig$pages) {
        lines <- .read_page(file.path(mig$path, p))
        code <- .code_lines(lines)
        touched <- logical(length(lines))
        for (rule in .bookdown_rules[["msmbstyle"]]) {
            hits <- which(!code & grepl(rule$pattern, lines, perl = isTRUE(rule$perl)))
            for (h in hits) {
                res <- .rewrite_text(lines[h], rule)
                lines[h] <- res$line
                touched[h] <- TRUE
                .bd_count(mig, rule$name, res$n)
            }
        }
        if (!any(touched)) next
        parts <- lapply(lines, .split_lines)
        .bd_write(mig, .tidy_touched(unlist(parts), rep(touched, lengths(parts))), p)
    }
    if (!length(mig$changed)) return(NULL)

    done <- .bd_delta(mig$counts, before)
    count <- function(name) if (is.na(done[name])) 0L else done[[name]]
    .commit_message(
        "Turn questions and solutions into callouts",
        sprintf(paste(
            "msmbstyle's question_begin()/question_end() and solution_begin()/solution_end()",
            "become quarto callouts, the solutions collapsed: %s and %s."
        ),
            .n(count("`question_begin()` -> question callout"), "question"),
            .n(count("`solution_begin()` -> collapsed answer callout"), "solution")
        ),
        paste(
            "Each one is wrapped in a .callout-question or .callout-answer div, which the",
            "BiocBook theme styles: quarto drops extra classes from callouts."
        )
    )
}

## 8. Figure layout options
.bookdown_figure_step <- function(mig) {
    before <- mig$counts
    for (p in mig$pages) {
        lines <- .read_page(file.path(mig$path, p))
        ch <- .convert_chunks(lines, character(0), character(0), labels = FALSE)
        for (name in names(ch$counts)) .bd_count(mig, name, ch$counts[[name]])
        if (!identical(ch$lines, lines)) .bd_write(mig, ch$lines, p)
    }
    if (!length(mig$changed)) return(NULL)
    .commit_message(
        "Translate figure layout options",
        sprintf(paste(
            "fig.margin = TRUE and fig.fullwidth = TRUE become quarto's column: margin and",
            "column: page, and fig.margin = FALSE and fig.fullwidth = FALSE, the defaults,",
            "go: %s."
        ), .n(sum(.bd_delta(mig$counts, before)), "chunk option"))
    )
}

## 9. The setup of `index.Rmd`, at the top of every chapter
.bookdown_setup_step <- function(mig) {

    bd <- mig$bd
    if (length(bd$config[["after_chapter_script"]])) .bd_todo(
        mig, "Unknown options", "`after_chapter_script` (`_bookdown.yml`) has no quarto equivalent"
    )
    new_session <- isTRUE(bd$config[["new_session"]]) || identical(bd$config[["new_session"]], "yes")
    if (new_session) return(NULL)
    setup <- c(.setup_calls(bd$pages[[1]]), mig$script_lines)
    mig$setup <- setup
    .bd_todo(mig, "Shared R session", paste(
        "bookdown ran every chapter in a single R session, quarto renders each in",
        "its own:", if (length(setup)) "the setup calls of `index.Rmd` are now repeated at the top of each chapter, but",
        "objects created in a chapter and used in a later one must be recreated there.",
        "The first full render shows which."
    ))
    if (!length(setup)) return(NULL)
    chunk <- c(
        "```{r}", "#| include: false",
        "## bookdown ran every chapter in a single R session, quarto runs",
        "## each in its own: setup repeated by BiocBook::from_bookdown()",
        setup, "```"
    )
    for (p in mig$pages[-1]) {
        .bd_write(mig, .insert_after_title(.read_page(file.path(mig$path, p)), chunk), p)
    }
    .bd_count(mig, "setup calls of `index.Rmd` repeated at the top of each chapter", length(mig$pages) - 1L)
    ## Its content is in every chapter now
    for (s in mig$scripts) .bd_remove(mig, s)
    .commit_message(
        "Set up every chapter as index.Rmd did",
        paste0(
            "bookdown rendered all chapters in a single R session, quarto renders each in its ",
            "own: the packages and options that index.Rmd sets up",
            if (length(mig$scripts)) sprintf(", and %s,", .and(mig$scripts)),
            " are now set up at the top of every chapter, in a hidden chunk."
        )
    )
}

## 10. DESCRIPTION
.bookdown_description_step <- function(mig) {
    res <- .write_bookdown_description(mig)
    n <- length(res$deps)
    imports <- if (n == 1L) "the package its pages use" else sprintf("the %d packages its pages use", n)
    .commit_message(
        "Describe the book in DESCRIPTION",
        paste0(
            "Title, description and authors from the book, ",
            if (n) sprintf("%s in Imports, ", imports),
            sprintf("and its licence, %s.", res$license)
        ),
        if (res$placeholder) "The email of the maintainer is a placeholder."
    )
}

## ---------------------------------------------------------------------------
## Changing the book in place
## ---------------------------------------------------------------------------

.read_page <- function(f) readLines(f, warn = FALSE, encoding = "UTF-8")

.ends_with_newline <- function(f) {
    size <- file.info(f)$size
    if (is.na(size) || size == 0) return(TRUE)
    con <- file(f, "rb")
    on.exit(close(con))
    seek(con, size - 1)
    identical(readBin(con, "raw", 1L), as.raw(10L))
}

## Writes a page byte for byte, whatever the locale, keeping its final
## newline, or its absence
.write_page <- function(lines, f) {
    eol <- .ends_with_newline(f)
    last <- seq_along(lines) == length(lines)
    bytes <- Map(function(l, last) c(charToRaw(l), if (!last || eol) as.raw(10L)), lines, last)
    writeBin(as.raw(unlist(bytes, use.names = FALSE)), f)
}

.bd_mark <- function(mig, paths) mig$changed <- c(mig$changed, paths)

.bd_write <- function(mig, lines, rel) {
    f <- file.path(mig$path, rel)
    dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
    .write_page(lines, f)
    .bd_mark(mig, rel)
}

.bd_copy <- function(mig, from, rel) {
    to <- file.path(mig$path, rel)
    dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
    file.copy(from, to, overwrite = TRUE)
    .bd_mark(mig, rel)
}

## Moves a file, or the files of a folder (merged into one already there)
.bd_move <- function(mig, from, to) {
    src <- file.path(mig$path, from)
    if (dir.exists(src)) {
        for (f in list.files(src, recursive = TRUE, all.files = TRUE, no.. = TRUE)) {
            .bd_move(mig, file.path(from, f), file.path(to, f))
        }
        unlink(src, recursive = TRUE)
        return(invisible())
    }
    dest <- file.path(mig$path, to)
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    if (file.exists(dest)) unlink(dest)
    if (!file.rename(src, dest)) {
        file.copy(src, dest, copy.date = TRUE)
        unlink(src)
    }
    .bd_mark(mig, c(from, to))
}

.bd_remove <- function(mig, rel) {
    f <- file.path(mig$path, rel)
    if (dir.exists(f)) {
        .bd_mark(mig, file.path(rel, list.files(f, recursive = TRUE, all.files = TRUE, no.. = TRUE)))
        unlink(f, recursive = TRUE)
    } else if (file.exists(f)) {
        .bd_mark(mig, rel)
        unlink(f)
    }
}

.bd_count <- function(mig, name, n = 1L) {
    if (n > 0) mig$counts[name] <- (if (is.na(mig$counts[name])) 0L else mig$counts[name]) + as.integer(n)
}

.bd_delta <- function(now, before) {
    common <- intersect(names(now), names(before))
    now[common] <- now[common] - before[common]
    now[now > 0]
}

.bd_todo <- function(mig, section, item) mig$todo[[section]] <- c(mig$todo[[section]], item)

.bookdown_fill <- function(lines, mig) {
    lines <- gsub("<Package_name>", mig$package, lines)
    lines <- gsub("<package_name>", tolower(mig$package), lines)
    gsub("<github_user>", mig$user, lines)
}

.commit_message <- function(subject, ...) {
    paragraphs <- Filter(function(p) length(p) && nzchar(p), list(...))
    c(subject, unlist(lapply(paragraphs, function(p) c("", strwrap(p, width = 72)))))
}

.and <- function(x) {
    if (length(x) < 2L) return(paste(x, collapse = ""))
    paste(paste(utils::head(x, -1L), collapse = ", "), "and", utils::tail(x, 1L))
}

.n <- function(n, what) sprintf("%d %s%s", n, what, if (n == 1L) "" else "s")

## Collapses the runs of blank lines that hold a line a rewrite touched (and
## drops such a run at the top of a page): the blank lines of the book itself
## are left alone
.tidy_touched <- function(lines, touched) {
    blank <- !nzchar(trimws(lines)) & !.code_lines(lines)
    keep <- rep(TRUE, length(lines))
    r <- rle(blank)
    ends <- cumsum(r$lengths)
    starts <- ends - r$lengths + 1L
    for (k in which(r$values)) {
        run <- seq(starts[k], ends[k])
        if (!any(touched[run])) next
        keep[if (starts[k] == 1L) run else run[-1]] <- FALSE
    }
    lines[keep]
}

## Drops lines, and the blank lines that would then pile up where they were
.drop_lines <- function(lines, drop) {
    i <- which(drop)
    touched <- logical(length(lines))
    touched[c(pmax(i - 1L, 1L), pmin(i + 1L, length(lines)))] <- TRUE
    .tidy_touched(lines[!drop], touched[!drop])
}

## ---------------------------------------------------------------------------
## Rewriting helpers
## ---------------------------------------------------------------------------

## Which lines of a page are code: fenced blocks, fences included. A block
## closes on a bare fence at least as long as the one that opened it.
.code_lines <- function(lines) {
    inside <- logical(length(lines))
    open <- 0L
    for (i in seq_along(lines)) {
        fence <- regmatches(lines[i], regexpr("^\\s*(`{3,}|~{3,})", lines[i]))
        if (length(fence)) {
            n <- nchar(trimws(fence))
            if (open == 0L) {
                open <- n
                inside[i] <- TRUE
                next
            }
            if (n >= open && grepl("^\\s*(`{3,}|~{3,})\\s*$", lines[i])) {
                open <- 0L
                inside[i] <- TRUE
                next
            }
        }
        inside[i] <- open > 0L
    }
    inside
}

## Apply a rule to a line, leaving its inline code spans alone
.rewrite_text <- function(line, rule) {
    perl <- isTRUE(rule$perl)
    if (identical(rule$scope, "line")) return(list(
        line = gsub(rule$pattern, rule$replacement, line, perl = perl),
        n = lengths(regmatches(line, gregexpr(rule$pattern, line, perl = perl)))
    ))
    spans <- gregexpr("`+[^`]*`+", line)
    text <- regmatches(line, spans, invert = TRUE)[[1]]
    n <- sum(lengths(regmatches(text, gregexpr(rule$pattern, text, perl = perl))))
    regmatches(line, spans, invert = TRUE) <- list(gsub(rule$pattern, rule$replacement, text, perl = perl))
    list(line = line, n = n)
}

.split_lines <- function(x) unlist(lapply(x, function(l) {
    parts <- strsplit(l, "\n", fixed = TRUE)[[1]]
    if (endsWith(l, "\n")) parts <- c(parts, "")
    if (length(parts)) parts else ""
}))

## Number of the code block each line belongs to (0 outside code)
.code_blocks <- function(lines) {
    code <- .code_lines(lines)
    opens <- code & !c(FALSE, utils::head(code, -1))
    ## A block that closes and is immediately followed by another one
    fence <- grepl("^\\s*(`{3,}|~{3,})", lines)
    closing <- code & fence & grepl("^\\s*(`{3,}|~{3,})\\s*$", lines)
    after_close <- code & fence & c(FALSE, utils::head(closing & code, -1)) & !closing
    id <- cumsum(opens | after_close)
    id[!code] <- 0L
    id
}

.strip_header <- function(x) trimws(sub("\\s*\\{[^}]*\\}\\s*$", "", x))

.escape_regex <- function(x) gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)

## `(\#eq:x)` sits inside the display math in bookdown; quarto wants
## `$$ ... $$ {#eq-x}`
.convert_equations <- function(lines, code) {
    n <- 0L
    unresolved <- character(0)
    pattern <- "\\s*\\(\\\\#eq:([^)]+)\\)"
    for (i in which(!code & grepl(pattern, lines))) {
        label <- sub(".*\\(\\\\#eq:([^)]+)\\).*", "\\1", lines[i])
        lines[i] <- sub(pattern, "", lines[i])
        ## The closing `$$`: on this line if the math opens and closes on it,
        ## otherwise on one of the next lines
        close <- NA_integer_
        for (j in seq(i, min(i + 30L, length(lines)))) {
            if (j > i && !nzchar(trimws(lines[j]))) break
            k <- lengths(regmatches(lines[j], gregexpr("\\$\\$", lines[j])))
            opens_here <- j == i && k == 1L && grepl("^\\s*\\$\\$", lines[j])
            if (k > 0L && !opens_here) {
                close <- j
                break
            }
        }
        if (is.na(close)) {
            unresolved <- c(unresolved, label)
            next
        }
        lines[close] <- sub("\\$\\$\\s*$", sprintf("$$ {#eq-%s}", label), lines[close])
        n <- n + 1L
    }
    list(lines = lines, n = n, unresolved = unresolved)
}

## Chunk headers: options with a quarto equivalent, and labels of the figures
## and tables referenced in the text
.convert_chunks <- function(lines, fig_refs, tab_refs, options = TRUE, labels = TRUE) {
    counts <- list()
    out <- character(0)
    header <- grepl("^\\s*```+\\s*\\{[rR][ ,}]", lines)
    for (i in seq_along(lines)) {
        line <- lines[i]
        extra <- character(0)
        if (header[i] && options) {
            for (rule in .bookdown_chunk_rules) {
                after_comma <- sprintf("\\s*,\\s*%s\\b", rule$option)
                first <- sprintf("(\\{[rR])\\s+%s\\b\\s*,?\\s*", rule$option)
                if (grepl(after_comma, line, perl = TRUE) || grepl(first, line, perl = TRUE)) {
                    line <- gsub(after_comma, "", line, perl = TRUE)
                    line <- sub(first, "\\1 ", line, perl = TRUE)
                    line <- sub("\\{([rR])\\s+\\}", "{\\1}", line)
                    extra <- c(extra, rule$add)
                    counts[[rule$name]] <- (if (is.null(counts[[rule$name]])) 0L else counts[[rule$name]]) + 1L
                }
            }
        }
        if (header[i] && labels) {
            for (ref in list(list(labels = fig_refs, prefix = "fig"), list(labels = tab_refs, prefix = "tbl"))) {
                for (label in ref$labels) {
                    pattern <- sprintf("^(\\s*```+\\s*\\{[rR][ ,]+)%s(\\s*[,}])", .escape_regex(label))
                    if (grepl(pattern, line)) {
                        line <- sub(pattern, sprintf("\\1%s-%s\\2", ref$prefix, label), line)
                        name <- sprintf("chunk `x` labelled `%s-x`", ref$prefix)
                        counts[[name]] <- (if (is.null(counts[[name]])) 0L else counts[[name]]) + 1L
                    }
                }
            }
        }
        out <- c(out, line, extra)
    }
    list(lines = out, counts = counts)
}

## `library()`, `require()` and `options()` calls of the evaluated R chunks of
## `index.Rmd`
.setup_calls <- function(lines) {
    code <- .code_lines(lines)
    calls <- character(0)
    in_r <- FALSE
    skip <- FALSE
    for (i in seq_along(lines)) {
        if (code[i] && grepl("^\\s*```+\\s*\\{[rR][ ,}]", lines[i])) {
            in_r <- TRUE
            skip <- grepl("eval\\s*=\\s*(FALSE|F)\\b", lines[i], perl = TRUE)
            next
        }
        if (!code[i] || grepl("^\\s*```+\\s*$", lines[i])) {
            in_r <- FALSE
            next
        }
        if (!in_r) next
        if (grepl("^\\s*#\\|\\s*eval:\\s*false", lines[i])) skip <- TRUE
        if (!skip && grepl("^\\s*(suppress(PackageStartup)?Messages\\(\\s*)?(library|require|options)\\(", lines[i])) {
            calls <- c(calls, trimws(lines[i]))
        }
    }
    unique(calls)
}

.insert_after_title <- function(lines, chunk) {
    code <- .code_lines(lines)
    yaml_end <- 0L
    if (length(lines) && grepl("^---\\s*$", lines[1])) {
        yaml_end <- which(grepl("^---\\s*$", lines))[2]
        if (is.na(yaml_end)) yaml_end <- 0L
    }
    h1 <- which(!code & grepl("^# ", lines) & seq_along(lines) > yaml_end)[1]
    at <- if (is.na(h1)) yaml_end else h1
    append(lines, c("", chunk), after = at)
}

## The landing page: BiocBook's own header chunk and its Docker, RStudio and
## session info sections, with the bookdown preamble as its `Welcome` text.
.index_body <- function(lines, dirs) {
    code <- .code_lines(lines)
    ## Headers of the landing page are not numbered
    h1 <- !code & grepl("^# ", lines) & !grepl("\\{(.*\\s)?(-|\\.unnumbered)(\\s.*)?\\}\\s*$", lines)
    lines[h1 & grepl("\\}\\s*$", lines)] <- sub("\\}\\s*$", " -}", lines[h1 & grepl("\\}\\s*$", lines)])
    lines[h1 & !grepl("\\}\\s*$", lines)] <- paste(lines[h1 & !grepl("\\}\\s*$", lines)], "{-}")
    ## The page sits one folder up from the chapters
    n <- 0L
    for (d in dirs) {
        pattern <- sprintf("([\"'(])(\\./)?%s/", .escape_regex(d))
        n <- n + sum(lengths(regmatches(lines, gregexpr(pattern, lines))))
        lines <- gsub(pattern, sprintf("\\1pages/%s/", d), lines)
    }
    if (!any(!code & grepl("^# ", lines))) lines <- c("# Welcome {-}", "", lines)
    structure(lines, n = n)
}

.write_index <- function(file, body) {
    lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
    start <- grep("^# Welcome \\{-\\}", lines)[1]
    end <- grep("^# ", lines)
    end <- end[end > start][1]
    if (is.na(start) || is.na(end)) {
        lines <- c(lines, "", body)
    }
    else {
        lines <- c(lines[seq_len(start - 1)], body, "", lines[seq(end, length(lines))])
    }
    writeLines(lines, file, useBytes = TRUE)
}

.yaml_quote <- function(x) sprintf('"%s"', gsub('"', '\\\\"', gsub("\\\\", "\\\\\\\\", x)))

.write_book_yml <- function(file, meta, targets, part_of, appendix) {
    lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
    title <- meta[["title"]]
    if (!is.null(title)) {
        lines <- sub("^  title: .*$", paste("  title:", .yaml_quote(title)), lines)
    }
    extra <- character(0)
    if (!is.null(meta[["subtitle"]])) extra <- c(extra, paste("  subtitle:", .yaml_quote(meta[["subtitle"]])))
    author <- .bookdown_author_names(meta[["author"]])
    if (length(author)) extra <- c(extra, "  author:", paste("    -", .yaml_quote(author)))
    if (!is.null(meta[["description"]])) extra <- c(extra, paste("  description:", .yaml_quote(meta[["description"]])))
    if (!is.null(meta[["date"]])) {
        date <- if (grepl("Sys\\.(Date|time)", meta[["date"]])) "today" else meta[["date"]]
        extra <- c(extra, paste("  date:", .yaml_quote(date)))
    }
    at <- grep("^  title: ", lines)[1]
    lines <- append(lines, extra, after = at)

    ## Chapters, parts and appendices, written in the same layout as the
    ## template (`.add_page()` expects four-space chapter entries)
    block <- c("  chapters:", "    - index.qmd")
    i <- 2L
    while (i <= length(targets)) {
        if (appendix[i]) {
            i <- i + 1L
            next
        }
        if (is.na(part_of[i])) {
            block <- c(block, paste("    -", targets[i]))
            i <- i + 1L
            next
        }
        part <- part_of[i]
        block <- c(block, paste("    - part:", .yaml_quote(part)), "      chapters:")
        while (i <= length(targets) && !appendix[i] && identical(part_of[i], part)) {
            block <- c(block, paste("        -", targets[i]))
            i <- i + 1L
        }
    }
    if (any(appendix)) block <- c(block, "  appendices:", paste("    -", targets[appendix]))
    start <- grep("^  chapters:\\s*$", lines)[1]
    end <- start
    while (end < length(lines) && grepl("^    ", lines[end + 1])) end <- end + 1L
    rest <- if (end < length(lines)) lines[seq(end + 1, length(lines))] else character(0)
    writeLines(c(lines[seq_len(start - 1)], block, rest), file, useBytes = TRUE)
}

## `_format.yml`: bibliographies, CSS, and the output options quarto has an
## equivalent for. Returns the options that could not be translated.
.write_format_yml <- function(file, bd, dirs) {
    lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
    opts <- bd$options
    bibs <- c("bibliography.bib", setdiff(attr(dirs, "bibs"), "bibliography.bib"))
    at <- grep("^    bibliography: ", lines)[1]
    bib_block <- c("    bibliography:", paste0("      - assets/", bibs))
    lines <- if (is.na(at)) c(lines, bib_block) else c(lines[seq_len(at - 1)], bib_block, lines[-seq_len(at)])
    extra <- character(0)
    if (length(attr(dirs, "css"))) {
        extra <- c(extra, "    css:", paste0("      - assets/", attr(dirs, "css")))
    }
    if (isTRUE(opts[["margin_references"]])) extra <- c(extra, "    citation-location: margin")
    if (!is.null(opts[["toc_depth"]])) extra <- c(extra, paste("    toc-depth:", opts[["toc_depth"]]))
    if (isTRUE(bd$meta[["link-citations"]]) || identical(bd$meta[["link-citations"]], "yes")) {
        extra <- c(extra, "    link-citations: true")
    }
    writeLines(c(lines, extra), file, useBytes = TRUE)

    ## Everything else has no direct equivalent
    known <- c("css", "margin_references", "toc_depth", "toc")
    todo <- character(0)
    if (!is.na(bd$format) && !bd$format %in% c("bookdown::gitbook", "bookdown::html_book", "bookdown::bs4_book")) {
        todo <- c(todo, sprintf("Output format `%s`: its look is not migrated, the book uses the BiocBook theme", bd$format))
    }
    if (!is.null(opts[["split_by"]]) && !opts[["split_by"]] %in% c("chapter", "chapter+number")) {
        todo <- c(todo, sprintf("`split_by: %s`: quarto books have one page per chapter", opts[["split_by"]]))
        known <- c(known, "split_by")
    }
    known <- c(known, "split_by")
    for (opt in setdiff(names(opts), known)) todo <- c(todo, sprintf(
        "Output option `%s: %s` has no quarto equivalent and was not migrated",
        opt, paste(format(opts[[opt]]), collapse = ", ")
    ))
    unknown_meta <- setdiff(names(bd$meta), c(
        "title", "subtitle", "author", "date", "description", "output",
        "bibliography", "link-citations", "site"
    ))
    for (key in unknown_meta) todo <- c(todo, sprintf(
        "`%s` (`index.Rmd` header) was not migrated", key
    ))
    todo
}

.bookdown_author_names <- function(author) {
    if (is.null(author)) return(character(0))
    if (is.list(author)) author <- vapply(author, function(a) {
        if (is.list(a)) as.character(a[["name"]]) else as.character(a)
    }, character(1))
    author <- unlist(strsplit(paste(author, collapse = ", "), ",|\\band\\b|&"))
    author <- trimws(author)
    author[nzchar(author)]
}

## Title, Description, Authors@R, Imports and License, from the bookdown
## project and from the packages the converted pages use
.write_bookdown_description <- function(mig) {

    bd <- mig$bd
    d <- desc::desc(file = file.path(mig$path, "DESCRIPTION"))
    title <- bd$meta[["title"]]
    if (!is.null(title)) d$set("Title", tools::toTitleCase(gsub("[.]$", "", title)))

    ## Description: the first paragraph of the preamble, and where it comes from
    body <- bd$pages[[1]]
    body <- body[!.code_lines(body)]
    para <- character(0)
    for (l in body) {
        if (grepl("^\\s*$", l)) {
            if (length(para)) break
            next
        }
        if (grepl("^(#|<|!|\\||:::|`r )", l)) {
            if (length(para)) break
            next
        }
        para <- c(para, trimws(l))
    }
    para <- gsub("\\[([^]]+)\\]\\([^)]+\\)", "\\1", paste(para, collapse = " "))
    para <- gsub("[*`]", "", para)
    if (nzchar(para) && !grepl("[.!?]$", para)) para <- paste0(para, ".")
    origin <- sprintf(
        "This book package was migrated from the bookdown book '%s' with BiocBook::from_bookdown().",
        if (is.null(title)) basename(bd$path) else title
    )
    subtitle <- if (is.null(bd$meta[["subtitle"]])) NULL else paste0(bd$meta[["subtitle"]], ".")
    d$set("Description", paste(c(para, subtitle, origin), collapse = " "))

    ## Authors: the first one maintains the package, with a placeholder email
    authors <- .bookdown_author_names(bd$meta[["author"]])
    if (length(authors)) {
        persons <- lapply(seq_along(authors), function(i) {
            parts <- strsplit(authors[i], "\\s+")[[1]]
            utils::person(
                given = if (length(parts) > 1) paste(utils::head(parts, -1), collapse = " ") else parts,
                family = if (length(parts) > 1) utils::tail(parts, 1) else NULL,
                email = if (i == 1) "first.last@example.com" else NULL,
                role = if (i == 1) c("aut", "cre") else "aut"
            )
        })
        d$set_authors(do.call(c, persons))
    }

    ## Dependencies: the same scan as `check_deps()`, over the converted pages,
    ## and those of the DESCRIPTION the project had
    found <- renv::dependencies(file.path(mig$path, "inst"), quiet = TRUE)$Package
    deps <- setdiff(sort(unique(found)), c(.bookdown_not_deps, mig$package, d$get_deps()$package))
    for (pkg in deps) d$set_dep(pkg, "Imports")
    old <- mig$old_deps
    if (!is.null(old)) for (i in seq_len(nrow(old))) {
        if (!old$package[i] %in% c(d$get_deps()$package, .bookdown_not_deps)) {
            d$set_dep(old$package[i], old$type[i], old$version[i])
        }
    }
    if (length(mig$old_remotes)) d$set_remotes(unique(c(d$get_remotes(), mig$old_remotes)))

    license <- .bookdown_licence(mig)
    d$set("License", license)
    d$write()
    .bd_mark(mig, "DESCRIPTION")
    mig$deps <- deps
    if (length(deps)) cli::cli_alert_success(cli::col_grey(
        "Added {length(deps)} dependenc{?y/ies} to {.file DESCRIPTION}: {.pkg {deps}}"
    ))
    placeholder <- length(authors) > 0
    if (placeholder) .bd_todo(mig, "DESCRIPTION", "Replace the placeholder email of the maintainer (`cre`) in `Authors@R`")
    .bd_todo(mig, "DESCRIPTION", "Check `Title` and `Description`")
    list(deps = deps, license = license, placeholder = placeholder)
}

## The licence of the book: a Creative Commons licence its pages link to, its
## LICENSE file, or else the MIT licence of the template, for its authors
.bookdown_licence <- function(mig) {
    cc <- .bookdown_cc(unlist(mig$bd$pages))
    if (!is.null(cc)) return(cc)
    if (mig$licence_file) return("file LICENSE")
    year <- format(Sys.Date(), "%Y")
    holder <- paste(.bookdown_author_names(mig$bd$meta[["author"]]), collapse = ", ")
    if (!nzchar(holder)) holder <- mig$package
    .bd_write(mig, c(paste("YEAR:", year), paste("COPYRIGHT HOLDER:", holder)), "LICENSE")
    if (!file.exists(file.path(mig$path, "LICENSE.md"))) {
        md <- readLines(file.path(mig$template, "LICENSE.md"), warn = FALSE, encoding = "UTF-8")
        md <- sub("^Copyright \\(c\\) .*$", sprintf("Copyright (c) %s %s", year, holder), md)
        .bd_write(mig, md, "LICENSE.md")
    }
    .bd_todo(mig, "DESCRIPTION", "Check `License`: the book did not state one, it now has the MIT licence of the template")
    "MIT + file LICENSE"
}

.bookdown_cc <- function(lines) {
    m <- regmatches(lines, regexpr("creativecommons\\.org/licenses/[a-z-]+/[0-9.]+", lines))
    if (!length(m)) return(NULL)
    licence <- sprintf(
        "CC %s %s",
        toupper(sub("^.*/licenses/([a-z-]+)/.*$", "\\1", m[1])),
        sub("^.*/", "", m[1])
    )
    if (licence %in% .bookdown_cc_licences) licence else NULL
}

## GitHub-only packages installed by the book, e.g.
## `BiocManager::install("org/pkg")` or `remotes::install_github("org/pkg")`
.github_only <- function(bd) {
    lines <- unlist(bd$pages)
    lines <- lines[grepl("install", lines)]
    repos <- unlist(regmatches(lines, gregexpr("[\"'][A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+[\"']", lines)))
    repos <- unique(gsub("[\"']", "", repos))
    vapply(repos, function(r) sprintf(
        "`%s` is installed from GitHub: add it to `Remotes:` in DESCRIPTION if the book needs it to build, or drop it",
        r
    ), character(1), USE.NAMES = FALSE)
}

.not_on_bioc <- function(deps) {
    if (!requireNamespace("BiocManager", quietly = TRUE)) return(character(0))
    available <- tryCatch(
        rownames(utils::available.packages(repos = BiocManager::repositories())),
        error = function(e) NULL
    )
    if (is.null(available)) return(
        "Could not check that the dependencies are on CRAN or Bioconductor (no network access?)"
    )
    vapply(setdiff(deps, available), function(p) sprintf(
        "`%s` is neither on CRAN nor on Bioconductor: add it to `Remotes:` in DESCRIPTION, or drop it", p
    ), character(1), USE.NAMES = FALSE)
}

## What the conversion cannot decide: downloads while the book builds, cached
## chunks, bibliographies written while it builds, and what quarto's own
## bookdown cross-reference detector still finds.
.scan_converted <- function(book, targets) {
    todo <- list()
    add <- function(section, item) todo[[section]] <<- c(todo[[section]], item)
    network <- "pxget\\(|download\\.file\\(|curl_download\\(|httr2?::|BiocFileCache|ExperimentHub\\(|AnnotationHub\\(|[\"']https?://"
    for (target in targets) {
        lines <- readLines(file.path(book, "inst", target), warn = FALSE, encoding = "UTF-8")
        code <- .code_lines(lines)
        block <- .code_blocks(lines)
        opening <- block > 0 & !duplicated(block)
        header <- opening & grepl("^\\s*```+\\s*\\{[rR][ ,}]", lines)
        r_blocks <- block[header]
        skipped <- unique(c(
            block[header & grepl("eval\\s*=\\s*(FALSE|F)\\b", lines, perl = TRUE)],
            block[code & grepl("^\\s*#\\|\\s*eval:\\s*false", lines)]
        ))
        live <- code & !opening & block %in% setdiff(r_blocks, skipped) & !grepl("^\\s*(#|```)", lines)
        where <- function(i) sprintf("`inst/%s` line %d", target, i)
        for (i in which(live & grepl(network, lines) & !grepl("include_graphics\\(", lines))) add(
            "Network access while the book builds",
            sprintf("%s: `%s`", where(i), trimws(lines[i]))
        )
        for (i in which(header & grepl("cache\\s*=\\s*(TRUE|T)\\b", lines, perl = TRUE) |
                        code & grepl("^\\s*#\\|\\s*cache:\\s*true", lines))) add(
            "Cached chunks",
            sprintf("%s: remove `cache = TRUE`, or keep `*_cache/` out of git and of the package", where(i))
        )
        for (i in which(live & grepl("write_bib\\(", lines))) add(
            "Bibliographies written while the book builds",
            sprintf("%s: generate the `.bib` file once, commit it to `inst/assets/` and drop the call", where(i))
        )
        for (i in which(!code & grepl("\\(ref:[^)]+\\)", lines))) add(
            "Unresolved cross-references",
            sprintf("%s: bookdown text reference `(ref:...)` to rewrite by hand", where(i))
        )
        for (i in which(header & grepl("^\\s*```+\\s*\\{(theorem|lemma|corollary|proposition|conjecture|definition|example|exercise|proof|remark|solution)", lines, ignore.case = TRUE))) add(
            "Unresolved cross-references",
            sprintf("%s: bookdown theorem environment, use a quarto cross-reference div instead", where(i))
        )
    }
    if (exists("detect_bookdown_crossrefs", envir = asNamespace("quarto"), inherits = FALSE)) {
        res <- NULL
        utils::capture.output(res <- tryCatch(
            suppressMessages(quarto::detect_bookdown_crossrefs(file.path(book, "inst"), verbose = FALSE)),
            error = function(e) NULL
        ))
        root <- normalizePath(book)
        for (r in res) {
            f <- normalizePath(r$file, mustWork = FALSE)
            if (!file.exists(f)) f <- normalizePath(file.path(book, "inst", r$file), mustWork = FALSE)
            if (file.exists(f)) {
                l <- readLines(f, warn = FALSE, encoding = "UTF-8")
                ## Left alone on purpose: code, and inline code
                if (.code_lines(l)[r$line]) next
                if (!grepl(r$bookdown_syntax, gsub("`+[^`]*`+", "", l[r$line]), fixed = TRUE)) next
            }
            add("Unresolved cross-references", sprintf(
                "`%s` line %s: `%s`", sub(paste0("^", .escape_regex(root), "/"), "", f), r$line, r$bookdown_syntax
            ))
        }
    }
    todo
}

## Top-level files that a BiocBook does not have, and that builds of the
## package do not ignore
.bookdown_leftovers <- function(root) {
    known <- c(
        "DESCRIPTION", "NAMESPACE", "NEWS.md", "NEWS", "README.md", "README.Rmd",
        "LICENSE", "LICENSE.md", "CODE_OF_CONDUCT.md", "CONTRIBUTING.md", "MIGRATION.md",
        "Dockerfile", "inst", "vignettes", "man", "R", "tests",
        ".git", ".github", ".gitignore", ".Rbuildignore", ".Rproj.user", ".Rhistory",
        ".RData", ".DS_Store", ".quarto"
    )
    top <- setdiff(list.files(root, all.files = TRUE, no.. = TRUE), known)
    top <- top[!grepl("\\.Rproj$", top)]
    top <- paste0(top, ifelse(dir.exists(file.path(root, top)), "/", ""))
    ignored <- tryCatch(
        gert::git_ignore_path_is_ignored(top, repo = root), error = function(e) rep(FALSE, length(top))
    )
    top <- top[!ignored]
    buildignore <- file.path(root, ".Rbuildignore")
    patterns <- if (file.exists(buildignore)) readLines(buildignore, warn = FALSE) else character(0)
    patterns <- patterns[nzchar(trimws(patterns))]
    excluded <- vapply(top, function(f) {
        any(vapply(patterns, function(p) grepl(p, sub("/$", "", f), perl = TRUE), logical(1)))
    }, logical(1))
    top[!excluded]
}

.write_migration_report <- function(mig) {

    for (f in .bookdown_leftovers(mig$path)) .bd_todo(
        mig, "Files that are not part of a BiocBook",
        sprintf("`%s`: remove it, or list it in `.Rbuildignore`", f)
    )
    scan <- .scan_converted(mig$path, mig$targets)
    for (section in names(scan)) for (item in scan[[section]]) .bd_todo(mig, section, item)
    for (item in .github_only(mig$bd)) .bd_todo(mig, "Dependencies", item)
    if (!mig$skip_availability && length(mig$deps)) {
        for (item in .not_on_bioc(mig$deps)) .bd_todo(mig, "Dependencies", item)
    }

    rules <- if (length(mig$counts)) {
        c("| Rule | Applied |", "|---|---:|", sprintf("| %s | %d |", names(mig$counts), mig$counts))
    } else "No rule applied."
    todo <- unlist(lapply(names(mig$todo), function(section) c(
        paste("###", section), "", paste("- [ ]", mig$todo[[section]]), ""
    )))
    moved <- c(paste0(mig$dirs, "/"), mig$bibs, basename(mig$css))
    lines <- c(
        "# Migration from bookdown",
        "",
        sprintf(
            "`BiocBook::from_bookdown()` (BiocBook %s) converted this book from bookdown, with the `%s` style%s%s.",
            utils::packageVersion("BiocBook"), mig$style,
            if (is.na(mig$bd$format)) "" else sprintf(" (output format: `%s`)", mig$bd$format),
            if (length(mig$commits)) sprintf(", in %s", .n(length(mig$commits), "commit")) else ""
        ),
        "This file is not part of the book: delete it once everything below is done.",
        "",
        "## Converted files",
        "",
        "| bookdown | BiocBook |", "|---|---|",
        sprintf("| `%s` | `%s` |", mig$files, mig$pages),
        "",
        if (length(moved)) c(
            sprintf("Assets moved: %s.", paste0("`", moved, "`", collapse = ", ")), ""
        ),
        "## Automatic rewrites",
        "",
        rules,
        "",
        if (length(mig$deps)) c(
            sprintf("Dependencies added to `Imports`: %s.", paste0("`", mig$deps, "`", collapse = ", ")), ""
        ),
        if (length(mig$setup)) c(
            "Setup repeated at the top of each chapter:", "", "```r", mig$setup, "```", ""
        ),
        "## To do by hand",
        "",
        if (length(todo)) todo else "Nothing left to do."
    )
    writeLines(lines, file.path(mig$path, "MIGRATION.md"), useBytes = TRUE)
    invisible(lines)
}
