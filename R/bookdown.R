#' @rdname BiocBook-bookdown
#' @name BiocBook-bookdown
#' @title Migrating a `bookdown` book to a BiocBook
#'
#' @description
#'
#' `from_bookdown()` turns a `bookdown` project into a new `BiocBook` in one
#' call. It creates the book package from the `BiocBook` template, as `init()`
#' does, and converts the `bookdown` project into it:
#'
#' - **Pages**: `index.Rmd` becomes the landing page (`inst/index.qmd`), and
#'   every chapter a page in `inst/pages/`, keeping its file name (e.g.
#'   `10-raw.Rmd` becomes `inst/pages/10-raw.qmd`). They are listed in
#'   `_book.yml` in the `bookdown` order, with its parts and appendices.
#' - **Syntax**: `bookdown` cross-references and figure layout options, and
#'   `msmbstyle` question and solution blocks, are rewritten for `quarto`
#'   (outside code chunks, see "Rewriting rules").
#' - **Assets**: bibliography files and CSS are copied to `inst/assets/`, and
#'   asset folders (e.g. `img/`) to `inst/pages/`, so that relative paths in
#'   chapters keep working. The `bookdown` output options are translated
#'   where `quarto` has an equivalent.
#' - **DESCRIPTION**: title, description and authors are taken from the
#'   `index.Rmd` header, and the packages the pages use are added to
#'   `Imports`.
#' - **Shared session**: `bookdown` runs every chapter in a single R session
#'   (unless `new_session: yes`), while `quarto` renders each chapter in its
#'   own. The `library()`, `require()` and `options()` calls of `index.Rmd`
#'   (and the `before_chapter_script`) are therefore repeated in a hidden
#'   chunk at the top of each chapter.
#'
#' `bookdown`'s `\@ref()` renders a number only, while `quarto`'s `@` 
#' references render their own "Figure", "Table" or "Chapter": a type word right 
#' before a reference is therefore dropped, and other references render their 
#' number only, as in `bookdown`.
#'
#' What still needs a human is listed in `MIGRATION.md`, at the root of the
#' new book: downloads during the build, cached chunks, `knitr::write_bib()`
#' calls, unresolved cross-references, dependencies that are not on CRAN or
#' Bioconductor, and output options with no `quarto` equivalent. It also
#' records how often each rewriting rule was applied.
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
#'   `index.Rmd` (and optionally `_bookdown.yml`).
#' @param new_package Name of the new book package, as in `init()`.
#' @param user GitHub user or organization of the new book, used in the
#'   template's links and `Docker` image name. Defaults to the user `init()`
#'   logs in with (`"dummy"` with `.local = TRUE`).
#' @param style Syntax to convert: `"bookdown"`, `"msmbstyle"` (`bookdown`
#'   with the `msmbstyle` question and solution blocks), or `"auto"` to detect
#'   it from the output format and the pages.
#' @param appendices Optional. Chapters to list as appendices of the new book
#'   (file names, with or without extension), in addition to those following a
#'   `# (APPENDIX)` header.
#' @param push,skip_availability,.local As in `init()`. `skip_availability`
#'   also skips checking that the dependencies of the book are on CRAN or
#'   Bioconductor, which requires network access.
#'
#' @return A `BiocBook` object, invisibly.
#'
#' @seealso \code{\link{BiocBook}}
#'
#' @examples
#' ## A minimal bookdown project
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
#'
#' ## In practice, you should not use `.local` argument.
#' unique_id <- as.numeric(Sys.time())
#' bookname <- paste(Sys.info()[['sysname']], unique_id, sep = '.')
#' bb <- from_bookdown(bd, bookname, .local = TRUE, skip_availability = TRUE)
#' chapters(bb)
#' readLines(file.path(bookname, "inst", "pages", "01-intro.qmd"))
#' unlink(c(bd, bookname), recursive = TRUE)
NULL

#' @rdname BiocBook-bookdown
#' @export

from_bookdown <- function(
    path,
    new_package,
    user = NULL,
    style = c("auto", "bookdown", "msmbstyle"),
    appendices = NULL,
    push = NA,
    skip_availability = FALSE,
    .local = FALSE
) {

    style <- match.arg(style)

    ## Read the bookdown project before creating anything, so that a wrong
    ## path fails early
    bd <- .read_bookdown(path)
    if (identical(style, "auto")) style <- .bookdown_style(bd)

    ## 0. Preflight checklist, as `init()`
    checks <- .preflight_checks(new_package, skip_availability, .local)
    if (is.null(user)) user <- checks[['gh_user']]

    ## 1. Create new local repo copied from BiocBook.template archive
    .init_folder(new_package, user)

    ## 2. Convert the bookdown project into it
    cli::cli_text("")
    cli::cat_rule("Migrating the `bookdown` book", col = "cyan", line = 2)
    report <- .convert_bookdown(bd, new_package, style, appendices, skip_availability)
    .write_migration_report(report, new_package)

    ## 3. Initiate local git repo
    .setup_git(
        new_package, checks[['gitsig']],
        message = "Migrate from bookdown with BiocBook::from_bookdown()"
    )

    ## 4. Syncing Github: create new repo, configure Pages, add remote, push
    if (!is.null(checks[['PAT']])) {
        .setup_github(new_package, checks[['gh_user']], checks[['PAT']], push)
    }

    ## 5. Wrap up info
    cli::cli_alert_info(cli::col_grey(
        "Review {.file {file.path(new_package, 'MIGRATION.md')}}: it lists what \\
        still needs to be done by hand."
    ))
    .wrap_up_info(new_package, .local)

    invisible(BiocBook(new_package))
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

## ---------------------------------------------------------------------------
## Conversion
## ---------------------------------------------------------------------------

.convert_bookdown <- function(bd, book, style, appendices = NULL, skip_availability = FALSE) {

    rules <- .bookdown_rules[["bookdown"]]
    if (identical(style, "msmbstyle")) rules <- c(rules, .bookdown_rules[["msmbstyle"]])
    counts <- structure(integer(0), names = character(0))
    count <- function(name, n = 1L) {
        if (n > 0) counts[name] <<- (if (is.na(counts[name])) 0L else counts[name]) + as.integer(n)
    }
    todo <- list()
    add_todo <- function(section, item) todo[[section]] <<- c(todo[[section]], item)

    pages <- bd$pages
    files <- names(pages)
    stems <- tools::file_path_sans_ext(basename(files))
    targets <- c("index.qmd", file.path("pages", paste0(stems[-1], ".qmd")))

    ## -- Book structure: `(PART)` and `(APPENDIX)` headers -------------------
    part_of <- rep(NA_character_, length(pages))
    appendix <- rep(FALSE, length(pages))
    current_part <- NA_character_
    in_appendix <- FALSE
    for (i in seq_along(pages)) {
        lines <- pages[[i]]
        code <- .code_lines(lines)
        is_part <- !code & grepl("^#\\s+\\(PART\\*?\\)", lines)
        is_appx <- !code & grepl("^#\\s+\\(APPENDIX\\)", lines)
        if (any(is_part)) {
            current_part <- .strip_header(sub("^#\\s+\\(PART\\*?\\)\\s*", "", lines[which(is_part)[1]]))
            count("`# (PART) X {-}` -> `part:` in `_book.yml`", sum(is_part))
        }
        if (any(is_appx)) {
            in_appendix <- TRUE
            count("`# (APPENDIX) X {-}` -> `appendices:` in `_book.yml`", sum(is_appx))
        }
        pages[[i]] <- lines[!(is_part | is_appx)]
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

    ## -- Labels referenced across the book ----------------------------------
    text <- unlist(lapply(pages, function(l) l[!.code_lines(l)]))
    text <- gsub("`+[^`]*`+", "", text)
    refs <- function(prefix) {
        m <- regmatches(text, gregexpr(sprintf("\\\\@ref\\(%s:([^)]+)\\)", prefix), text))
        unique(sub(sprintf("^\\\\@ref\\(%s:(.*)\\)$", prefix), "\\1", unlist(m)))
    }
    fig_refs <- refs("fig")
    tab_refs <- refs("tab")
    sec_refs <- unlist(regmatches(text, gregexpr("\\\\@ref\\(([^):]+)\\)", text)))
    sec_refs <- unique(sub("^\\\\@ref\\((.*)\\)$", "\\1", sec_refs))
    sec_rename <- sec_refs[!startsWith(sec_refs, "sec-")]

    ## -- Rewrite every page --------------------------------------------------
    for (i in seq_along(pages)) {
        lines <- pages[[i]]
        code <- .code_lines(lines)

        ## Text rules, outside code
        for (rule in rules) {
            hits <- which(!code & grepl(rule$pattern, lines, perl = isTRUE(rule$perl)))
            for (h in hits) {
                res <- .rewrite_text(lines[h], rule)
                lines[h] <- res$line
                count(rule$name, res$n)
            }
        }

        ## Header ids referenced as sections, e.g. `{#intro}` -> `{#sec-intro}`
        for (id in sec_rename) {
            pattern <- sprintf("(^#+ .*\\{[^}]*)#%s([ }])", .escape_regex(id))
            hits <- !code & grepl(pattern, lines)
            lines[hits] <- gsub(pattern, sprintf("\\1#sec-%s\\2", id), lines[hits])
            count("header id `{#x}` -> `{#sec-x}`", sum(hits))
        }

        ## Equation labels go after the closing `$$` of their display math
        eq <- .convert_equations(lines, code)
        lines <- eq$lines
        count("`(\\#eq:x)` -> `{#eq-x}`", eq$n)
        for (label in eq$unresolved) add_todo(
            "Equations to label by hand",
            sprintf("`%s`: no closing `$$` found after `(\\#eq:%s)`", targets[i], label)
        )

        ## Chunk options and labels
        ch <- .convert_chunks(lines, fig_refs, tab_refs)
        lines <- ch$lines
        for (name in names(ch$counts)) count(name, ch$counts[[name]])

        ## Rules producing several lines: split them, and tidy the blank lines
        pages[[i]] <- .tidy_blank_lines(.split_lines(lines))
    }

    ## Labels referenced but never found
    found <- function(prefix) unique(unlist(lapply(pages, function(l) {
        m <- regmatches(l, regexpr(sprintf("^\\s*```+\\s*\\{r[ ,]+%s-[^ ,}]+", prefix), l))
        sub(sprintf("^.*%s-", prefix), "", m)
    })))
    for (x in setdiff(fig_refs, found("fig"))) add_todo(
        "Unresolved cross-references", sprintf("`@fig-%s`: no chunk labelled `%s` found", x, x)
    )
    for (x in setdiff(tab_refs, found("tbl"))) add_todo(
        "Unresolved cross-references", sprintf("`@tbl-%s`: no chunk labelled `%s` found", x, x)
    )
    ids <- unique(unlist(lapply(pages, function(l) {
        attrs <- sub("^#+ [^{]*", "", l[!.code_lines(l) & grepl("^#+ ", l)])
        sub("^#", "", unlist(regmatches(attrs, gregexpr("#[A-Za-z][A-Za-z0-9_.:-]*", attrs))))
    })))
    for (x in sec_refs) {
        target <- if (startsWith(x, "sec-")) x else paste0("sec-", x)
        if (!target %in% ids) add_todo(
            "Unresolved cross-references",
            sprintf("`@%s`: no header with the id `{#%s}` found", target, target)
        )
    }
    if (length(tab_refs)) add_todo(
        "Tables",
        "Check the captions of the tables now labelled `tbl-*`: quarto takes them from the `tbl-cap` chunk option"
    )

    ## -- Shared R session ----------------------------------------------------
    new_session <- isTRUE(bd$config[["new_session"]]) || identical(bd$config[["new_session"]], "yes")
    setup <- character(0)
    if (!new_session) {
        setup <- .setup_calls(pages[[1]])
        script <- unlist(bd$config[["before_chapter_script"]])
        for (s in script) {
            f <- file.path(bd$path, s)
            if (file.exists(f)) setup <- c(setup, readLines(f, warn = FALSE, encoding = "UTF-8"))
        }
        if (length(setup)) {
            chunk <- c(
                "```{r}", "#| include: false",
                "## bookdown ran every chapter in a single R session, quarto runs",
                "## each in its own: setup repeated by BiocBook::from_bookdown()",
                setup, "```"
            )
            for (i in seq_along(pages)[-1]) pages[[i]] <- .insert_after_title(pages[[i]], chunk)
            count("setup calls of `index.Rmd` repeated at the top of each chapter", length(pages) - 1L)
        }
        add_todo("Shared R session", paste(
            "bookdown ran every chapter in a single R session, quarto renders each in",
            "its own:", if (length(setup)) "the setup calls of `index.Rmd` are now repeated at the top of each chapter, but",
            "objects created in a chapter and used in a later one must be recreated there.",
            "The first full render shows which."
        ))
    }
    if (length(bd$config[["after_chapter_script"]])) add_todo(
        "Unknown options",
        "`after_chapter_script` (`_bookdown.yml`) has no quarto equivalent"
    )

    ## -- Write pages ---------------------------------------------------------
    inst <- file.path(book, "inst")
    dirs <- .copy_bookdown_assets(bd, book)
    index_body <- .index_body(pages[[1]], dirs)
    count("asset paths of `index.Rmd` -> `pages/...`", attr(index_body, "n"))
    .write_index(file.path(inst, "index.qmd"), index_body)
    for (i in seq_along(pages)[-1]) {
        writeLines(pages[[i]], file.path(inst, targets[i]), useBytes = TRUE)
    }
    cli::cli_alert_success(cli::col_grey("Converted {length(pages)} page{?s}"))

    ## -- _book.yml, _format.yml ----------------------------------------------
    .write_book_yml(file.path(inst, "assets", "_book.yml"), bd$meta, targets, part_of, appendix)
    format_todo <- .write_format_yml(file.path(inst, "assets", "_format.yml"), bd, dirs)
    for (item in format_todo) add_todo("Unknown options", item)
    if (length(bd$config[["language"]])) add_todo(
        "Unknown options",
        "`language` (`_bookdown.yml`): translate the labels with quarto's `lang`/`language` options"
    )

    ## -- DESCRIPTION ---------------------------------------------------------
    deps <- .write_bookdown_description(book, bd)

    ## -- What still needs a human ---------------------------------------------
    scan <- .scan_converted(book, targets)
    for (section in names(scan)) for (item in scan[[section]]) add_todo(section, item)
    for (item in .github_only(bd)) add_todo("Dependencies", item)
    if (!skip_availability && length(deps)) {
        for (item in .not_on_bioc(deps)) add_todo("Dependencies", item)
    }
    add_todo("DESCRIPTION", c(
        "Replace the placeholder email of the maintainer (`cre`) in `Authors@R`",
        "Check `Title`, `Description` and `License`: the template's MIT licence may not be the book's"
    ))

    list(
        source = .bookdown_source(bd$path),
        style = style,
        format = bd$format,
        files = data.frame(from = files, to = file.path("inst", targets)),
        assets = attr(dirs, "copied"),
        rules = counts,
        deps = deps,
        setup = setup,
        todo = todo
    )
}

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

.tidy_blank_lines <- function(lines) {
    blank <- !nzchar(trimws(lines)) & !.code_lines(lines)
    keep <- !(blank & c(TRUE, utils::head(blank, -1)))
    lines <- lines[keep]
    while (length(lines) && !nzchar(trimws(lines[1]))) lines <- lines[-1]
    lines
}

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
.convert_chunks <- function(lines, fig_refs, tab_refs) {
    counts <- list()
    out <- character(0)
    code <- .code_lines(lines)
    header <- grepl("^\\s*```+\\s*\\{[rR][ ,}]", lines)
    for (i in seq_along(lines)) {
        line <- lines[i]
        extra <- character(0)
        if (header[i]) {
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

## Asset folders (e.g. `img/`) go next to the chapters, so that their relative
## paths keep working; bibliographies and CSS go to `inst/assets/`.
.copy_bookdown_assets <- function(bd, book) {
    skip <- "^(docs|lib|_book|_bookdown_files|_freeze|renv|packrat|rsconnect|libs|site_libs)$|_(cache|files)$|^\\."
    dirs <- list.dirs(bd$path, full.names = FALSE, recursive = FALSE)
    dirs <- dirs[!grepl(skip, dirs)]
    for (d in dirs) {
        file.copy(file.path(bd$path, d), file.path(book, "inst", "pages"), recursive = TRUE)
    }
    assets <- file.path(book, "inst", "assets")
    bibs <- list.files(bd$path, pattern = "\\.bib$")
    css <- as.character(unlist(bd$options[["css"]]))
    css <- css[file.exists(file.path(bd$path, css))]
    file.copy(file.path(bd$path, c(bibs, css)), assets, overwrite = TRUE)
    if (length(dirs)) cli::cli_alert_success(cli::col_grey(
        "Copied {.file {dirs}} to {.file {file.path(book, 'inst', 'pages')}}"
    ))
    structure(dirs, bibs = bibs, css = basename(css), copied = c(paste0(dirs, "/"), bibs, basename(css)))
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

## Title, Description, Authors@R and Imports, from the bookdown project and
## from the packages the converted pages use. Returns the dependencies added.
.write_bookdown_description <- function(book, bd) {
    d <- desc::desc(file = file.path(book, "DESCRIPTION"))
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

    ## Dependencies: the same scan as `check_deps()`, over the converted pages
    found <- renv::dependencies(file.path(book, "inst"), quiet = TRUE)$Package
    deps <- setdiff(sort(unique(found)), c(.bookdown_not_deps, book, d$get_deps()$package))
    for (pkg in deps) d$set_dep(pkg, "Imports")
    d$write()
    if (length(deps)) cli::cli_alert_success(cli::col_grey(
        "Added {length(deps)} dependenc{?y/ies} to {.file DESCRIPTION}: {.pkg {deps}}"
    ))
    deps
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

## How MIGRATION.md names the bookdown project: the URL of the git repository
## it comes from (without credentials), or its folder name. Never its local
## path, which would end up committed to the book's repository.
.bookdown_source <- function(path) {
    path <- normalizePath(path)
    folder <- sprintf("`%s`", basename(path))
    root <- tryCatch(normalizePath(gert::git_find(path)), error = function(e) NULL)
    if (is.null(root)) return(folder)
    remotes <- tryCatch(gert::git_remote_list(repo = root), error = function(e) NULL)
    url <- if (is.null(remotes)) NA_character_ else remotes$url[match("origin", remotes$name)]
    if (is.na(url) || !nzchar(url)) return(folder)
    url <- sub("^git@([^:/]+):", "https://\\1/", url)
    url <- sub("^(https?://)[^/@]*@", "\\1", url)
    url <- sub("\\.git$", "", url)
    if (identical(root, path)) return(sprintf("<%s>", url))
    sprintf("`%s` of <%s>", substring(path, nchar(root) + 2L), url)
}

.write_migration_report <- function(report, book) {
    rules <- if (length(report$rules)) {
        c("| Rule | Applied |", "|---|---:|", sprintf("| %s | %d |", names(report$rules), report$rules))
    } else "No rule applied."
    todo <- unlist(lapply(names(report$todo), function(section) c(
        paste("###", section), "", paste("- [ ]", report$todo[[section]]), ""
    )))
    lines <- c(
        "# Migration from bookdown",
        "",
        sprintf(
            "This book was converted from the bookdown project %s by `BiocBook::from_bookdown()` (BiocBook %s), with the `%s` style%s.",
            report$source, utils::packageVersion("BiocBook"), report$style,
            if (is.na(report$format)) "" else sprintf(" (output format: `%s`)", report$format)
        ),
        "",
        "## Converted files",
        "",
        "| bookdown | BiocBook |", "|---|---|",
        sprintf("| `%s` | `%s` |", report$files$from, report$files$to),
        "",
        if (length(report$assets)) c(
            sprintf("Assets copied: %s.", paste0("`", report$assets, "`", collapse = ", ")), ""
        ),
        "## Automatic rewrites",
        "",
        rules,
        "",
        if (length(report$deps)) c(
            sprintf("Dependencies added to `Imports`: %s.", paste0("`", report$deps, "`", collapse = ", ")), ""
        ),
        if (length(report$setup)) c(
            "Setup repeated at the top of each chapter:", "", "```r", report$setup, "```", ""
        ),
        "## To do by hand",
        "",
        if (length(todo)) todo else "Nothing left to do."
    )
    writeLines(lines, file.path(book, "MIGRATION.md"), useBytes = TRUE)
    rbuildignore <- file.path(book, ".Rbuildignore")
    writeLines(c(readLines(rbuildignore, warn = FALSE, encoding = "UTF-8"), "^MIGRATION\\.md$"), rbuildignore, useBytes = TRUE)
    cli::cli_alert_success(cli::col_grey(
        "Wrote the migration report to {.file {file.path(book, 'MIGRATION.md')}}"
    ))
    invisible(report)
}
