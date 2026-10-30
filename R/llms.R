#' @rdname BiocBook-llms
#' @name BiocBook-llms
#' @title Making a BiocBook readable by AI assistants
#'
#' @description
#'
#' Every `BiocBook` serves an `/llms.txt` file, following the
#' [llms.txt proposal](https://llmstxt.org/): a Markdown index of the book
#' that AI assistants can read instead of crawling its HTML pages.
#'
#' - With `quarto` >= 1.11, `quarto` itself writes `llms.txt` and a Markdown
#'   copy of every page (`<page>.llms.md`, next to `<page>.html`), when the
#'   book's `llms-txt` option is on. Books created from template >= 1.1.0
#'   switch it on through the `llms` `quarto` profile
#'   (`inst/_quarto-llms.yml`), which `vignettes/Makefile` activates only when
#'   the installed `quarto` supports it: older versions reject the option and
#'   would otherwise fail to render the book.
#' - `enrich_llms_txt()` then adds what only `BiocBook` knows: the book
#'   package and its version, how to install it, its `Docker` image, its
#'   `python` environment and where the other versions of the book live. Books
#'   created from template >= 1.1.0 run it after every render, from
#'   `inst/assets/post-render.R`. With an older `quarto`, which writes no
#'   `llms.txt` for books, it writes a minimal one listing the book chapters.
#'
#' `enrich_llms_txt()` can be run any number of times: the `BiocBook` context
#' is kept between `<!-- biocbook:start -->` and `<!-- biocbook:end -->`
#' markers, and replaced rather than duplicated. It also tidies two quirks of
#' `quarto`'s output: chapter numbers rendered as raw `pandoc` spans in
#' `llms.txt` links, and code blocks labelled `downlit` (by `code-link: true`)
#' rather than `r` in the Markdown copies of the pages.
#'
#' @section Markdown copies of the pages:
#'
#' `enrich_llms_txt()` renames the Markdown copy of each page from
#' `<page>.llms.md` to `<page>.md`, so that a page's Markdown lives at its URL
#' with `.md` in place of `.html` (e.g. `pages/intro.md` for
#' `pages/intro.html`). The links of `llms.txt`, and those between the copies,
#' follow. It then links every page to its copy:
#'
#' - in the page's `<head>`, with
#'   `<link rel="alternate" type="text/markdown" href="intro.md">`, which is
#'   how AI assistants find the Markdown version of a page;
#' - under the page's title, with a "View as Markdown" link, for readers. To
#'   hide it, add `.biocbook-md-link { display: none; }` to the book's
#'   `inst/assets/book.scss`.
#'
#' Unless a site holds a `.nojekyll` file, GitHub Pages runs `Jekyll` on it,
#' which turns every `.md` file into an HTML page of the same name: the
#' rendering of `<page>.md` would then collide with the book's own
#' `<page>.html`. The `biocbook` workflow of books created from template 1.1.3
#' or later writes `.nojekyll` at the root of the site. In older books, add
#' `touch llms-index/.nojekyll` to the "Index the llms.txt of every deployed
#' version" step of `.github/workflows/biocbook.yml`, right after
#' `mkdir -p llms-index`.
#'
#' @param output_dir Folder holding the rendered book. In a post-render
#'   script, `quarto` provides it as the `QUARTO_PROJECT_OUTPUT_DIR`
#'   environment variable.
#' @param project_dir The book's `quarto` project folder, i.e. the `inst/`
#'   folder holding `_quarto.yml`. Post-render scripts run from there, hence
#'   the default.
#'
#' @return The path to `llms.txt`, invisibly.
#'
#' @examples
#' ## In practice, `enrich_llms_txt()` runs after every render of the book,
#' ## from `inst/assets/post-render.R`
#' unique_id <- as.numeric(Sys.time())
#' bookname <- paste(Sys.info()[['sysname']], unique_id, sep = '.')
#' quick_init(bookname, user = "dummy")
#' docs <- file.path(bookname, "inst", "docs")
#' dir.create(docs)
#' enrich_llms_txt(docs, project_dir = file.path(bookname, "inst"))
#' cat(readLines(file.path(docs, "llms.txt")), sep = "\n")
#' unlink(bookname, recursive = TRUE)
NULL

#' @rdname BiocBook-llms
#' @export

enrich_llms_txt <- function(output_dir = "docs", project_dir = ".") {

    if (!dir.exists(output_dir)) cli::cli_abort(
        "Could not find the rendered book in {.file {output_dir}}."
    )
    info <- .llms_book_info(project_dir)
    block <- c(.llms_marker[["start"]], .llms_context(info), .llms_marker[["end"]])
    llms_txt <- file.path(output_dir, "llms.txt")

    if (file.exists(llms_txt)) {
        lines <- .llms_insert_block(readLines(llms_txt, warn = FALSE, encoding = "UTF-8"), block, info)
        lines <- .llms_twin_links(.llms_tidy_titles(lines))
        .llms_tidy_pages(output_dir)
        twins <- .llms_rename_twins(output_dir)
        .llms_link_twins(twins)
        msg <- "Added BiocBook context to {.file {llms_txt}}"
    }
    else {
        ## `quarto` < 1.11 writes no llms.txt for books: write a minimal one,
        ## pointing at the HTML pages since there is no Markdown copy of them
        lines <- c(
            paste("#", info$title), "",
            if (length(info$summary)) c(paste(">", info$summary), ""),
            block, "",
            .llms_chapters(info, project_dir)
        )
        msg <- "Wrote a minimal {.file {llms_txt}} (no `quarto` >= 1.11 output found)"
    }
    writeLines(lines, llms_txt, useBytes = TRUE)
    cli::cli_alert_success(cli::col_grey(msg))

    invisible(llms_txt)
}

.llms_marker <- c(start = "<!-- biocbook:start -->", end = "<!-- biocbook:end -->")

## Everything `enrich_llms_txt()` needs to know about the book, read from the
## book package (`../DESCRIPTION`, relative to the quarto project), its
## `quarto` configuration and its `requirements.yml`. Anything missing is left
## out rather than guessed.
.llms_book_info <- function(project_dir) {

    desc_f <- file.path(project_dir, "..", "DESCRIPTION")
    desc <- if (file.exists(desc_f)) as.list(read.dcf(desc_f)[1, ]) else list()
    oneline <- function(x) if (is.null(x)) NULL else trimws(gsub("\\s+", " ", x))

    urls <- if (is.null(desc$URL)) character(0) else trimws(strsplit(desc$URL, ",")[[1]])
    gh <- regmatches(urls, regexec("github\\.com/([^/[:space:]]+)/([^/[:space:]#?]+)", urls))
    on_gh <- lengths(gh) > 0
    owner <- repo <- NULL
    if (any(on_gh)) {
        owner <- gh[on_gh][[1]][2]
        repo <- sub("\\.git$", "", gh[on_gh][[1]][3])
    }

    book <- .read_book_config(project_dir)

    ## Only mention python if the book runs any: the template ships a
    ## requirements.yml whether or not a page uses it
    req_f <- file.path(project_dir, "requirements.yml")
    pages <- list.files(project_dir, pattern = "\\.(qmd|Rmd|md)$", recursive = TRUE)
    pages <- pages[!grepl("^(docs|_book)/|\\.llms\\.md$", pages)]
    has_python <- any(vapply(file.path(project_dir, pages), function(f) {
        any(grepl("^\\s*```\\s*\\{python", readLines(f, warn = FALSE, encoding = "UTF-8")))
    }, logical(1)))
    python <- NULL
    if (has_python && file.exists(req_f)) {
        reqs <- .read_requirements(req_f)
        python <- list(conda = reqs$conda, pip = reqs$pip)
    }

    list(
        package = desc$Package,
        version = desc$Version,
        title = oneline(book$title %||% desc$Title %||% desc$Package %||% "Untitled"),
        summary = oneline(book$description %||% desc$Description),
        url = if (any(on_gh)) urls[on_gh][1] else if (length(urls)) urls[1] else NULL,
        owner = owner,
        repo = repo,
        bioc = .bioc_version(),
        python = python,
        chapters = book$chapters,
        appendices = book$appendices
    )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

## `book:` options may sit in `_quarto.yml` or in any of its `metadata-files`
## (`assets/_book.yml` in a BiocBook); the last one read wins.
.read_book_config <- function(project_dir) {
    quarto_yml <- file.path(project_dir, "_quarto.yml")
    if (!file.exists(quarto_yml)) return(list())
    config <- yaml::read_yaml(quarto_yml, readLines.warn = FALSE)
    book <- config[["book"]] %||% list()
    for (f in config[["metadata-files"]]) {
        f <- file.path(project_dir, f)
        if (!file.exists(f)) next
        extra <- yaml::read_yaml(f, readLines.warn = FALSE)[["book"]]
        book[names(extra)] <- extra
    }
    book
}

.bioc_version <- function() {
    v <- tryCatch(utils::packageVersion("BiocVersion"), error = function(e) NULL)
    if (!is.null(v)) return(paste(unlist(v)[1:2], collapse = "."))
    for (var in c("BIOCONDUCTOR_VERSION", "BIOCONDUCTOR_DOCKER_VERSION")) {
        v <- Sys.getenv(var)
        if (grepl("^[0-9]+\\.[0-9]+", v)) return(sub("^([0-9]+\\.[0-9]+).*", "\\1", v))
    }
    NULL
}

## The llms.txt proposal allows no headings between the summary and the first
## `##` section, so the context is a plain list.
.llms_context <- function(info) {
    code <- function(x) paste0("`", x, "`")
    lines <- character(0)
    if (!is.null(info$package)) {
        lines <- c(lines, paste0(
            "- Book package: ", code(info$package),
            if (!is.null(info$version)) paste0(" ", info$version),
            if (!is.null(info$bioc)) paste0(" (Bioconductor ", info$bioc, ")"),
            if (!is.null(info$url)) paste0(", source: <", info$url, ">")
        ))
        lines <- c(lines, paste0(
            "- Install: ", code(sprintf('BiocManager::install("%s")', info$package)),
            if (!is.null(info$owner)) paste0(
                " (Bioconductor) or ",
                code(sprintf('BiocManager::install("%s/%s")', info$owner, info$repo)),
                " (GitHub)"
            )
        ))
        if (!is.null(info$owner)) lines <- c(lines, paste0(
            "- Docker image with every dependency: ",
            code(sprintf(
                "docker run -it ghcr.io/%s/%s:%s R",
                tolower(info$owner), tolower(info$package), info$version %||% "devel"
            ))
        ))
    }
    if (length(info$python$conda)) lines <- c(lines, paste0(
        "- Python environment (conda): ",
        paste(code(info$python$conda), collapse = ", "),
        if (length(info$python$pip)) paste0(
            "; from PyPI: ", paste(code(info$python$pip), collapse = ", ")
        ),
        ", provisioned by ", code("BiocBook::setup_python()"),
        " from ", code("inst/requirements.yml")
    ))
    if (!is.null(info$owner)) {
        root <- sprintf("https://%s.github.io/%s", info$owner, info$repo)
        lines <- c(lines, paste0(
            "- Other versions of this book: one per Bioconductor release, at ",
            code(paste0(root, "/<version>/")),
            " (e.g. <", root, "/devel/>), indexed in <", root, "/llms.txt>"
        ))
    }
    lines
}

## Put the context right after the title and summary blockquote, replacing
## any previous copy of it. A summary is added from DESCRIPTION if the book
## declares none.
.llms_insert_block <- function(lines, block, info) {
    start <- which(lines == .llms_marker[["start"]])
    end <- which(lines == .llms_marker[["end"]])
    if (length(start) && length(end) && end[1] > start[1]) {
        lines <- lines[-seq(start[1], end[1])]
    }
    h1 <- grep("^# ", lines)[1]
    if (is.na(h1)) {
        lines <- c(paste("#", info$title), "", lines)
        h1 <- 1L
    }
    i <- h1
    while (i < length(lines) && !nzchar(trimws(lines[i + 1]))) i <- i + 1
    if (i < length(lines) && grepl("^>", lines[i + 1])) {
        while (i < length(lines) && grepl("^>", lines[i + 1])) i <- i + 1
    }
    else if (length(info$summary)) {
        lines <- append(lines, c("", paste(">", info$summary)), after = h1)
        i <- h1 + 2L
    }
    lines <- append(lines, c("", block), after = i)
    ## Collapse the blank lines left around the block by a previous run
    keep <- !(lines == "" & c(FALSE, utils::head(lines, -1) == ""))
    lines[keep]
}

## quarto writes book chapter titles with their numbering spans, e.g.
## `[[1]{.chapter-number}  [Intro]{.chapter-title}](pages/intro.llms.md)`
## (separated by a non-breaking space), and wraps the titles of chapters with
## an id in one more span, e.g. `[[2 Intro]{#sec-intro .quarto-section-identifier}](...)`
.llms_tidy_titles <- function(lines) {
    lines <- gsub(
        "\\[([^][]*)\\]\\{\\.chapter-number\\}[^][]*\\[([^][]*)\\]\\{\\.chapter-title\\}",
        "\\1 \\2",
        lines
    )
    gsub("\\[\\[([^][]*)\\]\\{[^}]*\\}\\]\\(", "[\\1](", lines)
}

## With `code-link: true`, R code blocks carry a `downlit` class, which
## quarto's Markdown conversion takes for their language.
.llms_tidy_pages <- function(output_dir) {
    pages <- list.files(output_dir, pattern = "\\.llms\\.md$", recursive = TRUE, full.names = TRUE)
    for (page in pages) {
        lines <- readLines(page, warn = FALSE, encoding = "UTF-8")
        fixed <- sub("^((>\\s?)*\\s*```+)\\s*downlit\\s*$", "\\1 r", lines)
        if (!identical(fixed, lines)) writeLines(fixed, page, useBytes = TRUE)
    }
    invisible(length(pages))
}

## quarto writes the Markdown copy of `<page>.html` as `<page>.llms.md`.
## Renamed `<page>.md`, it lives at the page's URL with `.md` in place of
## `.html`. A copy left by a previous render is replaced: quarto only writes
## `<page>.llms.md` for the pages it has just rendered.
.llms_rename_twins <- function(output_dir) {
    olds <- list.files(output_dir, pattern = "\\.llms\\.md$", recursive = TRUE, full.names = TRUE)
    news <- sub("\\.llms\\.md$", ".md", olds)
    for (i in seq_along(olds)) {
        lines <- readLines(olds[i], warn = FALSE, encoding = "UTF-8")
        writeLines(.llms_twin_links(lines), news[i], useBytes = TRUE)
        unlink(olds[i])
    }
    invisible(news)
}

## Relative links to `<page>.llms.md` (with or without an anchor) point at
## `<page>.md` instead. URLs are left alone: their scheme has a colon.
.llms_twin_links <- function(lines) {
    gsub(
        "(\\]\\(<?[^):#\\s>]*)\\.llms\\.md(?=[#)>\\s])",
        "\\1.md",
        lines,
        perl = TRUE
    )
}

## Every HTML page with a Markdown copy links to it: in its `<head>`, as an
## alternate version for AI assistants, and under its title, for readers.
## Pages that already link to it are left as they are.
.llms_link_twins <- function(twins) {
    for (md in twins) {
        html_f <- sub("\\.md$", ".html", md)
        if (!file.exists(html_f)) next
        html <- paste(readLines(html_f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
        href <- basename(md)
        linked <- html
        if (!grepl('rel="alternate" type="text/markdown"', linked, fixed = TRUE)) {
            alternate <- sprintf('<link rel="alternate" type="text/markdown" href="%s">', href)
            linked <- sub("</head>", paste0(alternate, "\n</head>"), linked, fixed = TRUE)
        }
        if (!grepl("biocbook-md-link", linked, fixed = TRUE)) {
            link <- sprintf(paste0(
                '<p class="biocbook-md-link small"><a href="%s" type="text/markdown">',
                '<i class="bi bi-markdown" aria-hidden="true"></i> View as Markdown</a></p>'
            ), href)
            ## Right after the title block, else at the top of the page content.
            ## The navbar comes first and has a `</header>` of its own.
            if (grepl('id="title-block-header"', linked, fixed = TRUE)) {
                linked <- sub('(?s)(id="title-block-header".*?</header>)', paste0("\\1", link), linked, perl = TRUE)
            }
            else {
                linked <- sub('(<main[^>]*id="quarto-document-content"[^>]*>)', paste0("\\1", link), linked, perl = TRUE)
            }
        }
        if (!identical(linked, html)) writeLines(linked, html_f, useBytes = TRUE)
    }
    invisible(twins)
}

## Chapter list for a minimal llms.txt, following `_book.yml`: parts become
## nested lists, appendices their own section.
.llms_chapters <- function(info, project_dir) {
    entry <- function(f, indent = "") {
        title <- if (identical(f, "index.qmd")) info$title else .page_title(file.path(project_dir, f))
        paste0(indent, "- [", title, "](", sub("\\.(qmd|Rmd|md|ipynb)$", ".html", f), ")")
    }
    entries <- function(items) unlist(lapply(items, function(item) {
        if (is.character(item)) return(entry(item))
        part <- item[["part"]]
        if (is.null(part)) return(NULL)
        if (grepl("\\.(qmd|Rmd|md)$", part)) part <- .page_title(file.path(project_dir, part))
        c(paste("-", part), vapply(unlist(item[["chapters"]]), entry, character(1), indent = "  "))
    }))
    c(
        "## Chapters", "", entries(info$chapters),
        if (length(info$appendices)) c("", "## Appendices", "", entries(info$appendices))
    )
}

## Title of a page: its first level-1 heading outside code and front matter,
## else its `title:`, else its file name.
.page_title <- function(f) {
    if (!file.exists(f)) return(tools::file_path_sans_ext(basename(f)))
    lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
    yaml_end <- 0L
    if (length(lines) && grepl("^---\\s*$", lines[1])) {
        yaml_end <- which(grepl("^---\\s*$", lines))[2]
        if (is.na(yaml_end)) yaml_end <- 0L
    }
    in_code <- cumsum(grepl("^\\s*```", lines)) %% 2 == 1
    heading <- grep("^# ", lines[!in_code & seq_along(lines) > yaml_end], value = TRUE)[1]
    if (!is.na(heading)) return(trimws(sub("\\s*\\{[^}]*\\}\\s*$", "", sub("^# ", "", heading))))
    if (yaml_end > 2) {
        title <- yaml::yaml.load(paste(lines[2:(yaml_end - 1)], collapse = "\n"))[["title"]]
        if (!is.null(title)) return(title)
    }
    tools::file_path_sans_ext(basename(f))
}
