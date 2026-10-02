## A throwaway book with a fake rendered output folder
.llms_fixture <- function() {
    book <- paste0("LLMS", paste(sample(LETTERS, 6, replace = TRUE), collapse = ""))
    quick_init(book, user = "dummy")
    bb <- BiocBook(book)
    add_chapter(bb, title = "First chapter", open = FALSE)
    add_python_chapter(bb, title = "Py chapter", open = FALSE)
    writeLines(
        c("dependencies:", "    - python=3.12", "    - numpy=1.26"),
        file.path(book, "inst", "requirements.yml")
    )
    docs <- file.path(book, "inst", "docs")
    dir.create(file.path(docs, "pages"), recursive = TRUE)
    list(book = book, docs = docs, project = file.path(book, "inst"))
}

test_that("enrich_llms_txt() writes a minimal llms.txt for older quarto", {

    fx <- .llms_fixture()
    on.exit(unlink(fx$book, recursive = TRUE, force = TRUE), add = TRUE)

    out <- suppressMessages(enrich_llms_txt(fx$docs, project_dir = fx$project))
    expect_identical(   out, file.path(fx$docs, "llms.txt"))
    llms <- readLines(out)

    ## Title, then a summary, then the BiocBook context, then chapters
    expect_identical(   llms[1], paste("#", fx$book))
    expect_match(       llms[3], "^> This sentence is a placeholder")
    start <- which(llms == "<!-- biocbook:start -->")
    end <- which(llms == "<!-- biocbook:end -->")
    expect_length(      start, 1L)
    expect_true(        end > start)
    block <- llms[seq(start + 1, end - 1)]
    expect_true(        any(grepl(sprintf("^- Book package: `%s` 0.98.0", fx$book), block)))
    expect_true(        any(grepl(sprintf("source: <https://github.com/dummy/%s>", fx$book), block, fixed = TRUE)))
    expect_true(        any(grepl(sprintf('BiocManager::install\\("%s"\\)', fx$book), block)))
    expect_true(        any(grepl(sprintf('BiocManager::install\\("dummy/%s"\\)', fx$book), block)))
    expect_true(        any(grepl(sprintf("ghcr.io/dummy/%s:0.98.0", tolower(fx$book)), block, fixed = TRUE)))
    expect_true(        any(grepl("`python=3.12`, `numpy=1.26`", block, fixed = TRUE)))
    expect_true(        any(grepl(sprintf("https://dummy.github.io/%s/devel/", fx$book), block, fixed = TRUE)))

    ## Chapters point at the HTML pages, which is all older quarto renders
    chapters <- llms[seq(which(llms == "## Chapters"), length(llms))]
    expect_true(        sprintf("- [%s](index.html)", fx$book) %in% chapters)
    expect_true(        "- [First chapter](pages/first-chapter.html)" %in% chapters)
    expect_true(        "- [Py chapter](pages/py-chapter.html)" %in% chapters)

})

test_that("enrich_llms_txt() adds the BiocBook context to quarto's llms.txt", {

    fx <- .llms_fixture()
    on.exit(unlink(fx$book, recursive = TRUE, force = TRUE), add = TRUE)

    ## As written by quarto 1.11
    writeLines(c(
        paste("#", fx$book),
        "",
        "## Pages",
        "",
        sprintf("- [%s](index.llms.md)", fx$book),
        "- [[1]{.chapter-number}\u00a0 [First chapter]{.chapter-title}](pages/first-chapter.llms.md)",
        ""
    ), file.path(fx$docs, "llms.txt"))
    writeLines(
        c("# 1  First chapter", "", "``` downlit", "plot(1:10)", "```", "",
          "> **NOTE:**", ">", "> ``` downlit", "> x <- 1", "> ```", "", "``` python", "x = 1", "```"),
        file.path(fx$docs, "pages", "first-chapter.llms.md")
    )

    suppressMessages(enrich_llms_txt(fx$docs, project_dir = fx$project))
    llms <- readLines(file.path(fx$docs, "llms.txt"))
    expect_identical(   llms[1], paste("#", fx$book))
    expect_match(       llms[3], "^> This sentence is a placeholder")
    expect_identical(   llms[5], "<!-- biocbook:start -->")
    expect_true(        "## Pages" %in% llms)

    ## Chapter numbering spans are flattened
    expect_true(        "- [1 First chapter](pages/first-chapter.llms.md)" %in% llms)
    expect_false(       any(grepl("chapter-number", llms)))

    ## R code blocks are labelled as such, other languages are left alone
    page <- readLines(file.path(fx$docs, "pages", "first-chapter.llms.md"))
    expect_false(       any(grepl("downlit", page)))
    expect_true(        "``` r" %in% page)
    expect_true(        "> ``` r" %in% page)
    expect_true(        "``` python" %in% page)

    ## Running it again changes nothing
    suppressMessages(enrich_llms_txt(fx$docs, project_dir = fx$project))
    expect_identical(   readLines(file.path(fx$docs, "llms.txt")), llms)
    expect_length(      which(readLines(file.path(fx$docs, "llms.txt")) == "<!-- biocbook:start -->"), 1L)

})

test_that("enrich_llms_txt() keeps a summary quarto already wrote", {

    fx <- .llms_fixture()
    on.exit(unlink(fx$book, recursive = TRUE, force = TRUE), add = TRUE)
    writeLines(
        c("# A book", "", "> What the book is about.", "", "## Pages", "", "- [A book](index.llms.md)"),
        file.path(fx$docs, "llms.txt")
    )
    suppressMessages(enrich_llms_txt(fx$docs, project_dir = fx$project))
    llms <- readLines(file.path(fx$docs, "llms.txt"))
    expect_identical(   llms[1:5], c("# A book", "", "> What the book is about.", "", "<!-- biocbook:start -->"))
    expect_length(      grep("^>", llms), 1L)

})

test_that("enrich_llms_txt() only mentions python when the book runs some", {

    book <- paste0("LLMS", paste(sample(LETTERS, 6, replace = TRUE), collapse = ""))
    quick_init(book, user = "dummy")
    on.exit(unlink(book, recursive = TRUE, force = TRUE), add = TRUE)
    docs <- file.path(book, "inst", "docs")
    dir.create(docs)
    suppressMessages(enrich_llms_txt(docs, project_dir = file.path(book, "inst")))
    expect_false(       any(grepl("Python environment", readLines(file.path(docs, "llms.txt")))))
    expect_error(       enrich_llms_txt(file.path(book, "nope"), project_dir = file.path(book, "inst")))

})
