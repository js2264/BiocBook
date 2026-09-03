test_that("python chapters are created, detected and checked", {

    tmpdir <- paste0(paste0(
        sample(LETTERS, 5, replace = TRUE),
        sample(c(seq(0, 9)), 5, replace = TRUE),
        collapse = ""
    ))

    init(tmpdir, .local = TRUE)
    bb <- BiocBook(tmpdir)

    ## A book with no python page at all is trivially fine
    expect_invisible(   res <- check_freeze(bb))
    expect_equal(       nrow(res), 0L)

    ## add_python_chapter() creates a frozen page for both engines
    expect_invisible(   add_python_chapter(bb, title = 'Py chapter', open = FALSE))
    expect_invisible(   add_python_chapter(
        bb, title = 'Jupyter chapter', engine = "jupyter", open = FALSE
    ))
    expect_error(       add_python_chapter(
        bb, title = 'Bad', file = "bad", open = FALSE
    ))

    ## `_book.yml` must stay valid YAML after appending chapters
    expect_no_error(    chapters(bb))
    expect_true(        "pages/py-chapter.qmd" %in% chapters(bb))
    expect_true(        "pages/jupyter-chapter.qmd" %in% chapters(bb))

    reticulate_page <- file.path(tmpdir, "inst", "pages", "py-chapter.qmd")
    jupyter_page <- file.path(tmpdir, "inst", "pages", "jupyter-chapter.qmd")
    expect_true(        BiocBook:::.page_uses_python(reticulate_page))
    expect_true(        BiocBook:::.page_is_frozen(reticulate_page))
    expect_true(        any(grepl(
        "engine: jupyter", readLines(jupyter_page)
    )))

    ## Neither page has been executed yet, so both lack a frozen result
    res <- check_freeze(bb)
    expect_equal(       nrow(res), 2L)
    expect_true(        all(res$frozen))
    expect_false(       any(res$has_result))

    ## Chunks that are shown but never evaluated need no python and no freeze
    doc_only <- file.path(tmpdir, "inst", "pages", "doc-only.qmd")
    writeLines(c(
        "# Doc only", "", "```{python}", "#| eval: false", "1 + 1", "```"
    ), doc_only)
    expect_false(       BiocBook:::.page_uses_python(doc_only))

    ## Python chunks inside an HTML comment are documentation, not code
    commented <- file.path(tmpdir, "inst", "pages", "commented.qmd")
    writeLines(c(
        "# Commented", "", "<!--", "```{python}", "1 + 1", "```", "-->"
    ), commented)
    expect_false(       BiocBook:::.page_uses_python(commented))

    ## An unfrozen python page is reported as such
    loose <- file.path(tmpdir, "inst", "pages", "loose.qmd")
    writeLines(c("# Loose", "", "```{python}", "1 + 1", "```"), loose)
    res <- check_freeze(bb)
    expect_true(        "pages/loose.qmd" %in% res$page)
    expect_false(       res$frozen[res$page == "pages/loose.qmd"])

    unlink(tmpdir, recursive = TRUE, force = TRUE)

})

test_that("setup_python() finds the book's requirements.txt", {

    tmpdir <- file.path(tempdir(), "reqbook")
    dir.create(file.path(tmpdir, "inst", "pages"), recursive = TRUE)
    writeLines("project:\n  type: book", file.path(tmpdir, "inst", "_quarto.yml"))

    ## No requirements.txt yet
    expect_null(        BiocBook:::.find_requirements(
        file.path(tmpdir, "inst", "pages")
    ))

    writeLines("numpy==1.26.4", file.path(tmpdir, "inst", "requirements.txt"))

    ## Found from the page folder (where quarto runs) and from the book root
    expect_equal(
        normalizePath(BiocBook:::.find_requirements(
            file.path(tmpdir, "inst", "pages")
        )),
        normalizePath(file.path(tmpdir, "inst", "requirements.txt"))
    )
    expect_equal(
        normalizePath(BiocBook:::.find_requirements(file.path(tmpdir, "inst"))),
        normalizePath(file.path(tmpdir, "inst", "requirements.txt"))
    )

    unlink(tmpdir, recursive = TRUE, force = TRUE)

})
