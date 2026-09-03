test_that("python chapters are wired for build-time execution", {

    tmpdir <- paste0(paste0(
        sample(LETTERS, 5, replace = TRUE),
        sample(c(seq(0, 9)), 5, replace = TRUE),
        collapse = ""
    ))

    init(tmpdir, .local = TRUE)
    bb <- BiocBook(tmpdir)

    expect_invisible(   add_python_chapter(bb, title = 'Py chapter', open = FALSE))
    expect_error(       add_python_chapter(bb, title = 'Bad', file = "bad", open = FALSE))

    ## `_book.yml` must stay valid YAML after appending chapters
    expect_no_error(    chapters(bb))
    expect_true(        "pages/py-chapter.qmd" %in% chapters(bb))

    page <- readLines(file.path(tmpdir, "inst", "pages", "py-chapter.qmd"))

    ## Nothing is frozen: the book is meant to be re-executed on every render
    expect_false(       any(grepl("freeze", page)))

    ## An R chunk must be present, since it is what binds the page to the knitr
    ## engine (and therefore reticulate) rather than to a Jupyter kernel
    expect_true(        any(grepl("^```\\{r\\}", page)))
    expect_true(        any(grepl("^```\\{python\\}", page)))
    expect_true(        any(grepl("setup_python\\(\\)", page)))

    ## Later chapters re-activate the environment instead of rebuilding it
    add_python_chapter(bb, title = 'Second py', setup = FALSE, open = FALSE)
    page2 <- readLines(file.path(tmpdir, "inst", "pages", "second-py.qmd"))
    expect_false(       any(grepl("setup_python", page2)))
    expect_true(        any(grepl("use_virtualenv", page2)))

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
