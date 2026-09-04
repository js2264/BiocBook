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
    expect_true(        file.path(getwd(), tmpdir, "inst", "pages", "py-chapter.qmd") %in% chapters(bb))

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
    expect_true(       any(grepl("setup_python", page2)))

    unlink(tmpdir, recursive = TRUE, force = TRUE)

})

test_that("setup_python() finds the book's requirements.yml", {

    tmpdir <- file.path(tempdir(), "reqbook")
    dir.create(file.path(tmpdir, "inst", "pages"), recursive = TRUE)
    writeLines("project:\n  type: book", file.path(tmpdir, "inst", "_quarto.yml"))

    ## No requirements.yml yet
    expect_null(        BiocBook:::.find_requirements(
        file.path(tmpdir, "inst", "pages")
    ))

    writeLines(c("name:", "    BiocBook", "dependencies:", "    - numpy=1.26"),
               file.path(tmpdir, "inst", "requirements.yml"))

    ## Found from the page folder (where quarto runs) and from the book root
    expect_equal(
        normalizePath(BiocBook:::.find_requirements(
            file.path(tmpdir, "inst", "pages")
        )),
        normalizePath(file.path(tmpdir, "inst", "requirements.yml"))
    )
    expect_equal(
        normalizePath(BiocBook:::.find_requirements(file.path(tmpdir, "inst"))),
        normalizePath(file.path(tmpdir, "inst", "requirements.yml"))
    )

    unlink(tmpdir, recursive = TRUE, force = TRUE)

})

test_that("micromamba() resolves a binary without downloading when one exists", {

    ## An explicit RETICULATE_CONDA always wins
    fake <- tempfile(); file.create(fake)
    withr <- Sys.getenv("RETICULATE_CONDA", unset = NA)
    Sys.setenv(RETICULATE_CONDA = fake)
    on.exit({
        if (is.na(withr)) Sys.unsetenv("RETICULATE_CONDA")
        else Sys.setenv(RETICULATE_CONDA = withr)
        unlink(fake)
    }, add = TRUE)
    expect_equal(       micromamba(), fake)

})

test_that("the pinned micromamba release is fully specified", {

    ## Every platform BiocBook claims to support needs a checksum, or the
    ## download could not be verified there.
    sums <- BiocBook:::.micromamba_sha256
    expect_setequal(
        names(sums),
        c("linux-64", "linux-aarch64", "linux-ppc64le",
          "osx-64", "osx-arm64", "win-64")
    )
    expect_true(        all(nchar(sums) == 64L))
    expect_true(        all(grepl("^[0-9a-f]{64}$", sums)))
    expect_match(       BiocBook:::.micromamba_version, "^[0-9.]+-[0-9]+$")

})
