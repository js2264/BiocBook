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
    add_python_chapter(bb, title = 'Second py', open = FALSE)
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

test_that("conda environments are keyed on the contents of requirements.yml", {

    req <- tempfile(fileext = ".yml")
    on.exit(unlink(req), add = TRUE)
    writeLines(c("name:", "    BiocBook", "dependencies:", "    - python=3.12"), req)

    ## The same file always gives the same environment...
    p1 <- BiocBook:::.book_env_prefix("BiocBook", req)
    expect_identical(   BiocBook:::.book_env_prefix("BiocBook", req), p1)
    expect_match(       basename(p1), "^BiocBook-[0-9a-f]{12}$")

    ## ... and a one-character edit gives a new one
    writeLines(c("name:", "    BiocBook", "dependencies:", "    - python=3.11"), req)
    p2 <- BiocBook:::.book_env_prefix("BiocBook", req)
    expect_false(       identical(p1, p2))
    expect_identical(   dirname(p1), dirname(p2))

    ## Without a requirements file, the name alone is used
    expect_identical(   basename(BiocBook:::.book_env_prefix("BiocBook")), "BiocBook")

})

test_that("setup_python() leaves an already configured python alone", {

    ## The book's Docker image sets RETICULATE_PYTHON: nothing must be
    ## provisioned, whatever requirements.yml says
    fake <- tempfile(); file.create(fake)
    old <- Sys.getenv("RETICULATE_PYTHON", unset = NA)
    Sys.setenv(RETICULATE_PYTHON = fake)
    on.exit({
        if (is.na(old)) Sys.unsetenv("RETICULATE_PYTHON")
        else Sys.setenv(RETICULATE_PYTHON = old)
        unlink(fake)
    }, add = TRUE)
    expect_identical(   suppressMessages(setup_python(conda = "/nonexistent")), fake)

})

test_that("python_envs() lists and removes cached environments", {

    cache <- tempfile("cache")
    old <- Sys.getenv("R_USER_CACHE_DIR", unset = NA)
    Sys.setenv(R_USER_CACHE_DIR = cache)
    on.exit({
        if (is.na(old)) Sys.unsetenv("R_USER_CACHE_DIR")
        else Sys.setenv(R_USER_CACHE_DIR = old)
        unlink(cache, recursive = TRUE)
    }, add = TRUE)

    expect_identical(   nrow(python_envs()), 0L)

    envs <- file.path(BiocBook:::.book_envs_root(),
        c("BiocBook-0123456789ab", "BiocBook-ba9876543210", "OHCA-0123456789ab"))
    for (env in envs) {
        dir.create(file.path(env, "conda-meta"), recursive = TRUE)
        writeLines("==> 2026-10-02 <==", file.path(env, "conda-meta", "history"))
    }
    listed <- python_envs()
    expect_setequal(    listed$name, basename(envs))
    expect_named(       listed, c("name", "path", "size_mb", "modified"))
    expect_s3_class(    listed$modified, "POSIXct")

    ## Unknown names are a no-op
    expect_identical(   nrow(suppressMessages(python_envs(remove = "nope"))), 0L)
    expect_identical(   nrow(python_envs()), 3L)

    ## A bare `name:` matches every generation of that environment
    removed <- suppressMessages(python_envs(remove = "BiocBook"))
    expect_setequal(    removed$name, c("BiocBook-0123456789ab", "BiocBook-ba9876543210"))
    expect_identical(   python_envs()$name, "OHCA-0123456789ab")

    suppressMessages(python_envs(remove = "all"))
    expect_identical(   nrow(python_envs()), 0L)

})
