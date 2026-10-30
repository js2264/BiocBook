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
    fake <- file.path(tempfile("env"), "bin", "python")
    dir.create(dirname(fake), recursive = TRUE); file.create(fake)
    old <- Sys.getenv("RETICULATE_PYTHON", unset = NA)
    old_path <- Sys.getenv("PATH")
    Sys.setenv(RETICULATE_PYTHON = fake)
    on.exit({
        if (is.na(old)) Sys.unsetenv("RETICULATE_PYTHON")
        else Sys.setenv(RETICULATE_PYTHON = old)
        Sys.setenv(PATH = old_path)
        unlink(dirname(dirname(fake)), recursive = TRUE)
    }, add = TRUE)
    expect_identical(   suppressMessages(setup_python(conda = "/nonexistent")), fake)

    ## ... but the tools of its environment are put on the PATH
    path <- strsplit(Sys.getenv("PATH"), .Platform$path.sep, fixed = TRUE)[[1]]
    expect_identical(   path[1], dirname(fake))

})

test_that("requirements.yml can declare a pip: section", {

    req <- tempfile(fileext = ".yml")
    on.exit(unlink(req), add = TRUE)

    ## conda packages only: nothing for pip, and pip is not added
    writeLines(c("name:", "    Book", "channels:", "    - conda-forge",
        "dependencies:", "    - python=3.12", "    - samtools=1.24"), req)
    reqs <- BiocBook:::.read_requirements(req)
    expect_identical(   reqs$name, "Book")
    expect_identical(   reqs$channels, "conda-forge")
    expect_identical(   reqs$conda, c("python=3.12", "samtools=1.24"))
    expect_length(      reqs$pip, 0L)

    ## A pip: section: its packages are for pip, and pip joins the conda
    ## packages, since the environment needs it to install them
    writeLines(c("dependencies:", "    - python=3.12", "    - pip:",
        "        - cooler==0.10.4", "        - matplotlib>=3.11,<3.12"), req)
    reqs <- BiocBook:::.read_requirements(req)
    expect_identical(   reqs$conda, c("python=3.12", "pip"))
    expect_identical(   reqs$pip, c("cooler==0.10.4", "matplotlib>=3.11,<3.12"))

    ## ... unless the file already declares pip, pinned or not
    writeLines(c("dependencies:", "    - python=3.12", "    - pip=24.2",
        "    - pip:", "        - cooler==0.10.4"), req)
    expect_identical(   BiocBook:::.read_requirements(req)$conda, c("python=3.12", "pip=24.2"))
    writeLines(c("dependencies:", "    - conda-forge::pip", "    - pip:", "        - six"), req)
    expect_identical(   BiocBook:::.read_requirements(req)$conda, "conda-forge::pip")

    ## A package named like pip is not pip
    writeLines(c("dependencies:", "    - pip-tools", "    - pip:", "        - six"), req)
    expect_identical(   BiocBook:::.read_requirements(req)$conda, c("pip-tools", "pip"))

    ## Other blocks are refused rather than silently dropped
    writeLines(c("dependencies:", "    - python=3.12", "    - uv:", "        - six"), req)
    expect_error(       BiocBook:::.read_requirements(req), "unsupported block")

})

test_that("the pip: section is installed after the conda packages", {

    prefix <- file.path(tempfile("envs"), "Book-0123456789ab")
    on.exit(unlink(dirname(prefix), recursive = TRUE), add = TRUE)
    reqs <- list(conda = c("python=3.12", "pip"), channels = "conda-forge",
        pip = c("cooler==0.10.4", "h5py==3.16.0"))
    calls <- character(0)
    local_mocked_bindings(
        .setup_conda_env = function(prefix, packages, channels, conda) {
            calls <<- c(calls, paste("conda:", paste(packages, collapse = " ")))
            dir.create(prefix, recursive = TRUE)
        },
        .pip_install = function(python, packages) {
            calls <<- c(calls, paste("pip:", python, paste(packages, collapse = " ")))
        }
    )
    BiocBook:::.build_env(prefix, reqs, conda = "micromamba")
    expect_identical(   calls, c(
        "conda: python=3.12 pip",
        paste("pip:", BiocBook:::.env_python(prefix), "cooler==0.10.4 h5py==3.16.0")
    ))
    expect_true(        dir.exists(prefix))

    ## Without a pip: section, pip is not run at all
    calls <- character(0); unlink(prefix, recursive = TRUE)
    BiocBook:::.build_env(prefix, modifyList(reqs, list(pip = character(0))), "micromamba")
    expect_identical(   calls, "conda: python=3.12 pip")

})

test_that("an environment whose build fails is not left behind", {

    prefix <- file.path(tempfile("envs"), "Book-0123456789ab")
    on.exit(unlink(dirname(prefix), recursive = TRUE), add = TRUE)
    local_mocked_bindings(
        .setup_conda_env = function(prefix, packages, channels, conda) {
            dir.create(prefix, recursive = TRUE)
        },
        .pip_install = function(python, packages) stop("no wheel for this machine")
    )
    reqs <- list(conda = "python=3.12", channels = NULL, pip = "cooler==0.10.4")

    ## Otherwise the next render would re-use it as if it were complete
    expect_error(       BiocBook:::.build_env(prefix, reqs, "micromamba"), "no wheel")
    expect_false(       dir.exists(prefix))

})

test_that(".prepend_path() puts a directory first on the PATH, once", {

    old_path <- Sys.getenv("PATH")
    on.exit(Sys.setenv(PATH = old_path), add = TRUE)
    dir <- tempfile("bin")
    BiocBook:::.prepend_path(dir)
    BiocBook:::.prepend_path(dir)
    path <- strsplit(Sys.getenv("PATH"), .Platform$path.sep, fixed = TRUE)[[1]]
    expect_identical(   path[1], dir)
    expect_identical(   sum(path == dir), 1L)

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

test_that("add_python_chapter() declares BiocBook and reticulate", {

    tmpdir <- paste0(paste0(
        sample(LETTERS, 5, replace = TRUE),
        sample(c(seq(0, 9)), 5, replace = TRUE),
        collapse = ""
    ))
    quick_init(tmpdir, user = "dummy")
    on.exit(unlink(tmpdir, recursive = TRUE, force = TRUE), add = TRUE)

    ## Books created from older templates do not list them
    desc_f <- file.path(tmpdir, "DESCRIPTION")
    d <- read.dcf(desc_f)
    d[, "Suggests"] <- "knitr"
    write.dcf(d, desc_f)

    bb <- BiocBook(tmpdir)
    add_python_chapter(bb, title = "Py chapter", open = FALSE)
    suggests <- read.dcf(desc_f, fields = "Suggests")[1, 1]
    expect_match(       suggests, "\\bBiocBook\\b")
    expect_match(       suggests, "\\breticulate\\b")

    ## The page is pinned to knitr, and its title is still picked up
    page <- readLines(file.path(tmpdir, "inst", "pages", "py-chapter.qmd"))
    expect_true(        "engine: knitr" %in% page)
    expect_true(        "Py chapter" %in% names(chapters(bb)))

})
