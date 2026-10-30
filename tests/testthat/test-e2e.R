## End-to-end: render a new book with quarto. This builds a conda environment
## (network, minutes), so it only runs when asked to, e.g. by the `e2e` GitHub
## workflow, and never on the Bioconductor builders.

test_that("a new book renders python chunks through reticulate", {

    skip_if_not(nzchar(Sys.getenv("BIOCBOOK_E2E")), "set BIOCBOOK_E2E=true to run")
    skip_if(Sys.which("quarto") == "", "quarto not found")

    book <- paste0("E2E", paste(sample(LETTERS, 6), collapse = ""))
    quick_init(book, user = "dummy")
    on.exit(unlink(book, recursive = TRUE, force = TRUE), add = TRUE)
    bb <- BiocBook(book)
    add_python_chapter(bb, title = "Py chapter", open = FALSE)
    page <- file.path(book, "inst", "pages", "py-chapter.qmd")
    cat("", "```{r}", "x <- c(1, 2, 3)", "```", "",
        "```{python}", "print('sum from python:', sum(r.x))", "y = 42", "```", "",
        "```{r}", "cat('y from R:', reticulate::py$y)", "```",
        file = page, sep = "\n", append = TRUE)

    ## h5py from PyPI, imported once R has loaded the system's OpenSSL (as R
    ## packages using it do on the Bioconductor builders): conda's h5py fails
    ## there, as its HDF5 links a libcurl needing a more recent OpenSSL
    cat("", "    - pip:", "        - h5py==3.16.0",
        file = file.path(book, "inst", "requirements.yml"), sep = "\n", append = TRUE)
    cat("", "```{r}", "invisible(openssl::sha256('loads the system libssl'))", "```", "",
        "```{python}", "import h5py", "print('h5py from PyPI:', h5py.__version__)", "```",
        file = page, sep = "\n", append = TRUE)

    ## Render the way `vignettes/Makefile` does: with the `llms` profile when
    ## quarto supports it
    profile <- if (quarto::quarto_version() >= "1.11") "llms" else NULL
    quarto::quarto_render(file.path(book, "inst"), profile = profile, as_job = FALSE)

    ## Values go from R to python and back, so both ran in one session
    docs <- file.path(book, "inst", "docs")
    html <- readLines(file.path(docs, "pages", "py-chapter.html"), warn = FALSE)
    expect_true(        any(grepl("sum from python: 6", html)))
    expect_true(        any(grepl("y from R: 42", html)))
    expect_true(        any(grepl("h5py from PyPI: 3.16.0", html)))

    ## The post-render hook wrote llms.txt, with BiocBook context and only
    ## working links
    llms <- readLines(file.path(docs, "llms.txt"), warn = FALSE)
    expect_true(        "<!-- biocbook:start -->" %in% llms)
    expect_true(        any(grepl("Python environment (conda)", llms, fixed = TRUE)))
    expect_true(        any(grepl("from PyPI: `h5py==3.16.0`", llms, fixed = TRUE)))
    links <- unlist(regmatches(llms, gregexpr("\\]\\([^)]+\\)", llms)))
    links <- gsub("^\\]\\(|\\)$", "", links)
    links <- links[!grepl("^[a-z]+://", links)]
    expect_true(        length(links) >= 2L)
    expect_true(        all(file.exists(file.path(docs, links))))
    if (!is.null(profile)) {
        expect_true(    file.exists(file.path(docs, "pages", "py-chapter.llms.md")))
    }

})
