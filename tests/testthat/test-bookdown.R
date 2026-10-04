## Copy the `bookdown-mini` fixture (every rule from_bookdown() knows) to a
## throwaway git repository, and convert it there, in place. No network:
## `user` is given and `skip_availability = TRUE`.
.bookdown_fixture <- function(git = TRUE, extra = NULL) {
    dir <- tempfile("bookdown-mini")
    dir.create(dir)
    src <- test_path("fixtures", "bookdown-mini")
    file.copy(list.files(src, full.names = TRUE), dir, recursive = TRUE)
    for (f in names(extra)) writeLines(extra[[f]], file.path(dir, f))
    if (git) {
        gert::git_init(dir)
        gert::git_config_set("user.name", "Jane Doe", repo = dir)
        gert::git_config_set("user.email", "jane@example.com", repo = dir)
        gert::git_add(".", repo = dir)
        gert::git_commit("A bookdown book", repo = dir)
    }
    dir
}

.convert_fixture <- function(style = "auto", git = TRUE, extra = NULL, ...) {
    dir <- .bookdown_fixture(git, extra)
    suppressMessages(bb <- from_bookdown(
        dir, package = "BookdownMini", user = "dummy", style = style, skip_availability = TRUE, ...
    ))
    list(bb = bb, book = dir)
}

test_that("from_bookdown() converts a bookdown book in place", {

    fx <- .convert_fixture()
    on.exit(unlink(fx$book, recursive = TRUE, force = TRUE), add = TRUE)
    inst <- file.path(fx$book, "inst")

    expect_s4_class(    fx$bb, "BiocBook")
    expect_true(        file.exists(file.path(inst, "pages", "01-intro.qmd")))
    expect_true(        file.exists(file.path(inst, "pages", "02-annex.qmd")))

    ## The bookdown sources and configuration are gone
    expect_false(       any(file.exists(file.path(fx$book, c(
        "index.Rmd", "01-intro.Rmd", "02-annex.Rmd", "_bookdown.yml", "refs.bib", "style.css"
    )))))
    expect_false(       dir.exists(file.path(fx$book, "img")))

    ## `_book.yml` stays a valid BiocBook: parts and appendices are read back
    expect_named(       chapters(fx$bb), c("Preamble", "Introduction"))
    book_yml <- yaml::read_yaml(file.path(inst, "assets", "_book.yml"))$book
    expect_identical(   book_yml$chapters[[2]]$part, "Basics")
    expect_identical(   unlist(book_yml$appendices), "pages/02-annex.qmd")

    ## The landing page keeps BiocBook's sections, with the preamble as welcome
    index <- readLines(file.path(inst, "index.qmd"))
    expect_true(        "# Preamble {-}" %in% index)
    expect_false(       any(grepl("^# Welcome", index)))
    expect_true(        "# Docker image {-}" %in% index)
    expect_true(        "# Session info {-}" %in% index)
    expect_true(        any(grepl('include_graphics("pages/img/logo.png")', index, fixed = TRUE)))
    expect_true(        "The introduction is in @sec-intro, the annex in [-@sec-annex]." %in% index)
    expect_false(       any(grepl("^---$|^title:", index)))

    ## Assets
    expect_true(        file.exists(file.path(inst, "pages", "img", "logo.png")))
    expect_true(        file.exists(file.path(inst, "assets", "refs.bib")))
    expect_true(        file.exists(file.path(inst, "assets", "style.css")))

    ## The README gets the BiocBook badges
    expect_true(        any(grepl("dummy/BookdownMini/actions/workflows/biocbook.yml", readLines(file.path(fx$book, "README.md")), fixed = TRUE)))

    ## A book is converted once
    expect_error(       from_bookdown(fx$book, package = "BookdownMini", user = "dummy"), "already a")

    ## Converted pages and book structure (snapshots are skipped on CRAN, and
    ## a skip ends the test, so they come last)
    expect_snapshot_file(file.path(inst, "pages", "01-intro.qmd"))
    expect_snapshot_file(file.path(inst, "pages", "02-annex.qmd"))
    expect_snapshot_file(file.path(inst, "assets", "_book.yml"))
    expect_snapshot_file(file.path(inst, "assets", "_format.yml"))

})

test_that("from_bookdown() commits one step at a time", {

    fx <- .convert_fixture()
    on.exit(unlink(fx$book, recursive = TRUE, force = TRUE), add = TRUE)

    log <- gert::git_log(repo = fx$book)
    expect_identical(   rev(sub("\n.*$", "", log$message)), c(
        "A bookdown book",
        "Add the BiocBook template",
        "Move the pages to inst/",
        "Move the book settings to _book.yml and _format.yml",
        "Remove the bookdown build",
        "Use the BiocBook landing page",
        "Rewrite cross-references for quarto",
        "Turn questions and solutions into callouts",
        "Translate figure layout options",
        "Set up every chapter as index.Rmd did",
        "Describe the book in DESCRIPTION"
    ))
    expect_true(        all(grepl("Done by BiocBook::from_bookdown()", log$message[-nrow(log)], fixed = TRUE)))
    expect_true(        all(log$author == "Jane Doe <jane@example.com>"))

    ## Nothing is left to commit, but the migration report
    status <- gert::git_status(repo = fx$book)
    expect_identical(   status$file, "MIGRATION.md")

    ## The pages move as they are, so that git follows them
    skip_if_not(        nzchar(Sys.which("git")))
    move <- log$commit[nrow(log) - 2L]
    changes <- system2("git", c("-C", shQuote(fx$book), "show", "-M", "--name-status", "--format=", move), stdout = TRUE)
    expect_true(        all(startsWith(changes, "R100\t")))
    expect_true(        "R100\tindex.Rmd\tinst/index.qmd" %in% changes)
    expect_true(        "R100\t01-intro.Rmd\tinst/pages/01-intro.qmd" %in% changes)

})

test_that("from_bookdown() repeats the shared-session setup in each chapter", {

    fx <- .convert_fixture()
    on.exit(unlink(fx$book, recursive = TRUE, force = TRUE), add = TRUE)

    for (page in c("01-intro.qmd", "02-annex.qmd")) {
        lines <- readLines(file.path(fx$book, "inst", "pages", page))
        ## Right after the chapter title, in a hidden chunk
        expect_match(   lines[1], "^# ")
        expect_identical(lines[3:4], c("```{r}", "#| include: false"))
        expect_true(    "options(digits = 4)" %in% lines)
        expect_true(    'suppressPackageStartupMessages(library("tools"))' %in% lines)
        ## Only from evaluated chunks
        expect_false(   any(grepl("neverloaded", lines)))
    }

})

test_that("from_bookdown() fills DESCRIPTION", {

    fx <- .convert_fixture()
    on.exit(unlink(fx$book, recursive = TRUE, force = TRUE), add = TRUE)

    d <- desc::desc(file = file.path(fx$book, "DESCRIPTION"))
    expect_identical(   d$get_field("Package"), "BookdownMini")
    expect_identical(   d$get_field("Title"), "A Mini Bookdown Book")
    expect_match(       d$get_field("Description"), "^This mini book exercises every rewriting rule of BiocBook\\.")
    authors <- d$get_authors()
    expect_identical(   format(authors, include = c("given", "family")), c("Jane Doe", "John Smith"))
    expect_identical(   authors[[1]]$role, c("aut", "cre"))
    ## Packages used by the pages, but not the bookdown machinery
    deps <- d$get_deps()
    expect_true(        "fixturepkg" %in% deps$package[deps$type == "Imports"])
    expect_false(       any(c("bookdown", "msmbstyle", "neverloaded") %in% deps$package))
    ## The book states no licence: the MIT licence of the template, for its authors
    expect_identical(   d$get_field("License"), "MIT + file LICENSE")
    expect_identical(   readLines(file.path(fx$book, "LICENSE"))[2], "COPYRIGHT HOLDER: Jane Doe, John Smith")

})

test_that("from_bookdown() takes the licence of the book from its pages", {

    expect_identical(   .bookdown_cc('<a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/">'), "CC BY-SA 4.0")
    expect_identical(   .bookdown_cc(c("No licence here.", "https://creativecommons.org/licenses/by-nc/4.0/")), "CC BY-NC 4.0")
    expect_null(        .bookdown_cc("No licence here."))
    ## Only the licences R knows
    expect_null(        .bookdown_cc("http://creativecommons.org/licenses/by-nd/2.0/"))

})

test_that("from_bookdown() reports what still needs a human", {

    fx <- .convert_fixture(extra = list("build-notes.R" = "## not part of the book"))
    on.exit(unlink(fx$book, recursive = TRUE, force = TRUE), add = TRUE)

    report <- readLines(file.path(fx$book, "MIGRATION.md"))
    rule <- function(name, n) sprintf("| %s | %d |", name, n)

    ## Every rule of the fixture is counted
    expect_true(        rule("`question_begin()` -> question callout", 1) %in% report)
    expect_true(        rule("`solution_begin()` -> collapsed answer callout", 1) %in% report)
    expect_true(        rule("`Figure \\@ref(fig:x)` -> `@fig-x`", 1) %in% report)
    expect_true(        rule("`Table \\@ref(tab:x)` -> `@tbl-x`", 1) %in% report)
    expect_true(        rule("`(\\#eq:x)` -> `{#eq-x}`", 1) %in% report)
    expect_true(        rule("`fig.margin=TRUE` -> `#| column: margin`", 1) %in% report)
    expect_true(        rule("`fig.fullwidth=TRUE` -> `#| column: page`", 1) %in% report)
    expect_true(        rule("header id `{#x}` -> `{#sec-x}`", 1) %in% report)

    ## ... and what could not be converted is listed
    todo <- report[seq(which(report == "## To do by hand"), length(report))]
    expect_true(        any(grepl("`@sec-nowhere`", todo, fixed = TRUE)))
    expect_true(        any(grepl("download.file(", todo, fixed = TRUE)))
    expect_true(        any(grepl("remove `cache = TRUE`", todo, fixed = TRUE)))
    expect_true(        any(grepl("inst/index.qmd` line [0-9]+: generate the `.bib` file", todo)))
    expect_true(        any(grepl("`someone/somepkg` is installed from GitHub", todo, fixed = TRUE)))
    expect_true(        any(grepl("highlight: tango", todo, fixed = TRUE)))
    expect_true(        any(grepl("placeholder email", todo, fixed = TRUE)))
    expect_true(        any(grepl("`build-notes.R`: remove it, or list it in `.Rbuildignore`", todo, fixed = TRUE)))
    ## Intended leftovers (code, inline code) are not reported
    expect_false(       any(grepl("@sec-x`", todo, fixed = TRUE)))
    expect_false(       any(grepl("\\@ref(intro)", todo, fixed = TRUE)))

    ## The report holds no local paths
    expect_false(       any(grepl(normalizePath(test_path("fixtures")), report, fixed = TRUE)))
    expect_false(       any(grepl(fx$book, report, fixed = TRUE)))

})

test_that("from_bookdown() only applies the rules of the chosen style", {

    fx <- .convert_fixture(style = "bookdown", appendices = "01-intro")
    on.exit(unlink(fx$book, recursive = TRUE, force = TRUE), add = TRUE)

    page <- readLines(file.path(fx$book, "inst", "pages", "01-intro.qmd"))
    expect_true(        "`r msmbstyle::question_begin()`" %in% page)
    expect_false(       any(grepl("callout-question", page)))
    expect_true(        "see @fig-scatter and @tbl-tbl." %in% sub("^.*, ", "", page))
    book_yml <- yaml::read_yaml(file.path(fx$book, "inst", "assets", "_book.yml"))$book
    expect_identical(   unlist(book_yml$appendices), c("pages/01-intro.qmd", "pages/02-annex.qmd"))
    log <- gert::git_log(repo = fx$book)
    expect_false(       any(grepl("^Turn questions and solutions into callouts", log$message)))

})

test_that("from_bookdown() converts without committing", {

    fx <- .convert_fixture(commit = FALSE)
    on.exit(unlink(fx$book, recursive = TRUE, force = TRUE), add = TRUE)
    expect_identical(   nrow(gert::git_log(repo = fx$book)), 1L)
    expect_true(        file.exists(file.path(fx$book, "inst", "index.qmd")))
    expect_true(        nrow(gert::git_status(repo = fx$book)) > 1L)

    ## Outside of a git repository
    fx2 <- .convert_fixture(git = FALSE)
    on.exit(unlink(fx2$book, recursive = TRUE, force = TRUE), add = TRUE)
    expect_true(        file.exists(file.path(fx2$book, "inst", "index.qmd")))
    expect_false(       dir.exists(file.path(fx2$book, ".git")))

})

test_that("from_bookdown() checks its input before changing anything", {

    dir <- tempfile("not-bookdown")
    dir.create(dir)
    on.exit(unlink(dir, recursive = TRUE), add = TRUE)
    writeLines("# Not a bookdown book", file.path(dir, "README.md"))
    expect_error(       from_bookdown(dir, package = "NotABook", user = "dummy", skip_availability = TRUE), "bookdown")
    expect_identical(   list.files(dir), "README.md")
    expect_error(       from_bookdown(file.path(dir, "nope")), "not a folder")

    book <- .bookdown_fixture()
    on.exit(unlink(book, recursive = TRUE, force = TRUE), add = TRUE)
    ## A bookdown project in a subfolder of a repository
    dir.create(file.path(book, "sub"))
    file.copy(file.path(book, "index.Rmd"), file.path(book, "sub"))
    expect_error(       from_bookdown(file.path(book, "sub"), package = "Sub", user = "dummy"), "subfolder")
    unlink(file.path(book, "sub"), recursive = TRUE)
    ## Uncommitted changes
    cat("More text.\n", file = file.path(book, "01-intro.Rmd"), append = TRUE)
    expect_error(       from_bookdown(book, package = "BookdownMini", user = "dummy"), "uncommitted changes")
    expect_true(        file.exists(file.path(book, "index.Rmd")))
    expect_false(       file.exists(file.path(book, "DESCRIPTION")))
    ## A name that cannot be a package name
    expect_error(       .bookdown_package("my-book", list(config = list(), path = book)), "valid package name")

})

test_that("from_bookdown() names the book after bookdown, and its user after the repository", {

    bd <- list(config = list(book_filename = "R4MS"), path = "/books/book")
    expect_identical(   .bookdown_package(NULL, bd), "R4MS")
    bd <- list(config = list(book_filename = "my-book.Rmd"), path = "/books/mybook")
    expect_identical(   .bookdown_package(NULL, bd), "mybook")

    d <- tempfile("bookdown-src")
    dir.create(d)
    on.exit(unlink(d, recursive = TRUE, force = TRUE), add = TRUE)
    gert::git_init(d)
    ## Credentials and `.git` are dropped, ssh remotes work too
    gert::git_remote_add("https://someone:t0ken@github.com/owner/repo.git", name = "origin", repo = d)
    expect_identical(   .bookdown_user(NULL, d), "owner")
    gert::git_remote_set_url("git@github.com:owner2/repo.git", remote = "origin", repo = d)
    expect_identical(   .bookdown_user(NULL, d), "owner2")
    expect_identical(   .bookdown_user("given", d), "given")

})

test_that("rewrites leave the blank lines of the book alone", {

    ## Runs of blank lines are collapsed only where a rewrite touched them
    lines <- c("Text.", "", "", "More.", "", "", "new", "", "End.")
    touched <- c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)
    expect_identical(   .tidy_touched(lines, touched), c("Text.", "", "", "More.", "", "new", "", "End."))
    expect_identical(   .tidy_touched(c("", "", "# Title"), c(TRUE, FALSE, FALSE)), "# Title")
    expect_identical(   .drop_lines(c("Text.", "", "# (PART) X {-}", "", "# Title"), c(FALSE, FALSE, TRUE, FALSE, FALSE)), c("Text.", "", "# Title"))

    ## A page without a final newline keeps it so
    f <- tempfile()
    on.exit(unlink(f), add = TRUE)
    writeBin(charToRaw("a\nb"), f)
    .write_page(c("a", "c"), f)
    expect_identical(   readChar(f, 10L, useBytes = TRUE), "a\nc")

})
