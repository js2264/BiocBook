## Convert the `bookdown-mini` fixture (every rule from_bookdown() knows) in a
## throwaway folder. No network: `.local = TRUE` and `skip_availability = TRUE`.
.convert_fixture <- function(style = "auto", ...) {
    fixture <- normalizePath(test_path("fixtures", "bookdown-mini"))
    dir <- tempfile("from_bookdown")
    dir.create(dir)
    owd <- setwd(dir)
    on.exit(setwd(owd), add = TRUE)
    suppressMessages(bb <- from_bookdown(
        fixture, "BookdownMini", style = style, .local = TRUE, skip_availability = TRUE, ...
    ))
    list(bb = bb, dir = dir, book = file.path(dir, "BookdownMini"))
}

test_that("from_bookdown() converts pages and book structure", {

    fx <- .convert_fixture()
    on.exit(unlink(fx$dir, recursive = TRUE, force = TRUE), add = TRUE)
    inst <- file.path(fx$book, "inst")

    expect_s4_class(    fx$bb, "BiocBook")
    expect_true(        file.exists(file.path(inst, "pages", "01-intro.qmd")))
    expect_true(        file.exists(file.path(inst, "pages", "02-annex.qmd")))

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

    ## Assets
    expect_true(        file.exists(file.path(inst, "pages", "img", "logo.png")))
    expect_true(        file.exists(file.path(inst, "assets", "refs.bib")))
    expect_true(        file.exists(file.path(inst, "assets", "style.css")))

    ## A new git repository, with the migration as its first commit
    log <- gert::git_log(repo = fx$book)
    expect_identical(   nrow(log), 1L)
    expect_match(       log$message, "Migrate from bookdown with BiocBook::from_bookdown()", fixed = TRUE)

    ## Converted pages and book structure (snapshots are skipped on CRAN, and
    ## a skip ends the test, so they come last)
    expect_snapshot_file(file.path(inst, "pages", "01-intro.qmd"))
    expect_snapshot_file(file.path(inst, "pages", "02-annex.qmd"))
    expect_snapshot_file(file.path(inst, "assets", "_book.yml"))
    expect_snapshot_file(file.path(inst, "assets", "_format.yml"))

})

test_that("from_bookdown() repeats the shared-session setup in each chapter", {

    fx <- .convert_fixture()
    on.exit(unlink(fx$dir, recursive = TRUE, force = TRUE), add = TRUE)

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
    on.exit(unlink(fx$dir, recursive = TRUE, force = TRUE), add = TRUE)

    d <- desc::desc(file = file.path(fx$book, "DESCRIPTION"))
    expect_identical(   d$get_field("Title"), "A Mini Bookdown Book")
    expect_match(       d$get_field("Description"), "^This mini book exercises every rewriting rule of BiocBook\\.")
    authors <- d$get_authors()
    expect_identical(   format(authors, include = c("given", "family")), c("Jane Doe", "John Smith"))
    expect_identical(   authors[[1]]$role, c("aut", "cre"))
    ## Packages used by the pages, but not the bookdown machinery
    deps <- d$get_deps()
    expect_true(        "fixturepkg" %in% deps$package[deps$type == "Imports"])
    expect_false(       any(c("bookdown", "msmbstyle", "neverloaded") %in% deps$package))
    expect_true(        "^MIGRATION\\.md$" %in% readLines(file.path(fx$book, ".Rbuildignore")))

})

test_that("from_bookdown() reports what still needs a human", {

    fx <- .convert_fixture()
    on.exit(unlink(fx$dir, recursive = TRUE, force = TRUE), add = TRUE)

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
    ## Intended leftovers (code, inline code) are not reported
    expect_false(       any(grepl("@sec-x`", todo, fixed = TRUE)))
    expect_false(       any(grepl("\\@ref(intro)", todo, fixed = TRUE)))

    ## The report is committed: it does not hold local paths
    expect_false(       any(grepl(normalizePath(test_path("fixtures")), report, fixed = TRUE)))

})

test_that("from_bookdown() names the bookdown project by its git remote or folder", {

    d <- tempfile("bookdown-src")
    dir.create(file.path(d, "book"), recursive = TRUE)
    on.exit(unlink(d, recursive = TRUE, force = TRUE), add = TRUE)
    expect_identical(   .bookdown_source(d), sprintf("`%s`", basename(d)))
    gert::git_init(d)
    expect_identical(   .bookdown_source(d), sprintf("`%s`", basename(d)))

    ## Credentials and `.git` are dropped, ssh remotes become https URLs
    gert::git_remote_add("https://someone:t0ken@github.com/owner/repo.git", name = "origin", repo = d)
    expect_identical(   .bookdown_source(d), "<https://github.com/owner/repo>")
    expect_identical(   .bookdown_source(file.path(d, "book")), "`book` of <https://github.com/owner/repo>")
    gert::git_remote_set_url("git@github.com:owner/repo.git", remote = "origin", repo = d)
    expect_identical(   .bookdown_source(d), "<https://github.com/owner/repo>")

})

test_that("from_bookdown() only applies the rules of the chosen style", {

    fx <- .convert_fixture(style = "bookdown", appendices = "01-intro")
    on.exit(unlink(fx$dir, recursive = TRUE, force = TRUE), add = TRUE)

    page <- readLines(file.path(fx$book, "inst", "pages", "01-intro.qmd"))
    expect_true(        "`r msmbstyle::question_begin()`" %in% page)
    expect_false(       any(grepl("callout-question", page)))
    expect_true(        "see @fig-scatter and @tbl-tbl." %in% sub("^.*, ", "", page))
    book_yml <- yaml::read_yaml(file.path(fx$book, "inst", "assets", "_book.yml"))$book
    expect_identical(   unlist(book_yml$appendices), c("pages/01-intro.qmd", "pages/02-annex.qmd"))

})

test_that("from_bookdown() checks its input before creating anything", {

    dir <- tempfile("not-bookdown")
    dir.create(dir)
    on.exit(unlink(dir, recursive = TRUE), add = TRUE)
    writeLines("# Not a bookdown book", file.path(dir, "README.md"))
    expect_error(       from_bookdown(dir, "NotABook", .local = TRUE, skip_availability = TRUE), "bookdown")
    expect_false(       dir.exists("NotABook"))
    expect_error(       from_bookdown(file.path(dir, "nope"), "NotABook", .local = TRUE), "not a folder")

})
