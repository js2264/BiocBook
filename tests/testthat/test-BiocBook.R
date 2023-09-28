test_that("init work", {
    
    tmpdir <- paste0(paste0(
        sample(c(seq(0, 9), LETTERS), 8, replace = TRUE), collapse = ""
    ))
    
    ## Utilities
    expect_no_error(    
        init(tmpdir, .local = TRUE)
    )
    expect_no_error(    bb <- BiocBook(tmpdir))
    expect_no_error(    show(bb))
    expect_no_error(    path(bb))
    expect_no_error(    chapters(bb))
    expect_no_error(    releases(bb))
    # expect_no_error(    preview(bb))

    ## add_* functions
    expect_invisible(   add_preamble(bb, open = FALSE))
    expect_warning(     add_preamble(bb, open = FALSE))
    expect_invisible(   add_chapter(bb, title = 'Chapter 1', open = FALSE))
    expect_invisible(   add_chapter(bb, title = 'Chapter 1', file = "chapter1.qmd", open = FALSE))
    expect_warning(     add_chapter(bb, title = 'Chapter 1', file = "chapter1.qmd", open = FALSE))
    expect_error(       add_chapter(bb, title = 'Chapter 1', file = "chapter1", open = FALSE))

    ## edit_* functions
    expect_no_error(    edit_yml(bb))
    expect_no_error(    edit_css(bb))
    expect_no_error(    edit_bib(bb))
    expect_no_error(    edit_requirements_yml(bb))
    expect_no_error(    edit_page(bb, file = '/inst/index.qmd', open = FALSE))

    unlink(tmpdir, recursive = TRUE, force = TRUE)
    # quarto::quarto_preview_stop()

})

