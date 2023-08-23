test_that("BiocBook_init work", {
    
    ## -- Test `BiocBook_init` without user 
    
    tmpdir <- paste0('.', paste0(
        sample(c(seq(0, 9), LETTERS), 8, replace = TRUE), collapse = ""
    ))
    unlink(tmpdir, recursive = TRUE, force = TRUE)

    expect_no_error(    bb <- BiocBook_init(tmpdir, .local = TRUE))
    expect_no_error(    bb)
    expect_no_error(    path(bb))
    expect_no_error(    chapters(bb))
    expect_no_error(    releases(bb))
    expect_no_error(    BiocBook_preview(bb))
    expect_error(       BiocBook_versions(bb))

    expect_invisible(   add_preamble(bb, open = FALSE))
    expect_warning(     add_preamble(bb, open = FALSE))
    expect_invisible(   add_chapter(bb, title = 'Chapter 1', open = FALSE))
    expect_invisible(   add_chapter(bb, title = 'Chapter 1', file = "chapter1.qmd", open = FALSE))
    expect_warning(     add_chapter(bb, title = 'Chapter 1', file = "chapter1.qmd", open = FALSE))
    expect_error(       add_chapter(bb, title = 'Chapter 1', file = "chapter1", open = FALSE))

    unlink(tmpdir, recursive = TRUE, force = TRUE)
    quarto::quarto_preview_stop()

    ## -- Test `BiocBook_init` with user 

    tmpdir <- paste0('.', paste0(
        sample(c(seq(0, 9), LETTERS), 8, replace = TRUE), collapse = ""
    ))
    
    ## Utilities
    expect_no_error(    
        BiocBook_init(tmpdir, .local = TRUE, .github_user = 'js2264')
    )
    expect_no_error(    bb <- BiocBook(tmpdir))
    expect_no_error(    show(bb))
    expect_no_error(    path(bb))
    expect_no_error(    chapters(bb))
    expect_no_error(    releases(bb))
    expect_no_error(    BiocBook_preview(bb))
    expect_error(       BiocBook_versions(bb))

    ## add_* functions
    expect_invisible(   add_preamble(bb, open = FALSE))
    expect_warning(     add_preamble(bb, open = FALSE))
    expect_invisible(   add_chapter(bb, title = 'Chapter 1', open = FALSE))
    expect_invisible(   add_chapter(bb, title = 'Chapter 1', file = "chapter1.qmd", open = FALSE))
    expect_warning(     add_chapter(bb, title = 'Chapter 1', file = "chapter1.qmd", open = FALSE))
    expect_error(       add_chapter(bb, title = 'Chapter 1', file = "chapter1", open = FALSE))

    ## edit_* functions
    expect_no_error(            edit_book_yml(bb))
    expect_no_error(            edit_bib(bb))
    expect_no_error(            edit_requirements_yml(bb))
    expect_no_error(            edit_page(bb, file = '/inst/index.qmd', open = FALSE))

    unlink(tmpdir, recursive = TRUE, force = TRUE)
    quarto::quarto_preview_stop()

})

