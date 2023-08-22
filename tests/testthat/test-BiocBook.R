test_that("BiocBook_init work", {
    
    ## -- Test `BiocBook_init` without user 
    
    unlink('BiocBookTest', recursive = TRUE, force = TRUE)

    expect_no_error(    bb <- BiocBook_init('BiocBookTest', local = TRUE))
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

    unlink('BiocBookTest', recursive = TRUE, force = TRUE)
    quarto::quarto_preview_stop()

    ## -- Test `BiocBook_init` with user 

    expect_no_error(    
        BiocBook_init('BiocBookTest', local = TRUE, github_user = 'js2264')
    )
    expect_no_error(    bb <- BiocBook('BiocBookTest'))
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

    unlink('BiocBookTest', recursive = TRUE, force = TRUE)
    quarto::quarto_preview_stop()

})

