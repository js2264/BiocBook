test_that("Methods work", {
    
    skip('No PAT set up yet')

    gert::git_clone('https://github.com/js2264/BiocBook.GRanges')

    expect_no_error(    bb <- BiocBook('BiocBook.GRanges/'))
    expect_no_error(    bb)
    expect_no_error(    path(bb))
    expect_no_error(    releases(bb))
    expect_no_error(    chapters(bb))
    expect_no_error(    BiocBook_versions(bb))

    unlink('BiocBook.GRanges', recursive = TRUE, force = TRUE)
})

test_that("Editing functions work", {
    gert::git_clone('https://github.com/js2264/BiocBook.GRanges')
    
    expect_no_error(    bb <- BiocBook('BiocBook.GRanges/'))
    expect_invisible(   add_preamble(bb, open = FALSE))
    expect_warning(     add_preamble(bb, open = FALSE))
    expect_invisible(   add_chapter(bb, title = 'Chapter 1', open = FALSE))
    expect_invisible(   add_chapter(bb, title = 'Chapter 1', file = "chapter1.qmd", open = FALSE))
    expect_warning(     add_chapter(bb, title = 'Chapter 1', file = "chapter1.qmd", open = FALSE))
    expect_error(       add_chapter(bb, title = 'Chapter 1', file = "chapter1", open = FALSE))
    expect_warning(     add_chapter(bb, title = 'Join is an overlap', open = FALSE))

    unlink('BiocBook.GRanges', recursive = TRUE, force = TRUE)
})
