#' @rdname BiocBook-editing
#' @export 

edit_book_yml <- function(book, open = TRUE) {

    path <- file.path("inst", "assets", "_book.yml")
    file <- file.path(path(book), path)
    if (interactive() && open) usethis::edit_file(file)
    invisible(book)
    
}

#' @rdname BiocBook-editing
#' @export 

edit_bib <- function(book, open = TRUE) {

    path <- file.path("inst", "assets", "bibliography.bib")
    file <- file.path(path(book), path)
    if (interactive() && open) usethis::edit_file(file)
    invisible(book)
    
}

#' @rdname BiocBook-editing
#' @export 

edit_requirements_yml <- function(book, open = TRUE) {

    path <- file.path("inst", "requirements")
    file <- file.path(path(book), path)
    if (interactive() && open) usethis::edit_file(file)
    invisible(book)
    
}

