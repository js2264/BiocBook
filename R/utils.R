.sanitize_filename <- function(title) {
    allowed_chars <- '[a-zA-Z\\d\\-\\s:]'
    replace <- stringr::str_replace_all(title, allowed_chars, "")
    if (replace != '') 
        stop(glue::glue("Some characters are not allowed: {replace}"))
    filename <- stringr::str_replace_all(title, '\\s', '-') |> 
        stringr::str_replace_all(':', '-') |> 
        stringr::str_replace_all('--', '-') |> 
        stringr::str_replace_all('--', '-') |> 
        tolower() |> 
        paste0('.qmd')
    return(filename)
}

.find_path <- function(file, book, .from_book_root = FALSE) {
    path <- rprojroot::find_root_file(
        file, criterion = is_biocbook, path = path(book)
    )
    if (.from_book_root) {
        path <- gsub(path(book), "", path)
    }
    return(path)
}

#' @rdname BiocBook-editing
#' @export 

edit_book_yml <- function(book, open = TRUE) {

    path <- file.path("inst", "assets", "_book.yml")
    file <- file.path(path(book), path)
    if (interactive() && open) usethis::edit_file(file)

}

#' @rdname BiocBook-editing
#' @export 

edit_bib <- function(book, open = TRUE) {

    path <- file.path("inst", "assets", "bibliography.bib")
    file <- file.path(path(book), path)
    if (interactive() && open) usethis::edit_file(file)

}

#' @rdname BiocBook-editing
#' @export 

edit_requirements_yml <- function(book, open = TRUE) {

    path <- file.path("inst", "requirements")
    file <- file.path(path(book), path)
    if (interactive() && open) usethis::edit_file(file)

}
