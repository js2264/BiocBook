#' @importFrom usethis edit_file

.add_page <- function(book, title, file = NA, position = NULL, open = TRUE) {
    if (is.na(file)) file <- .sanitize_filename(title)
    full_path <- .find_path(file.path('pages', file), book)
    path_from_book_root <- .find_path(file.path('pages', file), book, .from_book_root = TRUE)
    path_from_book_root <- gsub("^[/\\]", "", path_from_book_root)
    path_from_book_root <- gsub("^inst[/\\]", "", path_from_book_root)

    ## Check new file name
    if (tools::file_ext(file) != 'qmd') {
        cli::cli_abort("Please provide a file name ending with `.qmd`")
    }

    ## If file exists, offer to edit it instead
    if (file.exists(full_path)) {
        cli::cli_warn("File `{full_path}` already exists.")
        if (interactive() && open) {
            msg <- glue::glue("Do you want to edit {full_path}?")
            if (usethis::ui_yeah(msg)) {
                usethis::edit_file(full_path)
            } 
        }
        return(invisible(full_path))
    }

    ## Create file in `pages/`
    if (!file.exists(dirname(full_path))) {dir.create(dirname(full_path))}
    writeLines(
        text = glue::glue("# {title}"), 
        full_path 
    )
    
    ## Add entry in `_book.yml`
    book.yml <- .find_path(file.path('assets', '_book.yml'), book)
    book.yml.lines <- readLines(book.yml)
    if (is.null(position)) position <- length(chapters(book)) + 1
    temp <- tempfile()
    writeLines(book.yml.lines[seq(1, position+3-1)], temp)
    write(glue::glue("    - {path_from_book_root}"), temp, append = TRUE)
    write(book.yml.lines[seq(position+3, length(book.yml.lines))], temp, append = TRUE)
    file.copy(temp, book.yml, overwrite = TRUE)
    cli::cli_alert_success("File created @ `{full_path}`")

    ## Open new page and edit
    if (interactive() && open) usethis::edit_file(full_path)

    invisible(full_path)
}

#' @rdname BiocBook
#' @export 

add_preamble <- function(book, open = TRUE) {
    .add_page(book, file = "preamble.qmd", title = "Preamble {-}", position = 2, open)
}

#' @rdname BiocBook
#' @export 

add_chapter <- function(book, title, file = NA, position = NULL, open = TRUE) {
    if (is.na(file)) file <- .sanitize_filename(title)
    .add_page(book, title, file, position, open)
}

#' @rdname BiocBook
#' @export 

edit_page <- function(book, file, open = TRUE) {

    file <- gsub("^[/\\]", "", file)
    file <- gsub("^inst[/\\]", "", file)
    full_path <- .find_path(file, book)
    if (!file.exists(full_path)) {
        cli::cli_abort("File `{full_path}` does not exist. To create a new chapter, please use `add_chapter()` instead.", wrap = TRUE)
    }
    if (interactive() && open) usethis::edit_file(full_path)

}
