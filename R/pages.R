#' @export 

add_page <- function(book, title, file = NA, position = NULL) {
    if (is.na(file)) file <- sanitize_filename(title)
    full_path <- .find_path(glue::glue('pages/{file}'), book)
    path_from_book_root <- .find_path(glue::glue('pages/{file}'), book, .from_book_root = TRUE)
    path_from_book_root <- gsub("^/", "", path_from_book_root)
    if (tools::file_ext(file) != 'qmd') {
        cli::cli_abort("Please provide a file name ending with `.qmd`")
        stop()
    }
    if (file.exists(full_path)) {
        cli::cli_abort("File `{full_path}` already exists")
        stop()
    }
    if (file.exists(full_path)) {
        cli::cli_abort("File `{full_path}` already exists")
        stop()
    }

    ## Create file in `pages/`
    if (!file.exists(dirname(full_path))) {dir.create(dirname(full_path))}
    writeLines(
        text = glue::glue("# {title}"), 
        full_path 
    )
    
    ## Add entry in `_book.yml`
    book.yml <- .find_path('inst/assets/_book.yml', book)
    book.yml.lines <- readLines(book.yml)
    if (is.null(position)) position <- length(chapters(book)) + 1
    temp <- tempfile()
    writeLines(book.yml.lines[seq(1, position+3-1)], temp)
    write(glue::glue("    - {path_from_book_root}"), temp, append = TRUE)
    write(book.yml.lines[seq(position+3, length(book.yml.lines))], temp, append = TRUE)
    file.copy(temp, book.yml, overwrite = TRUE)
    cli::cli_alert_success("File created @ `{full_path}`")
    invisible(full_path)
}

#' @export 

add_preamble <- function(book) {
    add_page(book, file = "preamble.qmd", title = "Preamble {-}", position = 2)
}

#' @export 

add_chapter <- function(book, title, file = NA, position = NULL) {
    if (is.na(file)) file <- .sanitize_filename(title)
    print(file)
    add_page(book, title, file, position)
}


