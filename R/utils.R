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
        file.path('inst', file), criterion = is_biocbook, path = path(book)
    )
    if (.from_book_root) {
        path <- gsub(path(book), "", path)
    }
    return(path)
}

.fix_placeholders <- function(file, pkg, usr) {
    lines <- readLines(file)
    lines <- gsub("<Package_name>", pkg, lines)
    lines <- gsub("<package_name>", tolower(pkg), lines)
    lines <- gsub("<github_user>", usr, lines)
    writeLines(lines, file)
}
