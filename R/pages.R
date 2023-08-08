add_preamble <- function(book, file, title) {
    .check_biocbook_proj()
    writeLines(
        text = glue::glue("# {title} {{-}}"), 
        is_biocbook$find_file(glue::glue("pages/{file}"))
    )
}

add_part <- function(book, part = "Part") {
    .check_biocbook_proj()
    book.yml <- is_biocbook$find_file(glue::glue("inst/assets/_book.yml"))
    book.yml.lines <- readLines(book.yml)
    book.yml.last <- grep("cover-image: ", book.yml.lines) - 1
    book.yml.lines[1:book.yml.last]
}

add_chapter <- function(book, file, title) {

}

add_appendix <- function(book, file, title) {

}

