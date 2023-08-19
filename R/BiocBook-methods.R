#' @importMethodsFrom BiocGenerics path
#' @importMethodsFrom methods show
#' @importFrom stringr str_trunc
#' @importFrom stringr str_pad
#' @include imports.R
#' @include doc.R
#' @include BiocBook.R

setMethod("path", signature("BiocBook"), function(object) object@local_path)

setGeneric("releases", function(object) {standardGeneric("releases")})
setMethod("releases", signature("BiocBook"), function(object) object@releases)

setGeneric("chapters", function(object) {standardGeneric("chapters")})
setMethod("chapters", signature("BiocBook"), function(object) {
    book.yml <- .find_path(file.path('assets', '_book.yml'), object)
    chapters <- rprojroot::find_root_file(
        file.path('inst', yaml::read_yaml(book.yml)$book$chapters), criterion = is_biocbook, path = path(object)
    )
    names(chapters) <- lapply(chapters, function(chap) {
        has_yaml <- readLines(chap, n = 1) |> grepl("^---", x = _)
        if (has_yaml) {
            chaplines <- readLines(chap)
            nlinesyaml <- which(grepl("^---", x = chaplines))[2] 
            chaplines <- chaplines[seq(nlinesyaml + 2, length(chaplines))]
        } 
        else {
            chaplines <- readLines(chap)
        }
        head <- gsub("^# ", "", chaplines[1])
        head <- gsub(" \\{-\\}", "", head)
    }) |> unlist()
    return(chapters)
})

setMethod("show", signature("BiocBook"), function(object) {

    cli::cli_text(cli::style_bold('BiocBook object\n'))
    cli::cli_text(paste0('- local path:   ', cli::col_cyan(path(object)), '\n'))
    cli::cli_text(paste0('- remote url:   ', cli::col_cyan(object@remote_repository), '\n'))
    cat(paste0('- Title: ', cli::style_underline(object@title), '\n'))
    cat(paste0("- Releases(", length(releases(object)), "):\n"))
    d <- cli::cli_div(theme = list(ul = list(`margin-left` = 2, before = "")))
    cli::cli_ul(cli::col_grey(releases(object)))
    cli::cli_end(d)
    cat(paste0("- Chapters(", length(chapters(object)), "):\n"))
    d <- cli::cli_div(theme = list(ul = list(`margin-left` = 2, before = "")))
    cli::cli_ul(paste0(
        cli::col_grey(stringr::str_trunc(names(chapters(object)), 30) |> stringr::str_pad(31, "right")),
        ' [', 
        cli::col_cyan(stringr::str_trunc(gsub(path(object), "", chapters(object)), 30)),
        ']')
    )
    cli::cli_end(d)

})
