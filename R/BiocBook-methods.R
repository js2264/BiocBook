#' @importMethodsFrom BiocGenerics path
#' @importMethodsFrom methods show
#' @importFrom stringr str_trunc
#' @importFrom stringr str_pad
#' @include BiocBook.R

setMethod("path", signature("BiocBook"), function(object) object@local_path)

setGeneric("releases", function(object) {standardGeneric("releases")})
setMethod("releases", signature("BiocBook"), function(object) object@releases)

setGeneric("chapters", function(object) {standardGeneric("chapters")})
setMethod("chapters", signature("BiocBook"), function(object) {
    book.yml <- .find_path(file.path('inst', 'assets', '_book.yml'), object)
    chapters <- rprojroot::find_root_file(
        yaml::read_yaml(book.yml)$book$chapters, criterion = is_biocbook, path = path(object)
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

    cat('BiocBook object\n')
    cat(paste0('local path:   ', path(object)), '\n')
    cat(paste0('remote url:   ', object@remote_repository), '\n')
    cat('-----------------\n')
    cat(paste0('Title: ', object@title, '\n'))
    cat(paste0("Releases(", length(releases(object)), "):\n"))
    writeLines(paste0('  ', releases(object)))
    cat(paste0("Chapters(", length(chapters(object)), "):\n"))
    writeLines(paste0(
        '  ', 
        stringr::str_trunc(names(chapters(object)), 30) |> stringr::str_pad(31, "right"), 
        ' [', 
        stringr::str_trunc(gsub(path(object), "", chapters(object)), 30), 
        ']')
    )

})
