#' @importFrom methods setClass
#' @exportClass BiocBook

methods::setClass("BiocBook", 
    slots = c(
        title = "character", 
        local_path = "character", 
        remote_repository = "character", 
        releases = "character", 
        chapters = "character"
    )
)

#' @export 
#' @importFrom methods new
#' @importFrom rprojroot find_root_file
#' @importFrom rprojroot has_file
#' @importFrom yaml read_yaml

BiocBook <- function(path = '.') {
    tryCatch(
        expr = {rprojroot::find_root_file(criterion = is_biocbook, path = path)}, 
        error = function(e) {
            cli::cli_abort("The provided path is not a `BiocBook` repository")
            stop()
        },
        finally = TRUE
    )
    tryCatch(
        expr = {normalizePath(path) != rprojroot::find_root_file(criterion = is_biocbook, path = path)}, 
        error = function(e) {
            cli::cli_abort("The provided path is not a `BiocBook` repository")
            stop()
        },
        finally = TRUE
    )
    local.path <- rprojroot::find_root_file(criterion = is_biocbook, path = path)
    book.yml <- rprojroot::find_root_file(
        'inst/assets/_book.yml', criterion = is_biocbook, path = path
    )
    title <- yaml::read_yaml(book.yml)$book$title
    repo <- gert::git_remote_list(repo = local.path)$url
    releases <- gert::git_branch_list(repo = local.path) |> 
        dplyr::filter(grepl('origin.*devel|origin.*RELEASE', name)) |> 
        dplyr::pull(name) |> 
        gsub("origin/", "", x = _)
    chapters <- rprojroot::find_root_file(
        yaml::read_yaml(book.yml)$book$chapters, criterion = is_biocbook, path = path
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

    biocbook <- methods::new("BiocBook", 
        title = title,
        local_path = local.path, 
        remote_repository = repo, 
        releases = releases, 
        chapters = chapters  
    )
    return(biocbook)
}

#' @importMethodsFrom BiocGenerics path
#' @importMethodsFrom methods show
#' @importFrom stringr str_trunc
#' @importFrom stringr str_pad
#' @exportMethod releases
#' @exportMethod chapters
#' @exportMethod path
#' @exportMethod title
#' @exportMethod show

setMethod("path", signature("BiocBook"), function(object) object@local_path)
setGeneric("releases", function(object) {standardGeneric("releases")})
setMethod("releases", signature("BiocBook"), function(object) object@releases)
setGeneric("chapters", function(object) {standardGeneric("chapters")})
setMethod("chapters", signature("BiocBook"), function(object) {
    book.yml <- .find_path('inst/assets/_book.yml', object)
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
setGeneric("title", function(object) {standardGeneric("title")})
setMethod("title", signature("BiocBook"), function(object) object@title)
setMethod("show", signature("BiocBook"), function(object) {

    cat('BiocBook object\n')
    cat(paste0('local path:   ', path(object)), '\n')
    cat(paste0('remote url:   ', object@remote_repository), '\n')
    cat('-----------------\n')
    cat(paste0('Title: ', title(object), '\n'))
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
