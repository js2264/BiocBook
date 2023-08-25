#' @include imports.R
#' @include doc.R
#' @include BiocBook.R
NULL

### New generics

#' @rdname BiocBook
#' @export
setGeneric("releases", function(object) standardGeneric("releases"))

#' @rdname BiocBook
#' @export
setGeneric("chapters", function(object) standardGeneric("chapters"))

### Methods

#' @rdname BiocBook
#' @export

setMethod("path", signature("BiocBook"), function(object) object@local_path)

#' @rdname BiocBook
#' @export

setMethod("releases", signature("BiocBook"), function(object) {
    local.path <- rprojroot::find_root_file(
        criterion = is_biocbook, path = path(object)
    )
    releases <- tryCatch(
        gert::git_branch_list(repo = local.path) |> 
                dplyr::filter(grepl('origin.*devel|origin.*RELEASE', name)) |> 
                dplyr::pull(name) |> 
                gsub("origin/", "", x = _), 
        error = function(e) {
            return("<unset>")
        }
    )
    releases
})

#' @rdname BiocBook
#' @export

setMethod("chapters", signature("BiocBook"), function(object) {
    book.yml <- rprojroot::find_root_file(
        file.path('inst', 'assets', '_book.yml'), 
        criterion = is_biocbook, path = path(object)
    )
    chapters <- rprojroot::find_root_file(
        file.path('inst', yaml::read_yaml(book.yml)$book$chapters), 
        criterion = is_biocbook, path = path(object)
    )
    purrr::map(chapters, ~ {
        if (!file.exists(.x)) cli::cli_abort(
            "The chapter `{.x}` is listed in `{book.yml}` but the file is not found."
        )
    })
    names(chapters) <- lapply(chapters, function(chap) {
        has_yaml <- readLines(chap, n = 1) |> grepl("^---|^`", x = _)
        if (has_yaml) {
            chaplines <- readLines(chap)
            nlinesyaml <- which(grepl("^---|^`", x = chaplines))[2] 
            chaplines <- chaplines[seq(nlinesyaml + 2, length(chaplines))]
        } 
        else {
            chaplines <- readLines(chap)
        }
        chaplines <- chaplines[grepl("^# ", chaplines)]
        head <- gsub("^# ", "", chaplines[1])
        head <- gsub(" \\{-\\}", "", head)
    }) |> unlist()
    chapters
})

#' @rdname BiocBook
#' @export

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
        cli::col_cyan(gsub(path(object), "", chapters(object))),
        ']')
    )
    cli::cli_end(d)

})
