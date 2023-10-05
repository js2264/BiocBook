#' @rdname BiocBook
#' @include imports.R
#' @include doc.R
#' @include init.R
#' @export 

methods::setClass("BiocBook", 
    slots = c(
        title = "character", 
        local_path = "character", 
        remote_repository = "character"
    )
)

#' @rdname BiocBook
#' @include imports.R
#' @include doc.R
#' @export 

BiocBook <- function(path) {

    ## Check that the provided path points to a BiocBook
    tryCatch(
        expr = {rprojroot::find_root_file(criterion = is_biocbook, path = path)}, 
        error = function(e) {
            cli::cli_abort("The provided path is not a `BiocBook` repository")
        },
        finally = TRUE
    )
    tryCatch(
        expr = {normalizePath(path) != rprojroot::find_root_file(criterion = is_biocbook, path = path)}, 
        error = function(e) {
            cli::cli_abort("The provided path is not a `BiocBook` repository")
        },
        finally = TRUE
    )
    local.path <- rprojroot::find_root_file(criterion = is_biocbook, path = path)

    ## Check _book.yml
    book.yml <- rprojroot::find_root_file(
        file.path('inst', 'assets', '_book.yml'), criterion = is_biocbook, path = path
    )
    if (!file.exists(book.yml)) cli::cli_abort("Missing `{book.yml}`.")

    ## Check title
    title <- yaml::read_yaml(book.yml)$book$title
    if (is.null(title)) cli::cli_abort("Missing `title` entry from `{book.yml}`.")

    ## Check remote
    repo <- tryCatch(
        gert::git_remote_list(repo = local.path)$url, 
        error = function(e) {
            cli::cli_alert_danger(
                "This book is not synced with Github."
            )
            return("<unset>")
        }
    )

    ## Initiate the new BiocBook object
    biocbook <- methods::new("BiocBook", 
        title = title,
        local_path = local.path, 
        remote_repository = repo
    )
    return(biocbook)
}
