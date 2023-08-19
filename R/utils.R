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

#' @rdname BiocBook-utils
#' @export 

BiocBook_publish <- function(book, message = "\U1F680 Publishing") {

    f <- gert::git_status(repo = path(book), pathspec = 'inst/')
    f <- f[!f$staged, ]
    if (nrow(f) == 0) cli::cli_abort(
        "No files to stage."
    )
    staged <- gert::git_add(files = f$file, repo = path(book))
    cli::cli_alert_success(cli::col_grey("Staged {nrow(staged)} updated/new file(s)"))
    hash <- gert::git_commit(message = message, repo = path(book))
    cli::cli_alert_success(paste0(
        cli::col_grey("Committed all staged files"), 
        " [commit: ", 
        cli::col_cyan(stringr::str_trunc(hash, 7, ellipsis = '')), 
        "]"
    ))
    gert::git_push(repo = path(book))
    cli::cli_alert_success(paste0(
        cli::col_grey("Pushed to Github"), 
        " [remote: ", 
        cli::col_cyan(book@remote_repository), 
        "]"
    ))

}

#' @rdname BiocBook-utils
#' @export 

BiocBook_preview <- function(book, browse = FALSE, watch = FALSE) {

    quarto::quarto_preview(file.path(path(book), 'inst'), browse = browse, watch = watch)

}

#' @rdname BiocBook-utils
#' @export

BiocBook_versions <- function(book) {
    purrr::map_dfr(releases(book), function(release) {

        GH_api <- "https://api.github.com"
        PAT <- gitcreds::gitcreds_get()$password
        gh_creds <- gh::gh_whoami(.token = PAT)
        user <- gh_creds$login
        headers <- httr::add_headers(
            Accept = "application/vnd.github+json", 
            Authorization = glue::glue("Bearer {PAT}"), 
            "X-GitHub-Api-Version" = "2022-11-28"
        )
        repo <- basename(book@remote_repository) |> tools::file_path_sans_ext()
        runs <- httr::GET(
            glue::glue("{GH_api}/repos/{user}/{repo}/actions/runs"), 
            headers, 
            query = list(branch = release), 
            encode = 'json'
        ) |> httr::content() |> 
            purrr::pluck(2) |>
            purrr::map_dfr(~ {tibble::tibble(
            branch = .x$head_branch, 
            id = .x$id, 
            commit = .x$head_sha, 
            commit_message = .x$display_title, 
            completed_at = .x$completed_at, 
            status = .x$status, 
            conclusion = .x$conclusion
        )})
        jobs_latest_run <- httr::GET(
            glue::glue("{GH_api}/repos/{user}/{repo}/actions/runs/{runs[1, ][['id']]}/jobs"), 
            headers, 
            encode = 'json'
        ) 
        purrr::map_dfr(httr::content(jobs_latest_run)[[2]], ~ {tibble::tibble(
            branch = .x$head_branch, 
            name = dplyr::case_when(grepl("^Build and push", .x$name) ~ "Docker image", grepl("^Render and publish", .x$name) ~ "Website", .default = "Other"), 
            conclusion = .x$conclusion, 
            commit = .x$head_sha, 
            completed_at = .x$completed_at
        )})

    })
}
