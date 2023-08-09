is_biocbook <- rprojroot::has_file("_quarto.yml", contents = "^  type: book", n = 3)

.get_biocbook_proj <- function() {
    return(is_biocbook$find_file())
}

.check_biocbook_proj <- function() {
    if (getwd() != .get_biocbook_proj()) 
        stop("Please go to a BiocBook root directory.")
}

.find_path <- function(file, book, .from_book_root = FALSE) {
    path <- rprojroot::find_root_file(
        file, criterion = is_biocbook, path = path(book)
    )
    if (.from_book_root) {
        path <- gsub(path(book), "", path)
    }
    return(path)
}

#' @importFrom tools file_path_sans_ext
#' @importFrom purrr pluck
#' @importFrom purrr map_dfr
#' @importFrom tibble tibble
#' @importFrom dplyr case_when
#' @export

BiocBook_status <- function(biocbook) {
    purrr::map_dfr(releases(biocbook), function(release) {

        GH_api <- "https://api.github.com"
        gh_creds <- gitcreds::gitcreds_get()
        user <- gh::gh_whoami()$login
        email <- gert::git_config_global()$value[gert::git_config_global()$name == 'user.email']
        sig <- gert::git_signature(name = user, email)
        headers <- httr::add_headers(
            Accept = "application/vnd.github+json", 
            Authorization = glue::glue("Bearer {gh_creds$password}"), 
            "X-GitHub-Api-Version" = "2022-11-28"
        )
        repo <- basename(biocbook@remote_repository) |> tools::file_path_sans_ext()
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

