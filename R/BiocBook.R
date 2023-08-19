#' @rdname BiocBook
#' @include imports.R
#' @include doc.R
#' @export 

methods::setClass("BiocBook", 
    slots = c(
        title = "character", 
        local_path = "character", 
        remote_repository = "character", 
        releases = "character", 
        chapters = "character"
    )
)

#' @rdname BiocBook
#' @include imports.R
#' @include doc.R
#' @export

BiocBook_init <- function(new_package, skip_availability = FALSE, template = "js2264/BiocBook.template", commit = NA) {

    ## Check that a folder named `new_package` can be created 
    cli::cat_rule("Running preflight checklist", col = "cyan", line = 2)
    cli::cli_progress_message(cli::col_grey("{cli::pb_spin} Checking that no folder named `{new_package}` already exists"))
    Sys.sleep(1)
    if (file.exists(new_package)) {
        cli::cli_abort("A folder named {new_package} already exists.")
    }

    ## Check that user is logged in Github
    cli::cli_progress_message(cli::col_grey("{cli::pb_spin} Checking Github credentials"))
    Sys.sleep(1)
    GH_api <- "https://api.github.com"
    PAT <- gitcreds::gitcreds_get()$password
    gh_creds <- gh::gh_whoami(.token = PAT)
    user <- gh_creds$login
    emails <- gh::gh("/user/emails", .token = PAT)
    email <- emails[[which(purrr::map_lgl(emails, ~ .x[['primary']]))]]$email
    sig <- gert::git_signature(name = user, email)
    headers <- httr::add_headers(
        Accept = "application/vnd.github+json", 
        Authorization = glue::glue("Bearer {PAT}"), 
        "X-GitHub-Api-Version" = "2022-11-28"
    )
    cli::cli_alert_success(cli::col_grey("Successfully logged in Github"))
    cli::cli_ul(c(
        cli::col_grey("user: `{user}`"),
        cli::col_grey("token: `{stringr::str_trunc(PAT, width = 18, side = 'center')}`")
    ))
    Sys.sleep(1)

    ## Check that package name is valid
    if (!skip_availability) {
        cli::cli_progress_message(cli::col_grey("{cli::pb_spin} Checking package name availability"))
        Sys.sleep(1)
        tryCatch(
            expr = {
                x <- available::available(name = new_package, browse = FALSE)
                if (any(!unlist(x[1:3]))) {
                    print(x)
                    cli::cli_abort("Package name is not available. Please pick another name for this BiocBook.")
                }
                cli::cli_alert_success(cli::col_grey("Package name `{new_package}` is available"))
            }, 
            warning = function(e) {
                cli::cli_alert_warning(cli::col_grey("`available` server is currently unavailable. Skipping validation..."))
            }, 
            error = function(e) {
                cli::cli_alert_warning(cli::col_grey("`available` server is currently unavailable. Skipping validation..."))
            }
        )
        Sys.sleep(1)
    }

    ## Create new repo from BiocBook.template 
    cli::cli_text("")
    cli::cat_rule("Initiating a new `BiocBook`", col = "cyan", line = 2)
    cli::cli_progress_message(cli::col_grey("{cli::pb_spin} Creating new Github repository from `{template}`"))
    Sys.sleep(1)
    repo <- httr::POST(
        glue::glue("{GH_api}/repos/{template}/generate"), 
        headers, 
        body = list(owner = user, name = new_package), 
        encode = 'json'
    )
    Sys.sleep(1)
    if (!is.null(httr::content(repo)$errors)) {
        if (httr::content(repo)$errors[[1]] == "Could not clone: Name already exists on this account") {
            cli::cli_abort("A Github repo named `{new_package}` already exists for user `{user}`.")
        }
    }
    if (!is.null(httr::content(repo)$message)) {
        if (httr::content(repo)$message == "Bad credentials") {
            cli::cli_abort("`{user}` [token: `{stringr::str_trunc(PAT, width = 18, side = 'center')}`] invalid.")
        }
    }
    cli::cli_alert_success(cli::col_grey("New Github repository `{user}/{new_package}` successfully created"))
    Sys.sleep(1)

    ## Clone package
    cli::cli_progress_message(cli::col_grey("{cli::pb_spin} Cloning `{user}/{new_package}`"))
    Sys.sleep(1)
    repo <- gert::git_clone(glue::glue("https://github.com/{user}/{new_package}"))
    cli::cli_alert_success(cli::col_grey("Remote Github repository `{user}/{new_package}` cloned: `{repo}`"))
    Sys.sleep(1)

    ## Fix placeholders
    # ---- in `_book.yml`
    path <- file.path("inst", "assets", "_book.yml")
    full.path <- file.path(repo, path)
    yml <- readLines(full.path)
    yml <- gsub("<Package_name>", new_package, yml)
    yml <- gsub("<package_name>", tolower(new_package), yml)
    yml <- gsub("<github_user>", user, yml)
    writeLines(yml, full.path)
    cli::cli_alert_success(cli::col_grey("Filled out `{path}` fields"))
    Sys.sleep(1)

    # ---- in `DESCRIPTION`
    path <- "DESCRIPTION"
    full.path <- file.path(repo, path)
    descr <- readLines(full.path)
    descr <- gsub("<Package_name>", new_package, descr)
    descr <- gsub("<package_name>", tolower(new_package), descr)
    descr <- gsub("<github_user>", user, descr)
    writeLines(descr, full.path)
    cli::cli_alert_success(cli::col_grey("Filled out `{path}` fields"))
    cli::cli_alert_info(cli::col_grey("Please finish editing the `{path}` fields, including:"))
    cli::cli_ul(c(
        "  Title", "  Description", "  Authors@R"
    ))
    Sys.sleep(1)

    # ---- in `index.qmd`
    path <- file.path("inst", "index.qmd")
    full.path <- file.path(repo, path)
    idx <- readLines(full.path)
    idx <- gsub("<Package_name>", new_package, idx)
    idx <- gsub("<package_name>", tolower(new_package), idx)
    idx <- gsub("<github_user>", user, idx)
    writeLines(idx, full.path)
    cli::cli_alert_success(cli::col_grey("Filled out `{path}` file"))
    cli::cli_alert_info(cli::col_grey("Please finish editing the `{path}` file, including the `Welcome` section"))
    Sys.sleep(1)

    # ---- in GHA workflow
    path <- file.path(".github", "workflows", "build-and-deploy.yaml")
    full.path <- file.path(repo, path)
    gha <- readLines(full.path)
    gha <- gsub("<Package_name>", new_package, gha)
    gha <- gsub("<package_name>", tolower(new_package), gha)
    gha <- gsub("<github_user>", tolower(user), gha)
    writeLines(gha, full.path)
    cli::cli_alert_success(cli::col_grey("Filled out `{path}` file"))
    Sys.sleep(1)

    ## Committing everything 
    cli::cli_alert_info(cli::col_grey("Several files have been automatically edited: "))
    cli::cli_ul(gert::git_status(repo = repo)$file)
    Sys.sleep(1)
    commit_sha <- gert::git_commit_all(repo = repo, message = "Adapted from BiocBook.template", sig)
    cli::cli_alert_success(cli::col_grey("These changes have been commited to the local repository."))
    Sys.sleep(1)
    msg <- glue::glue("Is it ok to push these changes to Github?")
    if (!is.na(commit)) {
        if (commit) {
            gert::git_push(repo = repo)
            cli::cli_alert_success(cli::col_grey("Commits pushed to origin: `{gert::git_remote_list(repo = repo)$url[1]}`"))
        }
        else {
            invisible(BiocBook(repo))
        }
    }
    else if (usethis::ui_yeah(msg)) {
        gert::git_push(repo = repo)
        cli::cli_alert_success(cli::col_grey("Commits pushed to origin: `{gert::git_remote_list(repo = repo)$url[1]}`"))
    } 
    else {
        cli::cli_alert_info(cli::col_grey("Don't forget to push the latest commit to the remote `origin`."))
    }

    ## Remaining placeholders
    cli::cli_text("")
    cli::cat_rule("NOTES", col = "grey", line = 1)
    cli::cli_alert_info(cli::col_grey("If you wish to change the cover picture, please replace the following file:"))
    cli::cli_ul(c(
        file.path("inst", "assets", "cover.png")
    ))
    Sys.sleep(1)

    ## Wrapping up
    cli::cli_text("")
    cli::cat_rule("Results", col = "cyan", line = 2)
    cli::cli_alert_success("Local `BiocBook` directory successfully created  : `{repo}`")
    cli::cli_alert_success("Remote `BiocBook` repository successfully created: `{gert::git_remote_list(repo = repo)$url[1]}`")
    cli::cli_text("")
    cli::cli_text(cli::col_grey('# You can connect to the local directory as follows: \n') )
    cli::cli_code(glue::glue("  biocbook <- BiocBook('{repo}')"))
    cli::cli_text("")
    invisible(BiocBook(repo))

}

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

    ## Check title
    repo <- gert::git_remote_list(repo = local.path)$url

    ## Check releases based on which branches exist on `origin` remote
    releases <- gert::git_branch_list(repo = local.path) |> 
        dplyr::filter(grepl('origin.*devel|origin.*RELEASE', name)) |> 
        dplyr::pull(name) |> 
        gsub("origin/", "", x = _)
    
    ## Check chapters
    chapters <- rprojroot::find_root_file(
        file.path('inst', yaml::read_yaml(book.yml)$book$chapters), criterion = is_biocbook, path = path
    )
    purrr::map(chapters, ~ {
        if (!file.exists(.x)) cli::cli_abort(
            "The chapter `{.x}` is listed in `{book.yml}` but the file is not found."
        )
    })
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

    ## Initiate the new BiocBook object
    biocbook <- methods::new("BiocBook", 
        title = title,
        local_path = local.path, 
        remote_repository = repo, 
        releases = releases, 
        chapters = chapters  
    )
    return(biocbook)
}

#' @rdname BiocBook
#' @include imports.R
#' @include doc.R
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
