#' @importFrom methods setClass
#' @importFrom methods new
#' @importFrom rprojroot find_root_file
#' @importFrom rprojroot has_file
#' @importFrom yaml read_yaml
#' @importFrom httr POST
#' @importFrom httr GET
#' @importFrom httr add_headers
#' @importFrom httr content
#' @importFrom glue glue
#' @importFrom available available
#' @importFrom cli cli_progress_message
#' @importFrom cli cli_alert_success
#' @importFrom cli cli_alert_info
#' @importFrom cli cli_abort
#' @importFrom cli cli_ul
#' @importFrom cli pb_spin
#' @importFrom gh gh
#' @importFrom gh gh_whoami
#' @importFrom gitcreds gitcreds_get
#' @importFrom gert git_push
#' @importFrom gert git_config_global
#' @importFrom gert git_commit_all
#' @importFrom gert git_remote_list
#' @importFrom gert git_clone
#' @importFrom gert git_signature
#' @importFrom usethis proj_activate
#' @importFrom usethis git_sitrep
#' @importFrom usethis ui_yeah
#' @importFrom tools file_path_sans_ext
#' @importFrom purrr pluck
#' @importFrom purrr map_dfr
#' @importFrom tibble tibble
#' @importFrom dplyr case_when
#' @exportClass BiocBook
NULL

#' @title Handling BiocBook directories
#' @name BiocBook
#' 
#' @description 
#' 
#' `BiocBook`s are local R packages containing an extra `pages` folder to 
#' write up online book chapters.
#' 
#' 1. A new `BiocBook` should be created using `BiocBook_init(new_package = "...")`.  
#' 2. A newly created `BiocBook` can be accessed to in R using `biocbook <- BiocBook(path = "...")`. 
#' 3. To edit an existing `BiocBook` object, several helper functions exist: 
#'   - `add_preamble(biocbook)` 
#'   - `add_chapter(biocbook, title = "...")` 
#'   - `edit_page(biocbook, page = "...")` 
#' 
#' Read the different sections down below for more details. 
#' 
#' @section The `BiocBook` class:
#' 
#' A `BiocBook` object acts as a pointer to a local package directory, with 
#' book chapters contained in a `pages/` folder as `.qmd` files.  
#' 
#' This package directory requires a specific architecture, which is 
#' best set up using the `BiocBook_init()` function. 
#' 
#' @section Creating a `BiocBook`:
#' 
#' A new `BiocBook` should be created using the `BiocBook_init(new_package = "...")` function.  
#' This function performs the following operations: 
#' 
#' 1. It checks that the provided package name is available;
#' 2. It logs in the GitHub user accounts; 
#' 3. It creates a new **remote** Github repository using the `BiocBook` template from `js2264/BiocBook`; 
#' 4. It clones the **remote** Github repository to a local folder; 
#' 5. It edits several placeholders from the template and commits the changes. 
#' 
#' The `BiocBook_init(new_package = "...")` function returns a `BiocBook` object. 
#' 
#' @section Editing an existing `BiocBook`:
#' 
#' `BiocBook` objects can be modified using the following helper functions: 
#' 
#' - `add_preamble(biocbook)` to start writing a preamble; 
#' - `add_chapter(biocbook, title = "...")` to start writing a new chapter;  
#' - `edit_page(biocbook, page = "...")` to edit an existing chapter.
#' 
#' @section Publishing an existing `BiocBook`:
#' 
#' As long as the local `BiocBook` has been initiated with `BiocBook_init()`, 
#' the writer simply has to commit changes and push them to the `origin` remote.  
#' 
#' In `R`, this can be done as follows: 
#' 
#' `gert::git_commit_all(message, repo = path(biocbook))`
#' 
#' The different available versions published in the `origin` `gh-pages` branch 
#' can be listed using `BiocBook_versions(biocbook)`
#' 
#' @param new_package Name to use when initiating a new `BiocBook`. 
#' This name should be compatible with package naming conventions 
#' from R and Bioconductor (i.e. no `_` or `-`, no name starting with a number).
#' @param skip_availability Optional. Whether to skip package name availability (default: FALSE).
#' @param template Optional. Github repository used for `BiocBook` template (default: `js2264/BiocBook.template`). 
#' @param commit Optional. Logical, whether to automatically push commits to remote Github origin (default: FALSE). 
#' @param path Path of an existing `BiocBook`. 
#' @param book A `BiocBook` object, created by `BiocBook` or `BiocBook_init()`.
#' @param title A character string for a title for the new chatper. If `file` is not explicitely provided, the 
#' title should only contain alphanumeric characters and spaces
#' @param file Optional. A character string for the name of the `.qmd` file to write the new chapter.
#' The extension `.qmd` has to be provided. If not provided, 
#' the file name is deduced from the `title` argument. 
#' @param position Optional. A position to insert the chapter. For example, 
#' if `position = 2`, the new chapter will be inserted after the first existing
#' chapter (i.e. the `Welcome` page)
#' @param open Optional. Whether to open the file for interactive editing (default: TRUE)
NULL

#' @rdname BiocBook
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
#' @export

BiocBook_init <- function(new_package, skip_availability = FALSE, template = "js2264/BiocBook.template", commit = FALSE) {

    ## Check that a folder named `new_package` can be created 
    cli::cli_progress_message("{cli::pb_spin} Checking that no folder named `{new_package}` already exists")
    Sys.sleep(1)
    if (file.exists(new_package)) {
        cli::cli_abort("A folder named {new_package} already exists.")
    }

    ## Check that user is logged in Github
    cli::cli_progress_message("{cli::pb_spin} Checking Github credentials")
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
    cli::cli_alert_success("Successfully logged in Github")
    cli::cli_ul(c(
        "user: `{user}`", 
        "token: `{PAT}`"
    ))
    Sys.sleep(1)

    ## Check that package name is valid
    if (!skip_availability) {
        cli::cli_progress_message("{cli::pb_spin} Checking package name availability")
        Sys.sleep(1)
        tryCatch(
            expr = {
                x <- available::available(name = new_package, browse = FALSE)
                if (any(!unlist(x[1:3]))) {
                    print(x)
                    cli::cli_abort("Package name is not available. Please pick another name for this BiocBook.")
                }
                cli::cli_alert_success("Package name `{new_package}` is available")
            }, 
            warning = function(e) {
                cli::cli_alert_warning("`available` server is currently unavailable. Skipping validation...")
            }, 
            error = function(e) {
                cli::cli_alert_warning("`available` server is currently unavailable. Skipping validation...")
            }
        )
        Sys.sleep(1)
    }

    ## Create new repo from BiocBook.template 
    cli::cli_progress_message("{cli::pb_spin} Creating new Github repository from `{template}`")
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
            cli::cli_abort("`{user}` [token: `{PAT}`] invalid.")
        }
    }
    cli::cli_alert_success("New Github repository `{user}/{new_package}` successfully created")
    Sys.sleep(1)

    ## Clone package
    cli::cli_progress_message("{cli::pb_spin} Cloning `{user}/{new_package}`")
    Sys.sleep(1)
    repo <- gert::git_clone(glue::glue("https://github.com/{user}/{new_package}"))
    cli::cli_alert_success("Remote Github repository `{user}/{new_package}` cloned: `{repo}`")
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
    cli::cli_alert_success("Filled out `{path}` fields")
    Sys.sleep(1)

    # ---- in `DESCRIPTION`
    path <- "DESCRIPTION"
    full.path <- file.path(repo, path)
    descr <- readLines(full.path)
    descr <- gsub("<Package_name>", new_package, descr)
    descr <- gsub("<github_user>", user, descr)
    writeLines(descr, full.path)
    cli::cli_alert_success("Filled out `{path}` fields")
    cli::cli_alert_info("Please finish editing the `{path}` fields, including:")
    cli::cli_ul(c(
        "Title", "Description", "Authors@R"
    ))
    Sys.sleep(1)

    # ---- in `index.qmd`
    path <- "index.qmd"
    full.path <- file.path(repo, path)
    idx <- readLines(full.path)
    idx <- gsub("<Package_name>", new_package, idx)
    idx <- gsub("<package_name>", tolower(new_package), idx)
    idx <- gsub("<github_user>", user, idx)
    writeLines(idx, full.path)
    cli::cli_alert_success("Filled out `{path}` file")
    cli::cli_alert_info("Please finish editing the `{path}` file, including the `Welcome` section")
    Sys.sleep(1)

    # ---- in GHA workflow
    path <- file.path(".github", "workflows", "build-and-deploy.yaml")
    full.path <- file.path(repo, path)
    gha <- readLines(full.path)
    gha <- gsub("<package_name>", tolower(new_package), gha)
    gha <- gsub("<github_user>", tolower(user), gha)
    writeLines(gha, full.path)
    cli::cli_alert_success("Filled out `{path}` file")
    Sys.sleep(1)

    ## Committing everything 
    cli::cli_alert_info("Several files have been automatically edited: ")
    cli::cli_ul(gert::git_status(repo = repo)$file)
    Sys.sleep(1)
    commit_sha <- gert::git_commit_all(repo = repo, message = "Adapted from BiocBook.template", sig)
    cli::cli_alert_success("These changes have been commited to the local repository.")
    Sys.sleep(1)
    msg <- glue::glue("Is it ok to push these changes to Github?")
    if (commit) {
        gert::git_push(repo = repo)
        cli::cli_alert_success("Commits pushed to origin: `{gert::git_remote_list(repo = repo)$url[1]}`")
    }
    else if (usethis::ui_yeah(msg)) {
        gert::git_push(repo = repo)
        cli::cli_alert_success("Commits pushed to origin: `{gert::git_remote_list(repo = repo)$url[1]}`")
    }

    ## Remaining placeholders
    cli::cli_alert_info("If you wish to change the cover picture, please replace the following file:")
    cli::cli_ul(c(
        file.path("inst", "assets", "bioc.png")
    ))
    Sys.sleep(1)

    invisible(BiocBook(repo))

}

#' @rdname BiocBook
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
        yaml::read_yaml(book.yml)$book$chapters, criterion = is_biocbook, path = path
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
