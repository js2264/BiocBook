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
#' @importFrom gh gh_whoami
#' @importFrom gitcreds gitcreds_get
#' @importFrom gert git_push
#' @importFrom gert git_commit_all
#' @importFrom gert git_remote_list
#' @importFrom gert git_clone
#' @importFrom usethis proj_activate
#' @importFrom usethis git_sitrep
#' @importFrom yaml read_yaml
#' @importFrom yaml write_yaml
#' @importFrom here here

create_BiocBook <- function(new_package = "BiocBook") {

    ## Check that a folder named `new_package` can be created 
    cli::cli_progress_message("{cli::pb_spin} Checking that no folder named `{new_package}` already exists")
    if (file.exists(new_package)) {
        cli::cli_abort("A folder named {new_package} already exists.")
    }

    ## Check that user is logged in Github
    cli::cli_progress_message("{cli::pb_spin} Checking Github credentials")
    GH_api <- "https://api.github.com"
    gh_creds <- gitcreds::gitcreds_get()
    user <- gh::gh_whoami()$login
    headers <- httr::add_headers(
        Accept = "application/vnd.github+json", 
        Authorization = glue::glue("Bearer {gh_creds$password}"), 
        "X-GitHub-Api-Version" = "2022-11-28"
    )
    Sys.sleep(5)
    cli::cli_alert_success("Successfully logged in Github")
    cli::cli_ul(c(
        "user: `{user}`", 
        "token: `{gh::gh_whoami()$token}`"
    ))

    ## Check that package name is valid
    cli::cli_progress_message("{cli::pb_spin} Checking package name availability")
    avail <- available::available(name = new_package, browse = FALSE)
    if (any(!unlist(avail[1:3]))) {
        print(avail)
        cli::cli_abort("Package name is not available. Please pick another name for this BiocBook.")
    }
    cli::cli_alert_success("Package name `{new_package}` is available")

    ## Create new repo from BiocBook.template 
    cli::cli_progress_message("{cli::pb_spin} Creating new Github repository from `js2264/BiocBook.template`")
    repo <- httr::POST(
        glue::glue("{GH_api}/repos/{user}/BiocBook.template/generate"), 
        headers, 
        body = list(owner = user, name = new_package), 
        encode = 'json'
    )
    Sys.sleep(5)
    if (!is.null(httr::content(repo)$errors)) {
        if (httr::content(repo)$errors[[1]] == "Could not clone: Name already exists on this account") {
            cli::cli_abort("A Github repo named `{new_package}` already exists for user `{user}`.")
        }
    }
    cli::cli_alert_success("New Github repository `{user}/{new_package}` successfully created")

    ## Clone package
    cli::cli_progress_message("{cli::pb_spin} Cloning `{user}/{new_package}`")
    repo <- gert::git_clone(glue::glue("https://github.com/{user}/{new_package}"))
    cli::cli_alert_success("Remote Github repository `{user}/{new_package}` cloned: `{repo}`")

    ## Activate project in newly cloned package
    usethis::proj_activate(repo)
    usethis::git_sitrep(scope = 'project')

    ## Fix placeholders
    Package = new_package
    package = tolower(new_package)
    
    # ---- in `inst/assets/_book.yml`
    yml <- readLines(file.path(repo, "inst/assets/_book.yml"))
    yml <- gsub("<Package_name>", Package, yml)
    yml <- gsub("<package_name>", package, yml)
    yml <- gsub("<github_user>", user, yml)
    writeLines(yml, file.path(repo, "inst/assets/_book.yml"))
    cli::cli_alert_success("Filled out `inst/assets/_book.yml` fields")
    cli::cli_alert_info("If you wish to change the cover picture, please replace the following file:")
    cli::cli_ul(c(
        "`inst/assets/bioc.png`"
    ))

    # ---- in `DESCRIPTION`
    descr <- readLines(file.path(repo, "DESCRIPTION"))
    descr <- gsub("<Package_name>", Package, descr)
    descr <- gsub("<github_user>", user, descr)
    writeLines(descr, file.path(repo, "DESCRIPTION"))
    cli::cli_alert_success("Filled out `DESCRIPTION` fields")
    cli::cli_alert_info("Please finish editing the `DESCRIPTION` file, including:")
    cli::cli_ul(c(
        "Title", "Description", "Authors@R"
    ))

    # ---- in `index.qmd`
    idx <- readLines(file.path(repo, "index.qmd"))
    idx <- gsub("<Package_name>", Package, idx)
    idx <- gsub("<package_name>", package, idx)
    idx <- gsub("<github_user>", user, idx)
    writeLines(idx, file.path(repo, "index.qmd"))
    cli::cli_alert_success("Filled out `index.qmd` file")
    cli::cli_alert_info("Please finish editing the `index.qmd` file, including the `Welcome` section")

    # ---- in GHA workflow
    gha <- readLines(file.path(repo, ".github/workflows/build-and-deploy.yaml"))
    gha[1] <- glue::glue("name: {package}")
    writeLines(gha, file.path(repo, ".github/workflows/build-and-deploy.yaml"))

    ## Committing everything 
    commit <- gert::git_commit_all(message = "First commit")
    gert::git_push()
    cli::cli_alert_success("Pushed all changes to origin: `{gert::git_remote_list()$url[1]}`")

    invisible(repo)

}

