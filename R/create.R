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
#' @export

create_BiocBook <- function(new_package = "BiocBook", template = "js2264/BiocBook.template") {

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
    email <- gert::git_config_global()$value[gert::git_config_global()$name == 'user.email']
    sig <- gert::git_signature(name = user, email)
    headers <- httr::add_headers(
        Accept = "application/vnd.github+json", 
        Authorization = glue::glue("Bearer {gh_creds$password}"), 
        "X-GitHub-Api-Version" = "2022-11-28"
    )
    Sys.sleep(1)
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
    cli::cli_progress_message("{cli::pb_spin} Creating new Github repository from `{template}`")
    repo <- httr::POST(
        glue::glue("{GH_api}/repos/{template}/generate"), 
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
    msg <- glue::glue("Is it ok to change directory and go to {repo}?")
    if (usethis::ui_yeah(msg)) {
        setwd(repo)
        cli::cli_alert_success("BiocBook `{new_package}` package active: {repo}")
    }
    else {
        cli::cli_alert_info("BiocBook `{new_package}` package correctly created @ {repo}")
        cli::cli_alert_warning("Placeholders in several files have to be manually filled out before the BiocBook is functional: ")
        cli::cli_ul(list(
            "inst/assets/_book.yml",
            "DESCRIPTION",
            "index.qmd",
            ".github/workflows/build-and-deploy.yaml"
        ))
        cli::cli_alert_success("Finishing now.")
        invisible(repo)
    }

    ## Fix placeholders
    # ---- in `inst/assets/_book.yml`
    yml.path <- is_biocbook$find_file("inst/assets/_book.yml")
    yml <- readLines(yml.path)
    yml <- gsub("<Package_name>", new_package, yml)
    yml <- gsub("<package_name>", tolower(new_package), yml)
    yml <- gsub("<github_user>", user, yml)
    writeLines(yml, yml.path)
    cli::cli_alert_success("Filled out `inst/assets/_book.yml` fields")
    cli::cli_alert_success("Filled out `inst/assets/_book.yml` fields")
    cli::cli_alert_info("If you wish to change the cover picture, please replace the following file:")
    cli::cli_ul(c(
        "`inst/assets/bioc.png`"
    ))

    # ---- in `DESCRIPTION`
    descr.path <- is_biocbook$find_file("DESCRIPTION")
    descr <- readLines(descr.path)
    descr <- gsub("<Package_name>", new_package, descr)
    descr <- gsub("<github_user>", user, descr)
    writeLines(descr, descr.path)
    cli::cli_alert_success("Filled out `DESCRIPTION` fields")
    cli::cli_alert_info("Please finish editing the `DESCRIPTION` file, including:")
    cli::cli_ul(c(
        "Title", "Description", "Authors@R"
    ))

    # ---- in `index.qmd`
    idx.path <- is_biocbook$find_file("index.qmd")
    idx <- readLines(idx.path)
    idx <- gsub("<Package_name>", new_package, idx)
    idx <- gsub("<package_name>", tolower(new_package), idx)
    idx <- gsub("<github_user>", user, idx)
    writeLines(idx, idx.path)
    cli::cli_alert_success("Filled out `index.qmd` file")
    cli::cli_alert_info("Please finish editing the `index.qmd` file, including the `Welcome` section")

    # ---- in GHA workflow
    gha.path <- is_biocbook$find_file(".github/workflows/build-and-deploy.yaml")
    gha <- readLines(gha.path)
    gha <- gsub("<package_name>", tolower(new_package), gha)
    gha <- gsub("<github_user>", tolower(user), gha)
    writeLines(gha, gha.path)
    cli::cli_alert_success("Filled out `.github/workflows/build-and-deploy.yaml` file")

    ## Committing everything 
    cli::cli_alert_info("Several files need to be pushed to Github: ")
    cli::cli_ul(gert::git_status()$file)
    msg <- glue::glue("Is it ok to commit and push them to Github?")
    if (usethis::ui_yeah(msg)) {
        commit <- gert::git_commit_all(message = "Fillout placeholders", sig)
        gert::git_push()
        cli::cli_alert_success("Pushed all changes to origin: `{gert::git_remote_list()$url[1]}`")
    }

    return(BiocBook(repo))

}
