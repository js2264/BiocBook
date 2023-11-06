#' @rdname BiocBook
#' @include imports.R
#' @include doc.R
#' @export

init <- function(
    new_package, 
    push = NA, 
    skip_availability = FALSE, 
    .local = FALSE  
) {

    ## 0. Preflight checklist
    checks <- .preflight_checks(new_package, skip_availability, .local)
    gitsig <- checks[['gitsig']]
    gh_user <- checks[['gh_user']]
    PAT <- checks[['PAT']]
    repo <- new_package

    ## 1. Create new local repo copied from BiocBook.template archive
    .init_folder(repo, gh_user)
    
    ## 2. Initiate local git repo
    .setup_git(repo, gitsig)

    ## 3. Syncing Github: create new repo, configure Pages, add remote, push
    if (!is.null(PAT)) .setup_github(repo, gh_user, PAT, push)

    ## 4. Wrap up info 
    .wrap_up_info(repo, .local)

    invisible(BiocBook(repo))

}

.preflight_checks <- function(new_package, skip_availability = FALSE, .local = FALSE) {

    cli::cat_rule("Running preflight checklist", col = "cyan", line = 2)
    cli::cli_progress_message(cli::col_grey("{cli::pb_spin} Checking that no folder named `{new_package}` already exists"))
    Sys.sleep(1)

    ## Check that a folder named `new_package` can be created 
    if (file.exists(new_package)) {
        cli::cli_abort("A folder named {new_package} already exists.")
    }

    ## Check that package name is valid
    if (!skip_availability) {
        cli::cli_progress_message(cli::col_grey("{cli::pb_spin} Checking package name availability"))
        Sys.sleep(1)
        tryCatch(
            expr = {
                x <- available::available(name = new_package, browse = FALSE)
                if (any(!unlist(x[c(1, 2, 3)]))) {
                    print(x)
                    cli::cli_abort("Package name is not available. Please pick another name for this BiocBook.")
                }
                cli::cli_alert_success(cli::col_grey("Package name `{new_package}` is available"))
            }
        )
        Sys.sleep(1)
    }

    ## Check git and github creds
    creds <- .check_creds(new_package, .local)

    return(creds)
}

.check_creds <- function(new_package, .local = FALSE) {

    ## git
    if (!.local) {
        git_creds <- .creds_git()
    }
    else {
        gituser <- "dummy"
        gitmail <- "dummy@dummy.com"
        gitsig <- gert::git_signature(name = gituser, email = gitmail)
        cli::cli_alert_warning(cli::col_grey("Dummy git configured"))
        cli::cli_ul(c(
            cli::col_grey("git user: `{gituser}`"),
            cli::col_grey("git email: `{gitmail}`")
        ))
        Sys.sleep(1)
        git_creds <- list(
            gituser = gituser, 
            gitmail = gitmail, 
            gitsig = gitsig 
        )
    }

    ## github
    if (!.local) {
        gh_creds <- .creds_github(new_package)
    }
    else {
        gh_user <- "dummy"
        PAT <- NULL
        cli::cli_alert_warning(cli::col_grey("No GitHub configured"))
        cli::cli_ul(c(
            cli::col_grey("github user: NULL"),
            cli::col_grey("PAT: NULL")
        ))
        Sys.sleep(1)
        gh_creds <- list(gh_user = gh_user, PAT = PAT)
    }

    ## final credentials
    creds <- c(git_creds, gh_creds)
    return(creds)
}

.creds_git <- function(.local = FALSE) {

    ## Check that git is configured 
    cli::cli_progress_message(cli::col_grey("{cli::pb_spin} Checking git configuration"))
    Sys.sleep(1)
    gitconf <- gert::git_config_global()
    if (!"user.name" %in% gitconf$name)
        cli::cli_abort("Missing `user.name` in the git global configuration.\
        Set it with `gert::git_config_global_set('user.name', value = '...')`.")
    if (!"user.email" %in% gitconf$name)
        cli::cli_abort("Missing `user.email` in the git global configuration.\
        Set it with `gert::git_config_global_set('user.email', value = '...')`.")
    gituser <- gitconf$value[gitconf$name == "user.name"]
    gitmail <- gitconf$value[gitconf$name == "user.email"]
    gitsig <- gert::git_signature(name = gituser, email = gitmail)
    cli::cli_alert_success(cli::col_grey("Git successfully configured"))
    cli::cli_ul(c(
        cli::col_grey("git user: `{gituser}`"),
        cli::col_grey("git email: `{gitmail}`")
    ))
    Sys.sleep(1)
    return(list(
        gituser = gituser, 
        gitmail = gitmail, 
        gitsig = gitsig 
    ))
}

.creds_github <- function(new_package) {

    ## Check that user is logged in Github
    cli::cli_progress_message(cli::col_grey("{cli::pb_spin} Checking Github credentials"))
    Sys.sleep(1)
    GH_api <- "https://api.github.com"
    PAT <- tryCatch(
        {gitcreds::gitcreds_get()$password}, 
        error =  function(e) {
            cli::cli_abort("Could not find any stored Github credentials. Consider adding a Github token (a.k.a. `PAT`) to your `.Renviron`.\n")
        }
    )
    gh_scopes <- gh::gh_whoami(.token = PAT)$scopes
    for (scope in c("repo", "workflow", "user:email")) {
        if (!grepl(scope, gh_scopes)) 
            cli::cli_abort("The provided PAT does not authorize the `{scope}` scope. Please change the PAT settings @ https://github.com/settings/tokens to enable this scope.\n")
    }
    gh_user <- gh::gh_whoami(.token = PAT)$login
    cli::cli_alert_success(cli::col_grey("Successfully logged in Github"))
    cli::cli_ul(c(
        cli::col_grey("user: `{gh_user}`"),
        cli::col_grey("token: `{stringr::str_trunc(PAT, width = 18, side = 'center')}`")
    ))
    Sys.sleep(1)
    
    ## Get all repos for one user
    req <- gh::gh("/users/{user}/repos", user = gh_user, per_page = 30, .token = PAT)
    gh_repos <- purrr::map_chr(req, 'name')
    while({length(gh_repos) %% 30} != 0) {
        req <- gh::gh_next(req)
        gh_repos <- c(gh_repos, purrr::map_chr(req, 'name'))
    }
    
    ## Check that new_package does not already exist
    if (new_package %in% gh_repos) {
        cli::cli_abort("A Github repo named `{new_package}` already exists for user `{gh_user}`.")
    }
    
    return(list(gh_user = gh_user, PAT = PAT))
}

.init_folder <- function(repo, user) {

    cli::cli_text("")
    cli::cat_rule("Initiating a new `BiocBook`", col = "cyan", line = 2)
    Sys.sleep(1)
    ## Extract template archive in a temp folder
    cli::cli_progress_message(cli::col_grey("{cli::pb_spin} Creating new repository from template provided in BiocBook `inst/` directory`"))
    tmpdir <- tempfile()
    dir.create(tmpdir)
    template <- system.file("template", "BiocBook.template.tar.gz", package = "BiocBook")
    utils::untar(template, exdir = tmpdir)
    ## Move files from temp folder to `new_package` folder
    dir.create(repo)
    content <- list.files(
        file.path(tmpdir, 'BiocBook.template'), 
        all.files=TRUE, full.names=TRUE, no..=TRUE
    )
    file.copy(content, repo, recursive=TRUE)
    cli::cli_alert_success(cli::col_grey(
        "New local book `{repo}` successfully created"
    ))
    Sys.sleep(1)
    ## Fix placeholders
    # ---- in `_book.yml`
    path <- file.path("inst", "assets", "_book.yml")
    .fix_placeholders(file.path(repo, path), pkg = repo, usr = user)
    cli::cli_alert_success(cli::col_grey("Filled out `{cli::col_cyan(path)}` fields"))
    Sys.sleep(1)
    # ---- in `README.md`
    path <- "README.md"
    .fix_placeholders(file.path(repo, path), pkg = repo, usr = user)
    cli::cli_alert_success(cli::col_grey("Filled out `{cli::col_cyan(path)}` fields"))
    Sys.sleep(1)
    # ---- in `DESCRIPTION`
    path <- "DESCRIPTION"
    .fix_placeholders(file.path(repo, path), pkg = repo, usr = user)
    cli::cli_alert_success(cli::col_grey("Filled out `{cli::col_cyan(path)}` fields"))
    cli::cli_alert_info(cli::col_grey("Please finish editing the `{cli::col_cyan(path)}` fields, including:"))
    d <- cli::cli_div(theme = list(ul = list(`margin-left` = 2, before = "")))
    cli::cli_ul(c("  Title", "  Description", "  Authors@R"))
    cli::cli_end(d)
    Sys.sleep(1)
    # ---- in `index.qmd`
    path <- file.path("inst", "index.qmd")
    .fix_placeholders(file.path(repo, path), pkg = repo, usr = user)
    cli::cli_alert_success(cli::col_grey("Filled out `{cli::col_cyan(path)}` fields"))
    cli::cli_alert_info(cli::col_grey("Please finish editing the `{cli::col_cyan(path)}` fields, including the `Welcome` section"))
    Sys.sleep(1)

}

.setup_git <- function(repo, gitsig) {

    gert::git_init(path = repo) 
    cli::cli_alert_info(cli::col_grey("The following files need to be committed: "))
    d <- cli::cli_div(theme = list(ul = list(`margin-left` = 2, before = "")))
    f <- gert::git_status(repo = repo)
    cli::cli_ul(f$file)
    cli::cli_end(d)
    Sys.sleep(1)

    ## Commit all changes to local git repo
    version <- read.dcf(file.path(repo, "DESCRIPTION"))[1,"BiocBookTemplate"]
    staged <- gert::git_add(files = f$file, repo = repo)
    commit_sha <- gert::git_commit(
        repo = repo, 
        message = paste0("Init BiocBook from template version ", version[[1]]), 
        author = gitsig
    )

    ## Make sure default `git` branch is named `devel`
    b <- gert::git_branch_list(repo = repo)
    if (b$name != 'devel') {
        gert::git_branch_move(repo = repo, b$name, 'devel')
    }
    cli::cli_alert_success(cli::col_grey("The new files have been commited to the `devel` branch."))
    Sys.sleep(1)

}

.setup_github <- function(repo, user, PAT, push) {

    ## Create a new Github repo
    remote <- gh::gh("POST /user/repos", name = repo, description = "BiocBook", .token = PAT)
    cli::cli_alert_success(cli::col_grey("New Github repository `{user}/{repo}` successfully created"))
    Sys.sleep(1)

    ## Connect local git to remote 
    cli::cli_progress_message(cli::col_grey("{cli::pb_spin} Adding remote Github branch to local repo"))
    Sys.sleep(1)
    gert::git_remote_add(url = remote$html_url, name = 'origin', repo = repo)

    ## Pushing first commit to Github
    prompt_msg <- glue::glue("Is it ok to push the first commit to Github?")
    success_msg <- cli::col_grey("Commits pushed to origin {cli::col_cyan(gert::git_remote_list(repo = repo)$url[1])}")
    abort_msg <- cli::col_grey("Don't forget to push the latest commit to the remote `origin`.")
    if (!is.na(push)) {
        if (push) {
            gert::git_push(repo = repo)
            cli::cli_alert_success(success_msg)
        }
        else {
            cli::cli_alert_info(abort_msg)
        }
    }
    else if (rlang::is_interactive()) {
        if (usethis::ui_yeah(prompt_msg)) {
            gert::git_push(repo = repo)
            cli::cli_alert_success(success_msg)
        }
        else {
            cli::cli_alert_info(abort_msg)
        }
    } 
    else {
        cli::cli_alert_info(abort_msg)
    }

    ## Create remote empty `gh-pages` branch [ripped from usethis]
    .setup_gh_pages_branch(repo, user)

    ## Enable Pages service
    .setup_pages(repo, user, PAT)

}

.setup_gh_pages_branch <- function(repo, user) {

    cli::cli_progress_message(cli::col_grey("{cli::pb_spin} Creating an empty local `gh-pages` branch"))
    tree <- gh(
        "POST /repos/{user}/{repo}/git/trees",
        tree = list(list(
            path = "_temp",
            mode = "100644",
            type = "blob",
            content = ""
        )), 
        user = user, repo = repo
    )
    commit <- gh(
        "POST /repos/{user}/{repo}/git/commits",
        message = "Init orphan branch",
        tree = tree$sha, 
        user = user, repo = repo
    )
    ref <- gh(
        "POST /repos/{user}/{repo}/git/refs",
        ref = glue("refs/heads/gh-pages"),
        sha = commit$sha, 
        user = user, repo = repo
    )
    del <- gh(
        "DELETE /repos/{user}/{repo}/contents/_temp",
        message = "\U1F9F9 Purging `gh-pages` branch",
        sha = purrr::pluck(tree, "tree", 1, "sha"),
        branch = 'gh-pages', user = user, repo = repo
    )
    cli::cli_alert_success(cli::col_grey("New empty `gh-pages` branch created on remote."))
    Sys.sleep(1)

}

.setup_pages <- function(repo, user, PAT) {

    cli::cli_progress_message(cli::col_grey("{cli::pb_spin} Configuring Pages service"))
    Sys.sleep(1)
    gh::gh(
        "PUT /repos/{user}/{repo}/pages", 
        user = user, repo = repo, 
        charToRaw('{ "source": { "branch": "gh-pages", "path": "/docs" } }'), 
        .token = PAT
    )
    res <- gh::gh(
        "/repos/{user}/{repo}/pages", 
        user = user, repo = repo, 
        .token = PAT
    )
    cli::cli_alert_success(cli::col_grey(
        "Github Pages are now serving the `docs/` directory from `gh-pages` branch."
    ))
    cli::cli_alert_success(cli::col_grey(
        "Book versions will be deployed to {cli::col_cyan(res$html_url)}."
    ))
    Sys.sleep(1)

}

.wrap_up_info <- function(repo, .local) {

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
    cli::cli_alert_success("Local `BiocBook` directory successfully created  : {cli::col_cyan(repo)}")
    if (!.local) cli::cli_alert_success("Remote `BiocBook` repository successfully created: {cli::col_cyan(gert::git_remote_list(repo = repo)$url[1])}")
    if (.local) cli::cli_alert_warning(cli::col_white(cli::style_bold(
        "This book will be only available in local until a Github remote is set."
    )))
    cli::cli_text("")
    cli::cli_text(cli::col_grey('# You can connect to the local directory as follows: \n') )
    cli::cli_code(glue::glue("  biocbook <- BiocBook('{repo}')"))
    cli::cli_text("")

}
