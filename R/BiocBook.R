#' @rdname BiocBook
#' @include imports.R
#' @include doc.R
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

init <- function(new_package, skip_availability = FALSE, template = "js2264/BiocBook.template", commit = NA, .local = FALSE, .github_user = NA) {

    ## Check that a folder named `new_package` can be created 
    cli::cat_rule("Running preflight checklist", col = "cyan", line = 2)
    cli::cli_progress_message(cli::col_grey("{cli::pb_spin} Checking that no folder named `{new_package}` already exists"))
    Sys.sleep(1)
    if (file.exists(new_package)) {
        cli::cli_abort("A folder named {new_package} already exists.")
    }

    ## Check that user is logged in Github
    if (!.local) {
        
        cli::cli_progress_message(cli::col_grey("{cli::pb_spin} Checking Github credentials"))
        Sys.sleep(1)
        GH_api <- "https://api.github.com"
        PAT <- tryCatch(
            {gitcreds::gitcreds_get()$password}, 
            error =  function(e) {
                cli::cli_abort("Could not find any stored Github credentials. Consider adding a Github token (a.k.a. `PAT`) to your `.Renviron`.\n")
            }
        )
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
    }
    else {
        if (is.na(.github_user)) {
            cli::cli_alert_danger(c("`.github_user` is not set. ", 
            "`<user>` placeholders in the template `BiocBook` won't be fixed."
            ))
            user <- '<user>'
        }
        else {
            user <- .github_user
        }
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
    if (!.local) {

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
        cli::cli_alert_success(cli::col_grey("Remote Github repository `{user}/{new_package}` cloned [{cli::col_cyan(repo)}]"))
        Sys.sleep(1)

        ## Create `gh-pages` branch on origin
        cli::cli_progress_message(cli::col_grey("{cli::pb_spin} Checking existing branches"))
        Sys.sleep(1)
        refs <- gh::gh("/repos/{user}/{new_package}/git/refs", user = user, new_package = new_package)
        sha <- refs[[which(purrr::map(refs, ~ .x$ref == 'refs/heads/devel') |> unlist())]]$object$sha
        gh::gh(
            "POST /repos/{user}/{new_package}/git/refs", 
            user = user, new_package = new_package, 
            ref = "refs/heads/gh-pages", 
            sha = sha,
            .token = PAT
        )
        cli::cli_alert_success(cli::col_grey("New `gh-pages` branch created."))
        Sys.sleep(1)

        ## Enable Pages service
        cli::cli_progress_message(cli::col_grey("{cli::pb_spin} Configuring Pages service"))
        Sys.sleep(1)
        gh::gh(
            "PUT /repos/{user}/{new_package}/pages", 
            user = user, new_package = new_package, 
            charToRaw('{ "source": { "branch": "gh-pages", "path": "/docs" } }'), 
            .token = PAT
        )
        res <- gh::gh(
            "/repos/{user}/{new_package}/pages", 
            user = user, new_package = new_package, 
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
    else {
        cli::cli_text("")
        cli::cat_rule("Initiating a new `BiocBook`", col = "cyan", line = 2)
        cli::cli_progress_message(cli::col_grey(
            "{cli::pb_spin} Creating new Github repository from `{template}`"
        ))
        Sys.sleep(1)
        
        repo <- new_package
        dir.create(repo)
        tmpdir <- paste0('.', paste0(
            sample(c(seq(0, 9), LETTERS), 8, replace = TRUE), collapse = ""
        ))
        dir.create(tmpdir)
        tmpfile <- file.path(tmpdir, 'BiocBook.template-devel')
        utils::download.file(
            paste0("https://github.com/", template, "/archive/refs/heads/devel.zip"), 
            paste0(tmpfile, '.zip')
        )
        utils::unzip(paste0(tmpfile, '.zip'), exdir = tmpdir)
        d <- list.dirs(tmpdir, full.names = TRUE, recursive = TRUE)
        d <- d[!grepl(paste0(tmpdir, "$"), d)]
        d <- d[!grepl(file.path(tmpdir, "BiocBook.template-devel$"), d)]
        f <- list.files(
            tmpdir, 
            all.files = TRUE, full.names = TRUE, recursive = TRUE
        )
        f <- f[!grepl("BiocBook.template-devel.zip$", f)]
        for (.d in d) dir.create(gsub(tmpfile, repo, .d))
        for (.f in f) file.copy(from = .f, to = gsub(tmpfile, repo, .f))

        unlink(tmpdir, recursive = TRUE)
        cli::cli_alert_success(cli::col_grey(
            "New local book `{new_package}` successfully created"
        ))
        Sys.sleep(1)
    }

    ## Fix placeholders
    # ---- in `_book.yml`
    path <- file.path("inst", "assets", "_book.yml")
    .fix_placeholders(file.path(repo, path), pkg = new_package, usr = user)
    cli::cli_alert_success(cli::col_grey("Filled out `{cli::col_cyan(path)}` fields"))
    Sys.sleep(1)

    # ---- in `DESCRIPTION`
    path <- "DESCRIPTION"
    .fix_placeholders(file.path(repo, path), pkg = new_package, usr = user)
    cli::cli_alert_success(cli::col_grey("Filled out `{cli::col_cyan(path)}` fields"))
    cli::cli_alert_info(cli::col_grey("Please finish editing the `{cli::col_cyan(path)}` fields, including:"))
    d <- cli::cli_div(theme = list(ul = list(`margin-left` = 2, before = "")))
    cli::cli_ul(c("  Title", "  Description", "  Authors@R"))
    cli::cli_end(d)
    Sys.sleep(1)

    # ---- in `index.qmd`
    path <- file.path("inst", "index.qmd")
    .fix_placeholders(file.path(repo, path), pkg = new_package, usr = user)
    cli::cli_alert_success(cli::col_grey("Filled out `{cli::col_cyan(path)}` fields"))
    cli::cli_alert_info(cli::col_grey("Please finish editing the `{cli::col_cyan(path)}` fields, including the `Welcome` section"))
    Sys.sleep(1)

    # ---- in GHA workflow
    path <- file.path(".github", "workflows", "build-and-deploy.yaml")
    .fix_placeholders(file.path(repo, path), pkg = new_package, usr = tolower(user))
    cli::cli_alert_success(cli::col_grey("Filled out `{cli::col_cyan(path)}` fields"))
    Sys.sleep(1)

    ## Committing everything 
    if (!.local) {

        cli::cli_alert_info(cli::col_grey("Several files have been automatically edited: "))
        d <- cli::cli_div(theme = list(ul = list(`margin-left` = 2, before = "")))
        cli::cli_ul(gert::git_status(repo = repo)$file)
        cli::cli_end(d)
        Sys.sleep(1)
        commit_sha <- gert::git_commit_all(repo = repo, message = "Adapted from BiocBook.template", sig)
        cli::cli_alert_success(cli::col_grey("These changes have been commited to the local repository."))
        Sys.sleep(1)
        msg <- glue::glue("Is it ok to push these changes to Github?")
        if (!is.na(commit)) {
            if (commit) {
                gert::git_push(repo = repo)
                cli::cli_alert_success(cli::col_grey("Commits pushed to origin {cli::col_cyan(gert::git_remote_list(repo = repo)$url[1])}"))
            }
            else {
                invisible(BiocBook(repo))
            }
        }
        else if (usethis::ui_yeah(msg)) {
            gert::git_push(repo = repo)
            cli::cli_alert_success(cli::col_grey("Commits pushed to origin {cli::col_cyan(gert::git_remote_list(repo = repo)$url[1])}"))
        } 
        else {
            cli::cli_alert_info(cli::col_grey("Don't forget to push the latest commit to the remote `origin`."))
        }

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
    cli::cli_alert_success("Local `BiocBook` directory successfully created  : {cli::col_cyan(repo)}")
    if (!.local) cli::cli_alert_success("Remote `BiocBook` repository successfully created: {cli::col_cyan(gert::git_remote_list(repo = repo)$url[1])}")
    if (.local) cli::cli_alert_warning(cli::col_white(cli::style_bold(
        "This book will be only available in local until a Github remote is set."
    )))
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
