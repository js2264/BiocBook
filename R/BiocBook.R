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

init <- function(
    new_package, 
    skip_availability = FALSE, 
    template = "js2264/BiocBook.template", 
    version = 'v1.0.1', 
    commit = NA, 
    .local = FALSE  
) {

    cli::cat_rule("Running preflight checklist", col = "cyan", line = 2)
    cli::cli_progress_message(cli::col_grey("{cli::pb_spin} Checking that no folder named `{new_package}` already exists"))
    Sys.sleep(1)

    ## Check that a folder named `new_package` can be created 
    if (file.exists(new_package)) {
        cli::cli_abort("A folder named {new_package} already exists.")
    }

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
        user <- gh::gh_whoami(.token = PAT)$login
        cli::cli_alert_success(cli::col_grey("Successfully logged in Github"))
        cli::cli_ul(c(
            cli::col_grey("user: `{user}`"),
            cli::col_grey("token: `{stringr::str_trunc(PAT, width = 18, side = 'center')}`")
        ))
        Sys.sleep(1)

        ## Get all repos for one user
        req <- gh::gh("/users/{user}/repos", user = user, per_page = 30)
        gh_repos <- purrr::map_chr(req, 'name')
        while({length(gh_repos) %% 30} == 0) {
            req <- gh::gh_next(req)
            gh_repos <- c(gh_repos, purrr::map_chr(req, 'name'))
        }

        ## Check that new_package does not already exist
        if (new_package %in% gh_repos) {
            cli::cli_abort("A Github repo named `{new_package}` already exists for user `{user}`.")
        }

    }
    else {
        PAT <- NULL
        user <- gituser
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

    cli::cli_text("")
    cli::cat_rule("Initiating a new `BiocBook`", col = "cyan", line = 2)
    Sys.sleep(1)
    cli::cli_progress_message(cli::col_grey("{cli::pb_spin} Creating new repository from `{template}@{version}`"))

    ## Create new local repo copied from BiocBook.template
    repo <- new_package
    dir.create(repo)
    tmpdir <- paste0('.', paste0(
        sample(c(seq(0, 9), LETTERS), 8, replace = TRUE), collapse = ""
    ))
    dir.create(tmpdir)
    tmpfile <- file.path(tmpdir, 'archive')
    utils::download.file(
        # paste0("https://github.com/", template, "/archive/refs/heads/devel.zip"), 
        paste0("https://github.com/", template, "/archive/refs/tags/", version, ".tar.gz"), 
        paste0(tmpfile, '.tar.gz')
    )
    utils::untar(paste0(tmpfile, '.tar.gz'), exdir = tmpdir)
    d <- list.dirs(tmpdir, full.names = TRUE, recursive = TRUE)
    d <- d[dirname(d) != '.']
    pattern <- file.path(tmpdir, basename(d)[dirname(dirname(d)) == '.'])
    d <- d[dirname(dirname(d)) != '.']
    f <- list.files(
        tmpdir, 
        all.files = TRUE, full.names = TRUE, recursive = TRUE
    )
    f <- f[!grepl("archive.zip$", f)]
    for (.d in d) dir.create(gsub(pattern, repo, .d))
    for (.f in f) file.copy(from = .f, to = gsub(pattern, repo, .f))
    unlink(tmpdir, recursive = TRUE)
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

    ## Init local git 
    gert::git_init(path = repo) 
    cli::cli_alert_info(cli::col_grey("The following files need to be committed: "))
    d <- cli::cli_div(theme = list(ul = list(`margin-left` = 2, before = "")))
    f <- gert::git_status(repo = repo)
    cli::cli_ul(f$file)
    cli::cli_end(d)
    Sys.sleep(1)
    staged <- gert::git_add(files = f$file, repo = repo)
    commit_sha <- gert::git_commit(
        repo = repo, message = "\U1F680 init BiocBook", author = gitsig
    )
    b <- gert::git_branch_list(repo = repo)
    if (b$name != 'devel') {
        gert::git_branch_move(repo = repo, b$name, 'devel')
    }
    cli::cli_alert_success(cli::col_grey("The new files have been commited to the `devel` branch."))
    Sys.sleep(1)

    ## Syncing Github: create new repo, configure Pgaes, add remote, push
    if (!is.null(PAT)) {

        ## Create a new Github repo
        remote <- gh::gh("POST /user/repos", name = repo, description = "BiocBook")
        cli::cli_alert_success(cli::col_grey("New Github repository `{user}/{repo}` successfully created"))
        Sys.sleep(1)

        ## Connect local git to remote 
        cli::cli_progress_message(cli::col_grey("{cli::pb_spin} Adding remote Github branch to local repo"))
        Sys.sleep(1)
        gert::git_remote_add(url = remote$html_url, name = 'origin', repo = repo)

        ## Pushing first commit to Github
        msg <- glue::glue("Is it ok to push the first commit to Github?")
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

        ## Create remote empty `gh-pages` branch [ripped from usethis]
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
            message = "🚀 Init orphan branch",
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

        ## Enable Pages service
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
