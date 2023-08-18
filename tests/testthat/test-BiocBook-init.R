test_that("BiocBook_init work", {
    
    skip('No PAT set up yet')

    ## -- Check that git/github creds are available
    PAT <- Sys.getenv("GITHUB_PAT")
    gh_creds <- gh::gh_whoami(.token = PAT)
    user <- gh_creds$login
    headers <- httr::add_headers(
        Accept = "application/vnd.github+json", 
        Authorization = glue::glue("Bearer {PAT}"), 
        "X-GitHub-Api-Version" = "2022-11-28"
    )

    ## -- Test `BiocBook_init`
    expect_success(     bb <- BiocBook_init('BiocBookTest', commit = TRUE))

    ## -- Purge everything
    unlink('BiocBookTest', recursive = TRUE, force = TRUE)
    gh::gh("/repos/{owner}/{repo}", owner = user, repo = "BiocBookTest", .method="DELETE")

})

