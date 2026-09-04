#' @rdname BiocBook-python
#' @export

micromamba <- function(version = .micromamba_version, quiet = FALSE) {

    ## 1. An explicit choice always wins
    from_env <- Sys.getenv("RETICULATE_CONDA", unset = NA)
    if (!is.na(from_env) && nzchar(from_env) && file.exists(from_env)) return(from_env)

    ## 2. One already on the PATH (the book's Docker image installs it there)
    on_path <- Sys.which("micromamba")
    if (nzchar(on_path)) return(unname(on_path))

    ## 3. One this function downloaded earlier
    cached <- .micromamba_cache_path(version)
    if (file.exists(cached)) return(cached)

    ## 4. Fetch it. A single self-contained binary, pinned and checksummed.
    .micromamba_download(version, cached, quiet = quiet)
}

.micromamba_platform <- function() {
    sysname <- Sys.info()[["sysname"]]
    arch <- Sys.info()[["machine"]]
    platform <- if (identical(sysname, "Windows")) {
        "win-64"
    } else if (identical(sysname, "Darwin")) {
        if (arch %in% c("arm64", "aarch64")) "osx-arm64" else "osx-64"
    } else if (identical(sysname, "Linux")) {
        switch(arch,
            "x86_64" = "linux-64",
            "aarch64" = "linux-aarch64",
            "arm64" = "linux-aarch64",
            "ppc64le" = "linux-ppc64le",
            NA_character_
        )
    } else {
        NA_character_
    }
    if (is.na(platform)) cli::cli_abort(c(
        "No `micromamba` build is available for {sysname} / {arch}.",
        "i" = "Install `conda`, `mamba` or `micromamba` yourself and point \\
               `BiocBook` at it with the {.envvar RETICULATE_CONDA} \\
               environment variable."
    ))
    platform
}

.micromamba_cache_path <- function(version) {
    exe <- if (identical(Sys.info()[["sysname"]], "Windows")) "micromamba.exe" else "micromamba"
    file.path(
        tools::R_user_dir("BiocBook", which = "cache"), "micromamba", version, exe
    )
}

.micromamba_download <- function(version, destination, quiet = FALSE) {

    platform <- .micromamba_platform()
    asset <- paste0("micromamba-", platform, if (identical(platform, "win-64")) ".exe" else "")
    url <- paste0(
        "https://github.com/mamba-org/micromamba-releases/releases/download/",
        version, "/", asset
    )

    if (!quiet) cli::cli_alert_info(cli::col_grey(
        "Downloading `micromamba` {version} for {platform}"
    ))

    dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
    tmp <- paste0(destination, ".part")
    on.exit(unlink(tmp), add = TRUE)
    ok <- tryCatch({
        utils::download.file(url, destfile = tmp, mode = "wb", quiet = TRUE)
        TRUE
    }, error = function(e) {
        cli::cli_abort(c(
            "Could not download `micromamba` from {.url {url}}.",
            "x" = conditionMessage(e),
            "i" = "If this machine has no network access, install `conda`, \\
                   `mamba` or `micromamba` yourself and point `BiocBook` at it \\
                   with the {.envvar RETICULATE_CONDA} environment variable."
        ))
    })
    if (!ok) return(invisible(NULL))

    ## Verify before trusting it. A truncated or substituted download must not
    ## silently become the thing that builds the book.
    expected <- .micromamba_sha256[[platform]]
    observed <- .sha256(tmp)
    if (!identical(observed, expected)) cli::cli_abort(c(
        "Checksum mismatch for the downloaded `micromamba`.",
        "*" = "expected: {expected}",
        "*" = "observed: {observed}",
        "i" = "The download was discarded. This is either corruption in \\
               transit or a changed release asset."
    ))

    file.rename(tmp, destination)
    Sys.chmod(destination, mode = "0755")
    if (!quiet) cli::cli_alert_success(cli::col_grey(
        "Installed `micromamba` to {.file {destination}}"
    ))
    destination
}

.sha256 <- function(path) {
    digest::digest(path, algo = "sha256", file = TRUE)
}
