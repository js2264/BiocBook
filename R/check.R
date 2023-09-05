is_biocbook <- rprojroot::has_file("inst/_quarto.yml", contents = "^  type: book", n = 3)

#' @rdname BiocBook-editing
#' @importFrom renv dependencies
#' @export 

check_deps <- function(book) {
    desc_f <- file.path(path(book), 'DESCRIPTION')
    listed_deps <- read.dcf(desc_f)[, c("Depends", "Imports", "Suggests")] |> 
        unlist() |> 
        paste(collapse = ',\n') |> 
        strsplit(',\n')
    found_deps <- renv::dependencies(
        path = file.path(path(book), 'inst', 'pages'), quiet = TRUE
    )$Package |> unique()
    found_deps <- found_deps[found_deps != 'rmarkdown'] # Quarto depends on rmarkdown so it's ok to skip rmarkdown
    is_missing <- unlist(lapply(found_deps, function(x) !grepl(x, listed_deps)))
    undeclared_deps <- found_deps[is_missing]
    if (length(undeclared_deps)) {
        cli::cli_alert_danger("Some dependencies found in book pages are not listed in DESCRIPTION: ")
        d <- cli::cli_div(theme = list(ul = list(`margin-left` = 2, before = "")))
        cli::cli_ul(undeclared_deps)
        cli::cli_end(d)
        cli::cli_alert_info("Consider adding these dependencies to DESCRIPTION")
    }
    else {
        cli::cli_alert_success("All dependencies seem to be listed in DESCRIPTION.")
    }
    invisible(book)
}
