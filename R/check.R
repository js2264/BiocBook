is_biocbook <- rprojroot::has_file("inst/_quarto.yml", contents = "^  type: book", n = 3)

.get_biocbook_proj <- function(path) {
    return(is_biocbook$find_file())
}

.check_biocbook_proj <- function() {
    if (getwd() != .get_biocbook_proj()) 
        stop("Please go to a BiocBook root directory.")
}
