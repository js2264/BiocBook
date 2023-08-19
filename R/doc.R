#' @title BiocBook utilities
#' @name BiocBook-utils
#' @param book,object A `BiocBook` object, created by `BiocBook` or `BiocBook_init()`.
#' @param message Optional. Message used when committing with `BiocBook_publish()`.
#' @param browse Optional. Passed to `quarto_preview()` (default: FALSE).
#' @param watch Optional. Passed to `quarto_preview()` (default: FALSE).
NULL 

#' @title Editing BiocBook accessory files
#' @name BiocBook-editing
#' @param book A `BiocBook` object, created by `BiocBook` or `BiocBook_init()`.
#' @param open Optional. Whether to open the file for interactive editing (default: TRUE)
NULL 

#' @title BiocBook generics
#' @name AllGenerics
#' @aliases releases
#' @aliases chapters
#' @param object Passed to specific methods
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
