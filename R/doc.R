#' @title Editing BiocBook accessory files
#' @name BiocBook-editing
#' 
#' @description 
#' 
#' Editing functions for `BiocBook`s
#' 
#' @section `add_*` functions:
#' 
#' `add_chapter()` and `add_preamble` are convenient functions 
#' to add pages to a `BiocBook`. 
#' 
#' @section `edit_*` functions:
#' 
#' Several accessory files can be manually edited: 
#' 
#' - `edit_page()`: manually edit any page listed in `chapters(book)`
#' - `edit_bib()`: manually edit `/inst/assets/bibliography.bib`
#' - `edit_book_yml()`: manually edit `/inst/assets/_book.yml`
#' - `edit_requirements_yml()`: manually edit `/inst/requirements.yml`
#' 
#' @section Maintenance functions:
#' 
#' Extra functions are provided to faciliate the maintenance of `BiocBook`s.  
#' 
#' - `BiocBook_preview()`: is used to dynamically render the book locally
#' - `BiocBook_publish()`: is used to commit and push to remote Github branch
#' - `BiocBook_versions()`: is used to list the book versions already 
#' deployed on the Github repository (branch `gh-pages`) and of the 
#' existing Dockerfiles
#' 
#' @return 
#' - `add_*`, `edit_*`: A `BiocBook` object (invisible). 
#' - `BiocBook_publish`: TRUE (invisible) if pushing to Github was successful;
#' - `BiocBook_preview`: Local URL to browse dynamically rendered book;
#' - `BiocBook_versions`: A tibble of the existing versions found on the Github
#' repository (branch `gh-pages`) and of the existing Dockerfiles. 
#' 
#' @param title A character string for a title for the new chatper. If `file` is not explicitely provided, the 
#' title should only contain alphanumeric characters and spaces
#' @param file Optional. A character string for the name of the `.qmd` file to write the new chapter.
#' The extension `.qmd` has to be provided. If not provided, 
#' the file name is deduced from the `title` argument. 
#' @param position Optional. A position to insert the chapter. For example, 
#' if `position = 2`, the new chapter will be inserted after the first existing
#' chapter (i.e. the `Welcome` page)
#' @param open Optional. Whether to open the file for interactive editing (default: TRUE)
#' @param book A `BiocBook` object, opened with `BiocBook` or created by `BiocBook_init()`.
#' @param open Optional. Whether to open the file for interactive editing (default: TRUE)
#' @param message Optional. Message used when committing with `BiocBook_publish()`.
#' @param browse Optional. Passed to `quarto_preview()` (default: FALSE).
#' @param watch Optional. Passed to `quarto_preview()` (default: FALSE).
#' 
#' @examples
#' bb <- BiocBook_init('localbook', local = TRUE, github_user = 'js2264')
#' add_preamble(bb, open = FALSE)
#' add_chapter(bb, title = "Chapitre Un", open = FALSE)
#' unlink('localbook', recursive = TRUE)

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
#' When created, 3 slots are defined: 
#' 
#' - `title`: The title contained in `/inst/assets/_book.yml`
#' - `local_path`: The absolute path to the book package directory
#' - `remote_repository`: If the book is synced with Github, this will indicate 
#' the remote
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
#' @return A `BiocBook` object (invisible). 
#' 
#' @param new_package Name to use when initiating a new `BiocBook`. 
#' This name should be compatible with package naming conventions 
#' from R and Bioconductor (i.e. no `_` or `-`, no name starting with a number).
#' @param skip_availability Optional. Whether to skip package name availability (default: FALSE).
#' @param template Optional. Github repository used for `BiocBook` template (default: `js2264/BiocBook.template`). 
#' @param commit Optional. Logical, whether to automatically push commits to remote Github origin (default: FALSE). 
#' @param local Optional. Whether to create a matching Github repository or stay local (default: FALSE).
#' @param github_user Optional. If `local = TRUE`, please do provide your Github username to edit BiocBook placeholders. If not provided, `<user>` placeholders will remain in several files until manually filled (default: NA).
#' @param path Path of an existing `BiocBook`. 
#' @param object A `BiocBook` object, created by `BiocBook` or `BiocBook_init()`.
#' 
#' @examples
#' BiocBook_init('localbook', local = TRUE, github_user = 'js2264')
#' bb <- BiocBook('localbook')
#' chapters(bb)
#' releases(bb)
#' unlink('localbook', recursive = TRUE)
NULL
