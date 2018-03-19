#' Spectra database class
#'
#' A specialized file hierarchy for storing spectra and metadata.
#'
#' The database is organized as follows.
#'  - Database root contains the following:
#'    - Folders for each project, named according to `project_code`
#'    - `species.csvy` -- Information about species
#'    - `instruments.yml` -- Information about instruments
#'    - `traits.yml` -- Information about traits and units
#'  - Each project folder contains the following:
#'    - `metadata.csvy` -- Metadata about the spectra
#'    - `spectra/` -- Directory containing spectr
#'  - Each `spectra` folder contains `<observation_id.csvy>` files
#' @param path
#' @return Object of class `specdb`
specdb <- function(path) {
  root <- fs::dir_create(path)

    fs::path(c("a", "b", "c"))
  projects <- fs::dir_ls(path, type = "directory")
}

path <- "spectra_db"
