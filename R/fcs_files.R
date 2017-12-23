#' @importFrom utils unzip
NULL

#' FCS Files
#'
#' FCS File High Level Functions
#' @name fcs_files
#' @param directory character representing a specific directory to which the file will be downloaded (optional ending directory slash), if left empty, the default will be the current working directory \strong{[optional]}
#' @param experiment_id integer representing an \link[=experiments]{experiment} ID
#' @param fcs_files vector/list of integers representing a list of FCS file IDs
#' @param timeout integer representing the request timeout time in seconds \strong{[optional]}
#' @param UserSession Cytobank UserSession object
#' @param zip_download logical representing whether or not to download each file one by one via ZIP download [default] or normal FCS download \strong{[optional]}
#' @examples \donttest{# Authenticate via username/password
#' cyto_session <- authenticate(site="premium", username="cyril_cytometry", password="cytobank_rocks!")
#' # Authenticate via auth_token
#' cyto_session <- authenticate(site="premium", auth_token="my_secret_auth_token")
#' }
NULL


create_fcs_experiment_folder <- function(experiment_id, directory)
{
    experiment_directory <- paste("experiment_", experiment_id, "_fcs_files", sep="")
    create_new_directory(directory, experiment_directory)
    setwd(file.path(directory, experiment_directory))
    return(getwd())
}


# Extract FCS files to selected directory
extract_fcs_files <- function(fcs_files_zip, extract_directory)
{
    utils::unzip(zipfile=fcs_files_zip, exdir=extract_directory)
    file.remove(fcs_files_zip)
}

