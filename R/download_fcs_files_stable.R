#' @rdname fcs_files
#' @aliases fcs_files.download_fcs_files_stable
#'
#' @details \code{fcs_files.download_fcs_files_stable} Apply cluster gate(s) to specific clusters via an integer vector
#' @examples \donttest{fcs_files.download_fcs_files_stable(cyto_session, 22, fcs_files=c(1,2,3),
#'   directory="/my/new/download/directory/")
#' }
#' @export
# Framework for downloading FCS files stable
fcs_files.download_fcs_files_stable <- function(UserSession, experiment_id, fcs_files, directory=getwd(), zip_download=TRUE, timeout=300)
{
    previous_directory <- getwd()
    directory <- create_fcs_experiment_folder(experiment_id, directory)

    downloaded_fcs_files <- c()

    download_fcs_files(UserSession, experiment_id, fcs_files, directory, zip_download, timeout)

    for (file in list.files(directory))
    {
        downloaded_fcs_files <- c(downloaded_fcs_files, file.path(directory, file))
    }

    setwd(previous_directory)
    return(downloaded_fcs_files)
}


###################
# HELPER FUNCTIONS
###################


# Stable download FCS files workhorse
download_fcs_files <- function(UserSession, experiment_id, fcs_files, directory, zip_download, timeout)
{
    for (file in fcs_files)
    {
        if (zip_download)
        {
            fcs_file_zip <- CytobankAPI::fcs_files.download_zip(UserSession, experiment_id, fcs_files=file, directory=directory, timeout=timeout)
            extract_fcs_files(fcs_file_zip, directory)
        }
        else
        {
            fcs_file <- CytobankAPI::fcs_files.download(UserSession, experiment_id, fcs_file_id=file, directory=directory, timeout=timeout)
        }
    }
}

