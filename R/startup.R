#' @import CytobankAPI
#' @importFrom stats setNames
NULL

#' News
#'
#' Get news on CytobankR updates
#' @name news
NULL


.onAttach <- function(libname, pkgname) {
    CytobankBridgeR_version <- read.dcf(file=system.file("DESCRIPTION", package=pkgname), fields="Version")
    packageStartupMessage(paste(pkgname, CytobankBridgeR_version))
    packageStartupMessage("Type CytobankBridgeR_news() to see new features/changes/bug fixes.")
}

#' @rdname news
#' @aliases CytobankBridgeR_news
#'
#' @details \code{CytobankR_news} View a log of CytobankR updates and release notes.
#' @export
CytobankBridgeR_news <- function() {
    news_file <- file.path(system.file(package="CytobankBridgeR"), "NEWS")
    file.show(news_file)
}

