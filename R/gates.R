#' Gates
#'
#' Gates High Level Functions
#' @name gates
#' @param channel_name character representing the channel short name
#' @param clusters integer vector representing the clusters to gate\cr
#' - There are 2 modes of operation:\cr
#'   1) \code{grouped=FALSE}: clusters represent individual integer clusters to gate, this will create \strong{multiple gates} that represents \strong{multiple individual populations}\cr
#'   2) \code{grouped=TRUE}:  clusters represent a multi-integer cluster to gate, this will create \strong{one gate} that represents \strong{one population} around the integer clusters specified
#' @param experiment_id integer representing an \link[CytobankAPI]{experiments} ID
#' @param grouped logical representing whether to gate a vector of integer clusters as individual integer clusters (grouped=FALSE), or together as a single multicluster (grouped=TRUE)
#' @param integer_max integer representing the maximum cluster integer for setting scales \strong{[optional]}\cr
#' - The default is the maximum number presented within the \code{clusters} vector, but can be set to any integer\cr
#'   - The max scale will be set to integer_max+1
#' @param integer_min integer representing the minimum cluster integer for setting scales \strong{[optional]}\cr
#' - The default is set to 1, this assumes clusters begin at 1, but can be changed if there is any +/- offset\cr
#'   - The minimum scale will be set to integer_min-1 (0 by)
#' @param name character representing a cluster prefix (if grouped=FALSE) or multicluster gate name
#' @param timeout integer representing the request timeout time in seconds \strong{[optional]}
#' @param UserSession Cytobank UserSession object (created via the \link[CytobankAPI]{authenticate} function)
NULL


get_max_gate_id <- function(UserSession, experiment_id, timeout=60)
{
    gates <- CytobankAPI::gates.list(UserSession, experiment_id, timeout=timeout)
    return(if (!is.null(gates)) max(unlist(gates$gateId)) else 0)
}

