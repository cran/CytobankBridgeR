#' @rdname spade
#' @aliases spade.bubble_spade_nodes
#'
#' @details \code{spade.bubble_spade_nodes} Bubble each individual node on a SPADE tree as their own bubble, with a specific prefix.
#' @examples \donttest{# Using the prefix "bubble_" will provide bubbles in the form:
#' #   "bubble_1", "bubble_2", "bubble_n", etc...
#' spade.bubble_spade_nodes(cyto_session, spade=cyto_spade, prefix="bubble_")
#' }
#' @export
# Bubble individual SPADE nodes based on given
spade.bubble_spade_nodes <- function(UserSession, spade, prefix="cluster_")
{
    temp_file <- CytobankAPI::spade.download_layout_table(UserSession, spade)
    number_of_nodes <- get_number_of_nodes(temp_file)
    nodes <- create_nodes_list(number_of_nodes, prefix)
    return(CytobankAPI::spade.bubbles_set(UserSession, spade, nodes))
}

