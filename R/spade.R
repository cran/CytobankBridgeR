#' SPADE
#'
#' SPADE High Level Functions
#' @name spade
#' @param prefix character representing a prefix to be used before each node number
#' @param spade Cytobank \link[CytobankAPI]{spade} object
#' @param UserSession Cytobank UserSession object (created via the \link[CytobankAPI]{authenticate} function)
NULL


# Create nodes list ready to pass into the spade.bubbles_set API call
create_nodes_list <- function(number_of_nodes, prefix)
{
    prefixes <- c()
    for (x in 1:number_of_nodes)
    {
        prefixes <- c(prefixes, paste(prefix, x, sep=""))
    }

    return(setNames(as.list(seq(number_of_nodes)), prefixes))
}


# Gets number of lines from layout.table SPADE file as a representation of how many nodes in the SPADE tree
get_number_of_nodes <- function(file)
{
    open_file <- file(file, open="r")
    read_size <- 200 # Line buffer size
    number_of_nodes <- 0
    while((lines_read <- length(readLines(open_file, read_size))) > 0)
        number_of_nodes <- number_of_nodes+lines_read
    close(open_file)
    file.remove(file)

    return(number_of_nodes)
}

