#' @rdname gates
#' @aliases gates.apply_cluster_gates
#'
#' @details \code{gates.apply_cluster_gates} Apply cluster gate(s) to specific clusters via an integer vector
#' @examples \donttest{gates.apply_cluster_gates(cyto_session, 22, name="cluster_names",
#'   clusters=c(1,5,9), channel_name="cluster_id")
#' }
#' @export
# Apply cluster gates via 1 of 2 options:
#   - 1) clusters representing individual clusters to gate (grouped=FALSE), this will create multiple gates around the individual clusters specified
#   - 2) clusters representing a multicluster to gate (grouped=TRUE), this will only create one gate around the clusters specified
gates.apply_cluster_gates <- function(UserSession, experiment_id, name, clusters, channel_name, integer_min=1, integer_max=max(clusters), grouped=FALSE, timeout=60)
{
    tryCatch(
        {
            options(warn=-1)

            experiment_info <- CytobankAPI::experiments.show(UserSession, experiment_id, timeout=timeout)
            experiment_name <- experiment_info$experimentName[[1]]
            site <- gsub("https://(.*)\\.cytobank.org.*", "\\1", UserSession@site)
            temp_file <- paste("./.experiment_", experiment_id, "_temp_gatingML.xml", sep="")

            max_gate_id <- get_max_gate_id(UserSession, experiment_id, timeout=timeout)

            if (!grouped)
            {
                write_single_cluster_applier_gatingML(temp_file, site, experiment_id, experiment_name, name, clusters, max_gate_id, channel_name, integer_min, integer_max)
            }
            else
            {
                write_multicluster_applier_gatingML(temp_file, site, experiment_id, experiment_name, name, clusters, max_gate_id, channel_name, integer_min, integer_max)
            }

            CytobankAPI::gates.gatingML_upload(UserSession, experiment_id, temp_file, timeout=timeout)
        },
        finally = {
            file.remove(temp_file)
            options(warn=0)
        }
    )
}


###################
# HELPER FUNCTIONS
###################


write_cluster_applier_gatingML_head <- function(file, site, experiment_id, experiment_name)
{
    head <- file(file.path(system.file('cluster_gates_templates', package='CytobankBridgeR'), "gatingML_head.xml"), open="r")
    gatingML <- paste(readLines(head), collapse="\n")

    # gsub for head part of gatingML
    gatingML <- gsub("\\*SITE\\*", site, gatingML)
    gatingML <- gsub("\\*EXPERIMENT_ID\\*", experiment_id, gatingML)
    gatingML <- gsub("\\*EXPERIMENT_NAME\\*", experiment_name, gatingML)

    write(gatingML, file)
    close(head)
}


write_cluster_applier_gatingML_single_body <- function(file, name, integer, max_gate_id, integer_min, integer_max, channel_name)
{
    body <- file(file.path(system.file('cluster_gates_templates', package='CytobankBridgeR'), "gatingML_body.xml"), open="r")
    gatingML <- paste(readLines(body), collapse="\n")

    # gsub for body of one cluster
    gatingML <- gsub("\\*NAME\\*", name, gatingML)
    gatingML <- gsub("\\*INTEGER\\*", integer, gatingML)
    gatingML <- gsub("\\*MAX_GATE_ID_PLUS_1\\*", max_gate_id+1, gatingML)
    gatingML <- gsub("\\*INTEGER_MIN_MINUS_1\\*", integer_min-1, gatingML)
    gatingML <- gsub("\\*INTEGER_MAX_PLUS_1\\*", integer_max+1, gatingML)
    gatingML <- gsub("\\*INTEGER_PLUS_1\\*", integer+1, gatingML)
    gatingML <- gsub("\\*CHANNEL_NAME\\*", channel_name, gatingML)

    gatingML <- gsub("\\*INTEGER_MINUS_0.35\\*", integer-0.35, gatingML)
    gatingML <- gsub("\\*INTEGER_PLUS_0.35\\*", integer+.35, gatingML)

    gatingML <- gsub("\\*INTEGER_MIN_MINUS_0.5\\*", integer_min-0.5, gatingML)
    gatingML <- gsub("\\*INTEGER_MAX_PLUS_0.5\\*", integer_max+0.5, gatingML)

    write(gatingML, file, append=TRUE)
    close(body)
}


write_cluster_applier_gatingML_multiple_body <- function(file, name, clusters, max_gate_id, integer_min, integer_max, channel_name)
{
    for (cluster_id in sort(clusters))
    {
        write_cluster_applier_gatingML_single_body(file, name, cluster_id, max_gate_id, integer_min, integer_max, channel_name)
        max_gate_id <- max_gate_id+1
    }
}


write_multicluster_applier_gatingML_body_beginning <- function(file, name, clusters, max_gate_id, integer_min, integer_max, channel_name)
{
    body_begin <- file(file.path(system.file('cluster_gates_templates', package='CytobankBridgeR'), "multi_gatingML_body_beginning.xml"), open="r")
    gatingML <- paste(readLines(body_begin), collapse="\n")

    # gsub for beginning of the body of a multicluster
    gatingML <- gsub("\\*NAME\\*", name, gatingML)
    gatingML <- gsub("\\*MAX_GATE_ID_PLUS_1\\*", max_gate_id+1, gatingML)
    gatingML <- gsub("\\*INTEGER_MIN_MINUS_1\\*", integer_min-1, gatingML)
    gatingML <- gsub("\\*INTEGER_MAX_PLUS_1\\*", integer_max+1, gatingML)
    gatingML <- gsub("\\*INTEGER_MIN\\*", min(clusters), gatingML)
    gatingML <- gsub("\\*INTEGER_PLUS_1\\*", min(clusters)+1, gatingML)
    gatingML <- gsub("\\*CHANNEL_NAME\\*", channel_name, gatingML)

    gatingML <- gsub("\\*CLUSTER_MIN_MINUS_0.35\\*", min(clusters)-0.35, gatingML)
    gatingML <- gsub("\\*CLUSTER_MAX_PLUS_0.35\\*", max(clusters)+.35, gatingML)

    gatingML <- gsub("\\*INTEGER_MIN_MINUS_0.75\\*", integer_min-0.75, gatingML)
    gatingML <- gsub("\\*INTEGER_MAX_PLUS_0.75\\*", integer_max+0.75, gatingML)

    write(gatingML, file, append=TRUE)
    close(body_begin)
}


write_multicluster_applier_gatingML_body_vertices <- function(file, clusters, integer_min, integer_max)
{
    clusters_decreasing <- sort(clusters, decreasing=TRUE)
    for (cluster_id in seq(length(clusters_decreasing)-1))
    {
        write_multicluster_applier_gatingML_body_vertex(file, integer=clusters_decreasing[[cluster_id]], integer_before=clusters_decreasing[[cluster_id+1]], integer_min, integer_max)
    }
}


write_multicluster_applier_gatingML_body_vertex <- function(file, integer, integer_before, integer_min, integer_max)
{
    vertex <- file(file.path(system.file('cluster_gates_templates', package='CytobankBridgeR'), "multi_gatingML_body_vertex.xml"), open="r")
    gatingML <- paste(readLines(vertex), collapse="\n")

    # gsub for vertex of the body of a multicluster
    gatingML <- gsub("\\*INTEGER_MINUS_0.35\\*", integer-0.35, gatingML)
    gatingML <- gsub("\\*INTEGER_BEFORE_PLUS_0.35\\*", integer_before+.35, gatingML)

    gatingML <- gsub("\\*INTEGER_MIN_MINUS_0.75\\*", integer_min-0.75, gatingML)
    gatingML <- gsub("\\*INTEGER_MAX_PLUS_0.25\\*", integer_max+0.25, gatingML)

    write(gatingML, file, append=TRUE)
    close(vertex)
}


write_multicluster_applier_gatingML_body_end <- function(file, name, max_gate_id)
{
    body_end <- file(file.path(system.file('cluster_gates_templates', package='CytobankBridgeR'), "multi_gatingML_body_end.xml"), open="r")
    gatingML <- paste(readLines(body_end), collapse="\n")

    # gsub for end of the body of a multicluster
    gatingML <- gsub("\\*NAME\\*", name, gatingML)
    gatingML <- gsub("\\*MAX_GATE_ID_PLUS_1\\*", max_gate_id+1, gatingML)

    write(gatingML, file, append=TRUE)
    close(body_end)
}


write_multicluster_applier_gatingML_body <- function(file, name, clusters, max_gate_id, integer_min, integer_max, channel_name)
{
    write_multicluster_applier_gatingML_body_beginning(file, name, clusters, max_gate_id, integer_min, integer_max, channel_name)
    write_multicluster_applier_gatingML_body_vertices(file, clusters, integer_min, integer_max)
    write_multicluster_applier_gatingML_body_end(file, name, max_gate_id)
}


write_cluster_applier_gatingML_foot <- function(file)
{
    foot <- file(file.path(system.file('cluster_gates_templates', package='CytobankBridgeR'), "gatingML_foot.xml"), open="r")
    gatingML <- paste(readLines(foot), collapse="\n")

    write(gatingML, file, append=TRUE)
    close(foot)
}


write_single_cluster_applier_gatingML <- function(file, site, experiment_id, experiment_name, name, clusters, max_gate_id, channel_name, integer_min, integer_max)
{
    write_cluster_applier_gatingML_head(file, site, experiment_id, experiment_name)
    write_cluster_applier_gatingML_multiple_body(file, name, clusters, max_gate_id, integer_min, integer_max, channel_name)
    write_cluster_applier_gatingML_foot(file)
}


write_multicluster_applier_gatingML <- function(file, site, experiment_id, experiment_name, name, clusters, max_gate_id, channel_name, integer_min, integer_max)
{
    write_cluster_applier_gatingML_head(file, site, experiment_id, experiment_name)
    write_multicluster_applier_gatingML_body(file, name, clusters, max_gate_id, integer_min, integer_max, channel_name)
    write_cluster_applier_gatingML_foot(file)
}

