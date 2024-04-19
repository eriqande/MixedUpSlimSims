#' Get list of nodes at which to determine ancestry segments
#'
#' When we link ancestors to the focal nodes we have to be careful!
#' If we include in the sample, any individuals that other individuals in the
#' sample descended from, then these "sampled ancestors" are what gets returned
#' rather than the actual earlier "founders."
#' So, we actually need to link_ancestors separately for each cohort of the
#' samples with the ancestors being the same ancestors every time.
#'
#' This function prepares a list of focal nodes, with each element being
#' focal nodes from a particular cohort.  This is what the `link_ancestors()`
#' function expects as input.
#' @param NI  The list of nodes and inds tibbles like that returned from
#' `ts_nodes_and_inds()`.
#' @param Focal a tibble with two columns: `pop` and `time`.  Each row is a combination
#' of a pop in the simulation and a cohort year that you want included amongst the
#' focal nodes.
#' @return Returns a list of vectors.  Each vector is a vector of node IDs that
#' are from the same cohort.
#' @export
get_focal_nodes <- function(NI, Focal) {

  tmp <- NI$nodes_tib %>%
    semi_join(Focal, by = join_by(node_pop == pop, node_time == time)) %>%
    select(node_time, node_id) %>%
    group_by(node_time) %>%
    nest() %>%
    arrange(node_time)

  ret <- lapply(tmp$data, function(x) pull(x, node_id))

  ret

}
