#' get the ids and pops of founder nodes
#'
#' The "Founders" are defined according to the time they
#' existed.  You give a year (in tskit time) and this returns
#' a list with two elements: $nodes---a vector of ids of the
#' founder nodes, and $pops---a vector of the pops that the
#' founder nodes belonged to.
#' @param NI a list with tibbles of tskit nodes and individuals like that
#' returned by `ts_nodes_and_inds()`.
#' @param FY the year (in tskit time) in which individuals are
#' designated as founders.
#' @export
founder_node_ids_and_pops <- function(NI, FY) {
  founder_nodes <- NI$nodes_tib %>%
    filter(node_time == FY) %>%
    pull(node_id)
  founder_node_pops <- NI$nodes_tib %>%
    filter(node_time == FY) %>%
    pull(node_pop)

  list(
    nodes = founder_nodes,
    pops = founder_node_pops
  )
}
