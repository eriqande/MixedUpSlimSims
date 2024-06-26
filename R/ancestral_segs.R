#' return a tibble with ancestral population of different segments
#'
#' Uses link_ancestors from tskit, internally using python, to return a tibble
#' with everything you need to know.
#' @param ts the tree sequence.  You pass it as the object in py in the
#' global env.  i.e., as py$ts, for example.
#' @param focal_nodes_list an unnamed list.  Each element is a vector of integer
#' node ids in which you want to track ancestry segments.  It each vector must
#' include all the focal individuals of each cohort.
#' @param founder_nodes vector of integer ids of nodes that are considered the founders
#' (typically the earliest ancestors of known population origin).
#' @param nodes_tib tibble of the nodes.  This is the `nodes_tib` component of the
#' list returned by `ts_nodes_and_inds()`.
#' @param indiv_tib tibble of the indivs.  This is the `indiv_tib` component of the
#' list returned by `ts_nodes_and_inds()`.
#' @export
ancestral_segs <- function(
    ts,
    focal_nodes_list,
    founder_nodes,
    nodes_tib,
    indiv_tib
  ) {

  # first, put the input values into the R global env so python can get them
  list2env(
    list(
      xx__TS = ts,
      xx__FOCAL_NODES_LIST = focal_nodes_list,
      xx__FOUNDER_NODES = founder_nodes
    ), envir = .GlobalEnv
  )


  # then run the python code that gets the linked ancestors table
  source_python(system.file("py/linking_ancestors.py", package = "MixedUpSlimSims"))



  # then get that back from python and prep it
  links_tib <- as_tibble(py$xx__LINKS_PD) %>%
    arrange(child, left, right) %>%
    left_join(nodes_tib, by = join_by(parent == node_id)) %>%
    rename(
      anc_pop = node_pop,
      anc_ind = node_ind,
      anc_time = node_time,
      node_id = child
    )

  # collapse adjacent segments from the same pop.  These could be made more
  # efficient, I am sure.  But it works for now.
  st2 <- links_tib %>%
    group_by(node_id) %>%
    mutate(
      grp = cumsum(anc_pop != lag(anc_pop, default = 1))
    ) %>%
    group_by(node_id, grp) %>%
    slice(1, n()) %>%
    summarise(left = left[1], right = right[2], anc_pop = anc_pop[1]) %>%
    ungroup()

  # join the node and indiv information back on there
  st3 <- st2 %>%
    left_join(nodes_tib, by = join_by(node_id)) %>%
    left_join(indiv_tib, by = join_by(node_ind == ind_id))

  # now, before returning anything, we want to remove anything in the
  # global environment of R or python that start with xx__
  # remove them from R
  objs <- ls(envir = .GlobalEnv)
  rm(list =  objs[str_detect(objs, "^xx__")], envir = .GlobalEnv)

  # remove them from Python
  py_run_string('
for name in dir():
    if name.startswith("xx__"):
        del globals()[name]
')

  # return the tibble
  st3

}
