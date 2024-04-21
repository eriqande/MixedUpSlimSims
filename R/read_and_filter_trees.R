#' Read a tree sequence and retain nodes from certain populations at certain times
#'
#' This uses glue to write values into a python script which is then run
#' by `reticulate::py_run_string` which deposits items into the `py` variable
#' in the global environment.
#' @param trees_path The path to the .trees file that should be read.
#' @param years_list A named list.  Each name is an integer signifying a SLiM
#' population, and the value is an integer vector of tree-seq years for which
#' individuals/nodes in the tree seq from the named population should be
#' retained in the tree-sequence. The integers specifying populations will
#' be 1-based if you obtained the tree sequence by running SLiM and have
#' not otherwise modified it.  Population 1 will correspond to `p1` in the
#' simulation and population `2` will correspond to `p2`, etc. The years
#' will have 0 as the latest time and they go backward.  If you ran a SLiM
#' simulation forward from time 1 to 10, then SLiM year 10 will be tskit year 0 and
#' SLiM year 1 will be tskit year 9. So, to retain p1 and p2 individuals
#' in tskit years 0 through four, you would set
#' ``years_list = list(`1` = 0:4, `2` = 0:4)``
#' @param run_py Logical. If true, it will run the python code, otherwise
#' it will just return in.
#' @export
read_and_filter_trees <- function(
  trees_path,
  years_list,
  run_py = TRUE
)
{

  # make a string to pick out the pops and years
  year_commas <- lapply(years_list, function(x) paste(x, collapse = ","))

  selector <- lapply(names(year_commas), function(x) {
    glue::glue("((XX__wops == {x}) & np.isin(XX__wimes, [{year_commas[x]}]))")
  }) %>%
    unlist() %>%
    paste(., collapse = " | " )

python_lines <- '

import tskit
import numpy as np
XX__whole_tree = tskit.load("{trees_path}")

# get sample nodes.
XX__wids = np.array([n.id for n in XX__whole_tree.nodes() ])
XX__wops = XX__whole_tree.nodes_population
XX__wimes = XX__whole_tree.nodes_time
XX__kept_nodes = XX__wids[{selector}] # keep the requested nodes

# simplify
ts = XX__whole_tree.simplify(
  XX__kept_nodes,
  keep_unary = True,  # this is critical.  Otherwise you only get segments that have coalesced
  keep_input_roots = True,
  filter_populations = False  # this is critical to keep population IDs 1-based (otherwise it gets turned to 0-based)
  )

'

  if(run_py) { reticulate::py_run_string(glue::glue(python_lines))  }

  glue::glue(python_lines)
}
