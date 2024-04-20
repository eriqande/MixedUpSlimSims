#' Read a tree sequence and retain nodes from certain populations at certain times
#'
#' This uses glue to write values into a python script which is then run
#' by `reticulate::py_run_string` which deposits items into the `py` variable
#' in the global environment.
#' @param trees_path The path to the .trees file that should be read.
#' @param years_list A named list.  Each name is an integer signifying a SLiM
#' population, and the value is an integer vector of tree-seq years for which
#' indivdividuals/nodes in the tree seq from the named population should be
#' retained in the tree-sequence. For example `list(`3` = 0:4)`
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
  keep_input_roots = True
  )

'

  if(run_py) { reticulate::py_run_string(glue::glue(python_lines))  }

  glue::glue(python_lines)
}
