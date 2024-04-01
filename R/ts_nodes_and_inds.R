

#' return a tibble of the nodes and indviduals and meta data from a tree sequence
#'
#' The way this works is a little non-standard.  It writes the input, `ts` into
#' the global environment under the name xx__TTKKSS where python can act upon it.
#' Then a bunch of other stuff gets written into the python environment which is
#' then pulled back into a tibble.  At the end, we try to remove all the `xx__`
#' variables that we put into different global environments.
#' @param ts the tree sequence object as you would access it from the R environment,
#' for example, `py$ts` or `py$tree`, etc.
#' @export
ts_nodes_and_inds <- function(ts) {

  list2env(list(xx__TTKKSS = ts), envir = .GlobalEnv)

  # get a bunch of stuff with python
  source_python(system.file("py/nodes_and_inds.py", package = "MixedUpSlimSims"))

  # and then make tibbles out of that
  indiv_tib <- tibble(
    ind_id = as.integer(py$xx__ind_ids),
    ind_pop = as.integer(py$xx__ind_pops),
    ind_time = as.double(py$xx__ind_times),
    sex = as.integer(py$xx__ind_sex),
    ped_id = as.integer(py$xx__ind_pedigree_ids),
    ped_p1 = as.integer(py$xx__ind_pedigree_p1),
    ped_p2 = as.integer(py$xx__ind_pedigree_p2)
  )

  nodes_tib <- tibble(
    node_id = as.integer(py$xx__node_ids),
    node_pop = as.integer(py$xx__node_pops),
    node_ind = as.integer(py$xx__node_indivs),
    node_time = as.integer(py$xx__node_times)
  )

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

  list(
    indiv_tib = indiv_tib,
    nodes_tib = nodes_tib
  )

}
