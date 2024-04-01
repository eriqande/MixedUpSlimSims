



#### Import the pipe operator from magrittr ####
#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


#' @importFrom dplyr arrange distinct filter group_by join_by lag left_join mutate n rename select slice starts_with summarise ungroup
#' @importFrom readr read_tsv write_tsv
#' @importFrom reticulate py_run_string r_to_py source_python
#' @importFrom stringr str_detect
#' @importFrom tibble as_tibble tibble
NULL




# quiets concerns of R CMD check re: the . and other column names
# that appear in dplyr chains
if (getRversion() >= "2.15.1")  {
  utils::globalVariables(
    c(
      "X1",
      "X2",
      "X3",
      "X4",
      "X8",
      "anc1",
      "anc2",
      "anc_pop",
      "child",
      "dose",
      "grp",
      "ind",
      "ind_id",
      "ind_pop",
      "ind_time",
      "left",
      "node_id",
      "node_idx_in_ind",
      "node_ind",
      "node_pop",
      "node_time",
      "parent",
      "ped_id",
      "ped_p1",
      "ped_p2",
      "py",
      "right",
      "sex"
)
)
}
