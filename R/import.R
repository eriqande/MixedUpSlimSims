



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


#' @importFrom dplyr arrange bind_cols distinct filter group_by join_by lag left_join mutate n pull rename select semi_join slice starts_with summarise ungroup
#' @importFrom ggplot2 aes facet_wrap geom_hline geom_rect ggplot scale_fill_manual
#' @importFrom readr read_tsv write_tsv
#' @importFrom reticulate py_run_string r_to_py source_python
#' @importFrom stats runif
#' @importFrom stringr str_detect
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyr nest
NULL




# quiets concerns of R CMD check re: the . and other column names
# that appear in dplyr chains
if (getRversion() >= "2.15.1")  {
  utils::globalVariables(
    c(
      ".",
      "POS",
      "X1",
      "X2",
      "X3",
      "X4",
      "X8",
      "admixture_1_score",
      "anc1",
      "anc2",
      "anc_pop",
      "anc_popc",
      "child",
      "dose",
      "dose_c",
      "grp",
      "ind",
      "ind_id",
      "ind_int",
      "ind_pop",
      "ind_time",
      "left",
      "node_id",
      "node_idx_in_ind",
      "node_ind",
      "node_pop",
      "node_time",
      "node_x",
      "parent",
      "ped_id",
      "ped_p1",
      "ped_p2",
      "pop",
      "py",
      "right",
      "sex",
      "time",
      "time_f"
)
)
}
