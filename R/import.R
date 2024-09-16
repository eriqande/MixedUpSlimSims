



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


#' @importFrom dplyr arrange bind_cols bind_rows distinct filter group_by join_by lag left_join mutate n pull rename select semi_join slice starts_with summarise ungroup
#' @importFrom ggplot2 aes facet_grid facet_wrap geom_hline geom_rect ggplot scale_fill_manual theme_bw
#' @importFrom purrr map
#' @importFrom readr read_tsv write_tsv
#' @importFrom reticulate py_run_string r_to_py source_python
#' @importFrom stats runif
#' @importFrom stringr str_c str_detect str_replace str_split
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyr expand_grid extract nest pivot_longer
NULL




# quiets concerns of R CMD check re: the . and other column names
# that appear in dplyr chains
if (getRversion() >= "2.15.1")  {
  utils::globalVariables(
    c(
      "1",
      "2",
      ".",
      "POS",
      "X1",
      "X2",
      "X3",
      "X4",
      "X8",
      "admix_fract",
      "admixture_1_score",
      "allele",
      "anc1",
      "anc2",
      "anc_pop",
      "anc_popc",
      "ancestors",
      "big_chrom",
      "bp",
      "child",
      "chrom",
      "cPOS",
      "cpm1",
      "cumul_start",
      "diag_spp",
      "diag_spp_idx",
      "dose",
      "dose_c",
      "geno",
      "grp",
      "haplo",
      "ind",
      "indiv",
      "ind_id",
      "ind_int",
      "ind_pop",
      "ind_time",
      "kid",
      "left",
      "ma",
      "node_id",
      "node_id1",
      "node_id2",
      "node_idx_in_ind",
      "node_ind",
      "node_pop",
      "node_time",
      "node_x",
      "pa",
      "parent",
      "path",
      "ped_id",
      "ped_p1",
      "ped_p2",
      "pop",
      "pos",
      "py",
      "relatives",
      "right",
      "sample_id",
      "sex",
      "time",
      "time_f",
      "trit",
      "trit_c",
      "vcf_id"
)
)
}
