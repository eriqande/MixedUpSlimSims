#' Plot the ancestry copy number along segments in a group of individuals
#'
#' More documentation needed.  Basically just plots what comes out of
#' [indiv_ancestry_tracts()].  Currently assumes just two ancestries.
#' Plots individuals in order of admixture fraction.
#' @param IndSegs the tibble that comes out of [indiv_ancestry_tracts()].
#' @param chrom_ends if desired, a tibble with column `chrom` giving the
#' chromosome name and column `end` giving the cumulative base pair
#' position of the end of the chromosome.  Makes the plot return horizontal
#' lines at the chromosome boundaries.
#' @export
#' @return returns a ggplot object
plot_ancestry_copy_number <- function(
    IndSegs,
    chrom_ends = NULL
) {

  ind_segs3 <- IndSegs %>%
    mutate(
      time_f = factor(ind_time, levels = rev(sort(unique(ind_time))))
    ) %>%
    group_by(ind_id) %>%
    mutate(admixture_1_score = sum( (right - left) * dose)) %>%
    ungroup() %>%
    arrange(time_f, admixture_1_score, ind_id) %>%
    group_by(time_f) %>%
    mutate(
      ind_int = as.integer(factor(ind_id, levels = unique(ind_id))),
      dose_c = as.character(dose)
    )




  g <- ggplot(ind_segs3) +
    geom_rect(aes(xmin = ind_int - 1, xmax = ind_int, ymin = left, ymax = right, fill = dose_c)) +
    facet_wrap(~ time_f, ncol = 1) +
    scale_fill_manual(values = c(`0` = "red", `1` = "orange", `2` = "blue"))

  if(!is.null(chrom_ends)) {
    g <- g + geom_hline(yintercept = chrom_ends$cumul_end, linewidth = 0.05)
  }

  g
}
