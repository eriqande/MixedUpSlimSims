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
plot_ancestry_tracts <- function(
    IndSegs,
    chrom_ends = NULL
) {

  # first, get the admixture from pop p2 to sort on
  adm2 <- admixture_fracts_from_ancestry_tracts(IndSegs) %>%
    filter(anc_pop == 2) %>%
    select(ind_id, admix_fract)

  ind_segs3 <- IndSegs %>%
    left_join(adm2, by = join_by(ind_id)) %>%
    mutate(
      time_f = factor(ind_time, levels = rev(sort(unique(ind_time))))
    ) %>%
    arrange(time_f, ind_pop, admix_fract, ind_id) %>%
    group_by(time_f, ind_pop) %>%
    mutate(
      ind_int = as.integer(factor(ind_id, levels = unique(ind_id))),
      trit_c = as.character(trit)
    ) %>%
    ungroup()




  g <- ggplot(ind_segs3) +
    geom_rect(aes(xmin = ind_int - 1, xmax = ind_int, ymin = left, ymax = right, fill = trit_c)) +
    facet_grid(time_f ~ ind_pop) +
    scale_fill_manual(values = c(
      `2` = "#d73027",
      `4` = "#fdae61",
      `6` = "#4575b4",
      `10` = "#ffffbf",
      `12` = "#e0f3f8",
      `18` = "#984ea3"
    )) +
    theme_bw()

  if(!is.null(chrom_ends)) {
    g <- g + geom_hline(yintercept = chrom_ends$cumul_end, linewidth = 0.05)
  }

  g
}
