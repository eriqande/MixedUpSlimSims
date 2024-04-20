#' Plot genome segments in focal individuals colored by ancestral population
#'
#' This just wraps up some ggplot.  Note that this plots the ancestry of each
#' of the two distinct genomes within an individual, but it does not plot the
#' two genomes within an individual necessarily adjacent to one another. For that
#' see `plot_ancestry_copy_number()`
#' @param Segs a tibble of segments like what comes out of `ancestral_segs()`
#' @export
#' @return a ggplot object
plot_ancestry_of_genomes <- function(Segs) {


  # get x and y values
  st_withx <- Segs %>%
    mutate(node_time_f = factor(node_time, levels = rev(sort(unique(node_time))))) %>%  # do this so earlier times are higher in the facets
    group_by(node_time, node_pop) %>%
    mutate(
      node_x = as.integer(factor(node_id)),
      anc_popc = as.character(anc_pop)
    )

  ggplot(st_withx) +
    geom_rect(aes(xmin=node_x - 1, xmax = node_x, ymin = left, ymax = right, fill = anc_popc), colour = NA) +
    facet_grid(node_time_f ~ node_pop, scales = "free_x") +
    theme_bw()

}
