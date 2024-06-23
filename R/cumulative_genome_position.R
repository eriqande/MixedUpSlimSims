#' Add a column called cPOS that is the cumulative genome position
#'
#' SLiM does everythign in terms of cumulative genome positions (i.e., there is
#' just one chromosome that happens to have locations where the recombination
#' fraction if 1/2).  So, if we have a genome with coordinates of chrom, pos,
#' we might want to get the cPOS for each.  Doing so can be done efficiently
#' by precomputing the cumulative start point of each chromosome like we
#' have in the package data object [`mykiss_chroms`].
#' @param X a tibble that has columns of `chrom` and `pos`
#' @param Cumul a tibble that has, at a minimum, the columns `chrom`, and `cumul_start`.
#' It will often be easiest to pass in your genome chromosome cumulative tibble
#' like [`mykiss_chroms`]
#' @return Returns a tibble like X but with an additional column cPOS that has the
#' cumulative position.
#' @export
#' @examples
#' cumulative_genome_position(example_spp_diag_markers, mykiss_chroms)
#'
cumulative_genome_position <- function(X, Cumul) {
  C2 <- Cumul %>%
    select(chrom, cumul_start)

  X %>%
    left_join(C2, by = join_by(chrom), relationship = "many-to-one") %>%
    mutate(cPOS = pos + cumul_start - 1L) %>%
    select(-cumul_start)
}
