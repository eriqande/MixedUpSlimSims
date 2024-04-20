#' calculate the admixture fraction of indivdiduals from ancestry tracts
#'
#' Super simple.  Just counting up the length in different ancestries, summing
#' the two diploid genotypes together.  This should extend to multiple ancestries
#' directly
#' @param AT the tibble of ancestry tracts like what comes out of [indiv_ancestry_tracts()].
#' @export
admixture_fracts_from_ancestry_tracts <- function(AT) {

  # first get stuff that we will join back on there
  meta <- AT %>%
    distinct(ind_id, ind_pop, ind_time, sex, ped_id, ped_p1, ped_p2)

  AT %>%
    select(ind_id, left, right, anc1, anc2) %>%
    pivot_longer(
      cols = c(anc1, anc2),
      names_to = "chrom_copy",
      values_to = "anc_pop"
    ) %>%
    group_by(ind_id, anc_pop) %>%
    summarise(
      bp = sum(right - left)
    ) %>%
    mutate(admix_fract = bp / sum(bp)) %>%
    ungroup() %>%
    select(-bp) %>%
    tidyr::complete(ind_id, anc_pop, fill = list(admix_fract = 0.0)) %>%
    left_join(meta, by = join_by(ind_id))

}
