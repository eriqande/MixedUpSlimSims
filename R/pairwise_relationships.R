#' Return pairwise relationships between the samples given the population pedigree
#'
#' This uses some functions from CKMRpop to deliver relationships.
#'
#' Note that for convenience, we discard the individual with ped_id 0,
#' which in-effect means that ped_id 0 just becomes another unobserved
#' founder of any other individuals.  Unless 0 is within num_gen generations
#' of any of your samples, this should have no effect at all.
#' @param IT The `indiv_tib` component of the return list from
#' `ts_nodes_and_inds()`.  Or any tibble with at least with columns
#' `ind_time`, `sex`, `ped_id`, `ped_p1`, and `ped_p2`.
#' @param sample_years A vector of years in TSKIT time from which all the
#' individuals in the pedigree will be deemed the samples.
#' @param num_gen number of generations back from the sampled individuals to return in each
#' individual's ancestor vector. 0 = self, 1 =  back to and including the parents,
#' 2 = back to and including the grandparents, and so on.
#' @export
pairwise_relationships <- function(IT, sample_years, num_gen) {

  # For testing/dev
  # IT <- ni$indiv_tib
  # sample_years = 0:2
  # num_gen <- 2

  # prepare the pedigree (make sure sex is consistent and founders are in there)
  # We assume 0 is male and 1 is female, for sex.  It might be reversed, but
  # it won't matter for our work here...at the moment.  Note that ped_p1 has sex 0
  # and ped_p2 has sex 1.  So we define ped_p1 is pa, and ped_p2 is ma.

  # also, note that the individuals are indexed base-0 coming out of tskit.  And
  # the founders get parents of -1.  So, if we just increment everyone by 1 we will
  # be ready to go, we just have to decrement them all in the end.
  IT2 <- IT %>%
    mutate(
      kid = as.character(ped_id + 1L),
      pa = as.character(ped_p1 + 1L),
      ma = as.character(ped_p2+ 1L)
    ) %>%
    select(kid, pa, ma, ind_time)


  samples <- IT2 %>%
    filter(ind_time %in% sample_years) %>%
    pull(kid)

  # find the ancestors and relatives of everyone
  ancr <- CKMRpop::find_ancestors_and_relatives_of_samples(
    P = IT2,
    S = samples,
    n = num_gen
  ) %>%
    mutate( # decrement all the IDs by 1
      sample_id = as.character(as.integer(sample_id) - 1L),
      ancestors = map(ancestors, function(x) as.character(as.integer(x) - 1L)),
      relatives = map(relatives, function(x) as.character(as.integer(x) - 1L))
    ) %>%
    rename(ID = sample_id)

  # compile the related pairs
  crel <- CKMRpop::compile_related_pairs(ancr)

  crel

}
