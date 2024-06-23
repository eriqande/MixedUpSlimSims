#' get ancestry tracts of individuals
#'
#' This takes the output of `ancestral_segs()` and returns a tibble of the
#' ancestry doses of the individuals themselves.  To do this, the function
#' relies on `bedtools` which must be on your path.
#' @param A a tibble of ancestral segments like that produced by `ancestral_segs()`.
#' @export
indiv_ancestry_tracts <- function(A) {

  # first, grab the individual meta data on here so that we can join it later
  meta <- A %>%
    select(node_ind, ind_pop, ind_time, sex, ped_id, ped_p1, ped_p2) %>%
    distinct()

  # We can pretend that each individual is a "chromosome" and
  # intersect the two nodes within each of those individuals in
  # one big swoop.

  # first we need to get two different tibbles, one for the first and another for the
  # second node within each individual.
  segs_in_inds_tmp <- A %>%
    select(node_ind, left, right, node_id, anc_pop) %>%
    group_by(node_ind) %>%
    mutate(node_idx_in_ind = as.integer(factor(node_id))) %>%
    ungroup()

  first_nodes <- segs_in_inds_tmp %>%
    filter(node_idx_in_ind == 1)
  second_nodes <- segs_in_inds_tmp %>%
    filter(node_idx_in_ind == 2)

  # set up a temp directory for running bedtools and then get paths for
  # various files
  tdir <- tempfile()
  message("Running bedtools in ", tdir)
  dir.create(tdir, recursive = TRUE)
  Abed = file.path(tdir, "A.bed")
  Bbed = file.path(tdir, "B.bed")
  Cbed = file.path(tdir, "C.bed")

  # write those out
  first_nodes %>%
    select(node_ind, left, right, anc_pop) %>%
    write_tsv(file = Abed, col_names = FALSE)

  second_nodes %>%
    select(node_ind, left, right, anc_pop) %>%
    write_tsv(file = Bbed, col_names = FALSE)

  CALL <- paste("bedtools intersect -a", Abed, "-b", Bbed, "-wb >", Cbed, sep = " ", collapse = " ")
  system(CALL)


  # here is a quick function to compute the trits from the ancestry.
  # each dose of ancestry from p1 adds a value of 1 to the 3^0 digit
  # in a ternary number.  Each dose of ancestry from p2 adds a value of
  # 1 to the 3^1 digit, and so forth.  By this, we can account for more than
  # just two populations/species
  trits <-  function(a1, a2) {
    3 ^ (a1 - 1) + 3 ^ (a2 - 1)
  }

  ind_segs <- read_tsv(Cbed, col_names = FALSE) %>%
    rename(
      ind = X1,
      left = X2,
      right = X3,
      anc1 = X4,
      anc2 = X8
    ) %>%
    select(-starts_with("X")) %>%
    mutate(
      trit = trits(anc1, anc2)
    ) %>%
    select(ind, left, right, anc1, anc2, trit) %>%
    arrange(ind, left, right)

  message("Collapsing adjacent common ancestry tracts...")

  # then collapse adjacent common intervals
  ind_segs2 <- ind_segs %>%
    group_by(ind) %>%
    mutate(
      grp = cumsum(trit != lag(trit, default = 1))
    ) %>%
    group_by(ind, grp) %>%
    slice(1, n()) %>%
    summarise(left = left[1], right = right[2], anc1 = anc1[1], anc2 = anc2[1], trit = trit[1]) %>%
    ungroup()

  # attach the meta data back on there and return the result.
  ind_segs2 %>%
    rename(ind_id = ind) %>%
    select(-grp) %>%
    left_join(meta, by = join_by(ind_id == node_ind))

}
