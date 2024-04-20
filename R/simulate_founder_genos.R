#' Simulate genotypes for founders from allele frequencies
#'
#' This takes in allele frequencies for different populations and the number
#' of individuals in those populations, and simulates genotypes for them into
#' a tibble.
#' @param Freqs A tibble with a column `POS` which are positions in the cumulative base pairs
#' that SLiM uses, and then columns `afreq_1`, `afreq_2`, `afreq_3`, etc.  These are the
#' alternate allele freqs in populations 1, 2, and 3, and so on. Note that the names
#' of those columns don't really matter---what matters is their position. The first
#' must be the freqs for population 1, the second for population 2, and so forth.
#' @param N A vector of number of diploids to simulate for each of the populations.
#' @param pop_names A vector of the same length as N, with short names for pops.
#' No whitespace in the names, please!
#' @return Returns a tibble with first column POS and subsequent columns
#' named pop_name + 0-based index.  Includes genotypes from all the
#' individuals in all the pops.
#' @export
#' @examples
#' # make allele freqs
#' freq <- tibble::tibble(
#'   POS = seq(10, 90, by = 10),
#'   af1 = rep(0.1, 9),
#'   af2 = rep(0.9, 9),
#'   af3 = rep(0.5, 9)
#' )
#'
#' # number of diploids to simulate
#' n <- c(3, 5, 7)
#'
#' # names of populations
#' pn <- c("a", "b", "c")
#'
#' # run the function
#' simulate_founder_genos(freq, n, pn)
simulate_founder_genos <- function(Freqs, N, pop_names) {

  stopifnot(length(N) == length(pop_names) && length(N) == ncol(Freqs) - 1)

  L <- nrow(Freqs)
  P <- length(N) # number of pops

  geno_tib <- lapply(1:P, function(i) {
    fr <- rep(Freqs[[i + 1]], N[i])
    g1 <- as.integer(runif(n = length(fr)) < fr)
    g2 <- as.integer(runif(n = length(fr)) < fr)
    geno <- paste(g1, g2, sep = "/") %>%
      matrix(nrow = L)
    colnames(geno) <- paste0(pop_names[i], (1:N[i]) - 1)
    gtib <- as_tibble(geno)
    return(gtib)
  }) %>%
    bind_cols()

  Freqs %>%
    select(POS) %>%
    bind_cols(geno_tib) %>%
    mutate(POS = as.integer(POS))
}
