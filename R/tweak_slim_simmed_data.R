#' Subsample SNPs or individuals and simulated genotyping error
#'
#' Once you have simulated a SLIM dataset with `slim_sim_a_dataset()`,
#' you can use this function to do the following:
#' - Downsample to a consistent subsample of some number NV variable
#'   markers and/or some number ND diagnostic ones.
#' - Create missing data at the remaining markers at a simple rate
#'   MV or MD across variable or diagnostic markers, respectively.
#' - Randomly toss each individual in the sample from the sample
#'   at a single rate S across all individuals, pops and times.
#' - Downsample the number of individuals to a specific number or
#'   fraction by population and time (not yet implemented).
#' @param X the return object of `slim_sim_a_dataset()`.
#' @param NV the number of variable markers to randomly downsample to, retaining
#' _the same markers across all individuals_ and completely discarding
#' the rest. If `NV` is greater than the total number of variable markers
#' a warning is issued and the full set of markers is returned.
#' @param ND the number of diagnostic markers to randomly downsample to, retaining
#' _the same markers across all individuals_ and completely discarding
#' the rest. If `ND` is greater than the total number of diagnostic markers
#' a warning is issued and the full set of markers is returned.
#' @param S the fraction of sampled individuals to be randomly
#' _retained_.  So, if you want to have sampled 40% of the population
#' then you would set S = 0.4.
#' @param MV the missing data data rate for variable markers. Each genotype in the
#' variable-marker data set (after subsampling to NV markers) has an independent
#' probability of MV of being missing applied.
#' @param MD the missing data data rate for diagnostic markers. Each genotype in the
#' variable-marker data set (after subsampling to NV markers) has an independent
#' probability of MV of being missing applied.
#' @param EV the probability that any non-missing variable marker genotype in an individual
#' has a genotyping error.  For a genotype that is simulated to have
#' a genotyping error, if the true genotype is a 1 we choose either 0 or 1, each
#' with probability of 1/2. If the true genotype is a 0 or a 2, the
#' genotyping error results in a 1.  (Genotyping error to the opposite
#' homozygote does not occur).
#' @param ED the probability that any non-missing diagnostic marker genotypes in an individual
#' has a genotyping error.  For a genotype that is simulated to have
#' a genotyping error, if the true genotype is a 1 we choose either 0 or 1, each
#' with probability of 1/2. If the true genotype is a 0 or a 2, the
#' genotyping error results in a 1.  (Genotyping error to the opposite
#' homozygote does not occur).
#' @return This returns a list with the downsampled and genotyping-errored
#' versions of the data set.
#' @export
#' @examples
#'
#' # get a data set to work with
#' X <- slim_sim_dataset_example
#'
#' tweaked <- tweak_slim_simmed_data(
#'   X,
#'   NV = 600,
#'   ND = 1000,
#'   S = 0.7,
#'   MV = 0.1,
#'   MD = 0.15,
#'   EV = 0.01,
#'   ED = 0.005
#' )
#'
#'
tweak_slim_simmed_data <- function(
  X,
  NV = NA,
  ND = NA,
  S = NA,
  MV = NA,
  MD = NA,
  EV = NA,
  ED = NA
) {

  # get some constants regarding the data set
  nV <- n_distinct(X$variable_snps$chrom, X$variable_snps$pos)  # number of variable markers
  nD <- n_distinct(X$spp_diag_snps$chrom, X$spp_diag_snps$pos)  # number of variable markers
  nS <- n_distinct(X$variable_snps$indiv)

  #### Downsample the variable markers ####
  if(is.na(NV)) { # return the full complement
    vm2 <- X$variable_snps
  } else { # otherwise we downsample the variable markers randomly.

    if(NV > nV) {
      warning("NV, ", NV, ", is larger than the total number of variable markers, ", nV, ". Returning all of them...")
      vm2 <- X$variable_snps
    } else {
    vm2 <- X$variable_snps %>%
      distinct(chrom, pos) %>%
      slice_sample(n = NV) %>%
      semi_join(X$variable_snps, ., by = join_by(chrom, pos))
    }
  }


  #### Downsample the diagnostic markers ####
  if(is.na(ND)) { # return the full complement
    dm2 <- X$spp_diag_snps
  } else { # otherwise we downsample the variable markers randomly.

    if(ND > nD) {
      warning("ND, ", ND, ", is larger than the total number of species diagnostic markers, ", nD, ". Returning all of them...")
      dm2 <- X$spp_diag_snps
    } else {
      dm2 <- X$spp_diag_snps %>%
        distinct(chrom, pos) %>%
        slice_sample(n = ND) %>%
        semi_join(X$spp_diag_snps, ., by = join_by(chrom, pos))
    }
  }


  #### Downsample the samples ####
  if(is.na(S)) { # return the full complement
    vm3 <- vm2
    dm3 <- dm2
  } else { # otherwise we downsample the sampled individuals randomly.

    if(S < 0 || S > 1) {
      stop("S is ", S, ", but it must be between 0 and 1 inclusive")
    }
    # get the list of subsampled individuals
    keeper_samples <- vm2 %>%
      distinct(indiv) %>%
      slice_sample(prop = S)

    vm3 <- vm2 %>%
      semi_join(keeper_samples, by = join_by(indiv))

    dm3 <- dm2 %>%
      semi_join(keeper_samples, by = join_by(indiv))
  }




  #### Create missing data in the variable markers ####
  if(is.na(MV)) { # return the full complement
    vm4 <- vm3
  } else { # otherwise we randomly make things missing

    if(MV < 0 || MV > 1) {
      stop("MV is ", MV, ", but it must be between 0 and 1 inclusive")
    }

    vm4 <- vm3 %>%
      mutate(
        rando = runif(n = n()),
        n = case_when(
          rando < MV ~ NA_integer_,
          TRUE ~ n
        )
      ) %>%
      select(-rando)
  }


  #### Create missing data in the diagnostic markers ####
  if(is.na(MD)) { # return the full complement
    dm4 <- dm3
  } else { # otherwise we randomly make things as missing

    if(MD < 0 || MD > 1) {
      stop("MD is ", MD, ", but it must be between 0 and 1 inclusive")
    }

    dm4 <- dm3 %>%
      mutate(
        rando = runif(n = n()),
        n = case_when(
          rando < MD ~ NA_integer_,
          TRUE ~ n
        )
      ) %>%
      select(-rando)
  }


  #### simulate some genotyping error in the variable markers ####
  if(is.na(EV)) { # return the full complement
    vm5 <- vm4 %>% mutate(n_orig = n)
  } else { # otherwise we randomly put genotyping errors in there

    if(EV < 0 || EV > 1) {
      stop("EV is ", EV, ", but it must be between 0 and 1 inclusive")
    }

    vm5 <- vm4 %>%
      mutate(
        rando1 = runif(n = n()),
        rando2 = runif(n - n()),
        n_orig = n,
        n = case_when(
          is.na(n_orig) ~ NA_integer_,
          rando1 < EV & n_orig == 1 & rando2 < 0.5 ~ 0L,
          rando1 < EV & n_orig == 1 & rando2 >= 0.5 ~ 2L,
          rando1 < EV & (n_orig == 0 | n_orig == 2) ~ 1L,
          TRUE ~ n_orig
        )
      ) %>%
      select(-rando1, -rando2)
  }


  #### simulate some genotyping error in the diagnostic markers ####
  if(is.na(ED)) { # return the full complement
    dm5 <- dm4 %>% mutate(n_orig = n)
  } else { # otherwise we randomly put genotyping errors in there

    if(ED < 0 || ED > 1) {
      stop("ED is ", ED, ", but it must be between 0 and 1 inclusive")
    }

    dm5 <- dm4 %>%
      mutate(
        rando1 = runif(n = n()),
        rando2 = runif(n - n()),
        n_orig = n,
        n = case_when(
          is.na(n_orig) ~ NA_integer_,
          rando1 < ED & n_orig == 1 & rando2 < 0.5 ~ 0L,
          rando1 < ED & n_orig == 1 & rando2 >= 0.5 ~ 2L,
          rando1 < ED & (n_orig == 0 | n_orig == 2) ~ 1L,
          TRUE ~ n_orig
        )
      ) %>%
      select(-rando1, -rando2)
  }

  # break the n_orig off of vm5 and dm5
  n_orig_diagnostic <- dm5 %>%
    select(n_orig)
  n_orig_variable <- vm5 %>%
    select(n_orig)

  dm6 <- dm5 %>%
    select(-n_orig)
  vm6 <- vm5 %>%
    select(-n_orig)

  # go ahead and return things
  ret <- list(
    tweak_pars = list(
      NV = NV,
      ND = ND,
      S = S,
      MV = MV,
      MD = MD,
      EV = EV,
      ED = ED
    ),
    variable_snps = vm6,
    spp_diag_snps = dm6,
    spp_names = X$spp_names
  )

  if(!is.na(S)) {
    ret$pairwise_relats = X$pairwise_relats %>%
      filter((id_1 %in% keeper_samples$indiv) & (id_2 %in% keeper_samples$indiv))
  }

  ret
}
