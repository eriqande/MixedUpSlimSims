


#' Chromosome raw and cumulative start and end points for rainbow trout (Oncorhynchus mykiss)
#'
#' These are for the chromosomes from Omy_v1.0.  This is an example of the specification
#' that one should use for their chromosomes.
#' @format A tibble with columns `chrom`, `start`, `end`, `cumul_start`, and `cumul_end`.
#' @source Omyk_v1.0 genome available on [NCBI](https://www.ncbi.nlm.nih.gov/datasets/genome/GCF_002163495.1/).
#' Eric got the fai via samtools and processed it into this form.
"mykiss_chroms"


#' Example data set of positions of species-diagnostic markers
#'
#' These were pulled from the Cyclone Creek data set.
#' @format A tibble with columns:
#' - `diag_spp`: 1 = WCT, 2 = RBT, 3 = YCT
#' - `chrom`: the chromosome in the mykiss genome
#' - `pos`: the position of the marker on chromosome chrom.
"example_spp_diag_markers"



#' Example indiv_segs.
#'
#' This is an example of the sort of output that comes out of [indiv_ancestry_tracts()].
#' This particular data set includes 10 individuals simulated when
#' developing this package.
#' @format See the documentation for [indiv_ancestry_tracts()].
"example_indiv_segs"



#' Example frequencies of alleles in two ancestries
#'
#' These were estimated from cyclone creek and are mapped to the
#' mykiss genome of `mykiss_chroms`
#' @format A tibble
"cyclone_creek_var_freqs"


#' Diagnostic markers by name
#'
#' These are WCT, RBT, and YCT diagnostic markers.  Identical to
#' `example_spp_diag_markers` except that the diag_spp column holds
#' letter codes for the subspecies.
#' @format A tibble
"wct_rbt_yct_diagnostic_markers"
