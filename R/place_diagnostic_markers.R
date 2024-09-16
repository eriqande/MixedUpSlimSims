#' Place diagnostic marker alleles within individauls according to their ancestry haplotypes
#'
#' Once individuals have been simulated and you have the individual segments
#' within them, you can easily simulate the alleles of the species-diagnostic
#' markers. That is what this function does.
#'
#' For this to work, you have to have both bedtools and awk on your PATH.
#' @param Markers a tibble that _must_ have columns:
#'  - `diag_spp_idx`, which gives a 1-based
#'    index of the species for which the `1` allele is fixed at the marker (with all
#'    other species being fixed for the `0` allele).
#'  - `cPOS`, which are positions in the cumulative base pairs
#'    that SLiM uses.
#'  - `chrom`: the name of the chromosome (in the actual genome being modeled)
#'  - `pos`: the position in base pairs on chromosome `chrom`.
#'  Each row of this tibble must specify a unique species-diagnostic marker, and
#'  all of the species-diagnostic markers you wish to place into the simulated
#'  individuals should be included in this tibble.
#' @param IndivSegs A tibble of individual segments like that which is returned
#' by the function [indiv_ancestry_tracts()]. Though it can have arbitrary columns,
#' it must include at least the following:
#'  - `ped_id`: The pedigree id of the individual
#'  - `left`: the left endpoint of the interval
#'  - `right`: the right endpoint of the interval
#'  - `anc1`: the 1-based index of the ancestry of haplotype 1 within the interval.
#'  - `anc2`: the 1-based index of the ancestry of haplotype 2 within the interval.
#'
#' @return This returns a tibble with the columns `diag_spp_idx`, `chrom`, `pos`,
#' `ped_id`, and `geno`.  This is almost what is needed to hand the data off to
#' the `ancestral_n_segments()` function in the MixedUpParents package. You just
#' have to change some column names and denote diag_spp by A, B, C.
#' @export
#' @examples
#' # Prepare Markers. Name the columns right, and get cumulative cPOS in there.
#' M <- example_spp_diag_markers %>%
#'   dplyr::filter(diag_spp < 3) %>%
#'   dplyr::rename(diag_spp_idx = diag_spp) %>%
#'   cumulative_genome_position(Cumul = mykiss_chroms)
#'
#' # Now, also get some example indiv-segs. We have some as a package
#' # data object.
#' IS <- example_indiv_segs
#'
#' # Then run the function:
#' place_diagnostic_markers(M, IS)
place_diagnostic_markers <- function(Markers, IndivSegs) {

  IS2 <- IndivSegs %>%
    mutate(big_chrom = 1) %>%
    select(big_chrom, left, right, ped_id, anc1, anc2)

  M2 <- Markers %>%
    mutate(
      big_chrom = 1,
      cpm1 = cPOS - 1,
    ) %>%
    select(big_chrom, cpm1, cPOS, diag_spp_idx, chrom, pos)

  # set up a temp directory for running bedtools and then get paths for
  # various files
  tdir <- tempfile()
  message("Running bedtools in ", tdir)
  dir.create(tdir, recursive = TRUE)
  Abed = file.path(tdir, "A.bed")
  Bbed = file.path(tdir, "B.bed")

  write_tsv(IS2, file = Abed, col_names = FALSE)
  write_tsv(M2, file =  Bbed, col_names = FALSE)

  CALL <- paste(
    'bedtools intersect -a ',
    Abed,
    ' -b ',
    Bbed,
    ' -wa -wb | awk \'BEGIN {OFS="\t"; print "diag_spp_idx", "chrom", "pos", "ped_id", "geno"} {print $(10), $(11), $(12), $4, int( ($5 == $(10)) + ($6 == $(10)))}\' ',
    sep = " ",
    collapse = " "
  )

  read_tsv(pipe(CALL))
}
