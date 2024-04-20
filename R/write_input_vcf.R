#' Write a tibble of genotypes into a VCF for input as founder genotypes in SLiM
#'
#' Basic and utilitarian.
#' @param Genos A tibble with a column POS and then multiple columns
#' of forward-slash separated genotypes
#' @param out_path  path to write out the resulting VCF
#' @param chrom Name of chromosome to use.  Defaults to Chr1
#' @export
write_input_vcf <- function(Genos, out_path, chrom = "Chr1") {
  Genos %>%
    mutate(
      `#CHROM` = chrom,
      .before = POS
    ) %>%
    mutate(
      ID = ".",
      REF = "A",
      ALT = "G",
      QUAL = 30,
      FILTER = "PASS",
      INFO = "Simmed=1;",
      FORMAT = "GT",
      .after = POS
    ) %>%
    write_tsv(out_path)
}
