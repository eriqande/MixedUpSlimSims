#' Read a single SLiM-formatted VCF into a tibble of genotypes
#'
#' By default this makes it long format, but if you want to keep it wide
#' format, you can request long = FALSE. Wide format could be better
#' if you need to make a wide-format data set out of it, or for space
#' purposes.
#'
#' MOre
#' @param path The path to the VCF file.
#' @param long Logical. If true (the default) this returns the long format
#' version.  Otherwise it returns the wide format.
#' @export
slim_vcf2tib <- function(path, long = TRUE) {
  NoNodeIDs <- FALSE

  # get the iXXX names
  p1 <- pipe(str_c("bcftools query -l ", path))
  id1 <- readLines(p1)
  close(p1)


  # get the Pedigree IDs of the nodes, if present, and turn
  # them into the pedigree ids of the individuals.  We call them
  # ped_id's here because that corresponds to what we call them
  # after getting them out of tskit.
  p2 <- pipe(str_c("head -n 40 ", path))
  lines <- readLines(p2)
  close(p2)
  node_id_line <- lines[str_detect(lines, "slimGenomePedigreeIDs")] %>%
    str_replace("##slimGenomePedigreeIDs=", "")
  if(length(node_id_line) == 0) {
    NoNodeIDs <- TRUE
  }
  if(NoNodeIDs == FALSE) {
    node_id_mat <- matrix(as.integer(str_split(node_id_line, ",")[[1]]), ncol = 2, byrow = TRUE)
    meta <- tibble(
      vcf_id = id1,
      ped_id = node_id_mat[,1] / 2L,
      node_id1 = node_id_mat[,1],
      node_id2 = node_id_mat[,2]
    ) %>%
      mutate(path = path, .before = vcf_id)
  } else {
    meta <- tibble(vcf_id = id1) %>%
      mutate(path = path, .before = vcf_id)
  }


  # Now, we get the actual vcf file
  p3 <- pipe(str_c("bcftools query -f '%CHROM\t%POS[\t%GT]\n' ", path))
  genos_wide <- read_tsv(p3, col_names = c("chrom", "pos", id1), progress = FALSE, show_col_types = FALSE)

  if(long == TRUE) {
    genos <- genos_wide %>%
      pivot_longer(cols = -c(chrom, pos), names_to = "vcf_id", values_to = "geno") %>%
      extract(geno, into = c("1", "2"), regex = "([01])[/|]([01])") %>%
      pivot_longer(cols = c(`1`, `2`), names_to = "haplo", values_to = "allele") %>%
      left_join(meta, ., by = join_by(vcf_id))
  } else {
      tmp <- genos_wide
      names(tmp)[-c(1,2)] <- meta$ped_id
      genos <- tmp %>%
        mutate(path = path, .before = chrom)
  }

  genos

}
