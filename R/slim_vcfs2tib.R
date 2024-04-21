#' Read in multiple SLiM formatted VCF file
#'
#' @param Flist a list of pops and years to get, like `list(p1 = 9:11, p2 = 9:11)`.
#' @param Dir directory the files are in.  Defaults to current working directory.
#' @param parse_path Logical.  If TRUE, the file name is parsed to get the population
#' and the slim time (i.e. the cycle or generation in SLiM). Default = TRUE.
#' @param drop_vcf_id Logical. If TRUE, drop the vcf_id column.  Default = TRUE. Has
#' no effect if long = FALSE.
#' @param drop_node_ids Logical.  If TRUE, drop the node_id1 and node_id2 columns. Has
#' no effect if long = FALSE.
#' @inheritParams slim_vcf2tib
#' @export
slim_vcfs2tib <- function(
    Flist,
    Dir = ".",
    long = TRUE,
    parse_path = TRUE,
    drop_vcf_id = TRUE,
    drop_node_ids = TRUE
) {

  big_tib <- lapply(names(Flist), function(p) {
    lapply(Flist[[p]], function(y) {
      fn = paste(p, "-", y, ".vcf", sep = "", collapse = "")
      slim_vcf2tib(file.path(Dir, fn), long = long)
    }) %>% bind_rows()
  }) %>% bind_rows()

  if(drop_vcf_id == TRUE && long == TRUE) {
    bt2 <- big_tib %>% select(-vcf_id)
  } else {
    bt2 <- big_tib
  }
  rm(big_tib)

  if(drop_node_ids == TRUE && long == TRUE) {
    bt3 <- bt2 %>% select(-node_id1, -node_id2)
  } else {
    bt3 <- bt2
  }
  rm(bt2)

  if(parse_path == TRUE) {
    bt4 <- bt3 %>%
      extract(path, into = c("pop", "slim_time"), regex = "p([0-9]+)-([0-9]+)\\.vcf$")
  } else {
    bt4 <- bt3
  }
  rm(bt3)

  bt4
}
