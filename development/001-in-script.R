


library(reticulate)
library(tidyverse)
library(MixedUpSlimSims)



# set up conda env
PYSLIM_CONDA <- normalizePath("~/mambaforge-x86_64/envs/pyslim")
use_condaenv(condaenv = PYSLIM_CONDA)


# run slim
system("slim -s 222 SLiM-models/wf3.slim")



# read the trees
dump <- read_and_filter_trees(
  trees_path = "slim_test.trees",
  years_list = list(`3` = 0:9)
)


# get the nodes and inds tibbles
ni <- ts_nodes_and_inds(py$ts)


# get the founders of the SLiM sim for finding pop-origins of segments
founders <- founder_node_ids_and_pops(ni, 9)


# get the focal nodes.  In this case we want population 2 from times 0 to 7
fnl <- get_focal_nodes(ni, Focal = tibble(pop = rep(2, 8), time = 0:7))


# now, get the ancestral origin of all the segments:
segments_tib <- ancestral_segs(
  ts = py$ts,
  focal_nodes_list = fnl,
  founder_nodes = founders$nodes,
  nodes_tib = ni$nodes_tib,
  indiv_tib = ni$indiv_tib
)


# plot those things
plot_ancestry_of_genomes(segments_tib)



# now get the individual-level ancestry tracts:
ind_segs2 <- indiv_ancestry_tracts(segments_tib)


# plot those segs:
plot_ancestry_copy_number(ind_segs2)


# to put lines at the ends of chromosomes, pass in
# a tibble with a column `cumul_end` which
# gives the cumulative position of the end of each
# chromosome.  We have package data mykiss_chroms that
# gives this
plot_ancestry_copy_number(ind_segs2, chrom_ends = mykiss_chroms)

ggsave("indiv-ancestry-doses.pdf", width = 12, height = 30)


### HAVEN'T CLEANED UP, OR FINISHED, STUFF BELOW HERE
# and now we can segment that stuff into chromosomes, too
ind_segs2 %>%
  mutate(
    chrom = 1
  ) %>%
  select(chrom, left, right, ind, dose) %>%
  write_tsv("D.bed", col_names = FALSE)

# and make a
