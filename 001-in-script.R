


library(reticulate)
library(tidyverse)
library(MixedUpSlimSims)



# set up conda env
PYSLIM_CONDA <- normalizePath("~/mambaforge-x86_64/envs/pyslim")
use_condaenv(condaenv = PYSLIM_CONDA)


# run slim
system("slim -s 222 SLiM-models/wf3.slim")




# now run some python
py_run_string('

import tskit
import numpy as np
whole_tree = tskit.load("slim_test.trees")

# get sample nodes.  In this case we will take all the generations
# of the admixed p3 population.
wids = np.array([n.id for n in whole_tree.nodes() ])
wops = whole_tree.nodes_population
wimes = whole_tree.nodes_time
p3_nodes0 = wids[(wops == 3) & (wimes >=0) & (wimes <= 9)] # keep all generations of the admixed population

# simplify
ts = whole_tree.simplify(
  p3_nodes0,
  #filter_populations = False,
  #filter_nodes = False,
  keep_unary = True,  # this is critical.  Otherwise you only get segments that have coalesced
  keep_input_roots = True
  )'
)


# get the nodes and inds tibbles
ni <- ts_nodes_and_inds(py$ts)



# set some founders of the SLiM sim for finding pop-origins of segments
founder_nodes <- ni$nodes_tib %>%
  filter(node_time == 9) %>%
  pull(node_id)
founder_node_pops <- ni$nodes_tib %>%
  filter(node_time == 9) %>%
  pull(node_pop)


# when we link ancestors, we have to be careful!!
# If we include in the sample, any individuals that other individuals in the
# sample descended from, then these "sampled ancestors" are what gets returned
# and it is totally uninformative!!

# So, we actually need to link_ancestors separately for each cohort of the
# samples from p3 with the ancestors being the same ancestors (p1 and p2 individuals
# in year 9) every time.

# So, in R we will make a list of vectors of the nodes that are our
# focal nodes each time:
focal_nodes_list <- lapply(0:7, function(x) {
  ni$nodes_tib %>%
    filter(node_time == x & node_pop == 2) %>%
    pull(node_id)
})



# now, get the ancestral origin of all the segments:
segments_tib <- ancestral_segs(
  ts = py$ts,
  focal_nodes_list = focal_nodes_list,
  founder_nodes = founder_nodes,
  nodes_tib = ni$nodes_tib,
  indiv_tib = ni$indiv_tib
)



# make a figure:
st_withx <- segments_tib %>%
  group_by(node_time) %>%
  mutate(
    node_x = as.integer(factor(node_id)),
    anc_popc = as.character(anc_pop)
  )

ggplot(st_withx) +
  geom_rect(aes(xmin=node_x - 1, xmax = node_x, ymin = left, ymax = right, fill = anc_popc), colour = NA) +
  facet_wrap(~ node_time, ncol = 1, scales = "free_x")



# now get the individual-level ancestry tracts:
ind_segs2 <- indiv_ancestry_tracts(segments_tib)


# here I want to plot these dudes
ind_segs3 <- ind_segs2 %>%
  mutate(
    time_f = factor(ind_time, levels = 7:0)
  ) %>%
  group_by(ind_id) %>%
  mutate(admixture_1_score = sum( (right - left) * dose)) %>%
  ungroup() %>%
  arrange(time_f, admixture_1_score, ind_id) %>%
  group_by(time_f) %>%
  mutate(
    ind_int = as.integer(factor(ind_id, levels = unique(ind_id))),
    dose_c = as.character(dose)
  )


# this is Omyk_v1.0.  We
fai <- read_tsv(
  "inputs/omyV6Chr.fasta.fai",
  col_names = c("chrom", "len", "cumul", "X1", "X2")
) %>%
  mutate(
    ends = cumsum(len)
  )

ggplot(ind_segs3) +
  geom_rect(aes(xmin = ind_int - 1, xmax = ind_int, ymin = left, ymax = right, fill = dose_c)) +
  facet_wrap(~ time_f, ncol = 1) +
  scale_fill_manual(values = c(`0` = "red", `1` = "orange", `2` = "blue")) +
  geom_hline(yintercept = fai$ends, linewidth = 0.05)

ggsave("indiv-ancestry-doses.pdf", width = 12, height = 30)

# and now we can segment that stuff into chromosomes, too
ind_segs2 %>%
  mutate(
    chrom = 1
  ) %>%
  select(chrom, left, right, ind, dose) %>%
  write_tsv("D.bed", col_names = FALSE)

# and make a
