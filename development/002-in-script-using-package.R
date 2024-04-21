


library(reticulate)
library(tidyverse)



# set up conda env
PYSLIM_CONDA <- normalizePath("~/mambaforge-x86_64/envs/pyslim")
use_condaenv(condaenv = PYSLIM_CONDA)


# run slim
system("slim -s 222 SLiM-models/wf3.slim")




# now run some python
py_run_string('

import tskit
import pyslim
import msprime
import numpy as np
import pandas as pd

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
  )


# basic tables about individuals
node_ids = np.array([n.id for n in ts.nodes() ])  # tskit ids of the nodes
node_pops = ts.nodes_population                # pops that the nodes belong to
node_indivs = ts.nodes_individual     # indivs that the nodes belong to
node_times = ts.nodes_time    # times that the nodes occur at



ind_ids = np.array([i.id for i in ts.individuals() ]) # get the tskit ids of the individuals
ind_pops = ts.individuals_population          # pops that the individuals belong to
ind_times = ts.individual_times          # times that are associated with the individuals

# metadata from the individuals
ind_pedigree_ids = np.array([i.metadata["pedigree_id"] for i in ts.individuals() ])
ind_pedigree_p1 = np.array([i.metadata["pedigree_p1"] for i in ts.individuals() ])
ind_pedigree_p2 = np.array([i.metadata["pedigree_p2"] for i in ts.individuals() ])
ind_sex = np.array([i.metadata["sex"] for i in ts.individuals() ])


')


## Now, back to R for making some tibbles

indiv_tib <- tibble(
  ind_id = as.integer(py$ind_ids),
  ind_pop = as.integer(py$ind_pops),
  ind_time = as.double(py$ind_times),
  sex = as.integer(py$ind_sex),
  ped_id = as.integer(py$ind_pedigree_ids),
  ped_p1 = as.integer(py$ind_pedigree_p1),
  ped_p2 = as.integer(py$ind_pedigree_p2)
)

nodes_tib <- tibble(
  node_id = as.integer(py$node_ids),
  node_pop = as.integer(py$node_pops),
  node_ind = as.integer(py$node_indivs),
  node_time = as.integer(py$node_times)
)


# set some founders of the SLiM sim for finding pop-origins of segments
founder_nodes <- nodes_tib %>%
  filter(node_time == 9) %>%
  pull(node_id)
founder_node_pops <- nodes_tib %>%
  filter(node_time == 9) %>%
  pull(node_pop)

  


# when we link ancestors, we have to be careful!! 
# If we include in the sample, any individuals that other individuals in the
# sample descended from, then these "sampled ancestors" are what gets returned
# and it is totally uninformative!!

# So, we actually need to link_ancestors separately for each cohort of the
# samples from p3 with the ancestors being the same ancestors (p1 and p2 individuals
# in year 9) every time.

# So, in R we will make a list of vectors of the individuals that are our
# focal indivs each time:
focal_nodes_list <- lapply(0:7, function(x) {
  nodes_tib %>%
    filter(node_time == x & node_pop == 2) %>%
    pull(node_id)
})



# link_ancestors
py_run_string('

# here is a function that takes a list of samples and founders and
# spits back a pandas data frame of the edge table
def ancestor_links_to_pd(samples, ancestors):
  edges = ts.tables.link_ancestors(samples, ancestors)
  df = pd.DataFrame(
    {
    "child": edges.child,
    "parent": edges.parent,
    "left":edges.left,
    "right": edges.right
    }
    )
  return(df)



fnl = r.focal_nodes_list # get the R list as a python object

links_pd = pd.concat(
  [ancestor_links_to_pd(x, r.founder_nodes) for x in fnl],
  ignore_index = True
)

')



# now get links_pd as a tibble and add the parent node populations on there
links_tib <- as_tibble(py$links_pd) %>%
  arrange(child, left, right) %>%
  left_join(nodes_tib, by = join_by(parent == node_id)) %>%
  rename(
    anc_pop = node_pop,
    anc_ind = node_ind,
    anc_time = node_time
  )



# now, just hold onto the child nodes and the pop of the parent
segs_tib <- links_tib %>%
  rename(
    node_id = child
  )


# collapse adjacent segments from the same pop
st2 <- segs_tib %>%
  group_by(node_id) %>%
  mutate( 
    grp = cumsum(anc_pop != lag(anc_pop, default = 1))
  ) %>%
  group_by(node_id, grp) %>% 
  slice(1, n()) %>% 
  summarise(left = left[1], right = right[2], anc_pop = anc_pop[1]) %>%
  ungroup()


# join the node and indiv information back on there
st3 <- st2 %>%
  left_join(nodes_tib, by = join_by(node_id)) %>%
  left_join(indiv_tib, by = join_by(node_ind == ind_id))


# make a figure:
st_withx <- st3 %>%
  group_by(node_time) %>%
  mutate(
    node_x = as.integer(factor(node_id)),
    anc_popc = as.character(anc_pop)
  ) 

ggplot(st_withx) +
  geom_rect(aes(xmin=node_x - 1, xmax = node_x, ymin = left, ymax = right, fill = anc_popc), colour = NA) +
  facet_wrap(~ node_time, ncol = 1, scales = "free_x")



# OK! That has worked very well.


# Now, what we need to do still is:
#  1. combine these ancestry tracts within indvividuals to get the ancestry
#     copy number intervals for individuals.
#  2. break things up into proper chromosomes


# We will try to deal with #1 using bedtools:
# We can pretend that each individual is a "chromosome" and
# intersect the two nodes within each of those individuals in
# one big swoop.

# first we need to get two different tibbles, one for the first and another for the
# second node within each individual.
segs_in_inds_tmp <- st3 %>%
  select(node_ind, left, right, node_id, anc_pop) %>%
  group_by(node_ind) %>%
  mutate(node_idx_in_ind = as.integer(factor(node_id))) %>%
  ungroup()

first_nodes <- segs_in_inds_tmp %>%
  filter(node_idx_in_ind == 1)
second_nodes <- segs_in_inds_tmp %>%
  filter(node_idx_in_ind == 2)

# write those out (testing with a highly segmented indiv at the moment)
first_nodes %>%
  select(node_ind, left, right, anc_pop) %>%
  write_tsv(file = "A.bed", col_names = FALSE)

second_nodes %>%
  select(node_ind, left, right, anc_pop) %>%
  write_tsv(file = "B.bed", col_names = FALSE)


#### this was for testing bedtools on easy input  ####
if(FALSE)  {
tibble(
  chrom = 1,
  left = seq(0, 100, by = 10)
)  %>%
  mutate(
    right = left + 10,
    anc_pop = rep(0:1, length.out = n())
  ) %>%
  write_tsv("a.bed")


tmp <- tibble(
  chrom = 1,
  left = c(0, seq(5, 100, by = 10))
)  %>%
  mutate(
    right = left + 10,
    anc_pop = rep(2:3, length.out = n())
  ) 

tmp$right[1] = 5
tmp$right[nrow(tmp)] = 110

tmp %>%
  write_tsv("b.bed")
}


#### Back to getting the individual-specific segments

system(" bedtools intersect -a a.bed -b b.bed -wb > C.bed ")

ind_segs <- read_tsv("C.bed", col_names = FALSE) %>%
  rename(
    ind = X1,
    left = X2,
    right = X3,
    anc1 = X4,
    anc2 = X8
  ) %>%
  select(-starts_with("X")) %>%
  mutate(
    dose = anc1 + anc2
  ) %>%
  select(ind, left, right, dose) %>%
  arrange(ind, left, right)

# then collapse adjacent common intervals
ind_segs2 <- ind_segs %>%
  group_by(ind) %>%
  mutate( 
    grp = cumsum(dose != lag(dose, default = 1))
  ) %>%
  group_by(ind, grp) %>% 
  slice(1, n()) %>% 
  summarise(left = left[1], right = right[2], dose = dose[1]) %>%
  ungroup()


# here I want to plot these dudes
ind_segs3 <- ind_segs2 %>%
  left_join(indiv_tib, by = join_by(ind == ind_id)) %>%
  mutate(
    time_f = factor(ind_time, levels = 7:0)
  ) %>%
  group_by(ind) %>%
  mutate(admixture_1_score = sum( (right - left) * dose)) %>%
  ungroup() %>%
  arrange(time_f, admixture_1_score, ind) %>%
  group_by(time_f) %>%
  mutate(
    ind_int = as.integer(factor(ind, levels = unique(ind))),
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
