

library(reticulate)
library(tidyverse)
library(MixedUpSlimSims)
library(CKMRpop)  # this is necessary to use the MixedUpSlimSims::pairwise_relationship()
                  # until I fix how data get loaded in CKMRpop.

# We have to do this now, since it won't get loaded properly
# with getOption for some reason. The user should set the value
# of MUP_CONDA in the .Rprofile in this directory with a line like:
#
# options(MUP_CONDA = "/Users/eriq/mambaforge-arm64/envs/mup")
# 
# Or you could just put the appropriate path as the argument to
# condaenv.
reticulate::use_condaenv(condaenv = getOption("MUP_CONDA"))






mykiss_chroms %>% head()


set.seed(1)
frqs <- tibble(
  POS = sort(runif(2000, min = 1, max = max(mykiss_chroms$cumul_end)))
) %>%
  mutate(af1 = 0.1, af2 = 0.9)

# have a look at that
frqs %>% head()


simulate_founder_genos(frqs, N = c(1000, 1000), pop_names = c("rbt", "wct")) %>%
  write_input_vcf(out_path = "slim_input.vcf") 


file <- system.file("SLiM-models/2-pop-10-gens-vcf-initialize.slim", package = "MixedUpSlimSims")
system(paste("slim -s 333 ", file, collapse = " "))


dir(pattern = "p[0-9]+-[0-9]+\\.vcf")


# read the trees. Dump just captures the python code that was
# evaluated to get things in the py variable in the global env.
dump <- read_and_filter_trees(
  trees_path = "SLiM.trees",
  years_list = list(`1` = 0:10, `2` = 0:10)
)



ni <- ts_nodes_and_inds(py$ts)


founders <- founder_node_ids_and_pops(ni, 10)


focal_tibble <- tibble(
  pop = rep(c(1,2), each = 10),
  time = rep(0:9, 2)
)

focal_tibble


fnl <- get_focal_nodes(
  ni, 
  Focal = focal_tibble
)


segments_tib <- ancestral_segs(
  ts = py$ts,
  focal_nodes_list = fnl,
  founder_nodes = founders$nodes,
  nodes_tib = ni$nodes_tib,
  indiv_tib = ni$indiv_tib
)




plot_ancestry_of_genomes(segments_tib)



indiv_segs <- indiv_ancestry_tracts(segments_tib)




plot_ancestry_tracts(indiv_segs, chrom_ends = mykiss_chroms)


Q_values <- admixture_fracts_from_ancestry_tracts(indiv_segs)




Q_values %>%
  mutate(time_f = factor(ind_time, levels = rev(sort(unique(ind_time))))) %>%
  ggplot() +
  geom_histogram(aes(x = admix_fract, fill = factor(anc_pop)), alpha = 0.8, bins = 50) +
  facet_grid(time_f ~ ind_pop, scales = "free_y") +
  theme_bw()


p1_10_genos <- slim_vcf2tib("p1-10.vcf")
head(p1_10_genos, n = 10)


p1_10_wide <- slim_vcf2tib("p1-10.vcf", long = FALSE)

# look at a piece of that
head(p1_10_wide[, 1:9], n = 10)


sample_genos_long <- slim_vcfs2tib(Flist = list(p1 = 9:11, p2 = 9:11))


efract1 <- Q_values %>%
  filter(anc_pop == 2) %>%
  select(ped_id, admix_fract) %>%
  mutate(exp_fract1 = 0.1 * (1 - admix_fract) + 0.9 * admix_fract)


allele_1_fracts <- sample_genos_long %>%
  group_by(pop, slim_time, ped_id) %>%
  summarise(obs_fract1 = mean(allele == 1)) %>%
  ungroup() %>%
  left_join(efract1, by = join_by(ped_id)) %>%
  select(everything(), obs_fract1)



ggplot(allele_1_fracts, aes(x = exp_fract1, y = obs_fract1)) +
  geom_point(colour = "blue", alpha = 0.2) + 
  facet_grid(as.integer(slim_time) ~ pop) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed")


pairwise_relats <- pairwise_relationships(
  IT = ni$indiv_tib,
  sample_years = 0:2,
  num_gen = 2
)


pairwise_relats %>%
  count(dom_relat, max_hit)

