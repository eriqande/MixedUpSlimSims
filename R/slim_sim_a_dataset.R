#' Simulate a complete data set to run in MixedUpParents
#'
#' There is a lot that goes into this, and note that you have to have
#' bedtools and bcftools installed on your system, etc.  Also, it is currently
#' up to the user to make sure that the parameters used here are correctly
#' set for the SLiM file used.
#'
#' Note also that as currently configured this infers the founder generation'
#' (in tskit time) by the time at which the smallest ped_id existed.
#' @param AF a tibble of allele frequencies with columns that must have
#' `chrom` and then `pos`, and then as many columns as you wish, each
#' one holding the frequency of the 1 allele in a population.  The names
#' of the columns give the names of the species/subspecies.
#' @param genome_info a tibble of cumulative base pairs of different chromosomes
#' in the genome giving the positions in AF.  This is necessary for easily translating
#' the positions into continuous coordinates for SLiM.  Must have the columns
#' `chrom`, `start`, `end`, `cumul_start`, and `cumul_end`.  See `mykiss_chroms`
#' for an example.
#' @param founder_pop_nums a vector of the number of diploids in each founder
#' population.  This should be commensurate with the beginning population
#' size in the SLiM simulation file.
#' @param slim_file relative or absolute path to the .slim file to be used
#' for the SLiM simulation. This must have been written so that it creates:
#' - VCF output in the format "p" + subpop.id + "-" + sim.cycle + ".vcf"
#' - tree sequence recorded output named "SLiM.trees"
#' @param slim_seed the single integer seed to use for SLiM.  Gets set automatically
#' from the R RNG if not set.
#' @param years_list A named list. Each name is an integer signifying a SLiM
#' population, and the value is an integer vector of tree-seq years for which
#' individuals/nodes in the tree seq from the named population should be
#' retained in the tree-sequence. See the documentation for `read_and_filter_trees()`
#' for more information.  This must be concordant with the SLiM file you have used.
#' @param focal_nodes a tibble specifying the populations (base-1 indexed) and cohorts
#' (base-0 tskit time---the most recent is 0) for which
#' we want to determine ancestry vectors.  If this is left NULL then this will be
#' done for all populations in all non-founder generations.
#' @param marker_pop_time_list a list of pops and years from which to extract
#' the genotypes from the VCF files.  This is a named list with names being the
#' names of the population as "p1", "p2", etc. and the years as the SLiM years
#' (they get higher later in time) from which the samples are desired.  For
#' example: `list(p1 = 9:11, p2 = 9:11)`.
#' @param num_gen The number of generations back to trace the relationships between
#' the individuals.  By default this is 2, which will get you parent-offspring, full-
#' and half siblings, grandparent/grandchild, aunts and uncles and first cousins.
#' @param diagnostic_markers a tibble with columns `diag_spp`, `chrom`, and `pos`.
#' The values in `diag_spp` must include the species codes used as column
#' headers in `AF`.
#' @export
#' @examples
#'
#' AF <- cyclone_creek_var_freqs
#' genome_info <- mykiss_chroms
#' founder_pop_nums <- c(1000, 1000)
#' slim_file <- system.file(
#'   "SLiM-models/2-pop-10-gens-vcf-initialize.slim",
#'   package = "MixedUpSlimSims"
#' )
#' slim_seed <- 55
#' years_list <- list(`1` = 0:10, `2` = 0:10)
#' marker_pop_time_list <- list(p1 = 9:11, p2 = 9:11)
#' diagnostic_markers <- wct_rbt_yct_diagnostic_markers
#'
#' \dontrun{
#' # this takes more time than CRAN likes, so don't run it on Check
#' simmed <- slim_sim_a_dataset(
#'   AF = AF,
#'   genome_info = genome_info,
#'   founder_pop_nums = founder_pop_nums,
#'   slim_file = slim_file,
#'   slim_seed = slim_seed,
#'   years_list = years_list,
#'   marker_pop_time_list = marker_pop_time_list,
#'   diagnostic_markers = diagnostic_markers
#' )
#'
#' }
#'
slim_sim_a_dataset <- function(
  AF,
  genome_info,
  founder_pop_nums,
  slim_file,
  years_list,
  marker_pop_time_list,
  diagnostic_markers,
  slim_seed = sample(1:1e6, 1),
  focal_nodes = NULL,
  num_gen = 2
) {


  # get species name
  spp_names <- AF %>%
    select(-chrom, -pos) %>%
    names()

  # get alle frequencies tibble with necessary names and cumulative positions
  AF2 <- AF
  names(AF2)[!(names(AF2) %in% c("chrom", "pos"))] <- paste0("af", 1:length(spp_names))

  frqs_pos_and_cPOS <- AF2 %>%
    cumulative_genome_position(genome_info)

  frqs <- frqs_pos_and_cPOS %>%
    rename(POS = cPOS) %>%
    select(POS, starts_with("af"))

  # make a temp directory for doing the simulation
  TMPDIR <- tempfile()
  dir.create(TMPDIR, recursive = TRUE, showWarnings = FALSE)
  message("Created temp directory ", TMPDIR, " for doing SLiM simulations")

  # make a VCF file for initializing the genotypes of the founder generation
  simulate_founder_genos(frqs, N = founder_pop_nums, pop_names = spp_names) %>%
    write_input_vcf(out_path = file.path(TMPDIR, "slim_input.vcf"))

  # run SLiM in the temp directory
  abs_slim_file <- normalizePath(slim_file)
  CALL <- paste("cd", TMPDIR, "; slim -s ", slim_seed, abs_slim_file, " > stout_from_slim.txt", collapse = " ")
  system(CALL)

  # collect the trees from the simulation
  dump <- read_and_filter_trees(
    trees_path = file.path(TMPDIR, "SLiM.trees"),
    years_list = years_list
  )

  # Now that we have read and simplified that tree into the `py$ts` object,
  # we can turn it into tibbles of nodes and indivs easily:
  require(reticulate)  # seems I need this
  ni <- ts_nodes_and_inds(py$ts)


  # get the founder time (in tskit time) as the time when the minimum ped_id occurred and
  # then use it to get the ids and pops of the founders
  founder_time <- ni$indiv_tib %>% filter(ped_id == min(ped_id)) %>% pull(ind_time)
  founders <- founder_node_ids_and_pops(ni, founder_time)

  # if needed, determine the focal nodes as all pops and all non-founder cohorts
  if(TRUE) {#is.null(focal_nodes)) {
    pop_idxs <- unique(ni$indiv_tib$ind_pop)
    focal_nodes <- expand_grid(
      pop = pop_idxs,
      time = 0:(founder_time - 1L)
    )
  }

  # make a focal nodes list and then get the ancestral segments for
  # those individuals
  fnl <- get_focal_nodes(
    ni,
    Focal = focal_nodes
  )
  segments_tib <- ancestral_segs(
    ts = py$ts,
    focal_nodes_list = fnl,
    founder_nodes = founders$nodes,
    nodes_tib = ni$nodes_tib,
    indiv_tib = ni$indiv_tib
  )

  # get the ancestry tracts within the focal individuals
  indiv_segs <- indiv_ancestry_tracts(segments_tib)

  # calculate the ancestry fractions of each focal indiv fromt he ancestral segs
  Q_values <- admixture_fracts_from_ancestry_tracts(indiv_segs)

  # get the variable marker genotypes from the VCFs
  sample_genos_long <- slim_vcfs2tib(
    Flist = marker_pop_time_list,
    Dir = TMPDIR
  )

  # Use the pedigree to get the relationships between the sampled
  # indivduals.
  require(CKMRpop)  # this is a hack for now because with the current version of
                    # CKMRpop it fails by not finding the CKMRpop object, relationship_zone_names
  pairwise_relats <- pairwise_relationships(
    IT = ni$indiv_tib,
    sample_years = 0:2,
    num_gen = 2
  )


  # simulate the diagnostic markers.
  # first step is to make sure that all the species in spp_names
  # appear in the diagnostic markers tibble
  diags <- unique(diagnostic_markers$diag_spp)
  spp_missing <- setdiff(spp_names, diags)
  if(length(spp_missing) > 0) {
    stop("AF file has species ", paste(spp_missing, collapse = ", "), " which do not appear in the diagnostic markers.")
  }
  extra_diags <- setdiff(diags, spp_names)
  if(length(extra_diags) > 0) {
    warning("AF file has additional species ", paste(extra_diags, collapse = ", "), " not found in AF. This could be by design...If so, then disregard this warning.")
  }
  # now, regardless, we need to get all the diagnostic species names, and
  # we must make sure that the ones shared with the spp_names are in the
  # correct order (and first) with the remaining ones in whatever order
  # is needed (after the first ones...).
  diag_species_names <- c(spp_names, extra_diags)

  # get the diagnostic markers only for the individuals that we have sampled
  # variable-marker genotypes for, replace the species names instead of the
  # indexes in there, and reconfigure some column names, etc.
  simulated_diagnostic_markers <- diagnostic_markers %>%
    mutate(diag_spp_idx = as.integer(factor(diag_spp, levels = diag_species_names))) %>%
    cumulative_genome_position(genome_info) %>%
    place_diagnostic_markers(
      IndivSegs = indiv_segs %>%
        filter(ped_id %in% unique(sample_genos_long$ped_id))
    ) %>%
    mutate(diag_spp = diag_species_names[as.integer(diag_spp_idx)], .before = diag_spp_idx) %>%
    select(-diag_spp_idx) %>%
    rename(
      n = geno,
      indiv = ped_id
    ) %>%
    mutate(indiv = as.character(indiv), pos = as.integer(pos), n = as.integer(n))


  # format the variable markers
  frq_pos_stuff <- frqs_pos_and_cPOS %>%
    select(pos, chrom, cPOS)

  simulated_variable_markers <- sample_genos_long %>%
    select(pos, ped_id, haplo, allele)  %>%
    rename(cPOS = pos) %>%
    left_join(frq_pos_stuff, by = join_by(cPOS)) %>%
    select(chrom, pos, ped_id, allele) %>%
    group_by(chrom, pos, ped_id) %>%
    summarise(n = sum(as.integer(allele))) %>%
    ungroup() %>%
    arrange(ped_id, chrom, pos) %>%
    rename(indiv = ped_id) %>%
    mutate(indiv = as.character(indiv), pos = as.integer(pos))

  # return everything in a list:
  sim_results <- list(
    spp_diag_snps = simulated_diagnostic_markers,
    variable_snps = simulated_variable_markers,
    genome_info = genome_info,
    trueQ_and_pedigree = Q_values,
    true_segs = indiv_segs,
    true_var_freqs = AF,
    spp_names = spp_names,
    pairwise_relats = pairwise_relats
  )
}
