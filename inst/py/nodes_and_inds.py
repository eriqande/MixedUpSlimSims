

import tskit
import pyslim
import msprime
import numpy as np
import pandas as pd



xx__node_ids = np.array([n.id for n in r.xx__TTKKSS.nodes() ])  # tskit ids of the nodes
xx__node_pops = r.xx__TTKKSS.nodes_population                # pops that the nodes belong to
xx__node_indivs = r.xx__TTKKSS.nodes_individual     # indivs that the nodes belong to
xx__node_times = r.xx__TTKKSS.nodes_time    # times that the nodes occur at



xx__ind_ids = np.array([i.id for i in r.xx__TTKKSS.individuals() ]) # get the tskit ids of the individuals
xx__ind_pops = r.xx__TTKKSS.individuals_population          # pops that the individuals belong to
xx__ind_times = r.xx__TTKKSS.individual_times          # times that are associated with the individuals

# metadata from the individuals
xx__ind_pedigree_ids = np.array([i.metadata["pedigree_id"] for i in r.xx__TTKKSS.individuals() ])
xx__ind_pedigree_p1 = np.array([i.metadata["pedigree_p1"] for i in r.xx__TTKKSS.individuals() ])
xx__ind_pedigree_p2 = np.array([i.metadata["pedigree_p2"] for i in r.xx__TTKKSS.individuals() ])
xx__ind_sex = np.array([i.metadata["sex"] for i in r.xx__TTKKSS.individuals() ])

