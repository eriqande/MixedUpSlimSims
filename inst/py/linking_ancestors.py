
# This creates a pandas table of ancestral links
# For this to run properly:

# `xx__TS` the tree sequence to work with.
#  
# `xx__FOCAL_NODES_LIST` must be an unnamed list of vectors of node IDs
#   defined in the R global environment. These are typically nodes
#   from different cohorts.
#
# `xx__FOUNDER_NODES` must be defined in the R global environment.  It is a
#   vector of node ids that are considered to be the founders
#



# here is a function that takes a list of samples and founders and
# spits back a pandas data frame of the edge table
def ancestor_links_to_pd(ts, samples, ancestors):
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




# Down here is where we actually do this stuff
xx__LINKS_PD = pd.concat(
  [ancestor_links_to_pd(r.xx__TS, x, r.xx__FOUNDER_NODES) for x in r.xx__FOCAL_NODES_LIST],
  ignore_index = True
)
