# Created so that Mike Mallonee could compare with the CBP taxonomic list.
# Date Created: 1/8/2018
#------------------------------------------------------------------------------
# Run all of the notebook scripts to obtain a complete bay.df.
source("notebook/raw_script/extract_notebook_scripts.R")
#------------------------------------------------------------------------------
# Keep only the columns related to taxonomic hierarchy and only unique rows.
taxa.df <- bay.df %>% 
  select(latinname, nodccode, speccode, serialnumber,
         tsn, final_tsn, final_id,
         kingdom, subkingdom, infrakingdom,
         superdivision, division, subdivision, 
         superphylum, phylum, subphylum,
         class, subclass, infraclass,
         superorder, order, suborder, infraorder,
         superfamily, family, subfamily,
         genus, species, subspecies) %>% 
  distinct()
#------------------------------------------------------------------------------
# Create a unique file name using date for each day the taxa hierarchy are exported.
file.name <- paste0("taxa_hierarchy_",
                   format(as.Date(Sys.Date()),"%m_%d_%Y"),
                           ".csv")
file.dir <- file.path("data/export", file.name)
#------------------------------------------------------------------------------
# Save the file to the data\export directory.
data.table::fwrite(taxa.df, file.dir)
