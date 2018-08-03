# Created so that Mike Mallonee could compare with the CBP taxonomic list.
# Date Created: 1/8/2018
#------------------------------------------------------------------------------
# Run all of the notebook scripts to obtain a complete bay.df.
source("notebook/raw_script/extract_notebook_scripts.R")
#------------------------------------------------------------------------------
# Keep only the columns related to taxonomic size and only unique rows.
size.df <- bay.df %>% 
  select(source,
         latinname, tsn, nodccode, speccode, serialnumber,
         reported_size, new_size, old_carbon) %>% 
  distinct()
#------------------------------------------------------------------------------
# Create a unique file name using date for each day the taxa sizes are exported.
file.name <- paste0("taxa_sizes_",
                   format(as.Date(Sys.Date()),"%m_%d_%Y"),
                   ".csv")
file.dir <- file.path("data/export", file.name)
#------------------------------------------------------------------------------
# Save the file to the data\export directory.
data.table::fwrite(size.df, file.dir)

