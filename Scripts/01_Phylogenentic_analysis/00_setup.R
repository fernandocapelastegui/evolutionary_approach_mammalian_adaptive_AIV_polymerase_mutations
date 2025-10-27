tdate <- format(Sys.Date(), format = "%Y%m%d")

if (!require("pacman", quietly = TRUE))
  install.packages("pacman")
library(pacman)

# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install(c("msa", "ggtree", "treeio", "DECIPHER"), force = TRUE)

p_load(tidyverse,
       dplyr,
       seqinr,
       Biostrings,
       ape,
       phangorn,
       phytools,
       geiger,
       ggtree,
       msa,
       treeio,
       bioseq,
       tinytex,
       ggmsa,
       janitor,
       ShortRead,
       genoPlotR,
       data.tree,
       DECIPHER,
       beepr,
       dispRity,
       tidytree,
       microseq,
       phylobase,
       castor,
       readxl,
       openxlsx,
       writexl,
       reactable,
       ggiraph,
       plotly,
       ggrepel,
       RColorBrewer,
       treemapify,
       patchwork)


