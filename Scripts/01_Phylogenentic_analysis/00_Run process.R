source("~/git/mutation_pathway/R/00_setup.R")


segment_list <- c("NP","PB2", "PB1", "PA")


for (segment in segment_list){
  
segment = segment
print(paste("Running segment:", segment))
print(paste(segment, " Running Start:", Sys.time()))

source("~/1_File_names.R")
source("~/2_Metadata_lookup.R")


print(paste(segment, " Import end:", Sys.time()))
source("~/3_subtree_split_approach.R")

source("~/3b_mismatch.R")

print(paste(segment, " Analysis end:", Sys.time()))

source("~/4_Frequency_analysis.R")

#Save all outputs into an excel
}