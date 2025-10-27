
######
#Mismatch

print(paste("Importing alignement for segment", segment))


aligned<- readAAMultipleAlignment(alignment_file_path, format = "fasta")

aligned_stringset <- AAStringSet(aligned)



#Aligned string set - useful sequences
preprocess_data <- function(data) {
  data <- gsub("/", "_", data)
  data<- gsub("\\(", "_", data)
  data <- gsub("\\)", "_", data)
  data <- gsub("'", "_", data)
  data <- gsub(",", "_", data)
  data <- gsub("\\.", "_", data)
  data <- gsub(":", "_", data)
  data <- gsub("\\+", "_", data)
  data <- gsub("\\|", "_", data)
  data <- gsub("\\;", "_", data)
  data <- gsub("\\?", "_", data)
  data <- gsub("<", "_", data)
  data <- gsub(">", "_", data)
  data <- gsub("&", "_", data)
  data <- gsub("`", "_", data)
  data <- gsub("\\-", "_", data)
  data <- gsub("\\-", "_", data)
  data <- gsub("=", "_", data)
  data <- gsub("#", "_", data)
  data <- gsub(" 759 bp", "", data)
  data <- gsub(" 757 bp", "", data)
  data <- gsub(" 498 bp", "", data)
  data <- gsub(" 716 bp", "", data)
  return(data)
  
}

aligned_stringset@ranges@NAMES <- preprocess_data(aligned_stringset@ranges@NAMES)

alignment_names<-aligned_stringset@ranges@NAMES %>% as_tibble() %>% mutate(al = 1)

alignment_names <- alignment_names %>%
  mutate(ISL = str_extract(value, "(?<=ISL_)[^_]+"))%>%  
  left_join(lookup, by="ISL") %>% 
  select(-label)

#Check that the names in the results are in the alignment

al_merge_check <- results_df_final %>% 
  pivot_longer(cols = c(avian_relative, mammal_id)) %>% 
  mutate(ISL = str_extract(value, "(?<=ISL_)[^_]+")) %>% 
  select(value, ISL) %>% 
  mutate(result = 1) %>% 
  left_join(alignment_names, by="value")

if (any(is.na(al_merge_check))) {
  print("Error: The tip names of the alignment do not fully align with the names from the results table.")
}

print(paste("Carrying out mis-match for paired sequences in segment", segment))


mm_results_list <- list()


for (i in 1:nrow(results_df_final)) {
  mammal_vectors <- results_df_final$mammal_id[i]
  avian_vectors <- results_df_final$avian_relative[i]
  spp_mammal<- results_df_final$spp_mammal[i]
  spp_avian<- results_df_final$spp_avian[i]

  mm_value<- mismatchTable(pairwiseAlignment(aligned_stringset[mammal_vectors], aligned_stringset[avian_vectors]))
  
  if (nrow(mm_value) > 0) {
    
    mm_results_list[[mammal_vectors]] <- cbind(mm_value, mammal_seq = mammal_vectors, avian_seq = avian_vectors, spp_mammal=spp_mammal, spp_avian=spp_avian)
  }
}



mm_results_list <- Filter(Negate(is.null), mm_results_list)

mm_results_list[1]

#Joiun all results into a dataframe
all <- rbindlist(mm_results_list)

#Orde the columns, rename the variables and select useful ones.
#Remove insertions (for now these look like sequence errors not real)
all_clean <- all %>% 
  select(PatternStart, PatternSubstring, SubjectSubstring, mammal_seq, avian_seq) %>% 
  filter(PatternSubstring!="-",
         SubjectSubstring!="-") %>% 
  arrange(PatternStart) %>% 
  dplyr::rename(avian_residue = SubjectSubstring,
                mammal_residue = PatternSubstring,
                residue = PatternStart) %>% 
  mutate(mutation = paste0(avian_residue, residue, mammal_residue)) %>% 
  left_join(results_df_final, by=c("avian_seq"="avian_relative", "mammal_seq"="mammal_id")) %>% 
  select(residue, mutation, avian_seq, mammal_seq, avian_residue, mammal_residue, distance, spp_mammal, spp_avian, subtype_mammal, subtype_avian)

#Make a count of how many mutations per pair
# Make a count of how many independent avian sequences per mutation. 
# i.e does one mutation happen loads because its comapring 1 avian sequence to many mammals (mammal clade)
table_site <- all_clean %>% 
  filter(!str_detect(mutation, "X")) %>% 
  filter(!str_detect(mutation, "B")) %>% 
  filter(!str_detect(mutation, "J")) %>% 
  filter(!str_detect(mutation, "U")) %>% 
  filter(!str_detect(mutation, "O")) %>% 
  filter(!str_detect(mutation, "Z")) %>% 
  group_by(mammal_seq,avian_seq) %>%
  mutate(pair_no_mutations = n()) %>%
  filter(pair_no_mutations <= 20) %>% #Filter pairs that have more than 50 mutations - likely to be inaprotriate
  group_by(mutation) %>%  
  add_count(name = "mutation_count") %>%
  mutate(avg_dist = round(mean(distance),2)) %>% 
  mutate(median_dist = round(median(distance),2)) %>% 
  mutate(unique_avian_mutation_events = n_distinct(avian_seq)) %>% 
  mutate(spp_number = n_distinct(spp_mammal)) %>% 
  mutate(spp_common = names(which.max(table(spp_mammal)))) %>% 
  mutate(species_list = toString(unique(spp_mammal))) %>%  
  mutate(subtype_list = toString(unique(subtype_mammal))) %>% 
  mutate(subtype_number = n_distinct(subtype_mammal)) %>% 
  ungroup() %>% 
  group_by(residue) %>% 
  mutate(residue_dupe = n_distinct(mutation)) %>% 
  ungroup() 


# Remove pairs that have more than 100 mutations - likely unrealistic
#table_site_clean <- table_site %>%
  #filter(pair_no_mutations <= 50) 

# Pure frequency count
table_site_compressed_freq <-table_site  %>%
  group_by(mutation, residue, unique_avian_mutation_events, avg_dist, spp_common, spp_number, species_list,residue_dupe, subtype_list, subtype_number) %>% 
  tally()%>% 
  arrange(-n)

#Table to show pairs, not very informative more for visuals
table_site_compressed_id <-table_site %>% arrange(mammal_seq, residue)

mutation_plot <- ggplot(table_site, aes(x=residue)) + 
  geom_histogram(binwidth=1)+theme_bw()



assign(paste0(segment,"_","alignment_names"), alignment_names) 
assign(paste0(segment,"_","aligned_stringset"), aligned_stringset) 
assign(paste0(segment,"_","all_clean"), all_clean) 
#assign(paste0(segment,"_","table_site_clean"), table_site_clean) 
assign(paste0(segment,"_","table_site"), table_site) 
assign(paste0(segment,"_","mutation_plot"), mutation_plot) 
assign(paste0(segment,"_","table_site_compressed_freq"), table_site_compressed_freq) 
assign(paste0(segment,"_","table_site_compressed_id"), table_site_compressed_id) 




