print(paste("Carrying out frequency tables for segment", segment))


al_av <- aligned_stringset[names(aligned_stringset) %in% alignment_names$value[alignment_names$class == "avian"]]
al_h <- aligned_stringset[names(aligned_stringset) %in% alignment_names$value[alignment_names$class == "human"]]
al_m <- aligned_stringset[names(aligned_stringset) %in% alignment_names$value[alignment_names$class == "mammal"]]

al_comb <- c(al_m,al_h)

al_av_mat <-as.matrix(al_av)
al_comb_mat <-as.matrix(al_comb)


# Function to calculate letter frequencies in a column
calculate_letter_frequencies <- function(column) {
  table(column)
}

# Apply the function to each column of the data frame
frequencies_per_column_av <- apply(al_av_mat, 2, calculate_letter_frequencies)
frequencies_per_column_comb<- apply(al_comb_mat, 2, calculate_letter_frequencies)

# Creating DataFrames for each element (avian)
dataframes_list_av <- lapply(seq_along(frequencies_per_column_av), function(i) {
  data.frame(Column = frequencies_per_column_av[[i]],
             pos = i)
})

# Creating DataFrames for each element (mammal)
dataframes_list_comb <- lapply(seq_along(frequencies_per_column_comb), function(i) {
  data.frame(Column = frequencies_per_column_comb[[i]],
             pos = i)
})


long_av <- bind_rows(dataframes_list_av) %>% 
  rename(letter= "Column.column",
         freq_a = "Column.Freq") %>% 
         select(pos, letter, freq_a) %>% 
  group_by(pos) %>% 
  mutate(percent_a = round(freq_a / sum(freq_a) * 100,2))

long_comb <- bind_rows(dataframes_list_comb) %>% 
  rename(letter= "Column.column",
         freq_m = "Column.Freq") %>% 
  select(pos, letter, freq_m) %>% 
  group_by(pos) %>% 
  mutate(percent_m = round(freq_m/ sum(freq_m) * 100,2))




long_combined <- long_av %>% full_join(long_comb)
long_combined[is.na(long_combined)] = 0

long_combined <- long_combined %>%  
  rename(residue = pos) %>%  # Renaming the 'pos' column to 'residue'
  mutate(p_x_times = ((percent_m - percent_a) / percent_a) * 100,
         only_mammal_flag = case_when(p_x_times==Inf ~1,
                              TRUE~0)) %>%  # Calculating the proportion difference
  mutate(p_x_times = case_when(
    p_x_times == Inf & percent_m >= 1 ~ percent_m * 100,  # Adjusting proportion difference if percent_m >= 1
    p_x_times == Inf & percent_m < 1 ~ percent_m,          # Keeping proportion difference if percent_m < 1
    TRUE ~ p_x_times  # Keeping proportion difference unchanged otherwise
  ))

#######

# of_interest <- long_combined %>% 
#   filter(percent_m>percent_a)

pulled_out <- table_site_compressed_freq %>% 
  mutate(og =1,
         letter = substr(mutation, nchar(mutation), nchar(mutation))) %>% 
  left_join(long_combined, by=c("residue", "letter"))%>% 
  arrange(-p_x_times) %>%
  ungroup() %>% 
  mutate(rank_px = dense_rank(-p_x_times)) %>% 
  arrange(-unique_avian_mutation_events) %>%
  mutate(rank_unique_avian = dense_rank(-unique_avian_mutation_events)) %>% 
  ungroup() %>% 
  arrange(-spp_number) %>%
  mutate(species_rank= dense_rank(-spp_number)) %>% 
  mutate(mean_rank= (rank_unique_avian+rank_px+species_rank)/3) %>% 
  arrange(mean_rank) 

# pulled_out <- of_interest %>% left_join(table_site_compressed_freq, by="residue") %>% 
#   mutate_at(vars(mutation, letter), as.character) %>% 
#   mutate(flag = if_else(str_ends(mutation, letter), 1, 0)) %>% 
#   filter(flag==1) %>% 
#   arrange(-p_x_times)
# 

pulled_out_final <- pulled_out %>% 
  select(residue, letter, percent_a, percent_m, p_x_times,only_mammal_flag, mutation, n, unique_avian_mutation_events, avg_dist, spp_number, spp_common, species_list, rank_px, rank_unique_avian, species_rank, mean_rank, subtype_list, subtype_number) %>% 
  arrange(-n) 



#top 100 by Px rank
top_100_px_rank <- pulled_out_final %>% 
  arrange(rank_px) %>% 
  head(100)
# Top 100 by unique avian rank
top_100_unique_event_rank <- pulled_out_final %>% 
  arrange(rank_unique_avian) %>% 
  head(100)
# Top 100 by spp count rank
top_100_spcecies_rank <- pulled_out_final %>% 
    arrange(species_rank) %>% 
  head(100) 

# Top 100 by combined rank
top_100_combined_rank <- pulled_out_final %>% 
  arrange(mean_rank) %>% 
  head(100) 



assign(paste0(segment,"_","long_comb"), long_comb) 
#assign(paste0(segment,"_","of_interest"), of_interest) 
assign(paste0(segment,"_","pulled_out_final"), pulled_out_final) 

assign(paste0(segment,"_","top_100_px_rank"), top_100_px_rank) 
assign(paste0(segment,"_","top_100_unique_event_rank"), top_100_unique_event_rank) 
assign(paste0(segment,"_","top_100_combined_rank"), top_100_combined_rank) 
assign(paste0(segment,"_","top_100_spcecies_rank"), top_100_spcecies_rank) 



