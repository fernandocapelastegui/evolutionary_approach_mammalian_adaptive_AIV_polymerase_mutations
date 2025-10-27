#Set folder date

folder_date <- '20250813'
segments <- c("PB1", "PB2", "PA", "NP")

# Create an empty list to store the data
data_list <- list()

for (segment in segments) {
  file_path <- paste0(
    "/Data/Output/Output_tables/",
    segment, "/",folder_date,"/",segment,"_table_site_compressed_id", folder_date, ".xlsx"
  )
  
  data_list[[segment]] <- read.xlsx(file_path)
}


# Keep only avian_seq and mammal_seq for each data frame in the list
data_list <- lapply(data_list, function(df) {
  unique(df[, c("avian_seq", "mammal_seq")])
})

combined_data <- bind_rows(
  lapply(names(data_list), function(seg) {
    df <- data_list[[seg]][, c("avian_seq", "mammal_seq")]
    df$segment <- seg
    unique(df)
  })
)

tallys <- combined_data %>% 
  group_by(segment) %>% 
  tally()

tallys <- bind_rows(
  tallys,
  summarise(tallys, segment = "Total", n = sum(n))
)

uniqe_emergeces <- combined_data %>% 
  group_by(segment, avian_seq) %>% 
  tally()


#All pairings:

data_list_all_pairs <- list()

for (segment in segments) {
  file_path <- paste0(
    "/Data/Output/Output_tables/",
    segment, "/",folder_date,"/",segment,"_results_df_clean", folder_date, ".xlsx"
  )
  
  data_list_all_pairs[[segment]] <- read.xlsx(file_path)
}

data_list_all_pairs <- lapply(data_list_all_pairs, function(df) {
  unique(df[, c("mammal_id", "avian_relative")])
})

combined_data_all_pairs <- bind_rows(
  lapply(names(data_list_all_pairs), function(seg) {
    df <- data_list_all_pairs[[seg]][, c("mammal_id", "avian_relative")]
    df$segment <- seg
    unique(df)
  })
)

tallys_all_pairs <- combined_data_all_pairs %>% 
  group_by(segment) %>% 
  tally()

tallys_all_pairs <- bind_rows(
  tallys_all_pairs,
  summarise(tallys_all_pairs, segment = "Total", n = sum(n))
)