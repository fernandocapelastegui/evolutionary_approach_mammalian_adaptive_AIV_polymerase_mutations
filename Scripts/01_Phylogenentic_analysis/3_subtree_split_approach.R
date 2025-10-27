
print(paste("Importing tree for segment:", segment))

tree<- read.tree(tree_file_path, text=NULL)

preprocess_data <- function(data) {
  data <- gsub("/", "_", data)
  data <- gsub("\\(", "_", data)
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
  data <- gsub("=", "_", data)
  data <- gsub("#", "_", data)
}

tree$tip.label <- preprocess_data(tree$tip.label)
tips <- tree$tip.label %>% as_tibble()

#Extract a list of tips, and extract the ISL number as a new column
tips <- tips %>%
  mutate(ISL = str_extract(value, "(?<=ISL_)[^_]+"))

#Compare to the lookup of all tips. Note, a sall portion do have weird names and cant fix that

merge_check <- lookup %>% 
  full_join(tips, by = "ISL")

merge_check_na <- merge_check %>% filter(is.na(class))

if (any(is.na(merge_check$class))) {
  print("Error: The tip names of the tree do not fully align with the sequence names from the raw fasta file.")
  print(paste0(nrow(merge_check_na), " sequences in the tree, not named the same in the alignment"))
}

################################################################################

#MAKE SUB TREES
print(paste("Generating sub-trees for segment:", segment))

subtree_list <- list()

# Iterate over both every 500
for (start in seq(1, length(tree$tip.label), by = 500)) {
  end <- min(start + 499, length(tree$tip.label))
  # Extract subtree with tips
  current_tip_labels <- tree$tip.label[start:end]
  current_subtree <- get_subtree_with_tips(tree, current_tip_labels)$subtree
  
  # Append the subtree to the list
  subtree_list <- c(subtree_list, list(current_subtree))
}

#now some big ones


# Iterate over both every 5000 
for (start in seq(1, length(tree$tip.label), by = 5000)) {
  end <- min(start + 4999, length(tree$tip.label))
  # Extract subtree with tips
  current_tip_labels <- tree$tip.label[start:end]
  current_subtree <- get_subtree_with_tips(tree, current_tip_labels)$subtree
  
  # Append the subtree to the list
  subtree_list <- c(subtree_list, list(current_subtree))
}

# Iterate over both every 10000 
for (start in seq(1, length(tree$tip.label), by = 10000)) {
  end <- min(start + 9999, length(tree$tip.label))
  # Extract subtree with tips
  current_tip_labels <- tree$tip.label[start:end]
  current_subtree <- get_subtree_with_tips(tree, current_tip_labels)$subtree
  
  # Append the subtree to the list
  subtree_list <- c(subtree_list, list(current_subtree))
}

# Iterate over both every 15000 
for (start in seq(1, length(tree$tip.label), by = 15000)) {
  end <- min(start + 14999, length(tree$tip.label))
  # Extract subtree with tips
  current_tip_labels <- tree$tip.label[start:end]
  current_subtree <- get_subtree_with_tips(tree, current_tip_labels)$subtree
  
  # Append the subtree to the list
  subtree_list <- c(subtree_list, list(current_subtree))
}


# Get the first 5000 and last 5000 tip labels
first_tip_labels <- tree$tip.label[1:5000]
last_tip_labels <- tail(tree$tip.label,5000)

# Combine the selected tip labels
selected_tip_labels <- c(first_tip_labels, last_tip_labels)

# Extract subtree with selected tip labels
selected_subtree <- get_subtree_with_tips(tree, selected_tip_labels)$subtree

subtree_list <- c(subtree_list, list(selected_subtree))

#Check all labels are somewhere in the subtree list

check_tip_labels <- function(subtrees_list, tip_labels_df) {
  all_labels <- unique(unlist(subtrees_list))
  not_in_subtree <- setdiff(tip_labels_df$value, all_labels)
  all_in_subtree <- length(not_in_subtree) == 0
  return(list(all_in_subtree = all_in_subtree, not_in_subtree = not_in_subtree))
}

# Usage
result <- check_tip_labels(subtree_list, tips)
print(result$all_in_subtree)
print(result$not_in_subtree)

################################################################################

print(paste("Importing look-up for segment and merging into trees:", segment))


#Add metadate to the sub trees
#Annpyingly it doesnt remain in the subt ress from the big tree when making it

merge_lookup <-tips %>% 
  na.omit() %>% 
  left_join(lookup, by="ISL") %>% 
  select(-label)%>% 
  rename(tip.label = value)

merge_species_data <- function(tree) {
  tree$species_data <- merge_lookup[match(tree$tip.label, merge_lookup$tip.label), ]
  return(tree)
}

# Apply the function to each tree in the list using lapply
subtrees_with_species_data <- lapply(subtree_list, merge_species_data)

#Check that all trees have metadata
check_metadata <- function(subtrees_list, merge_lookup) {
  all_metadata_correct <- TRUE
  incorrect_metadata <- list()
  
  for (subtree in subtrees_list) {
    subtree_metadata <- merge_lookup[match(subtree, merge_lookup$tip.label), ]
    incorrect_in_subtree <- subset(subtree_metadata, is.na("species_data") | !complete.cases("species_data"))
    if (nrow(incorrect_in_subtree) > 0) {
      all_metadata_correct <- FALSE
      incorrect_metadata[[length(incorrect_metadata) + 1]] <- incorrect_in_subtree
    }
  }
  
  return(list(all_metadata_correct = all_metadata_correct, incorrect_metadata = incorrect_metadata))
}

# Usage
result <- check_metadata(subtrees_with_species_data, merge_lookup)
print(result$all_metadata_correct)  # TRUE if each tip in each subtree has correct metadata assigned, otherwise FALSE
print(result$incorrect_metadata)    # List of data frames containing tips with incorrect metadata for each subtree




# Make distance matrices of all subtrees

print(paste("Creating matrices based on subtrees for segment", segment))


#Make a list of matrices based on the subtrees
matrix_list <- list()

for (i in seq_along(subtrees_with_species_data)) {
  # Apply cophenetic function to the subtree and store the result
  matrix_list[[i]] <- get_all_pairwise_distances(subtrees_with_species_data[[i]], as_edge_counts = TRUE)

}



#Matrix algorithm

print(paste("Applying algorithm to matrices for segment", segment))


# Function to find the closest related tip of a different category
find_closest_related_tip <- function(tree, tip_label, category) {
  tip_index <- which(tree$tip.label == tip_label)
  category_tips <- which(tree$species_data$mammal_def == category)
  distances <- dist_matrix[tip_index, category_tips]
  closest_tip_index <- category_tips[which.min(distances)]
  closest_tip_label <- tree$tip.label[closest_tip_index]
  result <- list(tip_label = closest_tip_label, distance = distances[which.min(distances)])
  return(result)
}


# #for names in mammal look up do the above
#mammal_names_list <- merge_lookup %>% filter(mammal_def==1) %>% select(tip.label)


#Make a blank dataframe to store results
results_df <- data.frame(mammal_id = "", avian_relative ="", distance ="")

for (i in seq_along(subtrees_with_species_data)) {
  dist_matrix<-matrix_list[[i]]
  subtree <- subtrees_with_species_data[[i]]
  mammal_names_list <- subtree$species_data$tip.label[subtree$species_data$mammal_def == 1]
  
  for (mammal_id in mammal_names_list) {# for each mammal in the list of mammal_names
    
    mammal_tip <- mammal_id  #Assign mammal_tip to mammal_id
    
    closest_avian_tip <- find_closest_related_tip(subtree, mammal_tip, 0) #Perform function above
    if (length(closest_avian_tip$tip_label) > 0) { #Some closest avian tips eill not be found - not sure why. This just dtops the loop breaking
      result <- data.frame(mammal_id = mammal_tip, avian_relative = closest_avian_tip$tip_label, distance=closest_avian_tip$distance)
      results_df <- rbind(results_df, result)
    } #bind results into a dataframe
  }
}


#clean empty 
results_df<- results_df %>% 
  mutate(distance = as.numeric(distance))%>%
  na.omit()

#Which mammal names in the full list are not in the results


results_check <- results_df %>% 
  select(mammal_id, distance) %>% 
  distinct(mammal_id, .keep_all = TRUE) %>% 
  mutate(ISL = str_extract(mammal_id, "(?<=ISL_)[^_]+"))

mammal_check <- merge_lookup %>% 
  filter(class=="mammal")

results_check_merge <- mammal_check %>% 
  left_join(results_check, by="ISL") 
  
results_check_na <- results_check_merge %>% filter(is.na(mammal_id))

if (any(is.na(results_check))) {
  print("Error: not all mammals have an avian tip relative; probably havent sampled enough tips")
}



#Get rid of straight duplicates i.e from taking multipe trees there will be duplicates of the same relationship
results_df_clean<-results_df %>% 
  mutate(mammal_isl= str_extract(mammal_id, "(?<=ISL_)[^_]+"),
         avian_isl = str_extract(avian_relative, "(?<=ISL_)[^_]+")) %>% 
  distinct(mammal_id, avian_relative,.keep_all = TRUE) %>% 
  group_by(mammal_id) %>%
  filter(distance == min(distance)) %>%
  ungroup()

duplicated_rows <- results_df_clean[duplicated(results_df_clean$mammal_id), ]


# Check ditribution
#distance_histogram <-hist(results_df_clean$distance, breaks = 200)
dist_quant <- quantile(results_df_clean$distance)

distance_histogram <-ggplot(results_df_clean, aes(x=distance)) + 
  geom_histogram(binwidth = 2)+
  geom_vline(aes(xintercept=10),
             color="red", linetype="dashed", size=1)+
  theme_bw()



#take the mammal avian pair with the closest distance
# merge in species
results_df_final<- results_df_clean %>% 
  filter(distance<10) %>% 
  left_join(spp_final, by = c("mammal_isl"="ISL")) %>% 
  select(-label,-species_mammal, -species_human, -species_avian) %>% 
  rename(spp_mammal = spp_final) %>% 
  left_join(spp_final, by = c("avian_isl"="ISL")) %>% 
  select(-label,-species_mammal, -species_human, -species_avian) %>% 
  rename(spp_avian = spp_final,
         subtype_avian = subtype.y,
         subtype_mammal = subtype.x) 
  
#save out useful segment specific datasets

assign(paste0(segment,"_","subtrees_with_species_data"), subtrees_with_species_data) 
assign(paste0(segment,"_","results_df_final"), results_df_final) 
assign(paste0(segment,"_","results_df_clean"), results_df_clean) 
assign(paste0(segment,"_","results_df"), results_df) 
assign(paste0(segment,"_","tree"), tree) 
#assign(paste0(segment,"_","tree_plot"), tree_plot) 
assign(paste0(segment,"_","distance_histogram"), distance_histogram) 

rm(matrix_list)
rm(subtree_list)
rm(subtrees_with_species_data)
gc()

