pb2_mutations <- read_excel(paste0("/Paper_final/mutation_loookup",tdate,".xlsx"), sheet = "PB2") %>% select(residue, n) %>% group_by(residue) %>% 
  summarise_at(vars(n), list(sum = sum)) %>% arrange(-sum) %>% top_n(50) %>% 
  mutate(normalised = (sum - min(sum)) / (max(sum) - min(sum)),
         segment = "PB2")

pb1_mutations <- read_excel(paste0("/Paper_final/mutation_loookup",tdate,".xlsx"), sheet = "PB1")%>% select(residue, n)%>% group_by(residue) %>% 
  summarise_at(vars(n), list(sum = sum)) %>% arrange(-sum)%>% top_n(50)%>% 
  mutate(normalised = (sum - min(sum)) / (max(sum) - min(sum)),
         segment = "PB1")


pa_mutations <- read_excel(paste0("/Paper_final/mutation_loookup",tdate,".xlsx"), sheet = "PA")%>% select(residue, n)%>% group_by(residue) %>% 
  summarise_at(vars(n), list(sum = sum)) %>% arrange(-sum)%>% top_n(50)%>% 
  mutate(normalised = (sum - min(sum)) / (max(sum) - min(sum)),
         segment = "PA")


np_mutations <- read_excel(paste0("/Paper_final/mutation_loookup",tdate,".xlsx"), sheet = "NP")%>% select(residue, n)%>% group_by(residue) %>% 
  summarise_at(vars(n), list(sum = sum)) %>% arrange(-sum)%>% top_n(50)%>% 
  mutate(normalised = (sum - min(sum)) / (max(sum) - min(sum)),
         segment = "NP")

n_colors <- 25

color_palette_pb2 <- colorRampPalette(c("blue", "white"))
color_palette_pb1 <- colorRampPalette(c("green", "white"))
color_palette_pa <- colorRampPalette(c("red", "white"))

# Ensure rank is between 1 and n_colors
rankspb2 <- rank(pb2_mutations$sum, ties.method = "min")
scaled_rankspb2 <- ceiling(rankspb2 / max(rankspb2) * n_colors)

rankspb1 <- rank(pb1_mutations$sum, ties.method = "min")
scaled_rankspb1 <- ceiling(rankspb1 / max(rankspb1) * n_colors)

rankspa <- rank(pa_mutations$sum, ties.method = "min")
scaled_rankspa <- ceiling(rankspa / max(rankspa) * n_colors)

# Assign colors based on ranked 'sum'
pb2_mutations$Color <- color_palette_pb2(n_colors)[scaled_rankspb2]
pb1_mutations$Color <- color_palette_pb1(n_colors)[scaled_rankspb1]
pa_mutations$Color <- color_palette_pa(n_colors)[scaled_rankspa]



colour_all <- rbind(pb2_mutations, pb1_mutations, pa_mutations)

pb2_colour <- colour_all %>% filter(segment=="PB2")
pb1_colour <- colour_all %>% filter(segment=="PB1")
pa_colour <- colour_all %>% filter(segment=="PA")

