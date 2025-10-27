

np_mutations <- read_excel(paste0("/Paper_final/mutation_loookup",tdate,".xlsx"), sheet = "NP")%>% select(residue, n)%>% group_by(residue) %>% 
  summarise_at(vars(n), list(sum = sum)) %>% arrange(-sum)%>% top_n(50)%>% 
  mutate(normalised = (sum - min(sum)) / (max(sum) - min(sum)),
         segment = "NP")

n_colors <- 25

color_palette_np <- colorRampPalette(c("yellow", "white"))

# Ensure rank is between 1 and n_colors
ranksnp<- rank(np_mutations$sum, ties.method = "min")
scaled_ranksnp <- ceiling(ranksnp / max(ranksnp) * n_colors)



# Assign colors based on ranked 'sum'
np_mutations$Color <- color_palette_np(n_colors)[scaled_ranksnp]



colour_all_np <- np_mutations

np_colour <- colour_all_np %>% filter(segment=="NP")


