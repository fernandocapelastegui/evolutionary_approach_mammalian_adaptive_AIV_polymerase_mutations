pa<- read_excel(paste0("/Paper_final/mutation_loookup",tdate,".xlsx"), sheet = "PA")
pb1<- read_excel(paste0("/Paper_final/mutation_loookup",tdate,".xlsx"), sheet = "PB1")
np<- read_excel(paste0("/Paper_final/mutation_loookup",tdate,".xlsx"), sheet = "NP")
pb2<- read_excel(paste0("/Paper_final/mutation_loookup",tdate,".xlsx"), sheet = "PB2")

output_dir<-"/Graph_figures/"


raw_processing <- function(df, df_segment){ 
  df %>%
  filter(letter !="X") %>% 
  mutate(segment = factor(df_segment)) %>% 
    group_by(residue) %>%
    mutate(
      sum_n = sum(n),
      sum_unique_avian_mutation_events = sum(unique_avian_mutation_events),
      sum_spp = sum(spp_number)) %>% 
    ungroup()
}

pb2_raw<-raw_processing(pb2, "PB2")
pb1_raw<-raw_processing(pb1, "PB1")
pa_raw<-raw_processing(pa, "PA")
np_raw<-raw_processing(np, "NP")

raw_all <-rbind(pb1_raw, pb2_raw, np_raw, pa_raw) %>% 
  left_join(selected_all, by = c("mutation", "segment"))


process_dataframe <- function(df, df_segment) {
  df %>%
    filter(letter !="X") %>% 
    select(residue, n, unique_avian_mutation_events, spp_number) %>%
    group_by(residue) %>%
    summarise(
      sum_n = sum(n),
      sum_unique_avian_mutation_events = sum(unique_avian_mutation_events),
      sum_spp = sum(spp_number)
    ) %>% 
    mutate(segment = factor(df_segment))
}



pb2_plot<-process_dataframe(pb2, "PB2")
pb1_plot<-process_dataframe(pb1, "PB1")
pa_plot<-process_dataframe(pa, "PA")
np_plot<-process_dataframe(np, "NP")


all <- rbind(pb1_plot, pb2_plot, np_plot, pa_plot) %>% arrange(desc(segment))


frequency_plot <- ggplot(raw_all, aes(x = residue, y = n, fill = n,
                                    text = paste("Residue:", residue,
                                                 "<br>Mutation:", mutation,
                                                 "<br>Frequency:", n,
                                                 "<br>Total mutation frequency for residue:", sum_n))) +
  geom_bar(stat = 'identity', width = 1) +
geom_text_repel(aes(label=selected_label),
                max.overlaps = 25,
          size = 3.5,
          nudge_y = 90,
          nudge_x=5,
          segment.size=0.1)+
  facet_grid(rows = vars(segment), scales = "free_x") +
  labs(x = "Residue Position", y = "Sum") +
  scale_x_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 20)) +
  coord_cartesian(xlim = c(min(all$residue), max(all$residue) + 5))+
  scale_y_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 5)) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 20, face= "bold"),
        axis.title.y = element_text(size = 20, face= "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position = "bottom",
        legend.text = element_text(size=12),
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(3, 'cm'),
        legend.key = element_blank(),
        strip.background = element_rect(fill = "#FFFFFF"),
        strip.text.y = element_text(size = 8, angle = 0, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.spacing = unit(1, "lines")) +
  expand_limits(y = 500) +
  scale_fill_gradient2(low = 'palegreen', 
                       mid = 'dodgerblue3', 
                       high = 'firebrick1', 
                       midpoint = 250,
                       guide = "colourbar",
                       name ="",
                       breaks = scales::pretty_breaks(n = 8))
frequency_plot

frequency_plot_paper <- ggplot(raw_all, aes(x = residue, y = n, fill = n,
                                      text = paste("Residue:", residue,
                                                   "<br>Mutation:", mutation,
                                                   "<br>Frequency:", n,
                                                   "<br>Total mutation frequency for residue:", sum_n))) +
  geom_bar(stat = 'identity', width = 1) +
  geom_text_repel(aes(label=selected_label),
                  max.overlaps = 25,
                  size = 3.5,
                  nudge_y = 90,
                  nudge_x=5,
                  segment.size=0.1)+
  facet_grid(rows = vars(segment), scales = "free_x") +
  labs(x = "Residue Position", y = "Sum") +
  scale_x_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 20)) +
  coord_cartesian(xlim = c(min(all$residue), max(all$residue) + 5))+
  scale_y_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 5)) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 20, face= "bold"),
        axis.title.y = element_text(size = 20, face= "bold"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        legend.position = "bottom",
        legend.text = element_text(size=12),
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(3, 'cm'),
        legend.key = element_blank(),
        strip.background = element_rect(fill = "#FFFFFF"),
        strip.text.y = element_text(size = 16, angle = 0, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.spacing = unit(1, "lines")) +
  expand_limits(y = 500) +
  scale_fill_gradient2(low = 'palegreen', 
                       mid = 'dodgerblue3', 
                       high = 'firebrick1', 
                       midpoint = 250,
                       guide = "colourbar",
                       name ="",
                       breaks = scales::pretty_breaks(n = 8))
frequency_plot_paper


ggsave(plot = frequency_plot_paper,
       file = paste0(output_dir,"Figure_S1.pdf"), 
       device = "pdf",
       height = 9,
       width = 20)
ggsave(plot = frequency_plot_paper,
       file = paste0(output_dir,"Figure_S1.png"), 
       device = "png")

frequency_plotly <- ggplotly(frequency_plot,
                   tooltip = "text",
                   width = 1550,
                   height = 700) %>%
  layout(
    margin = list(l = 80, r = 100, b = 80, t = 80)
  )
frequency_plotly


species_list <- all 

total_spp <- raw_all %>%
  separate_rows(species_list, sep = ", ") %>%    # Split the species_list into rows
  group_by(residue, segment) %>%                 # Group by residue and segment
  summarise(unique_spp_residue = n_distinct(species_list), .groups = "drop")  # Count unique species


spptest <- raw_all %>%
  separate_rows(species_list, sep = ", ") %>%   # Split the 'list' column into separate rows
  group_by(residue, segment, selected_label, mutation) %>%          # Group by position and term
  summarise(n = n(), .groups = "drop") %>% 
  left_join(total_spp, by=c("residue", "segment"))


frequency_plot_spp<-ggplot(spptest, aes(x = residue))+
  geom_bar(aes(y = n, fill = n,
               text = paste("Residue:", residue,
                "<br>Mutation:", mutation,
                  "<br>Number of unique species with mutation:", n,
                    "<br>Number of unique species at residue:",unique_spp_residue)),
  stat="identity")+
  geom_line(aes(y = unique_spp_residue, group = 1), color = "red", size = 0.25) +
  geom_text_repel(aes(label=selected_label, y=n),
                  max.overlaps = 25,
                  size = 3.5,
                  nudge_y = 10,
                  nudge_x=5,
                  segment.size=0.1)+
  facet_grid(rows = vars(segment), scales = "free_x")+
  scale_x_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 20)) +
  coord_cartesian(xlim = c(min(species_list$residue), max(species_list$residue) + 10),
                  ylim=c(0, 40))+
  xlab("Residue position")+
  ylab("Sum")+
  theme_bw()+ 
  theme(axis.title.x = element_text(size = 20, face= "bold"))+
  theme(axis.title.y = element_text(size = 20, face= "bold"))+
  theme(axis.text.x  = element_text(size = 14))+
  theme(axis.text.y  = element_text(size = 14))+
  theme(strip.background = element_rect(fill="#FFFFFF"))+
  theme(strip.text.y = element_text(size = 8, angle = 0, face= "bold"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  theme(legend.position = "bottom",
        legend.text = element_text(size=12),
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(3, 'cm'))+
          expand_limits(y = 25) +
  scale_fill_gradient2(low='palegreen', 
                       mid='dodgerblue3', 
                       high='firebrick1', 
                       midpoint =10,
                       guide="colourbar",
                       name="",
                       breaks = scales::pretty_breaks(n = 6))+
  theme(panel.spacing = unit(1.5, "lines"))

frequency_plot_spp_paper<-ggplot(spptest, aes(x = residue))+
  geom_bar(aes(y = n, fill = n,
               text = paste("Residue:", residue,
                            "<br>Mutation:", mutation,
                            "<br>Number of unique species with mutation:", n,
                            "<br>Number of unique species at residue:",unique_spp_residue)),
           stat="identity")+
  geom_line(aes(y = unique_spp_residue, group = 1), color = "red", size = 0.25) +
  geom_text_repel(aes(label=selected_label, y=n),
                  max.overlaps = 25,
                  size = 3.5,
                  nudge_y = 10,
                  nudge_x=5,
                  segment.size=0.1)+
  facet_grid(rows = vars(segment), scales = "free_x")+
  scale_x_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 20)) +
  coord_cartesian(xlim = c(min(species_list$residue), max(species_list$residue) + 10),
                  ylim=c(0, 40))+
  xlab("Residue position")+
  ylab("Sum")+
  theme_bw()+ 
  theme(axis.title.x = element_text(size = 20, face= "bold"))+
  theme(axis.title.y = element_text(size = 20, face= "bold"))+
  theme(axis.text.x  = element_text(size = 18))+
  theme(axis.text.y  = element_text(size = 18))+
  theme(strip.background = element_rect(fill="#FFFFFF"))+
  theme(strip.text.y = element_text(size = 16, angle = 0, face= "bold"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  theme(legend.position = "bottom",
        legend.text = element_text(size=16),
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(3, 'cm'))+
  expand_limits(y = 25) +
  scale_fill_gradient2(low='palegreen', 
                       mid='dodgerblue3', 
                       high='firebrick1', 
                       midpoint =10,
                       guide="colourbar",
                       name="",
                       breaks = scales::pretty_breaks(n = 6))+
  theme(panel.spacing = unit(1.5, "lines"))


frequency_plot_spp
ggsave(plot = frequency_plot_spp_paper,
       file = paste0(output_dir,"Figure_S3.pdf"), 
       device = "pdf",
       height=10,
       width=20)
ggsave(plot = frequency_plot_spp_paper,
       file = paste0(output_dir,"Figure_S3.png"), 
       device = "png")

frequency_plot_spp_plotly <- ggplotly(frequency_plot_spp,
                                      tooltip = "text",
                                      width = 1550,
                                      height = 700) %>%
  layout(
    margin = list(l = 80, r = 80, b = 80, t = 80)
  )

frequency_plot_spp_plotly

#Unique occurence

unique_list <- all 

frequency_plot_unique<-ggplot(raw_all, aes(x = residue, y =unique_avian_mutation_events , fill = unique_avian_mutation_events,
                                             text = paste("Residue:", residue,
                                                          "<br>Mutation", mutation,
                                                          "<br>Unique emergences:", unique_avian_mutation_events,
                                                          "<br>Total unique emergences for residue:", sum_unique_avian_mutation_events)))+
  geom_bar(stat = 'identity')+
  # geom_text_repel(aes(label=selected_label),
  #                 max.overlaps = 30,
  #                 size = 3.5,
  #                 nudge_y = 40,
  #                 nudge_x=5,
  #                 segment.size=0.1)+
  facet_grid(rows = vars(segment), scales = "free_x")+
  scale_x_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 20)) +
  coord_cartesian(xlim = c(min(unique_list$residue), max(unique_list$residue) + 5))+
  scale_y_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 5)) +
  xlab("Residue position")+
  ylab("Sum of all indepedent emergences at residue")+
  theme_bw()+ 
  theme(panel.spacing = unit(1.5, "lines"))+
  theme(axis.title.x = element_text(size = 20, face= "bold"))+
  theme(axis.title.y = element_text(size = 20, face= "bold"))+
  theme(axis.text.x  = element_text(size = 14))+
  theme(axis.text.y  = element_text(size = 14))+
  theme(strip.background = element_rect(fill = "#FFFFFF"))+
  theme(strip.text.y = element_text(size = 8, angle = 0, face= "bold"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  expand_limits(y = 180) +
  theme( legend.position = "bottom",
         legend.text = element_text(size=12),
         legend.key.height = unit(1, 'cm'), #change legend key height
         legend.key.width = unit(3, 'cm'),
         legend.key = element_blank())+
  scale_fill_gradient2(low='palegreen', 
                       mid='dodgerblue3', 
                       high='firebrick1', 
                       midpoint =80,
                       guide="colourbar",
                       name = "",
                       breaks = scales::pretty_breaks(n = 6))
frequency_plot_unique

frequency_plot_unique_paper<-ggplot(raw_all, aes(x = residue, y =unique_avian_mutation_events , fill = unique_avian_mutation_events,
                                           text = paste("Residue:", residue,
                                                        "<br>Mutation", mutation,
                                                        "<br>Unique emergences:", unique_avian_mutation_events,
                                                        "<br>Total unique emergences for residue:", sum_unique_avian_mutation_events)))+
  geom_bar(stat = 'identity')+
  # geom_text_repel(aes(label=selected_label),
  #                 max.overlaps = 30,
  #                 size = 3.5,
  #                 nudge_y = 40,
  #                 nudge_x=5,
  #                 segment.size=0.1)+
  facet_grid(rows = vars(segment), scales = "free_x")+
  scale_x_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 20)) +
  coord_cartesian(xlim = c(min(unique_list$residue), max(unique_list$residue) + 5))+
  scale_y_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 5)) +
  xlab("Residue position")+
  ylab("Sum of all indepedent emergences at residue")+
  theme_bw()+ 
  theme(panel.spacing = unit(1.5, "lines"))+
  theme(axis.title.x = element_text(size = 20, face= "bold"))+
  theme(axis.title.y = element_text(size = 20, face= "bold"))+
  theme(axis.text.x  = element_text(size = 18))+
  theme(axis.text.y  = element_text(size = 18))+
  theme(strip.background = element_rect(fill = "#FFFFFF"))+
  theme(strip.text.y = element_text(size = 16, angle = 0, face= "bold"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  expand_limits(y = 180) +
  theme( legend.position = "bottom",
         legend.text = element_text(size=12),
         legend.key.height = unit(1, 'cm'), #change legend key height
         legend.key.width = unit(3, 'cm'),
         legend.key = element_blank())+
  scale_fill_gradient2(low='palegreen', 
                       mid='dodgerblue3', 
                       high='firebrick1', 
                       midpoint =80,
                       guide="colourbar",
                       name = "",
                       breaks = scales::pretty_breaks(n = 6))


ggsave(plot = frequency_plot_unique_paper,
       file = paste0(output_dir,"Figure_3.pdf"), 
       device = "pdf",
       height=10,
       width = 20)
ggsave(plot = frequency_plot_unique_paper,
       file = paste0(output_dir,"Figure_3.png"), 
       device = "png")

frequency_plot_unique_plotly <- ggplotly(frequency_plot_unique,
                                      tooltip = "text",
                                      width = 1550,
                                      height = 700) %>%
  layout(
    margin = list(l = 80, r = 80, b = 80, t = 80)
  )

frequency_plot_unique_plotly

#Ammino acid freq?


process_dataframe_aa <- function(df, df_segment) {
  df %>%
    filter(letter !="X") %>% 
    select(residue, n, letter) %>%
    group_by(residue, letter) %>%
    summarise(
      sum_letter = sum(n),
    ) %>% 
    mutate(segment = factor(df_segment))
}



pb2_aa<-process_dataframe_aa(pb2, "PB2")
pb1_aa<-process_dataframe_aa(pb1, "PB1")
pa_aa<-process_dataframe_aa(pa, "PA")
np_aa<-process_dataframe_aa(np, "NP")


all_aa <- rbind(pb1_aa, pb2_aa, np_aa, pa_aa) %>% arrange(desc(segment))




aa<-all_aa %>% 
  select(residue, sum_letter, letter, segment) %>% 
  pivot_longer(c(letter)) %>%  
  rename(amino_acid=value) %>% 
  select(segment, residue, amino_acid, sum_letter) %>% 
  group_by(residue, segment, amino_acid) %>% 
  summarise( count = sum(sum_letter)) %>% 
  arrange(amino_acid) %>% 
  left_join(selected_all, by = c("amino_acid", "segment","residue"="position")) %>% 
  mutate(na = ifelse(!is.na(selected), "dot", "no_dot"))


tile <- ggplot(aa, aes(x = residue, 
                       y = factor(amino_acid), 
                       fill = log10(count + 1),
                       text = paste0("Residue: ", residue,
                                     "<br>Amino Acid: ", amino_acid,
                                     "<br>Count: ", count))) + 
  geom_tile(color = FALSE, lwd = 0.01, height=1, width=2) +
  scale_fill_gradient(low = "cadetblue1", high = "firebrick1", name = "log10(Count)") +
  scale_x_continuous(expand = c(0, 0), breaks = scales::pretty_breaks(n = 20)) +
  facet_grid(rows = vars(segment), scales = "free_x") +
  labs(x = "Residue Position", y = "Amino Acid") +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_text(size = 20, face= "bold"),
    axis.title.y = element_text(size = 20, face= "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, size=12,hjust = 1),
    axis.text.y = element_text(size = 14),
    strip.text.y = element_text(size = 16, face = "bold"),
    panel.grid.major.y = element_line(color = "lightgrey", size = 0.1),
    panel.grid.major.x = element_blank(),
    panel.spacing = unit(1.5, "lines"),
    legend.position = "bottom",
    legend.justification = "center",
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),  # increase right margin
    legend.box.margin = margin(0, 40, 0, 0),
    legend.text = element_text(size=12),
    legend.key.height = unit(1, 'cm'), #change legend key height
    legend.key.width = unit(3, 'cm'),
    legend.key = element_blank(),
  )

tile
tile_labelled<-tile+ 
  geom_point(
    data = subset(aa, selected == "yes"),
    aes(x = residue, y = factor(amino_acid)),
    shape = 21,        # circle
    size = 4,          # adjust size as needed
    stroke = 0.5,      # outline width
    color = "black",   # outline color
    fill = NA          # transparent inside
  )+
  geom_text_repel(
    data = subset(aa, selected == "yes"),
    aes(x = residue, y = factor(amino_acid), label = mutation),
    size =5,          # Adjust font size
    color = "black",
    max.overlaps=25)

tile_labelled

ggsave(plot = tile,
       file = paste0(output_dir,"Figure_S2.pdf"), 
       device = "pdf",height = 12, width=25)
ggsave(plot = tile,
       file = paste0(output_dir,"Figure_S2.png"), 
       device = "png",bg = "white",height = 12)

ggsave(plot = tile_labelled,
       file = paste0(output_dir,"Figure_S2_labelled.pdf"), 
       device = "pdf",height = 20, width=25)
ggsave(plot = tile_labelled,
       file = paste0(output_dir,"Figure_S2_labelled.png"), 
       device = "png",bg = "white",height = 12)

plotlytile <- ggplotly(tile_labelled, tooltip = "text", height = 1200, width =1500)
 
plotlytile

#######

#Species plot

my_colors <- data.frame(
  segment = c("PA", "PB1", "PB2", "NP"),
  colour = c("#B2FF8C", "#FF99BF", "#FFBF7F", "#19B2FF"),
  stringsAsFactors = FALSE
)
color_vector <- setNames(my_colors$colour, my_colors$segment)
# Paper in text counts

unique_species_segment<-raw_all %>%
  separate_rows(species_list, sep = ",\\s*") %>%  # split comma-separated species into separate rows
  group_by(segment) %>%
  summarise(
    unique_species_count = n_distinct(species_list),          # Count of unique species
    unique_species_list = list(unique(species_list))          # List of unique species
  )

species_table <- raw_all %>%
  separate_rows(species_list, sep = ",\\s*") %>%
  group_by(segment,species_list) %>% 
  tally()

seg_mutation <- raw_all %>% 
  group_by(segment) %>% 
  tally()

seg_unique_mutation <- raw_all %>% 
  group_by(segment) %>% 
  summarise(n = sum(unique_avian_mutation_events, na.rm = TRUE))

ggplot_look<-theme(
  axis.title.x = element_text(size = 18, face= "bold"),
  axis.title.y = element_text(size = 18, face= "bold"),
  axis.text.x = element_text( size=14),
  axis.text.y = element_text(size = 14),
  legend.position = "none",
  panel.grid = element_blank(),
  plot.title = element_text(hjust = 0.5, size = 20, face = "bold")
)


species_bar <- ggplot(species_table, aes(x=segment, y=n, fill=species_list))+
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Segment", y = "Count", fill = "Species") +
  theme_minimal()
species_bar

species_seg_bar <- ggplot(unique_species_segment, aes(x=segment, y=unique_species_count, fill=segment))+
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Unique species",x = "Segment", y = "Count") +
  theme_bw()+
  scale_fill_manual(values = color_vector)+
  ggplot_look
species_seg_bar


mutation_count_seg_bar <- ggplot(seg_mutation, aes(x=segment, y=n, fill=segment))+
  geom_bar(stat = "identity", position = "stack") +
  labs(title ="Total mutations identified", x = "Segment", y = "Count") +
  theme_bw()+
  scale_fill_manual(values = color_vector)+
  ggplot_look
mutation_count_seg_bar

unique_mutation_count_seg_bar <- ggplot(seg_unique_mutation, aes(x=segment, y=n, fill=segment))+
  geom_bar(stat = "identity", position = "stack") +
  labs(title="Unique emergences", x = "Segment", y = "Sum") +
  theme_bw()+
  scale_fill_manual(values = color_vector)+
  ggplot_look
  unique_mutation_count_seg_bar
  
  triple <- mutation_count_seg_bar + unique_mutation_count_seg_bar + species_seg_bar 
  triple
  ggsave(plot = triple,
         file = paste0(output_dir,"triple.pdf"), 
         device = "pdf",height = 12, width=25)
  ggsave(plot = triple,
         file = paste0(output_dir,"triple.png"), 
         device = "png",bg = "white",height = 12)


  seg_genomne_coverage <- raw_all %>% 
    group_by(segment) %>% 
    summarise(unique_residues = n_distinct(residue)) %>% 
    mutate(tot_length = case_when(segment == "NP"~498,
                                  segment=="PA"~716,
                                  segment=="PB1"~757,
                                  segment== "PB2"~759),
           coverage = (unique_residues/tot_length)*100)
  
  seg_genomne_coverage_plot <- ggplot(seg_genomne_coverage, aes(x=segment, y=coverage, fill=segment))+
    geom_bar(stat = "identity", position = "stack") +
    labs(title="Percent of genome mutated", x = "Segment", y = "Percent") +
    theme_bw()+
    scale_fill_manual(values = color_vector)+
    ggplot_look
  seg_genomne_coverage_plot
  
  
  segment_lengths <- list(
    PA = 716,
    PB1 = 757,
    PB2 = 759,
    NP = 498
  )
  
  # Create the full_residues data frame
  full_residues <- purrr::map_dfr(
    names(segment_lengths),
    ~ data.frame(segment = .x, residue = 1:segment_lengths[[.x]])
  ) %>% 
    mutate(in_data = "Yes")
  
  # View it
  head(full_residues)

  
  
  
 tt <- raw_all %>% 
 select(segment ,residue) %>%
   distinct() %>% 
   mutate(in_data = "Yes") %>% 
  full_join(full_residues, by=c("segment", "residue")) %>% 
   replace_na(list(in_data.x = "No")) %>% 
   select(- in_data.y) %>% 
   rename(in_data = in_data.x)
 
 coverage_percent <- raw_all %>% 
   group_by(segment) %>%
   summarise(unique_residues = n_distinct(residue)) %>% 
   mutate(tot_length = case_when(segment == "NP"~498,
                                 segment=="PA"~716,
                                 segment=="PB1"~757,
                                 segment== "PB2"~759),
          coverage = (unique_residues/tot_length)*100)
 
 tt<-tt %>% left_join(coverage_percent, by="segment")
 # Plot

 ggplot_look_coverage <- theme(
   axis.title.x = element_text(size = 18, face= "bold"),
   axis.title.y = element_text(size = 18, face= "bold"),
   axis.text.x = element_text(size=14),
   axis.text.y = element_text(size = 14),
   panel.grid = element_blank(),
   legend.position = "bottom",
   legend.key.size = unit(1, "cm"),
   legend.text = element_text(size = 20),
   legend.title = element_text(size = 20, face = "bold"),
   plot.title = element_text(hjust = 0.5, size = 20, face = "bold")
 )
 
 # Plot
 coverage_mini<-ggplot(tt, aes(x = residue, y = segment, fill = in_data)) +
   geom_tile(height = 0.9) +
   labs(title = "Location of mutations identified in each segment") +  # <-- Added title here
   scale_fill_manual(name = "Mutation observed at residue", values = c("No" = "#EE6363", "Yes" = "palegreen1")) +
   scale_x_continuous(breaks = seq(0, 759, 100), expand = c(0, 20)) +
   labs(x = "Residue Position", y = "Segment", fill = "In Data") +
   theme_bw() +
   ggplot_look_coverage
 
 ggsave(plot = coverage_mini,
        file = paste0(output_dir,"coverage_mini.pdf"), 
        device = "pdf",height = 12, width=25)
 ggsave(plot = coverage_mini,
        file = paste0(output_dir,"coverage_mini.png"), 
        device = "png",bg = "white",height = 12)
 
 quad_data <- triple + coverage_mini
 
 ggsave(plot = quad_data,
        file = paste0(output_dir,"quad_data.pdf"), 
        device = "pdf",height = 12, width=25)
 
 
 subtype_table <- raw_all %>%
   select(segment, subtype_list) %>%
   separate_rows(subtype_list, sep = ",\\s*") %>%
   group_by(segment, subtype_list) %>%
   tally(name = "subtype_count") %>%
   ungroup()
 
 
 subtype_table_grouped <- subtype_table %>% 
   group_by(subtype_list) %>% 
   summarise(n = sum(subtype_count), .groups = "drop") %>% 
   arrange(desc(n)) %>%
   mutate(subtype_list = factor(subtype_list, levels = subtype_list))
 
 # Step 2: Count unique subtypes per segment
 unique_subtype_counts <- subtype_table %>%
   group_by(segment) %>%
   summarise(unique_subtype_count = n_distinct(subtype_list), .groups = "drop")
 
 # Step 3 (Optional): Join the unique counts back to the main table
 final_table <- subtype_table %>%
   left_join(unique_subtype_counts, by = "segment") %>% 
  arrange(-subtype_count) %>% 
   mutate(subtype_list = factor(subtype_list, levels = names(sort(tapply(subtype_count, subtype_list, sum), decreasing = TRUE))))
 
 subtype_plot<-ggplot(final_table, aes(x = segment, y = subtype_count, fill = subtype_list)) +
   geom_col(width = 1, position = "fill") +
   coord_polar(theta = "y") +
   facet_wrap(~ segment) +
   labs(title = "Subtype Distribution by Segment") +
   theme_void()
 
 subtype_colourCount = length(unique(final_table$subtype_list))
 getPalette = colorRampPalette(brewer.pal(12, "Paired"))
 
 
 subtype_plot_facet <- ggplot(final_table, aes(area = subtype_count, fill = subtype_list, label=subtype_list)) +
   geom_treemap()+
   geom_treemap_text()+
   labs(title = "Relative subtype distribution of mammalian isolate in which mutation was identified across all segments") +  # <-- Added title here
   facet_wrap(~segment)+
   theme(
     plot.title = element_text(size = 24, face = "bold", hjust = 0.5),  # <-- Style the title
     axis.title.x = element_text(size = 20, face= "bold"),
         axis.title.y = element_text(size = 20, face= "bold"),
         axis.text.x = element_text(size = 14),
         axis.text.y = element_text(size = 14),
         legend.position = "bottom",
         legend.text = element_text(size=14),
         legend.key.height = unit(1, 'cm'), #change legend key height
         legend.key.width = unit(1, 'cm'),
         strip.background = element_rect(fill = "#FFFFFF"),
         strip.text.x = element_text(size = 20, angle = 0, face = "bold"))+
   scale_fill_manual(values = getPalette(subtype_colourCount))+
   guides(fill = guide_legend(title=NULL,nrow = 2))
   
 
 subtype_plot <- ggplot(subtype_table_grouped, aes(area = n, fill = subtype_list, label=subtype_list)) +
   geom_treemap()+
   geom_treemap_text()+
   labs(title = "Subtype of mammalian isolate in which mutation was identified") +  
   theme(
     plot.title = element_text(size = 24, face = "bold", hjust = 0.5),  
     axis.title.x = element_text(size = 20, face= "bold"),
     axis.title.y = element_text(size = 20, face= "bold"),
     axis.text.x = element_text(size = 14),
     axis.text.y = element_text(size = 14),
     legend.position = "bottom",
     legend.text = element_text(size=14),
     legend.key.height = unit(1, 'cm'), #change legend key height
     legend.key.width = unit(1, 'cm'),
     strip.background = element_rect(fill = "#FFFFFF"),
     strip.text.x = element_text(size = 20, angle = 0, face = "bold"))+
   scale_fill_manual(values = getPalette(subtype_colourCount))+
   guides(fill = guide_legend(title=NULL,nrow = 2))
 
 
 
   subtype_plot
 
 ggsave(plot = subtype_plot,
        file = paste0(output_dir,"subtype_plot.pdf"), 
        device = "pdf",height = 15, width=15)
 
 
 ggsave(plot = subtype_plot_facet,
        file = paste0(output_dir,"Figure_S4.pdf"), 
        device = "pdf",height = 20, width=20)
 
 mega <- (triple+coverage_mini)/subtype_plot
 
 ggsave(plot = mega,
        file = paste0(output_dir,"mega_plot.pdf"), 
        device = "pdf",height = 15, width=18)
 
 design <- "
  123
  123
  444
  444
  555
  555
  555
  555
  555
  555
  555
"
 
 test <- triple+coverage_mini+subtype_plot+
   plot_layout(design=design)+ 
   plot_annotation(tag_levels = "A")
 
 ggsave(plot = test,
        file = paste0(output_dir,"Figure_2.pdf"), 
        device = "pdf",height = 18, width=20)
 


   selected_manual <- read_xlsx('/Graph_figures/Table_S2_manual.xlsx') %>%
     arrange(Segment) %>% 
     select(-res)
#    