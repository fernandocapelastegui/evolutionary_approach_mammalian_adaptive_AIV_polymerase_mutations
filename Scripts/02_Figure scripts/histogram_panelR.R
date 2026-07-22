install.packages("cowplot")
library(cowplot)
library(ggplot2)

p1 <- ggdraw() + draw_image("gitfilepath/Output/Output_tables/NP/20250813/NP_distance_histogram20250813.png")  + draw_label("NP",  x = 0.05, y = 0.95, hjust = 0, fontface = "bold")
p2 <- ggdraw() + draw_image("gitfilepath/Output/Output_tables/PA/20250813/PA_distance_histogram20250813.png")  + draw_label("PA",  x = 0.05, y = 0.95, hjust = 0, fontface = "bold")
p3 <- ggdraw() + draw_image("gitfilepath/Output/Output_tables/PB1/20250813/PB1_distance_histogram20250813.png") + draw_label("PB1", x = 0.05, y = 0.95, hjust = 0, fontface = "bold")
p4 <- ggdraw() + draw_image("gitfilepath/Output//Data/Output/Output_tables/PB2/20250813/PB2_distance_histogram20250813.png") + draw_label("PB2", x = 0.05, y = 0.95, hjust = 0, fontface = "bold")

panel <- plot_grid(p1, p2, p3, p4, ncol = 2, nrow = 2)

ggsave("gitfilepath/git/evolutionary_approach_mammalian_adaptive_AIV_polymerase_mutations/Figures/histogram_panel.pdf", panel, width = 12, height = 8, dpi = 300)
panel
