#Import data:

pa<- read_excel(paste0("Data/Output/Output_tables/Paper_final/mutation_loookup",tdate,".xlsx"), sheet = "PA")
pb1<- read_excel(paste0("Data/Output/Output_tables/Paper_final/mutation_loookup",tdate,".xlsx"), sheet = "PB1")
np<- read_excel(paste0("Data/Output/Output_tables/Paper_final/mutation_loookup",tdate,".xlsx"), sheet = "NP")
pb2<- read_excel(paste0("Data/Output/Output_tables/Paper_final/mutation_loookup",tdate,".xlsx"), sheet = "PB2")

nice_table_layout_function <- function(data){
  data <- data %>% select(mutation, 
                          residue,
                          letter,
                          n, 
                          percent_a, 
                          percent_m,
                          unique_avian_mutation_events,
                          spp_number,
                          species_list,
                          subtype_list,
                          subtype_number) %>% 
    rename(count = n,
           Mutation = mutation,
           Residue = residue,
           "Amino Acid" = letter,
           'Freq. % in avian sequences'=percent_a,
           'Freq. % in mammal sequences'=percent_m,
           'Indpendent mutation occurence'= unique_avian_mutation_events,
           'Species count'=spp_number,
           "Species diversity"=species_list,
           "Subtype(s)" = subtype_list,
           "Subtype(s) n" = subtype_number)
}

pa_clean <-nice_table_layout_function(pa)
pb1_clean <-nice_table_layout_function(pb1)
pb2_clean <-nice_table_layout_function(pb2)
np_clean <-nice_table_layout_function(np)

rmarkdown::render("~/git/mutation_pathway/R/Paper_figures/HTML_markdown_scrit.Rmd",
                  output_file =paste0("~/git/mutation_pathway/Data/Output/Output_tables/Paper_final/report_", tdate,".html"))

rmarkdown::render("~/git/mutation_pathway/R/Paper_figures/Structure analysis/Structural_report.Rmd",
                  output_file =paste0("~/git/mutation_pathway/Data/Output/Output_tables/Paper_final/structural_report_", tdate,".html"))


rmarkdown::render("~/git/mutation_pathway/R/Paper_figures/Structure analysis/Structural_report_NP.Rmd",
                  output_file =paste0("~/git/mutation_pathway/Data/Output/Output_tables/Paper_final/structural_report_NP_", tdate,".html"))





