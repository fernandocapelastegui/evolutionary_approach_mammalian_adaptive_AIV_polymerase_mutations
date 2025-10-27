print(paste("File names assigned for segment:", segment))


if (segment =="PB1"){
  
tree_file_path <-"/PB1_trimal_cialigned_cleaned.fasta.treefile"

alignment_file_path <- "/PB1_trimal_cialigned_cleaned.fasta"

#Metdata

  #Avian file
avian_sequences_file_path <-"~/git/mutation_pathway/Data/Final/02_Combined fasta/PB1/20240326_PB1_avian.fasta"
  #Mammal file
mammal_sequences_file_path <-"~/git/mutation_pathway/Data/Final/02_Combined fasta/PB1/20240326_PB1_mammal.fasta"
  #Human file
human_sequences_file_path <-"~/git/mutation_pathway/Data/Final/02_Combined fasta/PB1/20240326_PB1_human.fasta"

}

if (segment =="PB2"){
  
  tree_file_path <-"/PB2_trimal_cialigned_cleaned.fasta.treefile"
  
  alignment_file_path <- "/PB2_trimal_cialigned_cleaned.fasta"
  
  #Metdata
  
  #Avian file
  avian_sequences_file_path <-"~/git/mutation_pathway/Data/Final/02_Combined fasta/PB2/20240326_PB2_avian.fasta"
  #Mammal file
  mammal_sequences_file_path <-"~/git/mutation_pathway/Data/Final/02_Combined fasta/PB2/20240326_PB2_mammal.fasta"
  #Human file
  human_sequences_file_path <-"~/git/mutation_pathway/Data/Final/02_Combined fasta/PB2/20240326_PB2_human.fasta"
  
}

if (segment =="NP"){
  
  tree_file_path <-"/NP_trimal_cialigned_cleaned.fasta.treefile"
  
  alignment_file_path <- "/NP_trimal_cialigned_cleaned.fasta"
  
  #Metdata
  
  #Avian file
  avian_sequences_file_path <-"/20240326_NP_avian.fasta"
  #Mammal file
  mammal_sequences_file_path <-"/20240326_NP_mammal.fasta"
  #Human file
  human_sequences_file_path <-"/20240326_NP_human.fasta"
  
}

if (segment =="PA"){
  
  tree_file_path <-"/PA_trimal_cialigned_cleaned.fasta.treefile"
  
  alignment_file_path <- "/PA_trimal_cialigned_cleaned.fasta"
  
  #Metdata
  
  #Avian file
  avian_sequences_file_path <-"/20240326_PA_avian.fasta"
  #Mammal file
  mammal_sequences_file_path <-"/20240326_PA_mammal.fasta"
  #Human file
  human_sequences_file_path <-"/20240326_PA_human.fasta"
  
}