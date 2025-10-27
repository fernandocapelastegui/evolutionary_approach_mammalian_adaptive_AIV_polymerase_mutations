


####METADATA###

preprocess_data <- function(data) {
  data$value <- gsub("/", "_", data$value)
  data$value <- gsub("\\(", "_", data$value)
  data$value <- gsub("\\)", "_", data$value)
  data$value <- gsub("'", "_", data$value)
  data$value <- gsub(",", "_", data$value)
  data$value <- gsub("\\.", "_", data$value)
  data$value <- gsub(":", "_", data$value)
  data$value <- gsub("\\+", "_", data$value)
  data$value <- gsub("\\|", "_", data$value)
  data$value <- gsub("\\;", "_", data$value)
  data$value <- gsub("\\?", "_", data$value)
  data$value <- gsub("<", "_", data$value)
  data$value <- gsub(">", "_", data$value)
  data$value <- gsub("&", "_", data$value)
  data$value <- gsub("`", "_", data$value)
  data$value <- gsub("`", "_", data$value)
  data$value <- gsub("\\-", "_", data$value)
  data$value <- gsub("=", "_", data$value)
  data$value <- gsub("#", "_", data$value)
  return(data)
}

# Read and process avian data
avian <- read.fasta(avian_sequences_file_path)
avian_names <- names(avian) %>% as_tibble()
avian_names_noproc <- avian_names
avian_names <- preprocess_data(avian_names)%>% 
  unique()

# Read and process mammal data
mammal <- read.fasta(mammal_sequences_file_path)
mammal_names <- names(mammal) %>% as_tibble()
mammal_names <- preprocess_data(mammal_names)%>% 
  unique()

# Read and preprocess human data
human <- read.fasta(human_sequences_file_path)
human_names <- names(human) %>% as_tibble()
human_names <- preprocess_data(human_names)%>% 
  unique()

# Create lookup tables
lookup_a <- avian_names %>% 
  dplyr::rename(label = value) %>% 
  mutate(class = "avian", class_num = 1) %>% 
  unique()

lookup_m <- mammal_names %>% 
  dplyr::rename(label = value) %>% 
  mutate(class = "mammal", class_num = 2) %>% 
  unique()

lookup_h <- human_names %>% 
  dplyr::rename(label = value) %>% 
  mutate(class = "human", class_num = 3)%>% 
  unique()

# Combine lookup tables
lookup <- rbind(lookup_a, lookup_m, lookup_h) %>% 
  mutate(mammal_def = case_when(class == "avian" ~0,
                                class == "mammal" ~1,
                                class == "human" ~ 1))
lookup <- lookup %>%
  mutate(ISL = str_extract(label, "(?<=ISL_)[^_]+")) %>% 
  mutate(subtype = str_extract(label, "(?<=_)H\\d+N\\d+(?=_)"))%>%
  mutate(subtype = replace_na(subtype, "Undefined"))

lookup_mammals <- lookup %>% filter(mammal_def==1)
lookup_avian<- lookup %>% filter(mammal_def==0)

print(paste("Metadata successfully imported for segment:", segment))


##############
#Species lookup
##############

lookup_spp <- lookup %>% 
  mutate(stub = tolower(str_extract(label, "^[^0-9]*"))) %>% 
  mutate(between2 = sapply(strsplit(as.character(stub), "_"), function(x) paste(x[2], x[3], sep="_"))) %>% 
  mutate(between1 = sapply(strsplit(as.character(stub), "_"), function(x) x[2])) %>% 
  mutate(species_human = case_when(class =="human"~"human")) %>% 
  select(label, ISL, stub, class, between1, between2, species_human, subtype)

#Mammals

pattern_mammal <- "(?i)(Fox|Red_Fox|Sea_Lion|Lion|Canine|Dog|Racoon_Dog|Tiger|polecat|Feline|Seal|swine|Turnstone|Equine|horse|mink|ferret|wolf|badger|equus|vulpes|dolphin|whale|orca|canis|pig|skunk|chukka|civet|meerkat|lynx|camel|caracal|cheetah|bat|mustela|felis|bear|porpoise|opossum|donkey|boar|otter|stone_marten|mouse|rattus|raccoon|muskrat|fox|mink|coyote|cat|sealion|panda|gian_panda|pika|Leopard|LutraLutra|Lutra|tanuki|giant_anteater|anteater|Sus_scrofa)"

pattern_avian <- "(?i)(Duck|Mallard|Teal|Goose|Swan|Gull|chicken|Turnstone|Turkey|shorebird|sparrowhawk|kestrel|scaup|shoveler|gadwall|wigeon|widgeon|pintail|red_knot|pheasant|quail|partidge|eagle|vulture|penguin|puffin|gannet|skua|magpie|crow|Guinea_Fowl|grebe|canvasback|cygnus|gallus|buzzard|pelecanus|pelican|stork|Anser|tern|heron|house_sparrow|pochard|goldeneye|spoonbill|larus|curlew|shoveller|avian|oystercatcher|owl|starling|Accipiter|coot|aanas|anas|sanderling|ostrich|garganey|Grus|readhead|bird|godwit|morus|hawk|broiler|hen|pigeon|Chroicocephalus|chukar|chukkar|Ciconia|Columba_palumbus|crane|parrot|stonechat|raven|poultry|Ardea_cinerea|Arenaria_interpres|kittiwake|bulbul|skimmer|scoter|bufflehead|cormorant|bussard|eiders|pigeon|egret|guillemot|ketrel|eider|murre|snipe|kestel|porchard|loon|condor|partridge|peacock|dove|Phasianus|flamingo|rhea|kiwi|whimbrel|sandpiper|pavo|falco|auk|munia|chick|cignus|golden_eye|Copsychus|corvus|thrush|shrike|rooster|razorbill|oyster|bill|msllard|redhead|bubo|ibis|dunlin|emu|shearwater|knot|woodcock|Gallinula|geese|Garrulus|Buteo|stilt|swift|brant_|branta|booby|budgerigar|Calidris|macaw|chiken|Chlidonias|jay|lapwing|Thalasseus|Rissa_tridactyla|Phalacrocorax|Ichtyaetus|Otus_scops|swallow|Tyto_alba|Tachybaptus|peregrine|Streptopelia|Aythya|Syrrhaptes|reed_warbler|grackle|Numenius|babbler|sparrow|fulmar|ruddy|Mergus|finch|layer|shag|megpie|yellow_headed_Amazon|stint|Streptopelia|towny|osprey|rook|parasitic_jaeger|pica|fulmar|goosander|smew|great_tit|white_eye|Luscinia|hobara|Cairina|Podiceps_cristatus|hill_myna|Gypaetus|parakeet|chukka|petrel|hoabara|Tadorna|mynah|galericulata|bustard|willet|avocet|plover|shoverl|brambling|duck|blackbird|duck|gull|goose|chicken|falcon)"

#Birds

# Extract species using str_extract and create a new column
lookup_spp$species_mammal <- str_extract(lookup_spp$stub, pattern_mammal)
lookup_spp$species_avian <- str_extract(lookup_spp$stub, pattern_avian)

lookup_spp<-lookup_spp %>% 
  unite(col = spp_final, c(species_mammal, species_avian, species_human), sep="+", na.rm = TRUE, remove=FALSE) 

replace_spp_final <- function(df) {
  # Check for "+" in spp_final and replace based on class
  df$spp_final <- ifelse(grepl("\\+", df$spp_final), 
                         ifelse(df$class == "mammal", df$species_mammal,
                                ifelse(df$class == "human", df$species_human,
                                       ifelse(df$class == "avian", df$species_avian, df$spp_final))),
                         df$spp_final)
  return(df)
}

# Apply the function to your data frame
spp_final <- replace_spp_final(lookup_spp)

spp_final <- spp_final %>% 
  mutate(spp_final = case_when(spp_final=="" ~class,
                               ISL=="235795" ~ "swine",#Hard code an issue here
                               spp_final =="mammal" ~ "mammal undefined",
                               spp_final =="horse" | spp_final == "equus" ~ "equine",
                               spp_final =="pig" | spp_final == "boar" ~ "swine",
                               spp_final == "boar"| spp_final == "sus_scrofa" | spp_final == "Sus_scrofa" ~ "boar",
                               spp_final =="dog"~ "canine",
                               spp_final =="fox" | spp_final =="red_fox" | spp_final =="vulpes"~ 'fox',
                               TRUE ~ spp_final)) %>% 
  select( -class,-between1, -between2, -stub) %>% 
  mutate(spp_final = gsub("_", " ", spp_final))

