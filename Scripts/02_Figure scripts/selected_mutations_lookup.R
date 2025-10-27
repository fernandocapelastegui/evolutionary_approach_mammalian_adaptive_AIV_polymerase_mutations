


pa_selected<- data.frame(
  segment = "PA",
  position = c(28,55,57,62,85,208,241,254,256,263,275,277,356,362,382,399,403,497,497,505,553,602,626),
  mutation = c("P28S", "D55N", "R57Q", "I62V", "T85I", "T208K", "C241Y", "N254S", "R256K", "T263E", "P275L", "S277F", "K356R", "K362R", "E382D", "E399K", "L403I", "D479E", "K497R", "I505V", "A553S", "V602I", "K626R")
  )

pb1_selected<- data.frame(
  segment = "PB1",
  position = c(12,40,105,111,154,179,211,317,361,374,433,469,486,524,571,581,584,598,621,633,642,646,708,728),
  mutation = c("V12I", "M40I", "N105S", "M111I", "G154S", "M179I", "R211K", "M317I", "S361R", "A374S", "K433R", "T469I", "R486K", "S524G", "R571K", "E581D", "R584Q", "L598P", "Q621R", "S633N", "S642N", "M646V", "P708S", "I728V"
)
)

pb2_selected<- data.frame(
  segment = "PB2",
  position = c(61,65,76,82,127,147,157,184,225,238,271,286,299,340,391,480,560,574,581,590,591, 627,645,701),
mutation = c("K61R", "E65D", "T76I", "N82T", "H127N", "I147T", "K157R", "T184A", "S225G", "T238A", "T271A", "S286G", "R299K", "R340K", "E391D", "V480I", "V560L", "K574R", "V584I", "G590S", "Q591R", "E627K", "M645L", "D701N")
)

np_selected<- data.frame(
  segment = "NP",
  position = c(21,22,33,38,48,98,100,136,217,283,284,289,313,350,351,357,384,400,418,425,426,452,456,485),
  mutation = c("N21D", "A22T", "V33I", "R38K", "K48Q", "R98K", "V100I", "L136I", "I217V", "L283P", "A284I", "Y289H", "F313V", "T350K", "R351K", "Q357K", "R384K", "R400K", "L418I", "I425V", "M426L", "R452K", "V456L", "G485R")
)

selected_all <- rbind(pa_selected, pb1_selected,pb2_selected, np_selected) %>% 
  mutate(selected = "yes",
         selected_label = mutation,
         star = "*",
         amino_acid = str_sub(mutation, -1))
