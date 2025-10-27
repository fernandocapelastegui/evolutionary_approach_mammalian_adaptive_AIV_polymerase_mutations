library(bio3d)
library(r3dmol)
install.packages("devtools")
library(devtools)
devtools::install_github("swsoyee/r3dmol")


pbd_id <- paste0("8R1J")

pdb<- m_fetch_pdb(pbd_id, save.pdb = FALSE, path = NULL)


pol_model <- r3dmol(                         # Set up the initial viewer
  viewer_spec = m_viewer_spec(
    cartoonQuality = 0,
  )
) %>%
  m_add_model(                  # Add model to scene
    data = pdb,
    format = "pdb"
  ) %>%
  m_set_style(style =m_style_surface()) %>%  # Clear all default styles
  m_zoom_to() %>%               # Zoom to encompass the whole scene
  m_add_outline() %>% 
  m_add_surface(atomsel = m_sel(chain = c("A","D")), #PA
                style = m_style_surface(opacity = 0.5, 
                                        colorScheme = NULL,
                                        color = "red")) %>% 
  m_add_surface(atomsel = m_sel(chain = c("B","E")), #PB1
                style = m_style_surface(opacity =0.5, 
                                        colorScheme = NULL,
                                        color = "green")) %>% 
  m_add_surface(atomsel = m_sel(chain = c("F","C")), #PB2
                style = m_style_surface(opacity = 0.5, 
                                        colorScheme = NULL,
                                        color = "blue")) %>% 
  m_add_surface(atomsel = m_sel(chain = c("G")), #ANP
                style = m_style_surface(opacity = 1, 
                                        colorScheme = NULL,
                                        color = "orange")) 

pol_model


pol_model1<-pol_model

for (i in 1:nrow(pb2_colour)) {
  
  resi <- pb2_colour$residue[i]
  colour <- pb2_colour$Color[i]
  scaled <- pb2_mutations$scaled[i]
  print(colour)
  print(resi)
  print(scaled)
  
  pol_model1 <- pol_model1 %>% 
    m_add_surface(atomsel = m_sel(resi = resi, 
                                  chain=c("C", "F")), 
                  style = m_style_surface(opacity = scaled,
                                          colorScheme = NULL, 
                                          color =colour)) 
  
}

pol_model1

for (i in 1:nrow(pb1_colour)) {
  
  resi <- pb1_colour$residue[i]
  colour <- pb1_colour$Color[i]
  scaled <- pb1_mutations$scaled[i]
  
  print(colour)
  print(resi)
  
  pol_model1 <- pol_model1 %>% 
    m_add_surface(atomsel = m_sel(resi = resi, 
                                  chain=c("B", "E")), 
                  style = m_style_surface(opacity = scaled,
                                          colorScheme = NULL, 
                                          color = colour)) 
  
}

pol_model1

for (i in 1:nrow(pa_colour)) {
  
  resi <- pa_colour$residue[i]
  colour <- pa_colour$Color[i]
  scaled <- pa_mutations$scaled[i]
  
  print(colour)
  print(resi)
  
  pol_model1 <- pol_model1 %>% 
    m_add_surface(atomsel = m_sel(resi = resi, chain=c("A", "D")), 
                  style = m_style_surface(opacity = scaled,
                                          colorScheme = NULL, 
                                          color = colour)) 
  
}

pol_model1

pol_model1_labels<- pol_model1 %>% 
  m_button_add_res_labels(
    sel = m_sel(resi=pa_colour$residue, chain = c("A","D")),
    m_style_label(backgroundColor="red",
                  fontSize = 12,
                  font="arial"),
    label = "Show PA Labels",
    byframe = FALSE,
    hideButton = FALSE,
    hideLabel = "Hide Labels"
  ) %>% 
  m_button_add_res_labels(
    sel = m_sel(resi=pb1_colour$residue,chain = c("B","E")),
    m_style_label(backgroundColor="green",
                  fontSize = 12,
                  font="arial"),
    label = "Show PB1 Labels",
    byframe = FALSE,
    hideButton = FALSE,
    hideLabel = "Hide PB1 Labels"
  ) %>% 
  m_button_add_res_labels(
    sel = m_sel(resi=pb2_colour$residue, chain = c("F","C")),
    m_style_label(backgroundColor="blue",
                  fontSize = 12,
                  font="arial"),
    label = "Show PB2 Labels",
    byframe = FALSE,
    hideButton = TRUE,
    hideLabel = "Hide Labels"
  )

pol_model1_labels

pol_model2 <- pol_model %>% 
  m_add_surface(atomsel = m_sel(resi=c(28,55,57,62,85,208,241,254,256,263,275,277,356,362,382,399,403,497,497,505,553,602,626), 
                                chain=c("A","D")),#PA
                style = m_style_surface(opacity = 1,
                                        colorScheme = NULL, 
                                        color ="red")) %>% 
  m_add_surface(atomsel = m_sel(resi=c(12,40,105,111,154,179,211,317,361,374,433,469,486,524,571,581,584,598,621,633,642,646,708,728), 
                                chain=c("B","E")),#PB1
                style = m_style_surface(opacity = 1,
                                        colorScheme = NULL, 
                                        color ="green")) %>% 
  m_add_surface(atomsel = m_sel(resi=c(61,65,76,82,127,147,157,184,225,238,271,286,299,340,391,480,560,574,581,590,591, 627,645,701), 
                                chain=c("F","C")),#PB1
                style = m_style_surface(opacity = 1,
                                        colorScheme = NULL, 
                                        color ="blue")) 

pol_model2

pol_model2_labels <-pol_model2%>% 
  m_button_add_res_labels(
    sel = m_sel(resi=c(61,65,76,82,127,147,157,184,225,238,271,286,299,340,391,480,560,574,581,590,591,627,645,701), chain = c("F","C")),
    m_style_label(backgroundColor="blue",
                  fontSize = 12,
                  font="arial",
                  fontColor = "white",inFront = TRUE),
    label = "Show PB2 mutations analysed",
    byframe = FALSE,
    hideButton = FALSE,
    hideLabel = "Hide labels"
  )%>% 
  m_button_add_res_labels(
    sel = m_sel(resi=c(12,40,105,111,154,179,211,317,361,374,433,469,486,524,571,581,584,598,621,633,642,646,708,728), chain = c("B","E")),
    m_style_label(backgroundColor="green",
                  fontSize = 12,
                  font="arial",
                  fontColor = "white"),
    label = "Show PB1 mutations analysed",
    byframe = FALSE,
    hideButton = FALSE,
    hideLabel = "Hide PB2 mutations analysed"
  )%>% 
  m_button_add_res_labels(
    sel = m_sel(resi=c(28,55,57,62,85,208,241,254,256,263,275,277,356,362,382,399,403,497,497,505,553,602,626), chain = c("A","D")),
    m_style_label(backgroundColor="red",
                  fontSize = 12,
                  font="arial",
                  fontColor = "white"),
    label = "Show PA mutations analysed",
    byframe = FALSE,
    hideButton = TRUE,
    hideLabel = "Hide Labels"
  )
pol_model2_labels

  