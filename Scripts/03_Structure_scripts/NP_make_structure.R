library(bio3d)
library(r3dmol)

#Influenza polymerase A/H7N9-4M replication complex, 
# an asymmetric polymerase dimer bound to human ANP32A PDB ID 8RMR, EMD-19368.

pbd_id <- paste0("2Q06")

pdb<- m_fetch_pdb(pbd_id, save.pdb = FALSE, path = NULL)


np_model <- r3dmol(                         # Set up the initial viewer
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
  m_add_surface(atomsel = m_sel(chain = c("A")), #PA
                style = m_style_surface(opacity = 1, 
                                        colorScheme = NULL,
                                        color = "#63B8FF"))

np_model


np_model1<-np_model

for (i in 1:nrow(np_colour)) {
  
  resi <- np_colour$residue[i]
  colour <- np_colour$Color[i]
  scaled <- np_colour$scaled[i]
  print(colour)
  print(resi)
  print(scaled)
  
  np_model1 <- np_model1 %>% 
    m_add_surface(atomsel = m_sel(resi = resi, 
                                  chain=c("A")), 
                  style = m_style_surface(opacity = scaled,
                                          colorScheme = NULL, 
                                          color =colour)) 
  
}

np_model1

np_model1_labels <- np_model1 %>% 
  m_button_add_res_labels( 
    sel = m_sel(resi = np_colour$residue, chain = c("A")),
    style = m_style_label(
      backgroundColor = "yellow",
      fontSize = 10,
      font = "arial",
      fontColor = "black"
    ),
    label = "Show labels",
    byframe = FALSE,
    hideButton = TRUE,
    hideLabel = "Hide Labels"
  )



np_model2<-np_model %>% 
    m_add_surface(atomsel = m_sel(resi=c(21,22,33,38,48,98,100,136,217,283,284,289,313,350,351,357,384,400,418,425,426,452,456,485), 
                                  chain=c("A")), 
                  style = m_style_surface(opacity = scaled,
                                          colorScheme = NULL, 
                                          color ="red")) 
  


np_model2


np_model3<-np_model2 %>% 
  m_button_add_res_labels(
    sel = m_sel(resi=c(21,22,33,38,48,98,100,136,217,283,284,289,313,350,351,357,384,400,418,425,426,452,456,485), chain = c("A")),
    m_style_label(backgroundColor="red",
                  fontSize = 10,
                  font="arial",
                  fontColor = "white"),
    label = "Show labels",
    byframe = FALSE,
    hideButton = TRUE,
    hideLabel = "Hide Labels"
  ) 

np_model3

### NP trimer

pbd_id_trimer <- paste0("2IQH")

pdb_trimer<- m_fetch_pdb(pbd_id_trimer, save.pdb = FALSE, path = NULL)


trimer_model <- r3dmol(                         # Set up the initial viewer
  viewer_spec = m_viewer_spec(
    cartoonQuality = 0,
  )
) %>%
  m_add_model(                  # Add model to scene
    data = pdb_trimer,
    format = "pdb"
  ) %>%
  m_set_style(style =m_style_surface()) %>%  # Clear all default styles
  m_zoom_to() %>%               # Zoom to encompass the whole scene
  m_add_outline() %>% 
  m_add_surface(atomsel = m_sel(chain = c("A")), #PA
                style = m_style_surface(opacity = 0.5, 
                                        colorScheme = NULL,
                                        color = "green")) %>% 
  m_add_surface(atomsel = m_sel(chain = c("B")), #PA
                style = m_style_surface(opacity = 0.5, 
                                        colorScheme = NULL,
                                        color = "red"))%>% 
  m_add_surface(atomsel = m_sel(chain = c("C")), #PA
                style = m_style_surface(opacity = 0.5, 
                                        colorScheme = NULL,
                                        color = "blue"))

trimer_model



trimer1<-trimer_model

for (i in 1:nrow(np_colour)) {
  
  resi <- np_colour$residue[i]
  colour <- np_colour$Color[i]
  scaled <- np_colour$scaled[i]
  print(colour)
  print(resi)
  print(scaled)
  
  trimer1 <- trimer1 %>% 
    m_add_surface(atomsel = m_sel(resi = resi), 
                  style = m_style_surface(opacity = scaled,
                                          colorScheme = NULL, 
                                          color =colour)) 
  
}

trimer1
trimer1_labelled <- trimer1 %>% 
  m_button_add_res_labels( 
    sel = m_sel(resi = np_colour$residue),
    style = m_style_label(
      backgroundColor = "yellow",
      fontSize = 10,
      font = "arial",
      fontColor = "black"
    ),
    label = "Show labels",
    byframe = FALSE,
    hideButton = TRUE,
    hideLabel = "Hide Labels"
  )

  
  

trimer2<-trimer_model %>% 
  m_add_surface(atomsel = m_sel(resi=c(21,22,33,38,48,98,100,136,217,283,284,289,313,350,351,357,384,400,418,425,426,452,456,485)), 
                style = m_style_surface(opacity = scaled,
                                        colorScheme = NULL, 
                                        color ="red")) 

trimer2


trimer3<-trimer2 %>% 
  m_button_add_res_labels(
    sel = m_sel(resi=c(21,22,33,38,48,98,100,136,217,283,284,289,313,350,351,357,384,400,418,425,426,452,456,485)),
    m_style_label(backgroundColor="red",
                  fontSize = 10,
                  font="arial",
                  fontColor = "white"),
    label = "Show labels",
    byframe = FALSE,
    hideButton = TRUE,
    hideLabel = "Hide Labels"
  ) 

trimer3
