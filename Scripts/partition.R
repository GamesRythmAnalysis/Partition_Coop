
# fichier_paul <- list_of_sources[1]
# fichier_renaud <- list_of_sources[5]
# truecredit <- "TRUE"

# rougeux_like_graph_poster <- function (nom_fichier, truecredit) {
  
  #d <- read_csv2 (nom_fichier[index_fichier])
  setwd ("Data")
  d.paul <- read_csv2 (fichier_paul)
  d.renaud <- read_csv2 (fichier_renaud)
  
  # on récupère le bon titre dans la base de données
  # mon_titre <- filter (data.titre, nomfichier == paste (str_sub (nom_fichier,1,-7),".zip", sep =""))$titre
  
  # on uniformise les données 
  names (d.paul) <- c("K.EVENEMENT","K.TOUCHE","K.TEMPS")
  d.paul$K.TEMPS <- as.integer(d.paul$K.TEMPS)
  
  names (d.renaud) <- c("K.EVENEMENT","K.TOUCHE","K.TEMPS")
  d.renaud$K.TEMPS <- as.integer(d.renaud$K.TEMPS)
  
  sort (table (d.renaud$K.TOUCHE), decreasing = TRUE)
  sort (table (d.paul$K.TOUCHE), decreasing = TRUE)
  
  # si Paul, convertir QWERTY en AZERTY ~ vérifier que la table de conversion est complète
  # if (str_detect(fichier_paul,"Paul")) {
    setwd (wd)
    setwd ("Scripts")
    conversion_clavier <- read_csv("azerty_to_qwerty.csv")
    conversion_clavier$Azerty <- toupper(conversion_clavier$Azerty)
    conversion_clavier$Qwertz <- toupper(conversion_clavier$Qwertz)
    setwd (wd)
    
    for (i in c(1:nrow(d.paul))) {
      if (d.paul$K.TOUCHE[i] %in% conversion_clavier$Qwertz) {
        d.paul$K.TOUCHE[i] <- conversion_clavier$Azerty[conversion_clavier$Qwertz == d.paul$K.TOUCHE[i]]
      }
    }
    
  d.paul$K.TOUCHE <- as.factor (d.paul$K.TOUCHE)
  d.renaud$K.TOUCHE <- as.factor (d.renaud$K.TOUCHE)
  
  # Recaler les temps : détecter le 1er R
  avanceJ2 <- d.paul$K.TEMPS[d.paul$K.TOUCHE == "R"][1] - d.renaud$K.TEMPS[d.renaud$K.TOUCHE == "R"][1]
  d.renaud$K.TEMPS <- d.renaud$K.TEMPS + avanceJ2
  
  # d <- cbind (d, Joueur = "J1")
  # d.j2 <- cbind (d.j2, Joueur = "J2")
  
  # d.all <- rbind (d,d.j2)
  
  # Sélectionner Plage
  # 140  - 155 s
  # 360 - 400 s
  
  
  plage_temps <- c(300,420)
  d.paul <- filter (d.paul, K.TEMPS > plage_temps[1]*1000 & K.TEMPS < plage_temps[2]*1000)
  d.renaud <- filter (d.renaud, K.TEMPS > plage_temps[1]*1000 & K.TEMPS < plage_temps[2]*1000)
  
  d.all <- rbind (d.paul,d.renaud)
  
  # on ne garde que les 10 appuis les plus fréquents
  # ou bien un set prédéfini
  # A Z E R D F
  
  # VALFREQ <- 0.01
  # tableTouches <- sort(table(d.all$K.TOUCHE), decreasing = TRUE)
  # touchesFreq <- subset (tableTouches,
  #                       tableTouches > nrow (d.all) * VALFREQ)
  # touchesFreq <- touchesFreq [1:10]
  # touchesFreq <- as.factor (rownames (touchesFreq))
  
  touchesFreq <- as.factor (c ("A","Z","E","R","D","F"))
  
  d.paul <- filter (d.paul, K.TOUCHE %in% touchesFreq)
  d.renaud <- filter (d.renaud, K.TOUCHE %in% touchesFreq)
  
  d.R1 <- lapply(levels(touchesFreq), key.rythm, data = d.paul)
  d.R1 <- do.call("rbind", d.R1)
  d.R1 <- d.R1[!is.na(d.R1$down.time),]
  
  d.R2 <- lapply(levels(touchesFreq), key.rythm, data = d.renaud)
  d.R2 <- do.call("rbind", d.R2)
  d.R2 <- d.R2[!is.na(d.R2$down.time),]
  
  d.R1 <- cbind (d.R1, joueur = "Paul")
  d.R2 <- cbind (d.R2, joueur = "Renaud")
  
  d.R <- rbind (d.R1,d.R2)
  
  ## Ordonner les touches
  
  mestouches <- levels(d.R$touche)
  
  # PLOT
  
  plotBW <- ggplot(data = d.R, aes (x = down.time, y = touche, 
                                    color = joueur)) +
    geom_point(aes(size = duration^2, alpha = 0, stroke = 2), shape = 1) +
    # coord_polar(theta = "x", direction= 1, start = 0) +
    scale_size(range = c(1,10)) +
    guides(size = "none", alpha = "none")+ 
    #labs (caption = mon_titre) +
    # theme_void () +
    theme(plot.background = element_rect(fill = "white", color ="white"),
          legend.position = "left",
          plot.margin= margin(t = 2, r = 0, b = 0, l = 0, unit = "cm"),
          plot.caption = element_text(hjust = 0.5, size = rel(10), family = "ArcadeClassic", color ="Black")) 
    #scale_color_brewer(palette = "Paired",
    #                   na.value ="white",
    #                   name = "",
    #                   breaks = mestouches)
    #scale_color_manual(values = c("#99d8c9", "#3182bd", "#9ecae1", "#2ca25f", 
    #                                       "#cb181d","#ff7f00", "#fdbf6f", 
    #                                       "#fb9a99","#6a3d9a","#cab2d6",
    #                                       "#252525", "#252525", "#252525","#252525"),
    #                                       # na.value ="white",
    #                   name = "",
    #                   breaks = mestouches)
  
  plotBW
  
  setwd("../")
  setwd("Resultats")
  
  # ggsave(filename = paste (nom_fichier,".png", sep =""), plot = plotBW)
  
  setwd("../")
  setwd("Data")
  return (plotBW)
}

