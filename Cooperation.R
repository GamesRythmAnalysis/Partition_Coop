rm(list=ls()) # on vide la mémoire de RStudio
wd <- ("~/Documents/Projets R/Rythmanalyse/Analyses/Cooperation MR P23")
setwd (wd)

# packages
library(tidyverse)
library (RColorBrewer)
library(data.table)

source("Scripts/KeyRythm.1.1.R")

# dezipper les fichiers de données dans le répertoire Data
setwd("Data")
list_of_sources <- list.files()
list_of_sources <- list_of_sources[c(str_which(list_of_sources,".zip"))]
lapply (list_of_sources, unzip)
list_of_sources <- list.files()

# à ce stade on ne retient que les fichiers clavier (K)
list_of_sources <- list_of_sources[c(str_which(list_of_sources,".K.csv"))]

# on détecte le fichier de Paul et Renaud
fichier_paul <- list_of_sources[c(str_which(list_of_sources,"Paul"))]
fichier_renaud <- list_of_sources[c(str_which(list_of_sources,"RM"))]

d.paul <- read_csv2 (fichier_paul)
d.renaud <- read_csv2 (fichier_renaud)

# on renomme les variables
names (d.paul) <- c("K.EVENEMENT","K.TOUCHE","K.TEMPS")
d.paul$K.TEMPS <- as.integer(d.paul$K.TEMPS)
names (d.renaud) <- c("K.EVENEMENT","K.TOUCHE","K.TEMPS")
d.renaud$K.TEMPS <- as.integer(d.renaud$K.TEMPS)

# on convertit Paul (Qwerty) en Azerty en utilisant la table de conversion
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

# On recale les temps en détectant le 1er R du signal de synchronisation
avanceJ2 <- d.paul$K.TEMPS[d.paul$K.TOUCHE == "R"][1] - d.renaud$K.TEMPS[d.renaud$K.TOUCHE == "R"][1]
d.renaud$K.TEMPS <- d.renaud$K.TEMPS + avanceJ2

# On peut choisir d'utiliser ou non une plage de temps (en secondes)
# plage_temps <- c(300,420)
# d.paul <- filter (d.paul, K.TEMPS > plage_temps[1]*1000 & K.TEMPS < plage_temps[2]*1000)
# d.renaud <- filter (d.renaud, K.TEMPS > plage_temps[1]*1000 & K.TEMPS < plage_temps[2]*1000)

# on ne garde que touches prédéfinies : A Z E R D F
touchesFreq <- as.factor (c ("A","Z","E","R","D","F"))
d.paul <- filter (d.paul, K.TOUCHE %in% touchesFreq)
d.renaud <- filter (d.renaud, K.TOUCHE %in% touchesFreq)

# on applique la fonction qui calcule la durée des appuis
d.R1 <- lapply(levels(touchesFreq), key.rythm, data = d.paul)
d.R1 <- do.call("rbind", d.R1)
d.R1 <- d.R1[!is.na(d.R1$down.time),]

d.R2 <- lapply(levels(touchesFreq), key.rythm, data = d.renaud)
d.R2 <- do.call("rbind", d.R2)
d.R2 <- d.R2[!is.na(d.R2$down.time),]

# on met tout ça dans un même tableau en ajoutant le nom des joueurs 
# (ce qu'on aurait pu faire depuis bien longtemps et aurait divisé les copier-coller par 2)
d.R1 <- cbind (d.R1, joueur = "Paul")
d.R2 <- cbind (d.R2, joueur = "Renaud")
d.R <- rbind (d.R1,d.R2)

# Ici, on pourrait choisir un ordre des touches
mestouches <- levels(d.R$touche)

# On génère enfin la sortie graphique

plot_partition <- ggplot(data = d.R, aes (x = down.time, y = touche, 
                                  color = joueur)) +
  geom_point(aes(size = duration), shape = 1, alpha = 0.6, stroke = 4) +
  scale_size(range = c(1,20)) +
  guides(size = "none", alpha = "none")+ 
  theme_minimal () +
  theme(plot.background = element_rect(fill = "white", color ="white"),
        legend.position = "right") +
  xlab ("") +
  ylab ("") +
  scale_color_manual (values = c("#3182bd", "#ff7f00"))

plot_partition

setwd("Resultats")
ggsave(filename = paste (fichier_paul,".png", sep =""), plot = plot_partition)
setwd("../")