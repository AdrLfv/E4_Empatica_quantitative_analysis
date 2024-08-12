# Charger les packages nécessaires
library(readxl)  # Pour lire les fichiers Excel
library(ggplot2) # Pour créer des graphiques
library(dplyr)   # Pour manipuler les données
library(tidyr)   # Pour transformer les données

file_path <- "D:/MIT project/2024_06 E4 Data/participants.csv"

# Lire les données du fichier Excel
data <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE, sep = ";")

# Construction d'un tableau à partir de la moyenne de A_diagram_before et A_diagram_after, et de celle de B_diagram_before et B_diagram_after
data_long <- data %>% select(ID, A_diagram_before, A_diagram_after, B_diagram_before, B_diagram_after) %>%
  mutate(mean_connection_before = (A_diagram_before + B_diagram_before) / 2,
         mean_connection_after = (A_diagram_after + B_diagram_after) / 2)

variation <- data_long %>% select(ID, mean_connection_before, mean_connection_after) %>%
  mutate(mean_variation = mean_connection_after - mean_connection_before)

#wilcoxon signed rank test for the connection level before and after the MirrorFugue experiment
wilcoxon_test <- wilcox.test(variation$mean_connection_before, variation$mean_connection_after, paired = TRUE, alternative = "two.sided")
print(wilcoxon_test)