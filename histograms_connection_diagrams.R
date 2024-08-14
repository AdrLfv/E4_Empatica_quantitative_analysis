# Charger les packages nécessaires
library(readxl)  # Pour lire les fichiers Excel
library(ggplot2) # Pour créer des graphiques
library(dplyr)   # Pour manipuler les données
library(tidyr)   # Pour transformer les données

# Définir le chemin vers le fichier Excel
file_path <- "D:/MIT project/2024_06 E4 Data/participants.csv"

# Lire les données du fichier Excel
data <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE, sep = ";")

# Construction d'un tableau à partir de la moyenne de A_diagram_before et A_diagram_after, et de celle de B_diagram_before et B_diagram_after
# data_long <- data %>% select(ID, A_diagram_before, A_diagram_after, B_diagram_before, B_diagram_after) #%>%
  # mutate(connection_before = (A_diagram_before + B_diagram_before) / 2,
  #        connection_after = (A_diagram_after + B_diagram_after) / 2)

# variation <- data_long %>% select(ID, connection_before, connection_after) %>%
#   mutate(mean_variation = connection_after - connection_before)

# Calculer la variation moyenne de la connexion avant et après l'expérience MirrorFugue
# mean_variation <- mean(variation$mean_variation)
# sd_variation <- sd(variation$mean_variation)
# print(paste("Mean variation of connection before and after MirrorFugue experiment:", mean_variation))
# print(paste("Standard deviation of variation of connection before and after MirrorFugue experiment:", sd_variation))

# Sélectionner les colonnes nécessaires pour les sessions A
data_A <- data %>% 
  select(ID, A_diagram_before, A_diagram_after) %>%
  mutate(connection_before = A_diagram_before,
         connection_after = A_diagram_after)

# Reshaping les données pour ggplot (sessions A)
data_A_long <- data_A %>%
  pivot_longer(cols = c(connection_before, connection_after),
               names_to = "Condition",
               values_to = "Connection") %>%
  mutate(Condition = factor(Condition, levels = c("connection_before", "connection_after"), labels = c("Before", "After")))

# Créer et sauvegarder l'histogramme pour la session A
ggplot(data_A_long, aes(x = factor(ID), y = Connection, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), alpha = 0.7) +
  scale_fill_manual(values = c("Before" = "blue", "After" = "red"), name = "Condition") +
  labs(#title = "Histogram of Felt Connection (Session A)",
       x = "Participant ID",
       y = "Connection Level") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_brewer(palette = "Set1")
ggsave("D:/MIT project/2024_06 E4 Data/connection_histogram_A.png", width = 10, height = 6)

# Sélectionner les colonnes nécessaires pour les sessions B
data_B <- data %>% 
  select(ID, B_diagram_before, B_diagram_after) %>%
  mutate(connection_before = B_diagram_before,
         connection_after = B_diagram_after)

# Reshaping les données pour ggplot (sessions B)
data_B_long <- data_B %>%
  pivot_longer(cols = c(connection_before, connection_after),
               names_to = "Condition",
               values_to = "Connection") %>%
  mutate(Condition = factor(Condition, levels = c("connection_before", "connection_after"), labels = c("Before", "After")))

# Créer et sauvegarder l'histogramme pour la session B
ggplot(data_B_long, aes(x = factor(ID), y = Connection, fill = Condition)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), alpha = 0.7) +
  scale_fill_manual(values = c("Before" = "blue", "After" = "red"), name = "Condition") +
  labs(#title = "Histogram of Felt Connection (Session B)",
       x = "Participant ID",
       y = "Connection Level") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_brewer(palette = "Set1")
ggsave("D:/MIT project/2024_06 E4 Data/connection_histogram_B.png", width = 10, height = 6)

