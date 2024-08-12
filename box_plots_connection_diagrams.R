# Charger les packages nécessaires
library(readxl)  # Pour lire les fichiers Excel
library(ggplot2) # Pour créer des graphiques
library(dplyr)   # Pour manipuler les données
library(tidyr)   # Pour transformer les données

# Définir le chemin vers le fichier Excel
base_path <- "D:/MIT project/2024_06 E4 Data/Cleaned data"
participant_file <- "D:/MIT project/2024_06 E4 Data/participants.csv"
stream_folders <- list.dirs(base_path, recursive = FALSE)

# Lire les données du fichier Excel
participants_data <- read.csv(participant_file, header = TRUE, stringsAsFactors = FALSE, sep = ";")

# Créer un dictionnaire pour stocker la familiarité et la couleur de chaque participant
participants_colors_familiarity <- list()

for (participant_folder in stream_folders) {
    participant_id <- substr(basename(participant_folder), 1, 3)
    participant_info <- participants_data %>% filter(ID == participant_id)
    familiarity <- substr(participant_info$Familiarity, 1, 1)
    # Attribuer une couleur et une description à chaque niveau de familiarité
    if (familiarity == "S") {
        participants_colors_familiarity[[participant_id]] <- c("Stranger", "red")
    } 
    # else if (familiarity == "H") {
    #     participants_colors_familiarity[[participant_id]] <- c("Hear of", "orangered")
    # } else if (familiarity == "M") {
    #     participants_colors_familiarity[[participant_id]] <- c("Met", "orange")
    # } 
    else if (familiarity == "A") {
        participants_colors_familiarity[[participant_id]] <- c("Acquaintance", "orange")
    } else if (familiarity == "F") {
        participants_colors_familiarity[[participant_id]] <- c("Friend", "yellow")
    } else if (familiarity == "R") {
        participants_colors_familiarity[[participant_id]] <- c("Relative", "yellowgreen")
    } else if (familiarity == "P") {
        participants_colors_familiarity[[participant_id]] <- c("Self", "green")
    } else {
      print(paste("Unknown familiarity level for participant", participant_id, ": ", familiarity))
    }
}

# Sélectionner les colonnes nécessaires pour les sessions A
data_A <- participants_data %>% 
  select(ID, A_diagram_before, A_diagram_after) %>%
  mutate(connection_before = A_diagram_before,
         connection_after = A_diagram_after)

# Reshaping les données pour ggplot (sessions A)
data_A_long <- data_A %>%
  pivot_longer(cols = c(connection_before, connection_after),
               names_to = "Condition",
               values_to = "Connection") %>%
  mutate(Condition = factor(Condition, levels = c("connection_before", "connection_after"), labels = c("Before", "After")),
         Familiarity = sapply(ID, function(id) participants_colors_familiarity[[id]][1]),
         Color = sapply(ID, function(id) participants_colors_familiarity[[id]][2]))

# Sélectionner les colonnes nécessaires pour les sessions B
data_B <- participants_data %>% 
  select(ID, B_diagram_before, B_diagram_after) %>%
  mutate(connection_before = B_diagram_before,
         connection_after = B_diagram_after)


# Reshaping les données pour ggplot (sessions B)
data_B_long <- data_B %>%
  pivot_longer(cols = c(connection_before, connection_after),
               names_to = "Condition",
               values_to = "Connection") %>%
  mutate(Condition = factor(Condition, levels = c("connection_before", "connection_after"), labels = c("Before", "After")),
         Familiarity = sapply(ID, function(id) participants_colors_familiarity[[id]][1]),
         Color = sapply(ID, function(id) participants_colors_familiarity[[id]][2]))
         
# Définir l'ordre des niveaux de Familiarity et des couleurs correspondantes
familiarity_levels <- c("Stranger", "Acquaintance", "Friend", "Relative", "Self")
familiarity_colors <- c("red", "orange", "yellow", "yellowgreen", "green")

if (!all(familiarity_levels %in% unique(data_A_long$Familiarity))) {
  stop("Familiarity levels do not match the data in session A")
}

# Mettre à jour les données avec l'ordre correct
data_A_long <- data_A_long %>%
  mutate(Familiarity = factor(Familiarity, levels = familiarity_levels))

# Créer et sauvegarder le graphique pour la session A
ggplot(data_A_long, aes(x = Condition, y = Connection)) +
  geom_boxplot(alpha = 0.7, color = "black") +  # Couleur fixe pour les boîtes à moustaches
  geom_jitter(width = 0.2, alpha = 0.7, aes(color = Familiarity)) +
  scale_color_manual(values = familiarity_colors) +  # Assigner les couleurs manuellement
  ggtitle("Box Plot of Felt Connection before and after experimenting MirrorFugue (Session A)") +
  xlab("Condition") +
  ylab("Connection Level") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(color = guide_legend(title = "Familiarity"))
ggsave("D:/MIT project/2024_06 E4 Data/connection_plot_A.png", width = 10, height = 6)

# Mettre à jour les données pour la session B avec l'ordre correct
data_B_long <- data_B_long %>%
  mutate(Familiarity = factor(Familiarity, levels = familiarity_levels))

# Créer et sauvegarder le graphique pour la session B
ggplot(data_B_long, aes(x = Condition, y = Connection)) +
  geom_boxplot(alpha = 0.7, color = "black") +  # Couleur fixe pour les boîtes à moustaches
  geom_jitter(width = 0.2, alpha = 0.7, aes(color = Familiarity)) +
  scale_color_manual(values = familiarity_colors) +  # Assigner les couleurs manuellement
  ggtitle("Box Plot of Felt Connection before and after experimenting MirrorFugue(Session B)") +
  xlab("Condition") +
  ylab("Connection Level") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(color = guide_legend(title = "Familiarity"))
ggsave("D:/MIT project/2024_06 E4 Data/connection_plot_B.png", width = 10, height = 6)
