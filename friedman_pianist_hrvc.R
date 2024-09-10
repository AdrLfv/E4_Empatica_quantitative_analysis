# Librairies
library(dplyr)
library(tidyr)
library(ggplot2)
library(FSA)
library(ggsignif)

# Chemins de base
base_path <- "D:/MIT project/2024_06 E4 Data/Cleaned data"
participant_file <- "D:/MIT project/2024_06 E4 Data/participants.csv"

# Charger les informations des participants
participant_data <- read.csv(participant_file, header = TRUE, stringsAsFactors = FALSE, sep = ";")

# Liste des dossiers contenant les données de HR
stream_folders <- list.dirs(base_path, recursive = FALSE)

# Fonction pour récupérer les données de HR
get_hr_data <- function(participant_folder, session, participant) {
    file_path <- file.path(participant_folder, "HR", paste0(session, "_HR.csv"))
    if (!file.exists(file_path)) return(NULL)
    data <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE, sep = ";")
    data <- data %>% 
        select(Variation.coefficient) %>% 
        head(60) %>%  # Assurer qu'on prend les 60 premières lignes seulement
        mutate(Participant = participant, Pianist = session)
    return(data)
}

# Combiner les données HR avec les informations des participants
global_data <- data.frame()
session_order <- c("A", "B", "C", "D")

for (participant_folder in stream_folders) {
    participant_id <- substr(basename(participant_folder), 1, 3)
    
    for (i in 1:4) {
        session_data <- get_hr_data(participant_folder, session_order[i], participant_id)
        if (!is.null(session_data)) {
            global_data <- rbind(global_data, session_data)
        }
    }
}

# Préparation des données pour le test de Friedman
# Ici, nous devons nous assurer que nous avons 60 mesures pour chaque participant et chaque pianiste
mean_variation_data <- global_data %>%
group_by(Participant, Pianist) %>%
summarise(mean_variation = mean(Variation.coefficient, na.rm = TRUE))

# Exécuter le test de Friedman
friedman_test_result <- friedman.test(y=mean_variation_data$mean_variation, groups=mean_variation_data$Pianist, blocks=mean_variation_data$Participant)

# Afficher les résultats du test
print(friedman_test_result)

# Test post hoc si significatif
if (friedman_test_result$p.value < 0.05) {
    # Test de Wilcoxon post-hoc
    post_hoc_result <- pairwise.wilcox.test(mean_variation_data$mean_variation, 
                                            mean_variation_data$Pianist, 
                                            p.adjust.method = "bonferroni", 
                                            paired = TRUE)
    print(post_hoc_result)
}
