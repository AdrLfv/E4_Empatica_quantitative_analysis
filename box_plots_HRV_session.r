library(dplyr)
library(tibble)
library(ggplot2)
library(tidyverse)

base_path <- "D:/MIT project/2024_06 E4 Data/Cleaned data"
combined_data_path <- "D:/MIT project/2024_06 E4 Data/combined_data"

# Liste des sessions
sessions <- c("A", "B", "C", "D")

# Fonction pour récupérer les données à partir du fichier CSV
get_session_data <- function(participant_folder, session, participant) {
    file_path <- file.path(participant_folder, "HR", paste0(session, "_HR.csv"))
    data_file <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE, sep = ";")
    data <- data_file %>%
        select(Variation.coefficient) %>%
        head(60) %>%
        pull(Variation.coefficient)  # Récupérer les données Variation.coefficient
    
    return(data)
}

# Fonction pour écrire les données dans un fichier CSV pour une session donnée
write_session_data <- function(session_name) {
    # Initialiser une liste nommée pour stocker les données de chaque participant
    data_list <- vector("list", length(stream_folders))
    names(data_list) <- paste0("P", formatC(1:length(stream_folders), width = 2, flag = "0"))
    
    # Boucler à travers chaque participant et récupérer les données
    for (i in seq_along(stream_folders)) {
        participant_folder <- stream_folders[i]
        participant <- names(data_list)[i]
        data_list[[participant]] <- get_session_data(participant_folder, session_name, participant)
    }
    
    # Créer un tibble à partir de la liste de données
    combined_data <- as_tibble(data_list)
    
    # Écrire les données dans un fichier CSV
    write.table(combined_data, file = file.path(combined_data_path, paste0("combined_data_", session_name, ".csv")), 
                row.names = FALSE, sep = ";")
    
    return(combined_data)
}
    

# Créer et sauvegarder le box plot pour chaque session
for (session_name in sessions) {
    # Charger les données combinées pour la session
    combined_data <- read.csv(file.path(combined_data_path, paste0("combined_data_", session_name, ".csv")), sep = ";")
    
    # Modifier l'esthétique aes pour refléter les colonnes de votre tibble
    p <- ggplot(combined_data, aes(x = factor(rep(colnames(combined_data), each = nrow(combined_data))), y = value)) +
        geom_boxplot(fill = "skyblue", color = "blue") +
        ggtitle(paste("Box Plot for Session", session_name)) +
        xlab("Participant") +
        ylab("Value") +
        theme_minimal()
    
    # Sauvegarder le box plot en tant qu'image PNG
    ggsave(file.path(combined_data_path, paste0("boxplot_", session_name, ".png")), plot = p, width = 8, height = 6)
}