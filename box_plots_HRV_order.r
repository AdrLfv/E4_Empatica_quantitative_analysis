# Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(FSA)
library(ggsignif)

# Base paths
base_path <- "D:/MIT project/2024_06 E4 Data/Cleaned data"
participant_file <- "D:/MIT project/2024_06 E4 Data/participants.csv"

# Load participant information
participant_data <- read.csv(participant_file, header = TRUE, stringsAsFactors = FALSE, sep = ";")

# List of folders containing HR data
stream_folders <- list.dirs(base_path, recursive = FALSE)

# Function to retrieve HR data for a given session
get_hr_data <- function(participant_folder, session) {
    file_path <- file.path(participant_folder, "HR", paste0(session, "_HR.csv"))
    if (!file.exists(file_path)) return(NULL)
    data <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE, sep = ";")
    data <- data %>% select(Variation.coefficient)
    return(data)
}

# Combine HR data with participant information
hr_data <- data.frame()

for (participant_folder in stream_folders) {
    participant_id <- substr(basename(participant_folder), 1, 3)
    participant_order <- participant_data[participant_data$ID == participant_id, "Order"]
    
    if (length(participant_order) == 0) next
    
    for (i in 1:4) {  # Boucle à travers les sessions 1 à 4
        session <- substr(participant_order, i, i)
        session_data <- get_hr_data(participant_folder, session)
        if (!is.null(session_data)) {
            # Calculer la moyenne de la variation du coefficient
            session_data <- session_data %>% mutate(
                Session = i, mean_HRV = mean(Variation.coefficient, na.rm = TRUE)
            )
            hr_data <- rbind(hr_data, session_data)
        }
    }
}
# Créer le box plot avec des couleurs différentes pour chaque session
p <- ggplot(hr_data, aes(x = factor(Session), y = mean_HRV, fill = factor(Session))) +
    geom_boxplot(color = "black") +
    ggtitle("Box Plot of the Heart Rate Variation regarding the sessions order") +
    xlab("Session") +  # Changer le label de l'axe X pour refléter la session
    ylab("Mean HRV") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_fill_brewer(palette = "Set3")  # Utiliser une palette de couleurs prédéfinie

# Sauvegarder le box plot en tant qu'image PNG
ggsave("D:/MIT project/2024_06 E4 Data/boxplot_order_HRV.png", plot = p, width = 8, height = 6)