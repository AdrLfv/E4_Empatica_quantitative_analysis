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

# Function to retrieve HR data
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
    
    for (i in 1:4) {
        session <- substr(participant_order, i, i)
        session_data <- get_hr_data(participant_folder, session) %>% head(60)
        if (!is.null(session_data)) {
            session_data$Session <- i
            hr_data <- rbind(hr_data, session_data)
        }
    }
}

# On résume les données pour avoir la moyenne de la variation du rythme cardiaque par session
hr_data_summary_mean <- hr_data %>%
    group_by(Session) %>%
    summarise(HR_variation_mean = mean(Variation.coefficient, na.rm = TRUE))

hr_data_summary_sd <- hr_data %>%
    group_by(Session) %>%
    summarise(HR_variation_sd = sd(Variation.coefficient, na.rm = TRUE))

cat("Mean HR variation by session:\n")
print(hr_data_summary_mean)

cat("Standard deviation of HR variation by session:\n")
print(hr_data_summary_sd)

# Create the bar plot
p_mean <- ggplot(hr_data_summary_mean, aes(x=factor(Session), y = HR_variation_mean, fill = factor(Session))) +
    geom_bar(stat="identity", position = "dodge", alpha = 0.7) +
    labs(
            x = "Session",
            y = "Mean Heart Rate Variation",
            fill = "Session" 
        ) + 
    theme(axis.text.x = element_text(angle = 0, hjust = 1),
        text = element_text(family = "Arial", size = 16)) +
    scale_fill_brewer(palette = "YlGnBu")  # Utiliser une palette de couleurs prédéfinie

p_sd <- ggplot(hr_data_summary_sd, aes(x=factor(Session), y = HR_variation_sd, fill = factor(Session))) +
    geom_bar(stat="identity", position = "dodge", alpha = 0.7) +
    labs(
            x = "Session",
            y = "Mean Heart Rate Variation",
            fill = "Session" 
        ) + 
    theme(axis.text.x = element_text(angle = 0, hjust = 1),
        text = element_text(family = "Arial", size = 16)) +
    scale_fill_brewer(palette = "YlGnBu")  # Utiliser une palette de couleurs prédéfinie

# Sauvegarder le graphique en local
ggsave("D:/MIT project/2024_06 E4 Data/hrv_mean_session_barplot.png", plot=p_mean, width = 10, height = 6)
ggsave("D:/MIT project/2024_06 E4 Data/hrv_sd_session_barplot.png", plot=p_sd, width = 10, height = 6)