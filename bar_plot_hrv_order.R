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
    data <- data %>% select(Variation.coefficient) %>% head(60)
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
        session_data <- get_hr_data(participant_folder, session)
        if (!is.null(session_data)) {
            session_data$Session <- i
            hr_data <- rbind(hr_data, session_data)
        }
    }
}

# On résume les données pour avoir la moyenne de la variation du rythme cardiaque par session
hr_data_summary <- hr_data %>%
    group_by(Session) %>%
    summarise(HR_variation = mean(Variation.coefficient, na.rm = TRUE))
    # summarise(HR_variation = sd(Variation.coefficient, na.rm = TRUE))

# Create the bar plot
p <- ggplot(hr_data_summary, aes(x=factor(Session), y = HR_variation, fill = factor(Session))) +
    geom_bar(stat="identity", position = "dodge", alpha = 0.7) +
    labs(title = "Mean Heart Rate Variation by order of sessions",
    # labs(title = "Standard deviation of Heart Rate Variation by order of sessions",
         x = "Session",
         y = "Mean Heart Rate Variation") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Sauvegarder le graphique en local
ggsave("D:/MIT project/2024_06 E4 Data/mean_hrv_order_barplot.png", plot=p, width = 10, height = 6)
# ggsave("D:/MIT project/2024_06 E4 Data/sd_hrv_order_barplot.png", plot=p, width = 10, height = 6)
