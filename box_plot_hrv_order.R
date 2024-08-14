library(dplyr)
library(tidyr)
library(ggplot2)
library(FSA)
library(ggsignif)

# Base paths
base_path <- "D:/MIT project/2024_06 E4 Data/Cleaned data"
participant_file <- "D:/MIT project/2024_06 E4 Data/participants.csv"

# Load participant information
participants <- read.csv(participant_file, header = TRUE, stringsAsFactors = FALSE, sep = ";")

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

    for (i in 1:4) {
      session <- substr(participant_order, i, i)
      session_data <- get_hr_data(participant_folder, session)
      if (!is.null(session_data)) {
          session_data$Participant <- participant_id
          session_data$Session <- i
          session_data$Group <- session
          hr_data <- rbind(hr_data, session_data)
      }
    }
}

# For each familiarity, calculate the mean HR variation, standard deviation, and number of observations
hr_summary <- hr_data %>%
    group_by(Session) %>%
    summarise(mean_variation = mean(Variation.coefficient),
              sd_variation = sd(Variation.coefficient),
              n = n())
cat("Mean HR variation by session:\n")
print(hr_summary)

# Ensure 'Session' has the desired order
hr_data_long <- hr_data %>%
    pivot_longer(cols = c(Variation.coefficient),
                 names_to = "Condition",
                 values_to = "HR_variation") %>%
    mutate(Condition = factor(Condition, levels = c("Variation.coefficient"), labels = c("Heart Rate Coefficient of Variation")),
    # levels va de 1 à 4
           Session = factor(Session, levels = c("1", "2", "3", "4")))

# Create the box plot
ggplot(hr_data_long, aes(x = Session, y = HR_variation, fill = Session)) +
    geom_boxplot(alpha = 0.7) +
    geom_jitter(width = 0.2, alpha = 0.5) +
    labs(
            x = "Session",
            y = "Heart Rate Variation",
            fill = "Session"
        ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_fill_brewer(palette = "Set3")  # Utiliser une palette de couleurs prédéfinie


# Sauvegarder le graphique en local
ggsave("D:/MIT project/2024_06 E4 Data/hrv_order_box_plot.png", width = 10, height = 6)