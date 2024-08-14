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
sessions <- c("A", "B", "C", "D")

for (participant_folder in stream_folders) {
    participant_id <- substr(basename(participant_folder), 1, 3)

    for (session in sessions) {
      # On choisit de ne considérer que la session A
      session_data <- get_hr_data(participant_folder, session) %>% head(60) %>% mutate(
          Participant = participant_id,
          Session = session
      )
      hr_data <- bind_rows(hr_data, session_data)
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
    mutate(Condition = factor(Condition, levels = c("Variation.coefficient"), labels = c("Hear Rate Coefficient of Variation")),
        Session = factor(Session, levels = c("A", "B", "C", "D")))

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
    scale_fill_brewer(palette = "Set3")


# Sauvegarder le graphique en local
ggsave("D:/MIT project/2024_06 E4 Data/hrv_pianist_box_plot.png", width = 10, height = 6)