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
get_hr_data <- function(participant_folder, Pianist) {
    file_path <- file.path(participant_folder, "HR", paste0(Pianist, "_HR.csv"))
    if (!file.exists(file_path)) return(NULL)
    data <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE, sep = ";")
    data <- data %>% select(Variation.coefficient)
    return(data)
}

# Combine HR data with participant information
hr_data <- data.frame()
Pianists <- c("A", "B", "C", "D")

for (participant_folder in stream_folders) {
    participant_id <- substr(basename(participant_folder), 1, 3)

    for (Pianist in Pianists) {
      # On choisit de ne considérer que la Pianist A
      Pianist_data <- get_hr_data(participant_folder, Pianist) %>% head(60) %>% mutate(
          Participant = participant_id,
          Pianist = Pianist
      )
      hr_data <- bind_rows(hr_data, Pianist_data)
    }
}

# For each familiarity, calculate the mean HR variation, standard deviation, and number of observations
hr_summary <- hr_data %>%
    group_by(Pianist) %>%
    summarise(mean_variation = mean(Variation.coefficient),
              sd_variation = sd(Variation.coefficient),
              n = n())
cat("Mean HR variation by Pianist:\n")
print(hr_summary)

# Ensure 'Pianist' has the desired order
hr_data_long <- hr_data %>%
    pivot_longer(cols = c(Variation.coefficient),
        names_to = "Condition",
        values_to = "HR_variation") %>%
    mutate(Condition = factor(Condition, levels = c("Variation.coefficient"), labels = c("Hear Rate Coefficient of Variation")),
        Pianist = factor(Pianist, levels = c("A", "B", "C", "D")))

# Create the box plot
ggplot(hr_data_long, aes(x = Pianist, y = HR_variation, fill = Pianist)) +
    # geom_boxplot(alpha = 0.7) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    #geom_jitter(width = 0.2, alpha = 0.5) +
    labs(
            x = "Pianist",
            y = "Heart Rate Coefficient of Variation",
            fill = "Pianist"
        ) +
    theme(axis.text.x = element_text(angle = 0, hjust = 1),
        text = element_text(family = "Arial", size = 18)) +
    scale_fill_brewer(palette = "Set3") +
    coord_cartesian(ylim = c(-15, 15)) +  # Définir les limites de l'axe y
    scale_y_continuous(breaks = seq(-15, 15, by = 5))  # Définir les indices de l'axe y toutes les 5 unités


# Sauvegarder le graphique en local
ggsave("D:/MIT project/2024_06 E4 Data/hrv_pianist_box_plot.png", width = 10, height = 6)