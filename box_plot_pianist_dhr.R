library(dplyr)
library(tidyr)
library(ggplot2)
library(FSA)
library(ggsignif)

# This script generates box plots to visualize the delta heart rate (ΔHR) across the different pianists.

# Base paths
base_path <- "cleaned_data"

# List of folders containing HR data
stream_folders <- list.dirs(base_path, recursive = FALSE)

# Function to retrieve HR data
get_hr_data <- function(participant_folder, Pianist) {
    file_path <- file.path(participant_folder, "HR", paste0(session, "_HR.rds"))
    if (!file.exists(file_path)) return(NULL)
    data <- readRDS(file_path)
    data <- data %>% select(Delta_Heart_Rate)
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

# For each familiarity, calculate the mean ΔHR, standard deviation, and number of observations
hr_summary <- hr_data %>%
    group_by(Pianist) %>%
    summarise(mean_variation = mean(Delta_Heart_Rate),
              sd_variation = sd(Delta_Heart_Rate),
              n = n())
cat("Mean ΔHR by Pianist:\n")
print(hr_summary)

# Ensure 'Pianist' has the desired order
hr_data_long <- hr_data %>%
    pivot_longer(cols = c(Delta_Heart_Rate),
        names_to = "Condition",
        values_to = "HR_variation") %>%
    mutate(Condition = factor(Condition, levels = c("Delta_Heart_Rate"), labels = c("Delta Heart Rate")),
        Pianist = factor(Pianist, levels = c("A", "B", "C", "D")))

# Create the box plot
ggplot(hr_data_long, aes(x = Pianist, y = HR_variation, fill = Pianist)) +
    # geom_boxplot(alpha = 0.7) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    #geom_jitter(width = 0.2, alpha = 0.5) +
    labs(
            x = "Pianist",
            y = "ΔHR",
            fill = "Pianist"
        ) +
    theme(axis.text.x = element_text(angle = 0, hjust = 1),
        text = element_text(family = "Arial", size = 18)) +
    scale_fill_brewer(palette = "Set3") +
    coord_cartesian(ylim = c(-15, 15)) +  # Définir les limites de l'axe y
    scale_y_continuous(breaks = seq(-15, 15, by = 5))  # Définir les indices de l'axe y toutes les 5 unités


# Sauvegarder le graphique en local
ggsave("plots/hrv_pianist_box_plot.png", width = 10, height = 6)