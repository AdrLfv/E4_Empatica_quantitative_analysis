library(dplyr)
library(tidyr)
library(ggplot2)
library(FSA)
library(ggsignif)

# This script generates box plots to visualize the delta heart rate (ΔHR) across different sessions and participants.
if (!dir.exists("plots")) {
    dir.create("plots")
}
# Base paths
base_path <- "cleaned_data"
participants_data <- readRDS("data_rds/participants.rds")
# List of folders containing HR data
stream_folders <- list.dirs(base_path, recursive = FALSE)

# Function to retrieve HR data
get_hr_data <- function(participant_folder, session) {
    file_path <- file.path(participant_folder, "HR", paste0(session, "_HR.rds"))
    if (!file.exists(file_path)) return(NULL)
    data <- readRDS(file_path)
    data <- data %>% select(Delta_Heart_Rate)
    return(data)
}

# Combine HR data with participant information
hr_data <- data.frame()

for (participant_folder in stream_folders) {
    participant_id <- substr(basename(participant_folder), 1, 3)
    participant_order <- participants_data[participants_data$ID == participant_id, "Order"]

    for (i in 1:4) {
      session <- substr(participant_order, i, i)
      session_data <- get_hr_data(participant_folder, session) %>% head(60)
      if (!is.null(session_data)) {
          session_data$Participant <- participant_id
          session_data$Session <- i
          session_data$Group <- session
          hr_data <- rbind(hr_data, session_data)
      }
    }
}

# For each familiarity, calculate the mean ΔHR, standard deviation, and number of observations
hr_summary <- hr_data %>%
    group_by(Session) %>%
    summarise(mean_variation = mean(Delta_Heart_Rate),
              sd_variation = sd(Delta_Heart_Rate),
              n = n())
cat("Mean ΔHR by session:\n")
print(hr_summary)

# Ensure 'Session' has the desired order
hr_data_long <- hr_data %>%
    pivot_longer(cols = c(Delta_Heart_Rate),
                 names_to = "Condition",
                 values_to = "HR_variation") %>%
    mutate(Condition = factor(Condition, levels = c("Delta_Heart_Rate"), labels = c("ΔHR")),
    # levels va de 1 à 4
           Session = factor(Session, levels = c("1", "2", "3", "4")))

# Create the box plot
ggplot(hr_data_long, aes(x = Session, y = HR_variation, fill = Session)) +
    # geom_boxplot(alpha = 0.7) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    #geom_jitter(width = 0.2, alpha = 0.5) +
    labs(
            x = "Session",
            y = "ΔHR",
            fill = "Session"
        ) +
    theme(axis.text.x = element_text(angle = 0, hjust = 1),
        text = element_text(family = "Arial", size = 16)) +
    scale_fill_brewer(palette = "Set3")  +
    coord_cartesian(ylim = c(-15, 15)) +  # Définir les limites de l'axe y
    scale_y_continuous(breaks = seq(-15, 15, by = 5))  # Définir les indices de l'axe y toutes les 5 unités


# Sauvegarder le graphique en local
ggsave("plots/hrv_order_box_plot.png", width = 10, height = 6)