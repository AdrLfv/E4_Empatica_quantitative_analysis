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
    if (participant_id == "P05" | participant_id == "P06" | participant_id == "P07" | participant_id == "P11" | participant_id == "P24" | participant_id == "P26") {
        next
    }
    participant_info <- participants %>% filter(ID == participant_id)
    if (nrow(participant_info) == 0) next
    familiarity <- substr(participant_info$Familiarity, 1, 1)
    if (familiarity %in% c("A", "F", "S")) {
        # On choisit de ne considérer que la session A
        session_data <- get_hr_data(participant_folder, "A") %>% head(60)
        if (!is.null(session_data)) {
            session_data <- session_data %>% mutate(
                Familiarity = case_when(
                    familiarity == "S" ~ "Stranger",
                    familiarity == "A" ~ "Acquaintance",
                    familiarity == "F" ~ "Friend"
                )
            )
            hr_data <- rbind(hr_data, session_data)
        }
    }
}

hr_data_long <- hr_data %>%
    pivot_longer(cols = c(Variation.coefficient),
                 names_to = "Condition",
                 values_to = "HR_variation") %>%
    mutate(Condition = factor(Condition, levels = c("Variation.coefficient"), labels = c("HR Variation")),
           Familiarity = factor(Familiarity, levels = c("Stranger", "Acquaintance", "Friend")))

# Créer le box plot avec des couleurs différentes pour chaque session
p_box_plot <- ggplot(hr_data_long, aes(x = Familiarity, y = HR_variation, fill = Familiarity)) +
    geom_boxplot(alpha = 0.7) +
    geom_jitter(width = 0.2, alpha = 0.5) +
    labs(#title = "Box plot of heart rate variation by familiarity",
         x = "Familiarity",
         y = "Heart Rate Coefficient of Variation") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +

    scale_fill_brewer(palette = "Set3")

# Résumé des données HRV en fonction de la Familiarité
hr_data_summary_mean <- hr_data %>%
    group_by(Familiarity) %>%
    summarise(mean_HRV = mean(Variation.coefficient, na.rm = TRUE), .groups = 'drop')

print(hr_data_summary_mean)

hr_data_summary_sd <- hr_data %>%
    group_by(Familiarity) %>%
    summarise(sd_HRV = sd(Variation.coefficient, na.rm = TRUE), .groups = 'drop')

print(hr_data_summary_sd)


familiarity_levels <- c("Stranger", "Acquaintance", "Friend")

# Create the bar plot
p_mean <- ggplot(hr_data_summary_mean, aes(x= factor(Familiarity, levels = familiarity_levels), y = mean_HRV, fill = factor(Familiarity, levels = familiarity_levels))) +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
    # scale_fill_manual(values = familiarity_colors) +
    labs(
            x = "Familiarity Level",
            y = "Heart Rate Variation",
            fill = "Familiarity Level" 
        ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_fill_brewer(palette = "YlGn")

# Create the bar plot
p_sd <- ggplot(hr_data_summary_sd, aes(x= factor(Familiarity, levels = familiarity_levels), y = sd_HRV, fill = factor(Familiarity, levels = familiarity_levels))) +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
    # scale_fill_manual(values = familiarity_colors) +
    labs(
            x = "Familiarity Level",
            y = "Heart Rate Variation",
            fill = "Familiarity Level" 
        ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_fill_brewer(palette = "YlGn") 

# Save the plot locally
# ggsave("D:/MIT project/2024_06 E4 Data/hrv_boxplot_familiarity_based_on_mean.png", plot = p_box_plot, width = 8, height = 6)
ggsave("D:/MIT project/2024_06 E4 Data/hrv_mean_familiarity_bar_plot_AFS.png", plot=p_mean, width = 10, height = 6)
ggsave("D:/MIT project/2024_06 E4 Data/hrv_sd_familiarity_bar_plot_AFS.png", plot=p_sd, width = 10, height = 6)
