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
global_data <- data.frame()
session_order <- c("A", "B", "C", "D")

for (participant_folder in stream_folders) {
    participant_id <- substr(basename(participant_folder), 1, 3)
    participant_info <- participants %>% filter(ID == participant_id)

    for (i in 1:4) {
        session_data <- get_hr_data(participant_folder, session_order[i])
        familiarity_letter <- substr(participant_info$Familiarity, i, i)

        if (familiarity_letter == "S") {
            familiarity <- "Stranger"
        } else if (familiarity_letter == "H") {
            familiarity <- "Heard of"
        } else if (familiarity_letter == "M") {
            familiarity <- "Met"
        } else if (familiarity_letter == "A") {
            familiarity <- "Acquaintance"
        } else if (familiarity_letter == "F") {
            familiarity <- "Friend"
        } else if (familiarity_letter == "R") {
            familiarity <- "Relative"
        } else if (familiarity_letter == "P") {
            familiarity <- "Self"
        } else {
            print(paste("Unknown familiarity level for participant", participant_id, ": ", familiarity_letter))
        }

        session_data <- session_data %>% mutate(
            Familiarity = familiarity
        )
        global_data <- rbind(global_data, session_data)
    }
}

hr_data_summary <- global_data %>%
    group_by(Familiarity) %>%
    # summarise(HRV = sd(Variation.coefficient, na.rm = TRUE))
    summarise(HRV = mean(Variation.coefficient, na.rm = TRUE))

# Define the order and colors for the bars
familiarity_levels <- c("Stranger", "Heard of", "Met", "Acquaintance", "Friend", "Relative", "Self")
familiarity_colors <- c("red", "orangered", "orange", "gold", "yellow", "yellowgreen", "green3")

# Create the bar plot
p <- ggplot(hr_data_summary, aes(x= factor(Familiarity, levels = familiarity_levels), y = HRV, fill = factor(Familiarity, levels = familiarity_levels))) +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
    scale_fill_manual(values = familiarity_colors) +
    labs(title = "Mean Heart Rate Variation by Familiarity",
    # labs(title = "Standard deviation of Heart Rate Variation by Familiarity",
         x = "Familiarity Level",
         y = "Heart Rate Variation") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Save the plot locally
# ggsave("D:/MIT project/2024_06 E4 Data/hrv_sd_familiarity_box_plot.png", plot=p, width = 10, height = 6)
ggsave("D:/MIT project/2024_06 E4 Data/hrv_mean_familiarity_box_plot.png", plot=p, width = 10, height = 6)
