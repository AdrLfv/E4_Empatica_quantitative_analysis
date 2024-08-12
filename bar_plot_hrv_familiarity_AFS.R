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
    if (participant_id == "P05" | participant_id == "P06" | participant_id == "P07" | participant_id == "P11" | participant_id == "P24" | participant_id == "P26") {
        next
    }
    participant_info <- participants %>% filter(ID == participant_id)

    familiarity <- substr(participant_info$Familiarity, 1, 1)
    if (familiarity %in% c("A", "F", "S")) {

        session_data <- get_hr_data(participant_folder, "A")
        if (!is.null(session_data)) {
            session_data <- session_data %>% mutate(
                Familiarity = case_when(
                    familiarity == "A" ~ "Acquaintance",
                    familiarity == "F" ~ "Friend",
                    familiarity == "S" ~ "Stranger"
                )
            )
            hr_data <- bind_rows(hr_data, session_data)
        }
        global_data <- rbind(global_data, session_data)
    }
}

hr_data_summary <- global_data %>%
    group_by(Familiarity) %>%
    # summarise(HRV = mean(Variation.coefficient, na.rm = TRUE))
    summarise(HRV = sd(Variation.coefficient, na.rm = TRUE))

# Assurez-vous que les Familiarity Levels et Colors sont définis
familiarity_levels <- c("Stranger", "Acquaintance", "Friend")
familiarity_colors <- c("red", "orange", "darkgreen")

# Vérifiez que les niveaux de familiarité sont correctement définis dans les données
hr_data_summary$Familiarity <- factor(hr_data_summary$Familiarity, levels = familiarity_levels)

# Créer le bar plot avec l'ordre et les couleurs spécifiés
p <- ggplot(hr_data_summary, aes(x = Familiarity, y = HRV, fill = Familiarity)) +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
    scale_fill_manual(values = familiarity_colors) +
    labs(title = "Standard deviation of Heart Rate Variation by Familiarity",
    # labs(title = "Mean Heart Rate Variation by Familiarity",
         x = "Familiarity Level",
         y = "Heart Rate Variation",
         fill = "Familiarity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Save the plot locally
# ggsave("D:/MIT project/2024_06 E4 Data/hrv_mean_familiarity_bar_plot_AFS.png", plot=p, width = 10, height = 6)
ggsave("D:/MIT project/2024_06 E4 Data/hrv_sd_familiarity_bar_plot_AFS.png", plot=p, width = 10, height = 6)
