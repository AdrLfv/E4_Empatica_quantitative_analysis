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

stream_folders <- list.dirs(base_path, recursive = FALSE)

# Combine HR data with participant information
hr_data <- data.frame()

for (participant_folder in stream_folders) {
    participant_id <- substr(basename(participant_folder), 1, 3)
    if (participant_id == "P05" | participant_id == "P06" | participant_id == "P07" | participant_id == "P11" | participant_id == "P24" | participant_id == "P26") {
        next
    }
    participant_info <- participants %>% filter(ID == participant_id)
    familiarity <- substr(participant_info$Familiarity, 1, 1)
    if (familiarity %in% c("A", "F", "S")) {
        diagram_variation <- participant_info$A_diagram_after - participant_info$A_diagram_before
        # on construit un data frame avec une colonne ID (de participants) et une colonne diagram_variation
        session_data <- data.frame(ID = participant_id, diagram_variation = diagram_variation)
        if (!is.null(session_data)) {
            session_data <- session_data %>% mutate(
                Group = case_when(
                    familiarity == "A" ~ "Acquaintance",
                    familiarity == "F" ~ "Friend",
                    familiarity == "S" ~ "Stranger"
                )
            )
            hr_data <- bind_rows(hr_data, session_data)
        }
    }
}

# Filter data for valid groups
hr_data <- hr_data %>% filter(Group %in% c("Acquaintance", "Friend", "Stranger"))

# Perform Kruskal-Wallis test
kruskal_test <- kruskal.test(diagram_variation ~ Group, data = hr_data)
print(kruskal_test)

# # Post-hoc analysis with Dunn's test
dunn_test <- dunnTest(diagram_variation ~ Group, data = hr_data, method = "bonferroni")
print(dunn_test)
