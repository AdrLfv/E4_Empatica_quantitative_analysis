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
    data <- data %>% select(Variation.coefficient) %>% head(60)
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
        session_data <- get_hr_data(participant_folder, "A")
        if (!is.null(session_data)) {
            session_data <- session_data %>% mutate(
                Participant = participant_id,
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
kruskal_test <- kruskal.test(Variation.coefficient ~ Group, data = hr_data)
print(kruskal_test)

# # Post-hoc analysis with Dunn's test
dunn_test <- dunnTest(Variation.coefficient ~ Group, data = hr_data, method = "bonferroni")
print(dunn_test)

# # Extract significant comparisons
#dunn_results <- dunn_test$res

pairwise_test <- pairwise.wilcox.test(hr_data$Variation.coefficient, hr_data$Group, p.adjust.method = "bonferroni")
print(pairwise_test)