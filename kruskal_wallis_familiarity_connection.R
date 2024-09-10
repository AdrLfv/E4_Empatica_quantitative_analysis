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
    # We do not exclude anyone before everyone listened to Marvin
    # if (participant_id == "P05" | participant_id == "P06" | participant_id == "P07" | participant_id == "P11" | participant_id == "P24" | participant_id == "P26") {
    #     next
    # }
    participant_info <- participants %>% filter(ID == participant_id)
    familiarity <- substr(participant_info$Familiarity, 1, 1)

    if (familiarity == "H") {
        familiarity <- "S"
    }

    # if (familiarity %in% c("A", "F", "S")) {
    connection_A <- participant_info$Connection_A
    connection_B <- participant_info$Connection_B
    # on construit un data frame avec une colonne ID (de participants) et une colonne connection
    session_data <- data.frame(ID = participant_id, Connection = (connection_A+connection_B)/2)
    if (!is.null(session_data)) {
        session_data <- session_data %>% mutate(
            Familiarity = case_when(
                familiarity == "A" ~ "Acquaintance",
                familiarity == "F" ~ "Friend",
                familiarity == "S" ~ "Stranger",
                familiarity == "R" ~ "Family",
                familiarity == "P" ~ "Self"
            )
        )
        hr_data <- bind_rows(hr_data, session_data)
    }
    # }
}

# Filter data for valid Familiaritys (should not be necessary)
hr_data <- hr_data %>% filter(Familiarity %in% c("Acquaintance", "Friend", "Stranger", "Family", "Self"))

# Perform Kruskal-Wallis test
kruskal_test <- kruskal.test(Connection ~ Familiarity, data = hr_data)
print(kruskal_test)

# # Post-hoc analysis with Dunn's test
dunn_test <- dunnTest(Connection ~ Familiarity, data = hr_data, method = "bonferroni")
print(dunn_test)
