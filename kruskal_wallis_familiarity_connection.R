library(dplyr)
library(tidyr)
library(ggplot2)
library(FSA)
library(ggsignif)

# This script performs a Kruskal-Wallis test to analyze the relationship between familiarity levels 
# and connection data, and generates visualizations to illustrate the results.

# Base paths
base_path <- "cleaned_data"
participants_data <- readRDS("data_rds/participants.rds")
stream_folders <- list.dirs(base_path, recursive = FALSE)
# Combine HR data with participant information
# File processing loop
hr_data <- data.frame()

for (participant_folder in stream_folders) {
    participant_id <- substr(basename(participant_folder), 1, 3)
    participant_id <- toupper(trimws(participant_id))  # Make sure the ID is formatted correctly

    # Extract the participant's information with Subset () instead of filter ()
    participant_info <- subset(participants_data, ID == participant_id)
    
    # Check if information is found
    if (nrow(participant_info) == 0) {
        print(paste("No info for the participant:", participant_id))
        next
    }
    
    familiarity <- substr(participant_info$Familiarity, 1, 1)
    
    if (familiarity == "H") {
        familiarity <- "S"
    }

    connection_A <- participant_info$Connection_A
    connection_B <- participant_info$Connection_B
    
    # Create a data frame with the ID and the connection
    session_data <- data.frame(ID = participant_id, Connection = (connection_A + connection_B) / 2)
    
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
}

# Ensure that hr_data is indeed a data.frame before filtering
hr_data <- as.data.frame(hr_data)

# Filter valid familiarity with subset instead of filter
hr_data <- subset(hr_data, Familiarity %in% c("Acquaintance", "Friend", "Stranger", "Family", "Self"))

# Perform the Kruskal-Wallis test
kruskal_test <- kruskal.test(Connection ~ Familiarity, data = hr_data)
print(kruskal_test)

# Post-hoc analysis with the Dunn test
dunn_test <- dunnTest(Connection ~ Familiarity, data = hr_data, method = "bonferroni")
print(dunn_test)
