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
session_order <- c("A", "B", "C", "D")

for (participant_folder in stream_folders) {
    participant_id <- substr(basename(participant_folder), 1, 3)
    
    for (i in 1:4) {
        session_data <- get_hr_data(participant_folder, session_order[i])
        if (!is.null(session_data)) {
            session_data$Participant <- participant_id
            session_data$Session <- session_order[i]
            hr_data <- rbind(hr_data, session_data)
        }
    }
}

# Prepare data for Friedman test
friedman_data <- hr_data %>%
    group_by(Participant, Session) %>%
    summarise(mean_coeff_var = mean(Variation.coefficient, na.rm = TRUE)) %>%
    spread(key = Session, value = mean_coeff_var)

# Perform Friedman test
friedman_test_result <- friedman.test(as.matrix(friedman_data[,-1]))

# Print test results
print(friedman_test_result)

# Post hoc test if significant
if (friedman_test_result$p.value < 0.05) {
    post_hoc_result <- pairwise.wilcox.test(as.vector(as.matrix(friedman_data[,-1])), 
                                            rep(1:4, each=nrow(friedman_data)), 
                                            p.adjust.method = "bonferroni")
    print(post_hoc_result)
}
