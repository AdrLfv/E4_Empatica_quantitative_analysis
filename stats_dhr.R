library(signal)
library(pracma)
library(dplyr)
library(FSA)
library(ggsignif)
library(Hmisc)
library(ggplot2)

# This script displays the mean delta heart rate (ΔHR) data for each participant and session.

# Function to retrieve HR data
get_hrv_data <- function(participant_folder, session) {
    file_path <- file.path(participant_folder, "HR", paste0(session, "_HR.csv"))
    if (!file.exists(file_path)) return(NULL)
    data <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE, sep = ";")
    data <- data %>% select(Delta_Heart_Rate)
    return(data)
}

base_path <- "D:/path_to_folder/Cleaned data"
stream_folders <- list.dirs(base_path, recursive = FALSE)

sessions <- c("A", "B", "C", "D")
global_data <- data.frame()

# Browse the participants' file
for (participant_folder in stream_folders) {
    participant_id <- substr(basename(participant_folder), 1, 3)
    # print(participant_id)
    for (i in 1:4) {
        session_data <- get_hrv_data(participant_folder, sessions[i]) %>% head(60) %>% pull()
        # print(head(session_data))
        # amplitude <- max(session_data) - min(session_data)
        mean_variation <- mean(session_data, na.rm = TRUE)
        cat(mean_variation, ";")
        # standard_deviation <- sd(session_data)
        # cat(standard_deviation, ";")
        # global_data <- rbind(global_data, data.frame(Participant = participant_id, Session = sessions[i], Delta_Heart_Rate = session_data))
    }
    print("\n")
}

# mean_variation <- mean(global_data$Delta_Heart_Rate, na.rm = TRUE)
# standard_deviation <- sd(global_data$Delta_Heart_Rate, na.rm = TRUE)
# cat("Mean Delta_Heart_Rate:", mean_variation, "\n")
# cat("Standard Deviation:", standard_deviation, "\n")