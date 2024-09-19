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
    file_path <- file.path(participant_folder, "HR", paste0(session, "_HR.rds"))
    if (!file.exists(file_path)) return(NULL)
    data <- readRDS(file_path)
    data <- data %>% select(Delta_Heart_Rate)
    return(data)
}

base_path <- "cleaned_data"
stream_folders <- list.dirs(base_path, recursive = FALSE)

sessions <- c("A", "B", "C", "D")
mean_data <- data.frame()
sd_data <- data.frame()

# Browse the participants' file
for (participant_folder in stream_folders) {
    participant_id <- substr(basename(participant_folder), 1, 3)
    # print(participant_id)
    for (i in 1:4) {
        session_data <- get_hrv_data(participant_folder, sessions[i]) %>% head(60) %>% pull()
        
        mean <- mean(session_data)
        standard_deviation <- sd(session_data)

        mean_data <- rbind(mean_data, data.frame(Participant = participant_id, Session = sessions[i], Mean = mean))
        sd_data <- rbind(sd_data, data.frame(Participant = participant_id, Session = sessions[i], SD = standard_deviation))
    }
}

# Pivot the data to have Participants as rows and Sessions as columns
mean_data_wide <- mean_data %>% 
    pivot_wider(names_from = Session, values_from = Mean)
sd_data_wide <- sd_data %>%
    pivot_wider(names_from = Session, values_from = SD)

# Display the overall average of the variation
print("Mean of HR Data:")
print(mean_data_wide, n = 28)

# Display the overall standard deviation of the variation
print("Standard Deviation of HR Data:")
print(sd_data_wide, n = 28)