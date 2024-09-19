library(signal)
library(pracma)
library(dplyr)
library(FSA)
library(ggsignif)
library(Hmisc)
library(ggplot2)
library(tidyr)

# This script processes accelerometer (ACC) data by calculating the magnitude of acceleration 
# from the x, y, and z components and centering the data for further analysis.

source("center_ACC.R")

base_path <- "cleaned_data"
stream_folders <- list.dirs(base_path, recursive = FALSE)

sessions <- c("A", "B", "C", "D")
mean_data <- data.frame()
sd_data <- data.frame()

# Browse the participants' file
for (participant_folder in stream_folders) {
    participant_id <- substr(basename(participant_folder), 1, 3)
    
    for (i in 1:4) {
        session_acc <- center_ACC(participant_folder, sessions[i]) %>% head(60*32) %>% pull()

        mean <- mean(session_acc)
        standard_deviation <- sd(session_acc)

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
print("Mean of Acceleration Data:")
print(mean_data_wide, n = 28)

# Display the overall standard deviation of the variation
print("Standard Deviation of Acceleration Data:")
print(sd_data_wide, n = 28)