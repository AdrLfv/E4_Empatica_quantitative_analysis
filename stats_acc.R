library(signal)
library(pracma)
library(dplyr)
library(FSA)
library(ggsignif)
library(Hmisc)
library(ggplot2)

# This script processes accelerometer (ACC) data by calculating the magnitude of acceleration from the x, y, and z components and centering the data for further analysis.

source("D:/path_to_folder/E4_quantitative_analysis/center_ACC.R")

base_path <- "D:/path_to_folder/Cleaned data"
stream_folders <- list.dirs(base_path, recursive = FALSE)

sessions <- c("A", "B", "C", "D")
global_data <- data.frame()

# Browse the participants' file
for (participant_folder in stream_folders) {
    participant_id <- substr(basename(participant_folder), 1, 3)
    # mean_acc_cumul <- 0 
    # print(participant_id)
    # sums_list <- list()
    for (i in 1:4) {
        session_acc <- center_ACC(participant_folder, sessions[i]) %>% head(60*32) %>% pull()
        # sum of the absolute value of all the values ​​of acceleration
        # mean_acc_cumul <- mean_acc_cumul + sum(abs(session_acc))/length(session_acc)  
        # sums_list[[i]] <- sum(abs(session_acc))/(length(session_acc)*100)
        standard_deviation <- sd(session_acc)
        cat(standard_deviation, ";")
        # global_data <- rbind(global_data, data.frame(Participant = participant_id, Session = sessions[i], ACC = session_acc))
    }
    print("\n")
    # for (i in 1:4) {
    #     cat(sums_list[[i]], ";", sep="")
    # }
    # mean_acc <- (mean_acc_cumul / 4)/100
    # cat(participant_id, ";", mean_acc, "\n", sep="")
}

# Display the overall average of the variation
# mean_variation <- mean(global_data$ACC, na.rm = TRUE)
# standard_deviation <- sd(global_data$ACC, na.rm = TRUE)
# cat("Mean Delta_Heart_Rate:", mean_variation, "\n")
# cat("Standard Deviation:", standard_deviation, "\n")