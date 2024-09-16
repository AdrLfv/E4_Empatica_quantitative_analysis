# Install the necessary packages if it is not already done
# install.packages("signal")
# install.packages("pracma")
# install.packages("dplyr")
# install.packages("FSA")
# install.packages("ggsignif")

library(signal)
library(pracma)
library(dplyr)
library(FSA)
library(ggsignif)

# This script processes accelerometer (ACC) data by calculating the magnitude of acceleration from the x, y, and z components and centering the data for further analysis.

base_path <- "D:/path_to_folder/Cleaned data"
stream_folders <- list.dirs(base_path, recursive = FALSE)
participant_path <- "D:/path_to_folder/participants.csv"
participant_data <- read.table(participant_path, header = TRUE, sep = ";", stringsAsFactors = FALSE)

center_ACC <- function(participant_folder, session) {
  file_path <- file.path(participant_folder, "ACC", paste(session, "_ACC.csv", sep=""))
  data_file <- read.table(file_path, header = TRUE, stringsAsFactors = FALSE, sep = ";") 
  acc_data <- data_file 
  acc_data$time_seconds <- as.numeric(as.POSIXct(acc_data$Time, format = "%H:%M:%S"))

  acc_data <- acc_data %>%
  mutate(
    ACC = sqrt(acc_data$ACC_x^2 + acc_data$ACC_y^2 + acc_data$ACC_z^2)
  )

  # Method with standard deviation
  #acc_data$ACC = (acc_data$ACC - mean(acc_data$ACC)) / sd(acc_data$ACC)
  
  # Typical deviation method
  acc_data$ACC = acc_data$ACC - mean(acc_data$ACC)
  acc_data <- acc_data %>% select(ACC)

  # Save standard data
  # write.csv(acc_data, file.path(participant_folder, "ACC", paste0(session, "_ACC_normalised.csv")), row.names = FALSE)

  return(acc_data)
}