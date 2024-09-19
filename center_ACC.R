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

base_path <- "cleaned_data"
stream_folders <- list.dirs(base_path, recursive = FALSE)
participants_data <- readRDS("data_rds/participants.rds")

center_ACC <- function(participant_folder, session) {

  file_path <- file.path(participant_folder, "ACC", paste0(session, "_ACC.rds"))
  if (!file.exists(file_path)) return(NULL)
  acc_data <- readRDS(file_path)
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

  return(acc_data)
}