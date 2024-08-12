# Installer les packages nécessaires si ce n'est pas déjà fait
# install.packages("signal")
# install.packages("pracma")
# install.packages("dplyr")
#install.packages("FSA")
#install.packages("ggsignif")

library(signal)
library(pracma)
library(dplyr)
library(FSA)
library(ggsignif)

base_path <- "D:/MIT project/2024_06 E4 Data/Cleaned data"
stream_folders <- list.dirs(base_path, recursive = FALSE)
participant_path <- "D:/MIT project/2024_06 E4 Data/participants.csv"
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

  #méthode avec écart type
  #acc_data$ACC = (acc_data$ACC - mean(acc_data$ACC)) / sd(acc_data$ACC)
  
  #méthode sans écart type
  acc_data$ACC = acc_data$ACC - mean(acc_data$ACC)
  acc_data <- acc_data %>% select(ACC)

  # enregistrer les données normalisées
  # write.csv(acc_data, file.path(participant_folder, "ACC", paste0(session, "_ACC_normalised.csv")), row.names = FALSE)

  return(acc_data)
}