
library(dplyr)
library(tibble)
library(tidyverse)

base_path <- "D:/MIT project/2024_06 E4 Data/Cleaned data"
# Lister tous les dossiers dans le chemin de base
stream_folders <- list.dirs(base_path, recursive = FALSE)

get_stat_ACC <- function(file_path) {
  data_file <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE, sep = ";")
  acc_data <- data_file %>% select(ACC) %>% head(60)
  
  # Calculer la moyenne et la variance
  mean_value <- mean(acc_data$ACC-33)
  variance_value <- sd(acc_data$ACC-33)
  max_value <- max(acc_data$ACC-33)
  min_value <- min(acc_data$ACC-33)
  # on return les deux valeurs
  return(c(mean_value, variance_value, max_value, min_value))
}

sum_mean_acc_list <- c(0, 0, 0, 0)
sum_variance_acc_list <- c(0, 0, 0, 0)
sum_max_acc_list <- c(0, 0, 0, 0)
sum_min_acc_list <- c(0, 0, 0, 0)
sessions_order <- c("A", "B", "C", "D")

for (participant_folder in stream_folders) {
  for (i in 1:4) {
      file_path <- file.path(participant_folder, "ACC", paste(sessions_order[i], "_ACC.csv", sep=""))
      # Calculer la moyenne et la variance du coefficient de variation
      stat_ACC <- get_stat_ACC(file_path)
      sum_mean_acc_list[i] <- sum_mean_acc_list[i] + stat_ACC[1]
      sum_variance_acc_list[i] <- sum_variance_acc_list[i] + stat_ACC[2]
      sum_max_acc_list[i] <- sum_max_acc_list[i] + stat_ACC[3]
      sum_min_acc_list[i] <- sum_min_acc_list[i] + stat_ACC[4]
  }
}

# Calculer la moyenne des moyennes
mean_acc_list <- sum_mean_acc_list / length(stream_folders)
mean_var_acc_list <- sum_variance_acc_list / length(stream_folders)
mean_max_acc_list <- sum_max_acc_list / length(stream_folders)
mean_min_acc_list <- sum_min_acc_list / length(stream_folders)

# Afficher les résultats
cat("Order of sessions: A, B, C, D\n")
cat("Mean of mean ACC for each session:", mean_acc_list, "\n")
cat("Mean of variance of ACC for each session:", mean_var_acc_list, "\n")
cat("Mean of max ACC for each session:", mean_max_acc_list, "\n")
cat("Mean of min ACC for each session:", mean_min_acc_list, "\n")