
library(dplyr)
library(tibble)
library(tidyverse)

base_path <- "D:/MIT project/2024_06 E4 Data/Cleaned data"
# List all files in the basic path
stream_folders <- list.dirs(base_path, recursive = FALSE)
participant_file_path <- "D:/MIT project/2024_06 E4 Data/participants.csv" 

# FORMAT
# participants.csv is in the format:
# ID;Order;A;B;C;D

participant_data <- read.csv(participant_file_path, header = TRUE, stringsAsFactors = FALSE, sep = ";")

get_stat_var_coeff <- function(file_path) {
  data_file <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE, sep = ";")
  coeff_var_data <- data_file %>% select(Variation.coefficient) %>% head(60)
  
  # Calculer la moyenne et la variance
  mean_value <- mean(coeff_var_data$Variation.coefficient)
  variance_value <- sd(coeff_var_data$Variation.coefficient)
  max_value <- max(coeff_var_data$Variation.coefficient)
  min_value <- min(coeff_var_data$Variation.coefficient)
  # on return les deux valeurs
  return(c(mean_value, variance_value, max_value, min_value))
}

# Les différentes sessions sont 1, 2, 3, 4
list_sum_mean_var_coef <- c(0, 0, 0, 0)
list_sum_variance_var_coef <- c(0, 0, 0, 0)
list_sum_max_var_coef <- c(0, 0, 0, 0)
list_sum_min_var_coef <- c(0, 0, 0, 0)

for (participant_folder in stream_folders) {
  participant_ID <- basename(participant_folder)
  participant_order <- participant_data[participant_data$ID == participant_ID, "Order"]
  
  for (i in 1:4) {
    file_path <- file.path(participant_folder, "HR", paste(substr(participant_order, i, i), "_HR.csv", sep=""))
    # Calculer la moyenne et la variance du coefficient de variation
    stat_var_coef <- get_stat_var_coeff(file_path)
    list_sum_mean_var_coef[i] <- list_sum_mean_var_coef[i] + stat_var_coef[1]
    list_sum_variance_var_coef[i] <- list_sum_variance_var_coef[i] + stat_var_coef[2]
    list_sum_max_var_coef[i] <- list_sum_max_var_coef[i] + stat_var_coef[3]
    list_sum_min_var_coef[i] <- list_sum_min_var_coef[i] + stat_var_coef[4]
  }
}

# Calculer la moyenne des moyennes et variances
mean_mean_var_coef <- list_sum_mean_var_coef / length(stream_folders)
mean_variance_var_coef <- list_sum_variance_var_coef / length(stream_folders)
mean_max_var_coef <- list_sum_max_var_coef / length(stream_folders)
mean_min_var_coef <- list_sum_min_var_coef / length(stream_folders)

# Afficher les résultats
cat("Order of sessions: 1, 2, 3, 4\n")
cat("Mean of mean variation coefficient for each session:", mean_mean_var_coef, "\n")
cat("Mean of variance of variation coefficient for each session:", mean_variance_var_coef, "\n")
cat("Mean of max variation coefficient for each session:", mean_max_var_coef, "\n")
cat("Mean of min variation coefficient for each session:", mean_min_var_coef, "\n")