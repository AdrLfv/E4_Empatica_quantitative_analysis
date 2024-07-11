
library(dplyr)
library(tibble)
library(tidyverse)

base_path <- "D:/MIT project/2024_06 E4 Data/Cleaned data"
# Lister tous les dossiers dans le chemin de base
stream_folders <- list.dirs(base_path, recursive = FALSE)

get_mean_var_coeff <- function(file_path) {
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

sum_mean_var_coeff_A <- 0
sum_mean_var_coeff_B <- 0
sum_mean_var_coeff_C <- 0
sum_mean_var_coeff_D <- 0

sum_variance_var_coeff_A <- 0
sum_variance_var_coeff_B <- 0
sum_variance_var_coeff_C <- 0
sum_variance_var_coeff_D <- 0

sum_max_var_coeff_A <- 0
sum_max_var_coeff_B <- 0
sum_max_var_coeff_C <- 0
sum_max_var_coeff_D <- 0

sum_min_var_coeff_A <- 0
sum_min_var_coeff_B <- 0
sum_min_var_coeff_C <- 0
sum_min_var_coeff_D <- 0

for (participant_folder in stream_folders) {
  sum_mean_var_coeff_A <- sum_mean_var_coeff_A + get_mean_var_coeff(file.path(participant_folder, "HR/A_HR.csv"))[1]
  sum_mean_var_coeff_B <- sum_mean_var_coeff_B + get_mean_var_coeff(file.path(participant_folder, "HR/B_HR.csv"))[1]
  sum_mean_var_coeff_C <- sum_mean_var_coeff_C + get_mean_var_coeff(file.path(participant_folder, "HR/C_HR.csv"))[1]
  sum_mean_var_coeff_D <- sum_mean_var_coeff_D + get_mean_var_coeff(file.path(participant_folder, "HR/D_HR.csv"))[1]
  
  sum_variance_var_coeff_A <- sum_variance_var_coeff_A + get_mean_var_coeff(file.path(participant_folder, "HR/A_HR.csv"))[2]
  sum_variance_var_coeff_B <- sum_variance_var_coeff_B + get_mean_var_coeff(file.path(participant_folder, "HR/B_HR.csv"))[2]
  sum_variance_var_coeff_C <- sum_variance_var_coeff_C + get_mean_var_coeff(file.path(participant_folder, "HR/C_HR.csv"))[2]
  sum_variance_var_coeff_D <- sum_variance_var_coeff_D + get_mean_var_coeff(file.path(participant_folder, "HR/D_HR.csv"))[2]

  sum_max_var_coeff_A <- sum_max_var_coeff_A + get_mean_var_coeff(file.path(participant_folder, "HR/A_HR.csv"))[3]
  sum_max_var_coeff_B <- sum_max_var_coeff_B + get_mean_var_coeff(file.path(participant_folder, "HR/B_HR.csv"))[3]
  sum_max_var_coeff_C <- sum_max_var_coeff_C + get_mean_var_coeff(file.path(participant_folder, "HR/C_HR.csv"))[3]
  sum_max_var_coeff_D <- sum_max_var_coeff_D + get_mean_var_coeff(file.path(participant_folder, "HR/D_HR.csv"))[3]

  sum_min_var_coeff_A <- sum_min_var_coeff_A + get_mean_var_coeff(file.path(participant_folder, "HR/A_HR.csv"))[4]
  sum_min_var_coeff_B <- sum_min_var_coeff_B + get_mean_var_coeff(file.path(participant_folder, "HR/B_HR.csv"))[4]
  sum_min_var_coeff_C <- sum_min_var_coeff_C + get_mean_var_coeff(file.path(participant_folder, "HR/C_HR.csv"))[4]
  sum_min_var_coeff_D <- sum_min_var_coeff_D + get_mean_var_coeff(file.path(participant_folder, "HR/D_HR.csv"))[4]
}

# Calculer la moyenne des moyennes
mean_mean_var_coeff_A <- sum_mean_var_coeff_A / length(stream_folders)
mean_mean_var_coeff_B <- sum_mean_var_coeff_B / length(stream_folders)
mean_mean_var_coeff_C <- sum_mean_var_coeff_C / length(stream_folders)
mean_mean_var_coeff_D <- sum_mean_var_coeff_D / length(stream_folders)

# Calculer la moyenne des variances
mean_variance_var_coeff_A <- sum_variance_var_coeff_A / length(stream_folders)
mean_variance_var_coeff_B <- sum_variance_var_coeff_B / length(stream_folders)
mean_variance_var_coeff_C <- sum_variance_var_coeff_C / length(stream_folders)
mean_variance_var_coeff_D <- sum_variance_var_coeff_D / length(stream_folders)

# Calculer la moyenne des max
mean_max_var_coeff_A <- sum_max_var_coeff_A / length(stream_folders)
mean_max_var_coeff_B <- sum_max_var_coeff_B / length(stream_folders)
mean_max_var_coeff_C <- sum_max_var_coeff_C / length(stream_folders)
mean_max_var_coeff_D <- sum_max_var_coeff_D / length(stream_folders)

# Calculer la moyenne des min
mean_min_var_coeff_A <- sum_min_var_coeff_A / length(stream_folders)
mean_min_var_coeff_B <- sum_min_var_coeff_B / length(stream_folders)
mean_min_var_coeff_C <- sum_min_var_coeff_C / length(stream_folders)
mean_min_var_coeff_D <- sum_min_var_coeff_D / length(stream_folders)

# Afficher les résultats
cat("Mean of variation coefficient for A:", mean_mean_var_coeff_A, "\n")
cat("Mean of variation coefficient for B:", mean_mean_var_coeff_B, "\n")
cat("Mean of variation coefficient for C:", mean_mean_var_coeff_C, "\n")
cat("Mean of variation coefficient for D:", mean_mean_var_coeff_D, "\n\n")

cat("Mean of variation coefficient variance for A:", mean_variance_var_coeff_A, "\n")
cat("Mean of variation coefficient variance for B:", mean_variance_var_coeff_B, "\n")
cat("Mean of variation coefficient variance for C:", mean_variance_var_coeff_C, "\n")
cat("Mean of variation coefficient variance for D:", mean_variance_var_coeff_D, "\n\n")

cat("Mean of variation coefficient max for A:", mean_max_var_coeff_A, "\n")
cat("Mean of variation coefficient max for B:", mean_max_var_coeff_B, "\n")
cat("Mean of variation coefficient max for C:", mean_max_var_coeff_C, "\n")
cat("Mean of variation coefficient max for D:", mean_max_var_coeff_D, "\n\n")

cat("Mean of variation coefficient min for A:", mean_min_var_coeff_A, "\n")
cat("Mean of variation coefficient min for B:", mean_min_var_coeff_B, "\n")
cat("Mean of variation coefficient min for C:", mean_min_var_coeff_C, "\n")
cat("Mean of variation coefficient min for D:", mean_min_var_coeff_D, "\n\n")