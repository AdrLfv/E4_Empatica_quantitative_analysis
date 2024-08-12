library(dplyr)
library(tidyr)
library(ggplot2)
library(FSA)
library(ggsignif)


base_path <- "D:/MIT project/2024_06 E4 Data/Cleaned data"
# Lister tous les dossiers dans le chemin de base
stream_folders <- list.dirs(base_path, recursive = FALSE)
participant_file <- "D:/MIT project/2024_06 E4 Data/participants.csv"
participant_data <- read.csv(participant_file, header = TRUE, stringsAsFactors = FALSE, sep = ";")

min_p_value_HR <- 10000
max_p_value_HR <- 0
min_p_value_HR_coeff <- 10000
max_p_value_HR_coeff <- 0

shapirowilk_test <- function(participant_folder, col1, col2) {
  participant_id <- substr(basename(participant_folder), 1, 3)
  connection <- participant_data %>% filter(ID == participant_id) %>% select(col1, col2)
  # creation d'un vecteur
  print(head(connection, 10))
  data <- c()
  # pour chaque ligne du fichier
  for (line in 1:28) {
    # on ajoute au vecteur la valeur de la ligne colonne col2 - la valeur de la ligne colonne col1
    data <- c(data, connection$col2[line] - connection$col1[line])    
  }
  # Perform Shapiro-Wilk normality test
  shapiro_test <- shapiro.test(data)
  
  return (shapiro_test$p.value)
}

p_values_A <- c()
p_values_B <- c()

for (participant_folder in stream_folders) {
  test_A <- shapirowilk_test(participant_folder, "A_diagram_before", "A_diagram_after")
  test_B <- shapirowilk_test(participant_folder, "B_diagram_before", "B_diagram_after")
  
  p_values_A <- c(p_values_A, test_A)
  p_values_B <- c(p_values_B, test_B)
}

cat("Min p-value for A:", min(p_values_A), "\n")
cat("Max p-value for A:", max(p_values_A), "\n")
cat("Min p-value for B:", min(p_values_B), "\n")
cat("Max p-value for B:", max(p_values_B), "\n")