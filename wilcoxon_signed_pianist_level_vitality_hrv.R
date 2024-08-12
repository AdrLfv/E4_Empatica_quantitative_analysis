library(dplyr)
library(tibble)
library(tidyverse)
library(openxlsx)

# Définir le chemin de base
cleaned_data_path <- "D:/MIT project/2024_06 E4 Data/Cleaned data"
stream_folders <- list.dirs(cleaned_data_path, recursive = FALSE)

get_data <- function(indep_variable, participant_id) {
  # Boucle sur chaque participant
  participant_path <- file.path(cleaned_data_path, participant_id, "HR")
  
  # Lire les fichiers "A_HR" et "B_HR"
  if (indep_variable == "pro") {
    a_hr_path <- file.path(participant_path, "C_HR.csv")
    b_hr_path <- file.path(participant_path, "D_HR.csv")
  } else if (indep_variable == "nonpro") {
    a_hr_path <- file.path(participant_path, "A_HR.csv")
    b_hr_path <- file.path(participant_path, "B_HR.csv")
  } else if (indep_variable == "alive") {
    a_hr_path <- file.path(participant_path, "A_HR.csv")
    b_hr_path <- file.path(participant_path, "C_HR.csv")
  }
  else if (indep_variable == "deceased") {
    a_hr_path <- file.path(participant_path, "B_HR.csv")
    b_hr_path <- file.path(participant_path, "D_HR.csv")
  }
  
  # Charger les données
  a_hr_data <- read.csv(a_hr_path, header = TRUE, stringsAsFactors = FALSE, sep = ";")
  b_hr_data <- read.csv(b_hr_path, header = TRUE, stringsAsFactors = FALSE, sep = ";")
  
  # Sélectionner la colonne appropriée en fonction de data_type
  a_hr_data <- a_hr_data %>% select(Variation.coefficient) %>% head(60)
  b_hr_data <- b_hr_data %>% select(Variation.coefficient) %>% head(60)

  # Ajouter le temps généré et les colonnes `Participant ID` et `Vital Status`
  if (indep_variable == "pro" || indep_variable == "nonpro") {
    a_hr_data <- a_hr_data %>% mutate(ID = participant_id, Dep_variable = "alive")
    b_hr_data <- b_hr_data %>% mutate(ID = participant_id, Dep_variable = "deceased")
    column_name <- "Vital Status"
  } else if (indep_variable == "alive" || indep_variable == "deceased") {
    a_hr_data <- a_hr_data %>% mutate(ID = participant_id, Dep_variable = "nonpro")
    b_hr_data <- b_hr_data %>% mutate(ID = participant_id, Dep_variable = "pro")
    column_name <- "Level"
  }
  
  return(bind_rows(a_hr_data, b_hr_data))
}

indep_variables <- c("pro", "nonpro", "alive", "deceased")
for (indep_variable in indep_variables) {
  all_data <- data.frame()
  for (participant_folder in stream_folders) {
    participant_id <- substr(basename(participant_folder), 1, 3)

    data <- get_data(indep_variable, participant_id)
    all_data <- bind_rows(all_data, data)
  }
  
  # Sauvegarder les données dans un fichier Excel
  # write.xlsx(all_data, file.path("D:/MIT project/2024_06 E4 Data/", paste("formatted_HR_", indep_variable, ".xlsx", sep="")), rowNames = FALSE)

  # Sélectionner les colonnes pertinentes et séparer les données selon le statut vital
  if (indep_variable == "pro" || indep_variable == "nonpro") {
    a_hr_data <- all_data %>% filter(Dep_variable == "alive") %>% pull(Variation.coefficient)
    b_hr_data <- all_data %>% filter(Dep_variable == "deceased") %>% pull(Variation.coefficient)
  } else if (indep_variable == "alive" || indep_variable == "deceased") {
    a_hr_data <- all_data %>% filter(Dep_variable == "pro") %>% pull(Variation.coefficient)
    b_hr_data <- all_data %>% filter(Dep_variable == "nonpro") %>% pull(Variation.coefficient)
  }

  # Afficher les résultats
  cat("Variable:", indep_variable, "\n")
  # Effectuer le test de Wilcoxon
  wilcox_test_result <- wilcox.test(a_hr_data, b_hr_data, paired = TRUE)
  print(wilcox_test_result)
}
