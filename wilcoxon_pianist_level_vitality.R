library(dplyr)
library(tibble)
library(tidyverse)
library(openxlsx)

# Définir le chemin de base
cleaned_data_path <- "D:/MIT project/2024_06 E4 Data/Cleaned data"

# Vérifier si le dossier existe, sinon le créer
if (!dir.exists(file.path(cleaned_data_path, "formatted_data"))) {
  dir.create(file.path(cleaned_data_path, "formatted_data"), recursive = TRUE)
}

# Initialiser une liste pour stocker les données
all_data <- list()

data_type <- "HR"
variables <- c("pro", "nonpro", "alive", "deceased")

prepare_data <- function(data_type, variable) {
  # Boucle sur chaque participant
  for (i in 1:28) {
    participant_id <- sprintf("P%02d", i)
    participant_path <- file.path(cleaned_data_path, participant_id, "HR")
    
    # Lire les fichiers "A_HR" et "B_HR"
    if (variable == "pro") {
      a_hr_path <- file.path(participant_path, "C_HR.csv")
      b_hr_path <- file.path(participant_path, "D_HR.csv")
    } else if (variable == "nonpro") {
      a_hr_path <- file.path(participant_path, "A_HR.csv")
      b_hr_path <- file.path(participant_path, "B_HR.csv")
    } else if (variable == "alive") {
      a_hr_path <- file.path(participant_path, "A_HR.csv")
      b_hr_path <- file.path(participant_path, "C_HR.csv")
    }
    else if (variable == "deceased") {
      a_hr_path <- file.path(participant_path, "B_HR.csv")
      b_hr_path <- file.path(participant_path, "D_HR.csv")
    }
    
    if (file.exists(a_hr_path) & file.exists(b_hr_path)) {
      print(paste("Processing data for participant", participant_id))
      
      # Charger les données
      a_hr_data <- read.csv(a_hr_path, header = TRUE, stringsAsFactors = FALSE, sep = ";")
      b_hr_data <- read.csv(b_hr_path, header = TRUE, stringsAsFactors = FALSE, sep = ";")
      
      # Sélectionner la colonne appropriée en fonction de data_type
      a_hr_data <- a_hr_data %>% select(HR) %>% head(60)
      b_hr_data <- b_hr_data %>% select(HR) %>% head(60)
      
      # Ajouter le temps généré et les colonnes `Participant ID` et `Vital Status`
      if (variable == "pro") {
        a_hr_data <- a_hr_data %>% mutate(`Participant ID` = participant_id, `Vital Status` = "alive")
        b_hr_data <- b_hr_data %>% mutate(`Participant ID` = participant_id, `Vital Status` = "deceased")
        column_name <- "Vital Status"
      } else if (variable == "nonpro") {
        a_hr_data <- a_hr_data %>% mutate(`Participant ID` = participant_id, `Vital Status` = "alive")
        b_hr_data <- b_hr_data %>% mutate(`Participant ID` = participant_id, `Vital Status` = "deceased")
        column_name <- "Vital Status"
      } else if (variable == "alive") {
        a_hr_data <- a_hr_data %>% mutate(`Participant ID` = participant_id, `Level` = "nonpro")
        b_hr_data <- b_hr_data %>% mutate(`Participant ID` = participant_id, `Level` = "pro")
        column_name <- "Level"
      } else if (variable == "deceased") {
        a_hr_data <- a_hr_data %>% mutate(`Participant ID` = participant_id, `Level` = "nonpro")
        b_hr_data <- b_hr_data %>% mutate(`Participant ID` = participant_id, `Level` = "pro")
        column_name <- "Level"
      }
      
      # Combiner les données
      participant_data <- bind_rows(a_hr_data, b_hr_data)
      
      # Ajouter aux données totales
      all_data[[participant_id]] <- participant_data
    } else {
      print(paste("Missing data for participant", participant_id))
    }
  }

  # Combiner toutes les données des participants
  final_data <- bind_rows(all_data)

  # Réorganiser les colonnes pour le format requis
  final_data <- final_data %>% select(`Participant ID`, HR, column_name)

  # Sauvegarder les données dans un fichier Excel
  write.xlsx(final_data, file.path(cleaned_data_path, "formatted_data", paste("formatted_", data_type, "_", variable, ".xlsx", sep = "")))
}

# Préparer les données pour chaque combinaison de type de données, niveau de pianiste et statut vital
for (variable in variables) {
  prepare_data(data_type, variable)
}

for (variable in variables) {
  # Charger les données depuis le fichier Excel
  file_path <- file.path("D:/MIT project/2024_06 E4 Data/Cleaned data/formatted_data", paste("formatted_HR_", variable, ".xlsx", sep=""))
  data <- read_excel(file_path)

  if (variable == "pro" || variable == "nonpro") {
    # Sélectionner les colonnes pertinentes
    data <- data %>% select(`HR`, `Participant ID`, `Vital Status`)
    
    # Séparer les données selon le statut vital
    a_hr_data <- data %>% filter(`Vital Status` == "alive") %>% pull(`HR`)
    b_hr_data <- data %>% filter(`Vital Status` == "deceased") %>% pull(`HR`)
  } else if (variable == "alive" || variable == "deceased") {
    # Sélectionner les colonnes pertinentes
    data <- data %>% select(`HR`, `Participant ID`, `Level`)
    
    # Séparer les données selon le statut vital
    a_hr_data <- data %>% filter(`Level` == "pro") %>% pull(`HR`)
    b_hr_data <- data %>% filter(`Level` == "nonpro") %>% pull(`HR`)
  } 
  
  
  # Afficher les résultats
  cat("Variable:", variable, "\n")
  # Effectuer le test de Wilcoxon
  wilcox_test_result  <- wilcox.test(a_hr_data, b_hr_data, paired = TRUE)
  print(wilcox_test_result)
}
