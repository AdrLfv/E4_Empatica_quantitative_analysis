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
  return(c(mean_value, variance_value, max_value, min_value))
}

# Initialiser les listes pour stocker les résultats
results <- tibble(
  Group = character(),
  Mean = double(),
  Variance = double(),
  Max = double(),
  Min = double()
)

for (participant_folder in stream_folders) {
  participant <- substr(basename(participant_folder), 1, 3)
  
  if (participant_id == "P05" | participant_id == "P06" | participant_id == "P07" | participant_id == "P11" | participant_id == "P24" | participant_id == "P26") {
      next
  }
  for (session in c("A", "B", "C", "D")) {
    file_path <- file.path(participant_folder, "HR", paste0(session, "_HR.csv"))
    # si la session in c("A", "B") le group est "calm" sinon "dynamic"
    session_group <- ifelse(session %in% c("A", "B"), "Calm", "Dynamic")
    if (file.exists(file_path)) {
      session_results <- get_mean_var_coeff(file_path)
      results <- results %>% add_row(
        Group = session_group,
        Mean = session_results[1],
        Variance = session_results[2],
        Max = session_results[3],
        Min = session_results[4]
      )
    }
  }
}

# Calculer les moyennes des statistiques pour chaque session
summary_results <- results %>%
  group_by(Group) %>%
  summarise(
    Mean_Mean = mean(Mean),
    Mean_Variance = mean(Variance),
    Mean_Max = mean(Max),
    Mean_Min = mean(Min)
  )

# Afficher les résultats
print(summary_results)
