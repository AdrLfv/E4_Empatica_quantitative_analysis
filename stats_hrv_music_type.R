library(dplyr)
library(tibble)

base_path <- "D:/MIT project/2024_06 E4 Data/Cleaned data"
# Lister tous les dossiers dans le chemin de base
stream_folders <- list.dirs(base_path, recursive = FALSE)

get_hrv_data <- function(participant_folder, session) {
    file_path <- file.path(participant_folder, "HR", paste0(session, "_HR.csv"))
    if (!file.exists(file_path)) return(NULL)
    data <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE, sep = ";")
    data <- data %>% select(Variation.coefficient) %>% head(60)
    return(data)
}

global_data <- data.frame()

for (participant_folder in stream_folders) {
  participant_id <- substr(basename(participant_folder), 1, 3)
  
  # if (participant_id %in% c("P05", "P06", "P07", "P11", "P24", "P26")) {
  #     next
  # }
  
  for (session in c("A", "B", "C", "D")) {
    file_path <- file.path(participant_folder, "HR", paste0(session, "_HR.csv"))
    session_group <- ifelse(session %in% c("A", "B"), "Calm", "Dynamic")
    session_data <- get_hrv_data(participant_folder, session) %>% mutate(Group = session_group)
    global_data <- bind_rows(global_data, session_data)
  }
}

# Calculer les moyennes et l'écart-type des statistiques pour chaque groupe
summary_results <- global_data %>%
  group_by(Group) %>%
  summarise(
    Mean_Variation_Coefficient = mean(Variation.coefficient, na.rm = TRUE),
    SD_Variation_Coefficient = sd(Variation.coefficient, na.rm = TRUE)
  )

# Afficher les résultats
print(summary_results)
