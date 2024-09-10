library(dplyr)
library(tibble)
library(tidyverse)
library(openxlsx)

# Définir le chemin de base
base_path <- "D:/MIT project/2024_06 E4 Data/Cleaned data"
stream_folders <- list.dirs(base_path, recursive = FALSE)

# A: alive, nonpro
# B: deceased, nonpro
# C: alive, pro
# D: deceased, pro

get_session_data <- function(participant_folder, session, participant_id) {
  file_path <- file.path(participant_folder, "HR", paste0(session, "_HR.csv"))
    data_file <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE, sep = ";")
    music_type <- ifelse(session %in% c("A", "B"), "Slow", "Fast")
    level <- ifelse(session %in% c("A", "B"), "Non-pro", "Pro")
    status <- ifelse(session %in% c("A", "C"), "Alive", "Deceased")
    data <- data_file %>%
        select(Variation.coefficient) %>%
        head(60) %>%
        mutate(Participant = participant_id, Session = session, Tempo = music_type, Level = level, Status = status)
  return(data)
}

all_data <- data.frame()

for (participant_folder in stream_folders) {
  participant_id <- substr(basename(participant_folder), 1, 3)
  for (session in c("A", "B", "C", "D")) {
    session_data <- get_session_data(participant_folder, session, participant_id)
    all_data <- bind_rows(all_data, session_data)
  }
}

# Each participant attends sessions A, B, C and D, so the data obtained are matched and therefore not independent, 
# since they come from the same participants. We therefore use a Wilcoxon signed-rank test to compare 
# matched pairs of data (alive and deceased, pro and nonpro): (paired = TRUE)

# Générer et sauvegarder le boxplot
p_Music_type <- ggplot(all_data, aes(x = Tempo, y = Variation.coefficient, fill = Tempo)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  labs(x = "Tempo", y = "Heart Rate Coefficient of Variation") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        text = element_text(family = "Arial", size = 18)) +
  scale_fill_brewer(palette = "Set3")+
  coord_cartesian(ylim = c(-10, 10)) +  # Définir les limites de l'axe y
  scale_y_continuous(breaks = seq(-10, 10, by = 5))  # Définir les indices de l'axe y toutes les 5 unités
ggsave("D:/MIT project/2024_06 E4 Data/boxplot_Music_type.png", plot = p_Music_type, width = 10, height = 6)

# Générer et sauvegarder le boxplot
p_vitality <- ggplot(all_data, aes(x = Status, y = Variation.coefficient, fill = Status)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  labs(x = "Pianists Vital Status", y = "Heart Rate Coefficient of Variation") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        text = element_text(family = "Arial", size = 18)) +
  scale_fill_brewer(palette = "Set3") +
  coord_cartesian(ylim = c(-10, 10)) +  # Définir les limites de l'axe y
  scale_y_continuous(breaks = seq(-10, 10, by = 5))  # Définir les indices de l'axe y toutes les 5 unités
ggsave("D:/MIT project/2024_06 E4 Data/boxplot_vital_status.png", plot = p_vitality, width = 10, height = 6)

# Effectuer le test de Wilcoxon pour les différentes combinaisons
cat("Variable: alive vs. deceased\n")
wilcox_test_result_1 <- wilcox.test(all_data$Variation.coefficient[all_data$Status == "Alive"], 
                                    all_data$Variation.coefficient[all_data$Status == "Deceased"], 
                                    paired = TRUE)
print(wilcox_test_result_1)

cat("Variable: nonpro vs. pro\n")
wilcox_test_result_2 <- wilcox.test(all_data$Variation.coefficient[all_data$Level == "Non-pro"], 
                                    all_data$Variation.coefficient[all_data$Level == "Pro"], 
                                    paired = TRUE)

print(wilcox_test_result_2)

# Résumé des données avec moyenne, médiane, écart type, minimum et maximum
summary_data_level <- all_data %>%
  group_by(Level) %>%
  summarize(
    mean_variation = mean(Variation.coefficient, na.rm = TRUE),
    median_variation = median(Variation.coefficient, na.rm = TRUE),
    sd_variation = sd(Variation.coefficient, na.rm = TRUE)
  )

summary_data_status <- all_data %>%
  group_by(Status) %>%
  summarize(
    mean_variation = mean(Variation.coefficient, na.rm = TRUE),
    median_variation = median(Variation.coefficient, na.rm = TRUE),
    sd_variation = sd(Variation.coefficient, na.rm = TRUE)
  )

cat("Summary for Piano Level:\n")
print(summary_data_level)

cat("Summary for Vital Status:\n")
print(summary_data_status)