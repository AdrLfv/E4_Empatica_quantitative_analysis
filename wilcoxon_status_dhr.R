library(dplyr)
library(tibble)
library(tidyverse)
library(openxlsx)
library(ggplot2)

# This script performs Wilcoxon tests and calculates summary statistics for the delta heart rate (ΔHR) data

# Define the basic path
base_path <- "D:/path_to_folder/Cleaned data"
stream_folders <- list.dirs(base_path, recursive = FALSE)

# Load participant information
participant_file <- "D:/path_to_folder/participants.csv"
participants_data <- read.csv(participant_file, header = TRUE, stringsAsFactors = FALSE, sep = ";")

# A: alive, nonpro
# B: deceased, nonpro
# C: alive, pro
# D: deceased, pro

get_session_data <- function(participant_folder, session, participant_id) {
  file_path <- file.path(participant_folder, "HR", paste0(session, "_HR.csv"))
    data_file <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE, sep = ";")
    music_type <- ifelse(session %in% c("A", "B"), "Calm", "Dynamic")
    level <- ifelse(session %in% c("A", "B"), "Non-pro", "Pro")
    status <- ifelse(session %in% c("A", "C"), "Alive", "Deceased")
    data <- data_file %>%
        select(Delta_Heart_Rate) %>%
        head(60) %>%
        mutate(Participant = participant_id, Session = session, Music_type = music_type, Level = level, Status = status)
  return(data)
}

# Define a function to get familiarity based on session
get_familiarity <- function(familiarity, session) {
  switch(session,
         "A" = substr(familiarity, 1, 1),
         "B" = substr(familiarity, 2, 2),
         "C" = substr(familiarity, 3, 3),
         "D" = substr(familiarity, 4, 4),
         NA_character_)
}
# Define a function to map familiarity code to description
map_familiarity <- function(familiarity_code) {
  switch(familiarity_code,
        "A" = "Heard of/Acquaintance",
        "F" = "Family/Friend",
        "S" = "Stranger",
        "R" = "Family/Friend",
        "P" = "Self",
        "H" = "Heard of/Acquaintance",
        "Unknown")
}

all_data <- data.frame()

for (participant_folder in stream_folders) {
  participant_id <- substr(basename(participant_folder), 1, 3)
  participant_info <- participants_data %>% filter(ID == participant_id)
  for (session in c("C", "D")) {
    familiarity <- get_familiarity(participant_info$Familiarity, session)
    session_data <- get_session_data(participant_folder, session, participant_id) %>%
      mutate(Familiarity = map_familiarity(familiarity))
    all_data <- bind_rows(all_data, session_data)
  }
}

filtered_data <- all_data %>%
  filter(Familiarity == "Stranger", Session %in% c("C", "D"))

# Create a new column to identify specific comparisons
filtered_data <- filtered_data %>%
  mutate(Comparison = case_when(
    Session == "B" ~ "B (Deceased Non-Pro)",
    Session == "C" ~ "C (Alive Pro)",
    Session == "D" ~ "D (Deceased Pro)"
  ))

# Generate the Box Plot for B vs C and C vs D
p_vital_status <- ggplot(filtered_data, aes(x = Comparison, y = Delta_Heart_Rate, fill = Comparison)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  labs(x = "Pianist Comparison", y = "ΔHR") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        text = element_text(family = "Arial", size = 18)) +
  scale_fill_brewer(palette = "Set3") +
  coord_cartesian(ylim = c(-10, 10)) +  # Limit the values ​​of the y axis
  scale_y_continuous(breaks = seq(-10, 10, by = 5))  # Adjust the labels of the Y axis

# Save the BOX PLOT
ggsave("D:/path_to_folder/boxplot_C_D_vital_status.png", plot = p_vital_status, width = 10, height = 6)

# # Generate and save the boxplot for vital status
# p_vitality <- ggplot(all_data, aes(x = Status, y = Delta_Heart_Rate, fill = Status)) +
#   geom_boxplot(alpha = 0.7, outlier.shape = NA) +
#   labs(x = "Pianists Vital Status", y = "ΔHR") +
#   theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
#         text = element_text(family = "Arial", size = 18)) +
#   scale_fill_brewer(palette = "Set3") +
#   coord_cartesian(ylim = c(-10, 10)) +  # Limites de l'axe y
#   scale_y_continuous(breaks = seq(-10, 10, by = 5))  # Incréments sur l'axe y
# ggsave("D:/path_to_folder/boxplot_vital_status.png", plot = p_vitality, width = 10, height = 6)

# Perform the Wilcoxon test for the different combinations

# Filter the data to keep only the Stranger familiarity
all_data <- all_data %>% filter(Familiarity == "Stranger")
cat("Variable: alive vs. deceased\n")
wilcox_test_result_1 <- wilcox.test(all_data$Delta_Heart_Rate[all_data$Status == "Alive"], 
                                    all_data$Delta_Heart_Rate[all_data$Status == "Deceased"], 
                                    paired = FALSE)
print(wilcox_test_result_1)

# cat("Variable: nonpro vs. pro\n")
# wilcox_test_result_2 <- wilcox.test(all_data$Delta_Heart_Rate[all_data$Level == "Non-pro"], 
#                                     all_data$Delta_Heart_Rate[all_data$Level == "Pro"], 
#                                     paired = TRUE)

# print(wilcox_test_result_2)

# Data summary with average, median, standard deviation, minimum and maximum
summary_data_level <- all_data %>%
  group_by(Level) %>%
  summarize(
    mean_variation = mean(Delta_Heart_Rate, na.rm = TRUE),
    median_variation = median(Delta_Heart_Rate, na.rm = TRUE),
    sd_variation = sd(Delta_Heart_Rate, na.rm = TRUE)
  )

summary_data_status <- all_data %>%
  group_by(Status) %>%
  summarize(
    mean_variation = mean(Delta_Heart_Rate, na.rm = TRUE),
    median_variation = median(Delta_Heart_Rate, na.rm = TRUE),
    sd_variation = sd(Delta_Heart_Rate, na.rm = TRUE)
  )

cat("Summary for Piano Level:\n")
print(summary_data_level)

cat("Summary for Vital Status:\n")
print(summary_data_status)