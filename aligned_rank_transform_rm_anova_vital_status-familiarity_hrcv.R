# Installer les bibliothèques nécessaires si elles ne sont pas installées
# install.packages("ARTool")
# install.packages("lme4")

library(dplyr)
library(tidyr)
library(ggplot2)
library(FSA)
library(ggsignif)
library(ARTool)
library(lme4)

# Base paths
base_path <- "D:/MIT project/2024_06 E4 Data/Cleaned data"
participant_file <- "D:/MIT project/2024_06 E4 Data/participants.csv"

# Load participant information
participants <- read.csv(participant_file, header = TRUE, stringsAsFactors = FALSE, sep = ";")

# List of folders containing HR data
stream_folders <- list.dirs(base_path, recursive = FALSE)
# Combine HR data with participant information
hr_data <- data.frame()

# Function to retrieve HR data
get_hr_data <- function(participant_folder, session) {
    file_path <- file.path(participant_folder, "HR", paste0(session, "_HR.csv"))
    if (!file.exists(file_path)) return(NULL)
    data <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE, sep = ";")
    data <- data %>% select(Variation.coefficient) %>% head(60)
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
         "A" = "Acquaintance",
         "F" = "Friend",
         "S" = "Stranger",
         "R" = "Family",
         "P" = "Self",
         "H" = "Heard of",
         "Unknown")
}

# Collect HR data across all participants and sessions
collect_hr_data <- function(chosen_vital_status) {
    pianists <- ifelse(chosen_vital_status == "Alive", c("A", "C"), c("B", "D"))
    for (participant_folder in stream_folders) {
        participant_id <- substr(basename(participant_folder), 1, 3)
        participant_info <- participants %>% filter(ID == participant_id)

        for (session in pianists) {
            familiarity_data <- get_familiarity(participant_info$Familiarity, session)
            session_data <- get_hr_data(participant_folder, session)
            if (!is.null(session_data)) {
                session_data <- session_data %>% mutate(
                    Participant = participant_id,
                    Familiarity = map_familiarity(familiarity_data),
                    VitalStatus = chosen_vital_status
                )
                hr_data <<- bind_rows(hr_data, session_data)
            }
        }
    }
}

# Collect data for both vital statuses
collect_hr_data("Alive")
collect_hr_data("Deceased")

# Convert Familiarity, VitalStatus, and Participant to factors
hr_data <- hr_data %>%
  mutate(Familiarity = factor(Familiarity),
         VitalStatus = factor(VitalStatus),
         Participant = factor(Participant))

# Perform Aligned Rank Transform
art_model <- art(Variation.coefficient ~ Familiarity * VitalStatus + (1 | Participant), data = hr_data)

# Perform ANOVA on the aligned ranks
anova_result <- anova(art_model)

# Print the ANOVA results
print("ART ANOVA results for the fixed effects (Familiarity * VitalStatus):")
print(anova_result)

# Mixed-design ANOVA with lme4
# After aligning the ranks, we use a linear mixed model
# We use the aligned residuals and include random effects for Participant
lmm_model <- lmer(art(Variation.coefficient) ~ Familiarity * VitalStatus + (1 | Participant), data = hr_data)

# Summary of the mixed model
summary(lmm_model)

# Perform ANOVA on the mixed model
anova_mixed <- anova(lmm_model)

# Print the mixed-design ANOVA results
print("Mixed-design ANOVA results:")
print(anova_mixed)

# Optionally, perform post-hoc analysis if needed
# posthoc <- art.con(art_model, pairwise = TRUE)
# print("Post-hoc results:")
# print(posthoc)
