# Librairies
library(dplyr)
library(tidyr)
library(ggplot2)
library(FSA)
library(ggsignif)

# This script processes performs a Friedman test to compare the mean delta heart rate (ΔHR) across sessions, 
# and conducts post hoc Wilcoxon tests if the results are significant.

# Basic paths
base_path <- "cleaned_data"
participants_data <- readRDS("data_rds/participants.rds")
# List of files containing HR data
stream_folders <- list.dirs(base_path, recursive = FALSE)

# Function to recover HR data
get_hr_data <- function(participant_folder, session, participant) {
    file_path <- file.path(participant_folder, "HR", paste0(session, "_HR.rds"))
    if (!file.exists(file_path)) return(NULL)
    data <- readRDS(file_path)
    data <- data %>% 
        select(Delta_Heart_Rate) %>% 
        head(60) %>%  # Ensure that we take the first 60 lines only
        mutate(Participant = participant, Pianist = session)
    return(data)
}

# Combine HR data with participants' information
global_data <- data.frame()
session_order <- c("A", "B", "C", "D")

for (participant_folder in stream_folders) {
    participant_id <- substr(basename(participant_folder), 1, 3)
    
    for (i in 1:4) {
        session_data <- get_hr_data(participant_folder, session_order[i], participant_id)
        if (!is.null(session_data)) {
            global_data <- rbind(global_data, session_data)
        }
    }
}

# Preparation of data for the Friedman test
# Here we must make sure that we have 60 measures for each participant and each pianist
mean_variation_data <- global_data %>%
group_by(Participant, Pianist) %>%
summarise(mean_variation = mean(Delta_Heart_Rate, na.rm = TRUE))

# Execute the Friedman test
friedman_test_result <- friedman.test(y=mean_variation_data$mean_variation, groups=mean_variation_data$Pianist, blocks=mean_variation_data$Participant)

# Show test results
print(friedman_test_result)

# Post hoc test if significant
if (friedman_test_result$p.value < 0.05) {
    # testDeWilcoxonPostHoc
    post_hoc_result <- pairwise.wilcox.test(mean_variation_data$mean_variation, 
                                            mean_variation_data$Pianist, 
                                            p.adjust.method = "bonferroni", 
                                            paired = TRUE)
    print(post_hoc_result)
}
