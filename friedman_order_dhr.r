# Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(FSA)
library(ggsignif)

# This script performs a Friedman test to compare the mean delta heart rate (ΔHR) across sessions, and conducts post hoc tests if the results are significant.

# Base paths
base_path <- "cleaned_data"
participants_data <- readRDS("data_rds/participants.rds")
# List of folders containing HR data
stream_folders <- list.dirs(base_path, recursive = FALSE)

# Function to retrieve HR data
get_hr_data <- function(participant_folder, session) {
    file_path <- file.path(participant_folder, "HR", paste0(session, "_HR.rds"))
    if (!file.exists(file_path)) return(NULL)
    data <- readRDS(file_path)
    data <- data %>% select(Delta_Heart_Rate) %>% head(60)
    return(data)
}

# Combine HR data with participant information
hr_data <- data.frame()

for (participant_folder in stream_folders) {
    participant_id <- substr(basename(participant_folder), 1, 3)
    participant_order <- participants_data[participants_data$ID == participant_id, "Order"]
    
    if (length(participant_order) == 0) next
    
    for (i in 1:4) {
        session <- substr(participant_order, i, i)
        session_data <- get_hr_data(participant_folder, session)
        if (!is.null(session_data)) {
            session_data$Participant <- participant_id
            session_data$Session <- i
            session_data$Group <- session
            hr_data <- rbind(hr_data, session_data)
        }
    }
}

# Prepare data for Friedman test
friedman_data <- hr_data %>%
    group_by(Participant, Session) %>%
    summarise(mean_var = mean(Delta_Heart_Rate, na.rm = TRUE)) %>%
    spread(key = Session, value = mean_var)

# Perform Friedman test
friedman_test_result <- friedman.test((as.matrix(friedman_data[,-1])))

# Print test results
print(friedman_test_result)

# Post hoc test if significant
if (friedman_test_result$p.value < 0.05) {
    post_hoc_result <- pairwise.wilcox.test(as.vector(as.matrix(friedman_data[,-1])), 
                                            rep(1:4, each=nrow(friedman_data)), 
                                            p.adjust.method = "bonferroni")
    print(post_hoc_result)
}

tau_result <- cor.test(friedman_data$`1`, friedman_data$`2`, method = "kendall")

# Print the result
print(tau_result)