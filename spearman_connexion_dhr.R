library(signal)
library(pracma)
library(dplyr)
library(FSA)
library(ggsignif)
library(Hmisc)
library(ggplot2)

# This script performs a Spearman correlation test between connection data and delta heart rate (ΔHR) data.

base_path <- "cleaned_data"
stream_folders <- list.dirs(base_path, recursive = FALSE)
participants_data <- readRDS("data_rds/participants.rds")
# Function to retrieve HR data
get_hr_data <- function(participant_folder, session) {
    file_path <- file.path(participant_folder, "HR", paste0(session, "_HR.rds"))
    if (!file.exists(file_path)) return(NULL)
    data <- readRDS(file_path)
    # data <- data %>% select(Delta_Heart_Rate) %>% head(60)
    data <- data %>% select(Delta_Heart_Rate)
    return(data)
}

global_data <- data.frame()

for (participant_folder in stream_folders) {
    participant_id <- substr(basename(participant_folder), 1, 3)
    if (participant_id %in% c("P05", "P06", "P07", "P11", "P24", "P26")) {
        next
    }

    connection <- filter(participants_data, ID == participant_id) %>% select(Connection_A) %>% pull()
    
    session_data <- get_hr_data(participant_folder, "A") %>% head(60)
    if (!is.null(session_data)) {
        session_data$Participant <- participant_id
        session_data$Connection <- connection
        global_data <- bind_rows(global_data, session_data)
    }
}

# Normalized data recording
# write.table(global_data, "hr_data.csv", row.names = FALSE, sep=";")

# Perform the Spearman test
spearman_test <- cor.test(global_data$Connection, global_data$Delta_Heart_Rate, method = "spearman")
print(spearman_test)

# Calculate Kendall's Tau
kendall_tau <- cor.test(global_data$Connection, global_data$Delta_Heart_Rate, method = "kendall")
cat("Kendall's tau:", kendall_tau$estimate, "\n")

# Calculate Somers' D
# somers_d <- somers2(global_data$Connection, global_data$Delta_Heart_Rate)
# cat("Somers' D:", somers_d$Dxy, "\n")


# Perform the test functions
print("done")
