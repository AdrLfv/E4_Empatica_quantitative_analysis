library(dplyr)
library(tibble)

# This script calculates the mean and standard deviation of the delta heart rate (ΔHR) data for each category of music (calm/dynamic).

base_path <- "cleaned_data"
# List all files in the basic path
stream_folders <- list.dirs(base_path, recursive = FALSE)

get_hrv_data <- function(participant_folder, session) {
    file_path <- file.path(participant_folder, "HR", paste0(session, "_HR.rds"))
    if (!file.exists(file_path)) return(NULL)
    data <- readRDS(file_path)
    data <- data %>% select(Delta_Heart_Rate) %>% head(60)
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

# Calculate the averages and the standard deviation of statistics for each group
summary_results <- global_data %>%
  group_by(Group) %>%
  summarise(
    Mean_Delta_Heart_Rate = mean(Delta_Heart_Rate, na.rm = TRUE),
    SD_Delta_Heart_Rate = sd(Delta_Heart_Rate, na.rm = TRUE)
  )

# Show the results
print(summary_results)
