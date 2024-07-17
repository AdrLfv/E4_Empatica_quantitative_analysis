library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)

# Base paths
base_path <- "D:/MIT project/2024_06 E4 Data/Cleaned data"
combined_data_path <- "D:/MIT project/2024_06 E4 Data/combined_data"
participant_file <- "D:/MIT project/2024_06 E4 Data/participants.csv"

# Load participant information
participants_data <- read.csv(participant_file, header = TRUE, stringsAsFactors = FALSE, sep = ";")

# List of folders containing HR data
stream_folders <- list.dirs(base_path, recursive = FALSE)

# Function to retrieve HR data
get_hr_data <- function(participant_folder, session) {
    file_path <- file.path(participant_folder, "HR", paste0(session, "_HR.csv"))
    if (!file.exists(file_path)) return(NULL)
    data <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE, sep = ";")
    data <- data %>% select(Variation.coefficient) %>% head(60)
    return(data)
}

# Combine HR data with participant information
hr_data <- data.frame()

for (participant_folder in stream_folders) {
    participant_id <- substr(basename(participant_folder), 1, 3)
    participant_info <- participants_data %>% filter(ID == participant_id)
    if (nrow(participant_info) == 0) next
    familiarity <- substr(participant_info$Familiarity, 1, 1)
    if (familiarity %in% c("A", "F", "S")) {
        for (session in c("A", "B", "C", "D")) {
            session_data <- get_hr_data(participant_folder, session)
            if (!is.null(session_data)) {
                session_data <- session_data %>% mutate(
                    Participant = participant_id,
                    Session = session,
                    Group = case_when(
                        familiarity == "A" ~ "Acquaintance",
                        familiarity == "F" ~ "Friend",
                        familiarity == "S" ~ "Stranger"
                    )
                )
                hr_data <- bind_rows(hr_data, session_data)
            }
        }
    }
}

# Filter data for valid groups
hr_data <- hr_data %>% filter(Group %in% c("Acquaintance", "Friend", "Stranger"))

# Plotting the boxplot
ggplot(hr_data, aes(x = Group, y = Variation.coefficient)) +
    geom_boxplot(fill = "skyblue", color = "blue") +
    ggtitle("Box Plot of Variation Coefficient of HR by Group") +
    xlab("Group") +
    ylab("Variation Coefficient of HR") +
    theme_minimal()

# Performing the Wilcoxon test between groups
acq_vs_friend <- wilcox.test(
    hr_data %>% filter(Group == "Acquaintance") %>% pull(Variation.coefficient),
    hr_data %>% filter(Group == "Friend") %>% pull(Variation.coefficient)
)

acq_vs_stranger <- wilcox.test(
    hr_data %>% filter(Group == "Acquaintance") %>% pull(Variation.coefficient),
    hr_data %>% filter(Group == "Stranger") %>% pull(Variation.coefficient)
)

friend_vs_stranger <- wilcox.test(
    hr_data %>% filter(Group == "Friend") %>% pull(Variation.coefficient),
    hr_data %>% filter(Group == "Stranger") %>% pull(Variation.coefficient)
)

# Print Wilcoxon test results
acq_vs_friend
acq_vs_stranger
friend_vs_stranger
