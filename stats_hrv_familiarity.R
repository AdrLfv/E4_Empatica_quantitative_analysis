library(dplyr)
library(tidyr)
library(ggplot2)
library(FSA)
library(ggsignif)

# Base paths
base_path <- "D:/MIT project/2024_06 E4 Data/Cleaned data"
participant_file <- "D:/MIT project/2024_06 E4 Data/participants.csv"

# Load participant information
participants <- read.csv(participant_file, header = TRUE, stringsAsFactors = FALSE, sep = ";")

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
    if (participant_id == "P05" | participant_id == "P06" | participant_id == "P07" | participant_id == "P11" | participant_id == "P24" | participant_id == "P26") {
        next
    }
    participant_info <- participants %>% filter(ID == participant_id)
    if (nrow(participant_info) == 0) next
    familiarity <- substr(participant_info$Familiarity, 1, 1)
    if (familiarity %in% c("A", "F", "S")) {
        # On choisit de ne considérer que la session A
        session_data <- get_hr_data(participant_folder, "A")
        if (!is.null(session_data)) {
            session_data <- session_data %>% mutate(
                Participant = participant_id,
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

# Filter data for valid groups
hr_data <- hr_data %>% filter(Group %in% c("Stranger", "Acquaintance", "Friend"))

# For each familiarity, calculate the mean HR variation, standard deviation, and number of observations
hr_summary <- hr_data %>%
    group_by(Group) %>%
    summarise(mean_variation = mean(Variation.coefficient),
              sd_variation = sd(Variation.coefficient),
              n = n())
cat("Mean HR variation by familiarity:\n")
print(hr_summary)

# Ensure 'Group' has the desired order
hr_data_long <- hr_data %>%
    pivot_longer(cols = c(Variation.coefficient),
                 names_to = "Condition",
                 values_to = "HR_variation") %>%
    mutate(Condition = factor(Condition, levels = c("Variation.coefficient"), labels = c("HR Variation")),
           Group = factor(Group, levels = c("Stranger", "Acquaintance", "Friend")))

# Create the box plot
ggplot(hr_data_long, aes(x = Group, y = HR_variation, fill = Group)) +
    geom_boxplot(alpha = 0.7) +
    geom_jitter(width = 0.2, alpha = 0.5) +
    labs(title = "Box plot of heart rate variation by familiarity",
         x = "Familiarity Group",
         y = "Heart Rate Variation") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Sauvegarder le graphique en local
ggsave("D:/MIT project/2024_06 E4 Data/hr_variation_familiarity_histogram.png", width = 10, height = 6)

# Create the histogram
ggplot(hr_data_long, aes(x = Group, y = HR_variation, fill = Group)) +
    geom_histogram(binwidth = 0.5, position = "dodge", alpha = 0.7) +
    labs(title = "Histogram of heart rate variation by familiarity",
         x = "Heart Rate Variation",
         y = "Frequency") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Sauvegarder le graphique en local
ggsave("D:/MIT project/2024_06 E4 Data/hr_variation_familiarity_boxplot.png", width = 10, height = 6)