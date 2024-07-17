library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)

base_path <- "D:/MIT project/2024_06 E4 Data/Cleaned data"
stream_folders <- list.dirs(base_path, recursive = FALSE)

get_session_data <- function(participant_folder, session, participant) {
    file_path <- file.path(participant_folder, "HR", paste0(session, "_HR.csv"))
    data_file <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE, sep = ";")
    data <- data_file %>%
        select(Variation.coefficient) %>%
        head(60) %>%
        mutate(Participant = participant, Session = session) %>%
        rename(Variation_Coefficient = Variation.coefficient)
    return(data)
}

all_data <- data.frame()
for (participant_folder in stream_folders) {
    participant <- substr(basename(participant_folder), 1, 3)
    if (participant != "P07" & participant != "P11" & participant != "P24" & participant != "P26") {
        next
    }
    for (session in c("A", "B", "C", "D")) {
        session_data <- get_session_data(participant_folder, session, participant)
        all_data <- bind_rows(all_data, session_data)
    }
}

all_data <- all_data %>%
    mutate(Session_Group = ifelse(Session %in% c("A", "B"), "Calm", "Dynamic"))

# Summarize data by participant and session group
summarized_data <- all_data %>%
    group_by(Participant, Session_Group) %>%
    summarize(Mean_Variation_Coefficient = mean(Variation_Coefficient, na.rm = TRUE)) %>%
    spread(Session_Group, Mean_Variation_Coefficient)

# Wilcoxon test comparing Calm vs. Dynamic sessions
wilcox_test <- wilcox.test(summarized_data$Calm, summarized_data$Dynamic, paired = TRUE)
print(wilcox_test)

# Combine A and C data as Calm and B and D data as Dynamic
combined_data <- all_data %>%
    mutate(Session_Group = ifelse(Session %in% c("A", "B"), "Calm", "Dynamic"))

# Create box plot
p <- ggplot(combined_data, aes(x = Session_Group, y = Variation_Coefficient, fill = Session_Group)) +
    geom_boxplot() +
    ggtitle("Box Plot for Calm vs Dynamic Sessions") +
    xlab("Session Group") +
    ylab("Variation Coefficient") +
    theme_minimal()

ggsave("D:/MIT project/2024_06 E4 Data/boxplot_calm_vs_dynamic.png", plot=p, width = 10, height = 8)
