library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)

base_path <- "D:/MIT project/2024_06 E4 Data/Cleaned data"
stream_folders <- list.dirs(base_path, recursive = FALSE)
participant_file <- "D:/MIT project/2024_06 E4 Data/participants.csv"
participants_data <- read.csv(participant_file, header = TRUE, stringsAsFactors = FALSE, sep = ";")

get_session_data <- function(participant_folder, session, participant) {
    file_path <- file.path(participant_folder, "HR", paste0(session, "_HR.csv"))
    data_file <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE, sep = ";")
    data <- data_file %>%
        select(Variation.coefficient) %>%
        head(60) %>%
        mutate(Participant = participant, Session = session)
    return(data)
}

all_data <- data.frame()
for (participant_folder in stream_folders) {
    participant_id <- substr(basename(participant_folder), 1, 3)
    participant_info <- participants_data %>% filter(ID == participant_id)
    if (nrow(participant_info) == 0) {
        next # Skip if participant ID is not found in participants_data
    }

    for (session in c("A", "B", "C", "D")) {
        session_data <- get_session_data(participant_folder, session, participant_id)
        pianist <- participant_info %>% select(session) %>% pull()
        vitality <- case_when(
            pianist %in% c("XX", "Donal", "Alisa", "JoeP", "Peter") ~ "alive",
            pianist %in% c("Marvin", "Toussaint") ~ "deceased",
            TRUE ~ NA_character_
        )
        session_data <- session_data %>% mutate(Vitality = vitality)
        all_data <- bind_rows(all_data, session_data)
    }
}
print(all_data)

# Résumer les données par vitalité
summarized_data <- all_data %>%
    group_by(Participant, Session, Vitality) %>%
    summarise(Mean_Variation_Coefficient = mean(Variation.coefficient)) 

print(summarized_data)

# Tester la différence entre les groupes "alive" et "deceased"
wilcox_test <- wilcox.test(
    summarized_data %>% filter(Vitality == "alive") %>% pull(Mean_Variation_Coefficient),
    summarized_data %>% filter(Vitality == "deceased") %>% pull(Mean_Variation_Coefficient)
)
print(wilcox_test)

#post hoc test
post_hoc_test <- pairwise.wilcox.test(all_data$Variation.coefficient, all_data$Vitality, p.adjust.method = "BH")
print(post_hoc_test)

# Créer un box plot pour visualiser les différences
p <- ggplot(all_data, aes(x = Vitality, y = Variation.coefficient, fill = Vitality)) +
    geom_boxplot(alpha = 0.7) +
    geom_jitter(width = 0.2, alpha = 0.5) +
    ggtitle("Box Plot of HR Variation Coefficient by Vitality") +
    xlab("Vitality Group") +
    ylab("Variation Coefficient") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave("D:/MIT project/2024_06 E4 Data/boxplot_vitality.png", plot=p, width = 10, height = 8)
