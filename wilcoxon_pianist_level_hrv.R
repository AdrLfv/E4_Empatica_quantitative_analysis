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
        mutate(Participant = participant, Session = session) %>%
        rename(Variation_Coefficient = Variation.coefficient)
    return(data)
}

all_data <- data.frame()
for (participant_folder in stream_folders) {
    participant_id <- substr(basename(participant_folder), 1, 3)

    for (session in c("A", "B", "C", "D")) {
        session_data <- get_session_data(participant_folder, session, participant_id)
        is_pianist <- participants_data %>% filter(ID == participant_id) %>% select(isPianist) %>% pull()
        session_data <- session_data %>% mutate(isPianist = is_pianist)
        all_data <- bind_rows(all_data, session_data)
    }
}

# Résumer les données par statut de pianiste
summarized_data <- all_data %>%
    group_by(Participant, Session, isPianist) %>%
    summarise(Mean_Variation_Coefficient = mean(Variation_Coefficient, na.rm = TRUE))

print(summarized_data)

# Tester la différence entre les groupes "pianist" et "non-pianist"
wilcox_test <- wilcox.test(
    summarized_data %>% filter(isPianist == "Yes") %>% pull(Mean_Variation_Coefficient),
    summarized_data %>% filter(isPianist == "No") %>% pull(Mean_Variation_Coefficient)
)
print(wilcox_test)

# Post hoc test
post_hoc_test <- pairwise.wilcox.test(summarized_data$Mean_Variation_Coefficient, summarized_data$isPianist, p.adjust.method = "BH")
cat("Post-hoc test results:\n")
print(post_hoc_test)

# Créer un box plot pour visualiser les différences
p <- ggplot(all_data, aes(x = isPianist, y = Variation_Coefficient, fill = isPianist)) +
    geom_boxplot(alpha = 0.7) +
    geom_jitter(width = 0.2, alpha = 0.5) +
    ggtitle("Box Plot of HR Variation Coefficient by Pianist Status") +
    xlab("Pianist Status") +
    ylab("Variation Coefficient") +
    theme_minimal()

ggsave("D:/MIT project/2024_06 E4 Data/boxplot_pianist_status.png", plot=p, width = 10, height = 8)
