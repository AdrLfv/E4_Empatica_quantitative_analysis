library(dplyr)
library(tidyr)
library(ggplot2)
library(FSA)
library(ggsignif)

# Base paths
base_path <- "D:/MIT project/2024_06 E4 Data/Cleaned data"
participant_file <- "D:/MIT project/2024_06 E4 Data/participants.csv"

# Load participant information
participants_data <- read.csv(participant_file, header = TRUE, stringsAsFactors = FALSE, sep = ";")

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
        "A" = "Heard of/Acquaintance",
        "F" = "Family/Friend",
        "S" = "Stranger",
        "R" = "Family/Friend",
        "P" = "Self",
        "H" = "Heard of/Acquaintance",
        "Unknown")
}

calculate_Kruskal <- function(chosen_vital_status) {
    pianists <- case_when(
        chosen_vital_status == "Alive" ~ c("A", "C"),
        chosen_vital_status == "Deceased" ~ c("B", "D"),
        chosen_vital_status == "Non-pro" ~ c("A", "B"),
        chosen_vital_status == "Pro" ~ c("C", "D"),
        chosen_vital_status == "Mixed" ~ c("B", "C"),
        TRUE ~ NA_character_
    )

    # TO REMOVE
    # pianists <- c("A")

    for (participant_folder in stream_folders) {
        participant_id <- substr(basename(participant_folder), 1, 3)
        participant_info <- participants_data %>% filter(ID == participant_id)

        for (session in pianists) {
            familiarity_data <- get_familiarity(participant_info$Familiarity, session)
            session_data <- get_hr_data(participant_folder, session)
            if (!is.null(session_data)) {
                session_data <- session_data %>% mutate(
                    Participant = participant_id,
                    Familiarity = map_familiarity(familiarity_data)
                )
                hr_data <- bind_rows(hr_data, session_data)
            }
        }
        
    }

    #Filter hr_data to keep only the considered pianists not in the list of excluded participants
    hr_data <- hr_data %>% filter(Familiarity != "Unknown" & Familiarity != "Self")

    # Perform Kruskal-Wallis test
    kruskal_test <- kruskal.test(Variation.coefficient ~ Familiarity, data = hr_data)
    cat("Considered pianists: ", chosen_vital_status, "\n")
    print(kruskal_test)

    hr_data_long <- hr_data %>%
    pivot_longer(cols = c(Variation.coefficient),
                 names_to = "Condition",
                 values_to = "HRCV") %>%
    mutate(Condition = factor(Condition, levels = c("Variation.coefficient"), labels = c("HRCV")),
           Familiarity = factor(Familiarity, levels = c("Stranger", "Heard of/Acquaintance", "Family/Friend", "Self")))

    # Créer le box plot avec des couleurs différentes pour chaque session
    p_box_plot <- ggplot(hr_data_long, aes(x = Familiarity, y = HRCV, fill = Familiarity)) +
        geom_boxplot(alpha = 0.7, outlier.shape = NA) +
        #geom_jitter(width = 0.2, alpha = 0.5) +
        labs(#title = "Box plot of heart rate variation by familiarity",
            x = "Familiarity",
            y = "Heart Rate Coefficient of Variation") +
        theme(axis.text.x = element_blank(),
            text = element_text(family = "Arial", size = 18)) +

        scale_fill_brewer(palette = "Set3")  +
        coord_cartesian(ylim = c(-20, 15)) +  # Définir les limites de l'axe y
        scale_y_continuous(breaks = seq(-20, 15, by = 5))  # Définir les indices de l'axe y toutes les 5 unités

    ggsave("D:/MIT project/2024_06 E4 Data/boxplot_hrv_familiarity_A.png", plot = p_box_plot, width = 8, height = 6)


    # # # Post-hoc analysis with Dunn's test
    # dunn_test <- dunnTest(Variation.coefficient ~ Familiarity, data = hr_data, method = "bonferroni")
    # print(dunn_test)

    # # # Extract significant comparisons
    # #dunn_results <- dunn_test$res

    # pairwise_test <- pairwise.wilcox.test(hr_data$Variation.coefficient, hr_data$Familiarity, p.adjust.method = "bonferroni")
    # print(pairwise_test)
}

calculate_Kruskal("Pro")
# calculate_Kruskal("Mixed")