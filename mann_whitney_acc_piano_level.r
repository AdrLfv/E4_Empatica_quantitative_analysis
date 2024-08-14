library(signal)
library(pracma)
library(dplyr)
library(FSA)
library(ggsignif)
library(rstatix)


source("D:/MIT project/E4_quantitative_analysis/center_ACC.R")

base_path <- "D:/MIT project/2024_06 E4 Data/Cleaned data"
stream_folders <- list.dirs(base_path, recursive = FALSE)
participant_path <- "D:/MIT project/2024_06 E4 Data/participants.csv"
participant_data <- read.table(participant_path, header = TRUE, sep = ";", stringsAsFactors = FALSE)
sessions_order <- c("A", "B", "C", "D")

normalised_data <- data.frame()

for (participant_folder in stream_folders) {
    participant_id <- substr(basename(participant_folder), 1, 3)
    participant_info <- participant_data %>% filter(ID == participant_id)
    if (nrow(participant_info) == 0) next
    isPianist <- substr(participant_info$isPianist, 1, 1)

    for (i in 1:4) {
        session_data <- center_ACC(participant_folder, sessions_order[i]) %>% head(60*32)
        if (!is.null(session_data)) {
            session_data <- session_data %>% mutate(
                Participant = participant_id,
                Group = isPianist,
                Session = sessions_order[i]
            )
            normalised_data <- bind_rows(normalised_data, session_data)
        }
    }
}

# Enregistrement de la data normalisée
# write.csv(normalised_data, "D:/MIT project/2024_06 E4 Data/normalised_ACC.csv", row.names = FALSE)

make_test <- function(entry_data) {
    data <- entry_data
    if (!is.numeric(data$ACC)) {
        data$ACC <- as.numeric(data$ACC)
    }
    if (!is.factor(data$Group)) {
        data$Group <- as.factor(data$Group)
    }

    #Test de Mann-Whitney pour isPianist
    print("Mann-Whitney Test for isPianist")
    mw_test_pianist <- wilcox.test(ACC ~ Group, data = data)
    
    # Global test
    print("Global Mann-Whitney Test")
    mw_test_global <- wilcox.test(ACC ~ Group, data = data)
    print(mw_test_global)
    
    # Post hoc test (Dunn Test)
    dunn_test <- dunnTest(ACC ~ Group, data = data, method = "bonferroni")
    print("Dunn Test Results")
    print(dunn_test)
}

get_stats <- function(data) {
    for (session in sessions_order) {
        data_copy <- data %>% filter(Session == session)
        print(paste("Session:", session))
        
        means <- data_copy %>% group_by(Group) %>% summarise(mean = mean(ACC))
        print(means)
        
        sds <- data_copy %>% group_by(Group) %>% summarise(sd = sd(ACC))
        print(sds)
        
        medians <- data_copy %>% group_by(Group) %>% summarise(median = median(ACC))
        print(medians)
    }
    
    print("Global stats")
    means <- data %>% group_by(Group) %>% summarise(mean = mean(ACC))
    print(means)

    sds <- data %>% group_by(Group) %>% summarise(sd = sd(ACC))
    print(sds)

    medians <- data %>% group_by(Group) %>% summarise(median = median(ACC))
    print(medians)
}

# Exécuter les fonctions de test
make_test(normalised_data)
# get_stats(normalised_data)
