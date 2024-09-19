library(signal)
library(pracma)
library(dplyr)
library(FSA)
library(ggsignif)
library(rstatix)

# This script performs data processing and statistical analysis on accelerometer (ACC) data 
# collected from participants during different sessions. The primary goal is to center 
# the ACC data and then conduct a Mann-Whitney U test to compare the accelerometer data 
# between different groups (e.g., pianists vs. non-pianists).

source("center_ACC.R")

base_path <- "cleaned_data"
stream_folders <- list.dirs(base_path, recursive = FALSE)
participants_data <- readRDS("data_rds/participants.rds")
sessions_order <- c("A", "B", "C", "D")

normalised_data <- data.frame()

for (participant_folder in stream_folders) {
    participant_id <- substr(basename(participant_folder), 1, 3)
    participant_info <- participants_data %>% filter(ID == participant_id)
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

make_tests <- function(data) {
    if (!is.numeric(data$ACC)) {
        data$ACC <- as.numeric(data$ACC)
    }
    if (!is.factor(data$Group)) {
        data$Group <- as.factor(data$Group)
    }

    #Test by Mann-Whitney for Ispianist
    print("Mann-Whitney Test for isPianist")
    mw_test_pianist <- wilcox.test(ACC ~ Group, data = data)
    
    #GlobalTest
    print("Global Mann-Whitney Test")
    mw_test_global <- wilcox.test(ACC ~ Group, data = data)
    print(mw_test_global)
    
    #PostHocTest (dunnTest)
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

# Perform the test functions
make_tests(normalised_data)
# get_stats(normalised_data)
