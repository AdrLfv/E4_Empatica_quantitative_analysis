library(signal)
library(pracma)
library(dplyr)
library(FSA)
library(ggsignif)
library(Hmisc)
library(ggplot2)


base_path <- "D:/MIT project/2024_06 E4 Data/Cleaned data"
stream_folders <- list.dirs(base_path, recursive = FALSE)
participant_file <- "D:/MIT project/2024_06 E4 Data/participants.csv"
participants_data <- read.csv(participant_file, header = TRUE, stringsAsFactors = FALSE, sep = ";")

# Function to retrieve HR data
get_hr_data <- function(participant_folder, session) {
    file_path <- file.path(participant_folder, "HR", paste0(session, "_HR.csv"))
    if (!file.exists(file_path)) return(NULL)
    data <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE, sep = ";")
    # data <- data %>% select(Variation.coefficient) %>% head(60)
    data <- data %>% select(Variation.coefficient)
    return(data)
}

global_data <- data.frame()

for (participant_folder in stream_folders) {
    participant_id <- substr(basename(participant_folder), 1, 3)
    if (participant_id %in% c("P05", "P06", "P07", "P11", "P24", "P26")) {
        next
    }
    print(head(participants_data))


    connection <- filter(participants_data, ID == participant_id) %>% select(Connection) %>% pull()
    
    session_data <- get_hr_data(participant_folder, "A") %>% head(60)
    if (!is.null(session_data)) {
        session_data$Participant <- participant_id
        session_data$Connection <- connection
        global_data <- bind_rows(global_data, session_data)
    }
}

# Enregistrement de la data normalisée
# write.table(global_data, "D:/MIT project/2024_06 E4 Data/hr_data.csv", row.names = FALSE, sep=";")

# Effectuer le test de Spearman
spearman_test <- cor.test(global_data$Connection, global_data$Variation.coefficient, method = "spearman")
print(spearman_test)

# Calculer kendall's tau
kendall_tau <- cor.test(global_data$Connection, global_data$Variation.coefficient, method = "kendall")
cat("Kendall's tau:", kendall_tau$estimate, "\n")

# Calculer Somers' D
somers_d <- somers2(global_data$Connection, global_data$Variation.coefficient)
cat("Somers' D:", somers_d$Dxy, "\n")


# Exécuter les fonctions de test
print("done")
