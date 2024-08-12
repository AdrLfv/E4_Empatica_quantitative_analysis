library(signal)
library(pracma)
library(dplyr)
library(FSA)
library(ggsignif)
library(Hmisc)
library(ggplot2)

source("D:/MIT project/E4_quantitative_analysis/center_ACC.R")

base_path <- "D:/MIT project/2024_06 E4 Data/Cleaned data"
stream_folders <- list.dirs(base_path, recursive = FALSE)


# Function to retrieve HR global_data
get_hr_data <- function(participant_folder, session) {
    file_path <- file.path(participant_folder, "HR", paste0(session, "_HR.csv"))
    if (!file.exists(file_path)) return(NULL)
    global_data <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE, sep = ";")
    # global_data <- global_data %>% select(Variation.coefficient) %>% head(60)
    global_data <- global_data %>% select(Variation.coefficient)
    return(global_data)
}

sessions <- c("A", "B", "C", "D")
global_data <- data.frame()

# Parcourir le dossier des participants
for (participant_folder in stream_folders) {
    participant_id <- substr(basename(participant_folder), 1, 3)
    for (i in 1:4) {
        session_acc <- center_ACC(participant_folder, sessions[i]) %>% head(60*32) *10
        # session_acc <- normalise_ACC(participant_folder, sessions[i])
        session_hr_data <- get_hr_data(participant_folder, sessions[i]) %>% head(60)

        if (!is.null(session_acc) && !is.null(session_hr_data)) {
            session_hr_data_repeated <- session_hr_data %>% 
                slice(rep(1:n(), each = 32))

            session_data <- cbind(session_acc, session_hr_data_repeated)

            session_data <- session_data %>% mutate(
                Participant = participant_id,
                Session = sessions[i]
            )
            global_data <- bind_rows(global_data, session_data)
        }
    }
}

# Enregistrement de la global_data normalisée
write.table(global_data, "D:/MIT project/2024_06 E4 Data/hr_acc.csv", row.names = FALSE, sep=";")

# Effectuer le test de Spearman
spearman_test <- cor.test(global_data$ACC, global_data$Variation.coefficient, method = "spearman")
print(spearman_test)

# Calculer kendall's tau
kendall_tau <- cor.test(global_data$ACC, global_data$Variation.coefficient, method = "kendall")
cat("Kendall's tau:", kendall_tau$estimate, "\n")

# Calculer Somers' D
somers_d <- somers2(global_data$ACC, global_data$Variation.coefficient)
cat("Somers' D:", somers_d$Dxy, "\n")

# # Plot les deux courbes sur le meme graphique (session_acc et session_hr_data)
        # # ACC
        # p <- ggplot(session_data, aes(x = 1:nrow(session_data))) +
        #     geom_line(aes(y = ACC), color = "blue") +
        #     geom_point(aes(y = ACC), color = "blue") +
        #     labs(title = paste("Participant", participant_id, "Session", sessions[i], "ACC"),
        #          x = "Time", y = "ACC") +
        #     theme(axis.text.x = element_text(angle = 45, hjust = 1))

        # # HR
        # p <- p + geom_line(aes(y = Variation.coefficient), color = "red") +
        #     geom_point(aes(y = Variation.coefficient), color = "red") +
        #     labs(title = paste("Participant", participant_id, "Session", sessions[i], "HR"),
        #          x = "Time", y = "HR") +
        #     theme(axis.text.x = element_text(angle = 45, hjust = 1))

        # # Save the plot
        # save_plot_path <- file.path(participant_folder, paste0(sessions[i], "_ACC_HR_plot.png"))
        # ggsave(save_plot_path, plot = p, device = "png", width = 10, height = 6)