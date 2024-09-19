library(signal)
library(pracma)
library(dplyr)
library(FSA)
library(ggsignif)
library(Hmisc)
library(ggplot2)

# This script performs a Spearman correlation test between accelerometer data (ACC) and delta heart rate (ΔHR) data.

source("center_ACC.R")

base_path <- "cleaned_data"
stream_folders <- list.dirs(base_path, recursive = FALSE)


# Function to retrieve dHR data
get_hr_data <- function(participant_folder, session) {
    file_path <- file.path(participant_folder, "HR", paste0(session, "_HR.rds"))
    if (!file.exists(file_path)) return(NULL)
    data <- readRDS(file_path)
    data <- data %>% select(Delta_Heart_Rate)
    return(data)
}

sessions <- c("A", "B", "C", "D")
global_data <- data.frame()

# Browse the participants' file
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

# Normalized Global_Data Registration
# write.table(global_data, "hr_acc.csv", row.names = FALSE, sep=";")

# Perform the Spearman test
spearman_test <- cor.test(global_data$ACC, global_data$Delta_Heart_Rate, method = "spearman")
print(spearman_test)

# Calculate Kendall's Tau
# kendall_tau <- cor.test(global_data$ACC, global_data$Delta_Heart_Rate, method = "kendall")
# cat("Kendall's tau:", kendall_tau$estimate, "\n")

# Calculate Somers' D
# somers_d <- somers2(global_data$ACC, global_data$Delta_Heart_Rate)
# cat("Somers' D:", somers_d$Dxy, "\n")

# # Plot the two curves on the same graphic (session_acc and session_hr_data)
        # # ACC
        # p <- ggplot(session_data, aes(x = 1:nrow(session_data))) +
        #     geom_line(aes(y = ACC), color = "blue") +
        #     geom_point(aes(y = ACC), color = "blue") +
        #     labs(title = paste("Participant", participant_id, "Session", sessions[i], "ACC"),
        #          x = "Time", y = "ACC") +
        #     theme(axis.text.x = element_text(angle = 45, hjust = 1))

        # # HR
        # p <- p + geom_line(aes(y = Delta_Heart_Rate), color = "red") +
        #     geom_point(aes(y = Delta_Heart_Rate), color = "red") +
        #     labs(title = paste("Participant", participant_id, "Session", sessions[i], "HR"),
        #          x = "Time", y = "HR") +
        #     theme(axis.text.x = element_text(angle = 45, hjust = 1))

        # # Save the plot
        # save_plot_path <- file.path(participant_folder, paste0(sessions[i], "_ACC_HR_plot.png"))
        # ggsave(save_plot_path, plot = p, device = "png", width = 10, height = 6)