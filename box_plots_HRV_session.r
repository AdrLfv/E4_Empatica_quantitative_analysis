library(dplyr)
library(tibble)
library(ggplot2)
library(tidyverse)

base_path <- "D:/MIT project/2024_06 E4 Data/Cleaned data"
combined_data_path <- "D:/MIT project/2024_06 E4 Data/combined_data"

# Sessions
sessions <- c("A", "B", "C", "D")

# Function to recover data from the CSV file
get_session_data <- function(participant_folder, session, participant) {
    file_path <- file.path(participant_folder, "HR", paste0(session, "_HR.csv"))
    data_file <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE, sep = ";")
    data <- data_file %>%
        select(Variation.coefficient) %>%
        head(60) %>%
        pull(Variation.coefficient)  # Recover the Variation.Cefficient data
    
    return(data)
}

# Function to write the data in a CSV file for a given session
write_session_data <- function(session_name) {
    # Initialize a list named to store the data of each participant
    data_list <- vector("list", length(stream_folders))
    names(data_list) <- paste0("P", formatC(1:length(stream_folders), width = 2, flag = "0"))
    
    # Close through each participant and recover the data
    for (i in seq_along(stream_folders)) {
        participant_folder <- stream_folders[i]
        participant <- names(data_list)[i]
        data_list[[participant]] <- get_session_data(participant_folder, session_name, participant)
    }
    
    # Create a Tibble from the data list
    combined_data <- as_tibble(data_list)
    
    # Write data in a CSV file
    write.table(combined_data, file = file.path(combined_data_path, paste0("combined_data_", session_name, ".csv")), 
                row.names = FALSE, sep = ";")
    
    return(combined_data)
}
    

# Create and save the plot box for each session
for (session_name in sessions) {
    # Load combined data for the session
    combined_data <- read.csv(file.path(combined_data_path, paste0("combined_data_", session_name, ".csv")), sep = ";")
    
    # Modify the AES Aesthetics to reflect the columns of your Tibble
    p <- ggplot(combined_data, aes(x = factor(rep(colnames(combined_data), each = nrow(combined_data))), y = value)) +
        geom_boxplot(fill = "skyblue", color = "blue") +
        ggtitle(paste("Box Plot for Session", session_name)) +
        xlab("Participant") +
        ylab("Value") +
        theme_minimal()
    
    # Save the BOX PLOT as a PNG Image
    ggsave(file.path(combined_data_path, paste0("boxplot_", session_name, ".png")), plot = p, width = 8, height = 6)
}