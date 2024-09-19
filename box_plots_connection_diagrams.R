# Load the necessary packages
library(readxl)  # To read Excel files
library(ggplot2) # To create graphics
library(dplyr)   # To manipulate the data
library(tidyr)   # To transform data

# This script generates box plots to visualize the felt connections before and after sessions for different familiarity levels.

# Define the path to the Excel file
base_path <- "cleaned_data"
stream_folders <- list.dirs(base_path, recursive = FALSE)
participants_data <- readRDS("data_rds/participants.rds")

#Create a dictionary to store the familiarity and color of each participant
participants_colors_familiarity <- list()

for (participant_folder in stream_folders) {
    participant_id <- substr(basename(participant_folder), 1, 3)
    participant_info <- participants_data %>% filter(ID == participant_id)
    familiarity <- substr(participant_info$Familiarity, 1, 1)
    # Assign a color and a description to each level of familiarity
    if (familiarity == "S" ||familiarity == "H") {
        participants_colors_familiarity[[participant_id]] <- c("Stranger", "red")
    } 
    # else if (familiarity == "H") {
    #     participants_colors_familiarity[[participant_id]] <- c("Hear of", "orangered")
    # } else if (familiarity == "M") {
    #     participants_colors_familiarity[[participant_id]] <- c("Met", "orange")
    # } 
    else if (familiarity == "A" ||familiarity == "M") {
        participants_colors_familiarity[[participant_id]] <- c("Acquaintance", "orange")
    } else if (familiarity == "F") {
        participants_colors_familiarity[[participant_id]] <- c("Friend", "yellow")
    } else if (familiarity == "R") {
        participants_colors_familiarity[[participant_id]] <- c("Relative", "yellowgreen")
    } else if (familiarity == "P") {
        participants_colors_familiarity[[participant_id]] <- c("Self", "green")
    } else {
      print(paste("Unknown familiarity level for participant", participant_id, ": ", familiarity))
    }
}

# Select the necessary columns for sessions A
data_A <- participants_data %>% 
  select(ID, A_diagram_before, A_diagram_after) %>%
  mutate(connection_before = A_diagram_before,
         connection_after = A_diagram_after)

# Reshaping data for ggplot (sessions a)
data_A_long <- data_A %>%
  pivot_longer(cols = c(connection_before, connection_after),
               names_to = "Condition",
               values_to = "Connection") %>%
  mutate(Condition = factor(Condition, levels = c("connection_before", "connection_after"), labels = c("Before", "After")),
         Familiarity = sapply(ID, function(id) participants_colors_familiarity[[id]][1]),
         Color = sapply(ID, function(id) participants_colors_familiarity[[id]][2]))

# Select the necessary columns for B sessions
data_B <- participants_data %>% 
  select(ID, B_diagram_before, B_diagram_after) %>%
  mutate(connection_before = B_diagram_before,
         connection_after = B_diagram_after)


# Reshaping data for ggplot (B sessions)
data_B_long <- data_B %>%
  pivot_longer(cols = c(connection_before, connection_after),
               names_to = "Condition",
               values_to = "Connection") %>%
  mutate(Condition = factor(Condition, levels = c("connection_before", "connection_after"), labels = c("Before", "After")),
         Familiarity = sapply(ID, function(id) participants_colors_familiarity[[id]][1]),
         Color = sapply(ID, function(id) participants_colors_familiarity[[id]][2]))
         
# Define the order of familiarity levels and corresponding colors
familiarity_levels <- c("Stranger", "Acquaintance", "Friend", "Relative", "Self")
familiarity_colors <- c("red", "orange", "yellow", "yellowgreen", "green")

if (!all(familiarity_levels %in% unique(data_A_long$Familiarity))) {
  stop("Familiarity levels do not match the data in session A")
}

# Update the data with the correct order
data_A_long <- data_A_long %>%
  mutate(Familiarity = factor(Familiarity, levels = familiarity_levels))

# Create and save the graph for the session A
ggplot(data_A_long, aes(x = Condition, y = Connection)) +
  geom_boxplot(alpha = 0.7, color = "black") +  # Fixed color for mustache boxes
  geom_jitter(width = 0.06, size = 5, alpha = 0.7, aes(color = Familiarity)) +
  scale_color_manual(values = familiarity_colors) +  # Assign colors manually
  #ggtitle("Box Plot of Felt Connection before and after experimenting MirrorFugue (Session A)") +
  xlab("Questionnaire moment") +
  ylab("Connection Level") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        text = element_text(family = "Arial", size = 18)) +
  guides(color = guide_legend(title = "Familiarity"))
ggsave("plots/connection_plot_A.png", width = 10, height = 6)

# Update the data for the B session with the correct order
data_B_long <- data_B_long %>%
  mutate(Familiarity = factor(Familiarity, levels = familiarity_levels))

# Create and save the graphic for session B
ggplot(data_B_long, aes(x = Condition, y = Connection)) +
  geom_boxplot(alpha = 0.7, color = "black") +  # Fixed color for mustache boxes
  geom_jitter(width = 0.06, size = 5, alpha = 0.7, aes(color = Familiarity)) +
  scale_color_manual(values = familiarity_colors) +  # Assign colors manually
  #ggtitle("Box Plot of Felt Connection before and after experimenting MirrorFugue(Session B)") +
  xlab("Questionnaire moment") +
  ylab("Connection Level") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        text = element_text(family = "Arial", size = 18)) +
  guides(color = guide_legend(title = "Familiarity"))
ggsave("plots/connection_plot_B.png", width = 10, height = 6)
