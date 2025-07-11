# Load the necessary packages
library(readxl)  # To read Excel files
library(ggplot2) #To create graphics
library(dplyr)   # To manipulate the data
library(tidyr)   # To transform data

# This script processes connection diagram data and generates box plots to visualize the connections before and after experimenting MirrorFugue.

participants_data <- readRDS("data_rds/participants.rds")

# To count the number of occurences
all_familiarities = "SSFFSSFSPSRAPFSSFSFSPFRSSSFSFSSSSSASASFAFSFSFRRSFFRAASFR"
familiarities_A = "SFSFSFFRSFFSSAAFFFFFAF"

# Show the number of each familiarity in the session A
# print(table(strsplit(familiarities_A, "")[[1]]))

# Construction of a table from the average of a_diagram_before and a_diagram_after, and that of B_Diagram_before and B_Diagram_after
data_long <- participants_data %>% select(ID, A_diagram_before, A_diagram_after, B_diagram_before, B_diagram_after) %>%
  mutate(mean_connection_before = (A_diagram_before + B_diagram_before) / 2,
         mean_connection_after = (A_diagram_after + B_diagram_after) / 2)

variation <- data_long %>% select(ID, mean_connection_before, mean_connection_after) %>%
  mutate(mean_variation = mean_connection_after - mean_connection_before)

#wilcoxonSignedRankTestForTheConnectionLevelBeforeAndAfterTheMirrorFugueExperiment
wilcoxon_test <- wilcox.test(variation$mean_connection_before, variation$mean_connection_after, paired = TRUE, alternative = "two.sided")
print(wilcoxon_test)