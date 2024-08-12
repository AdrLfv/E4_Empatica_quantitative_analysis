library(dplyr)
library(tidyr)
library(ggplot2)
library(FSA)
library(ggsignif)


participant_file <- "D:/MIT project/2024_06 E4 Data/participants.csv"
participant_data <- read.csv(participant_file, header = TRUE, stringsAsFactors = FALSE, sep = ";")



data <- c()
data$diff_A <- participant_data$A_diagram_after - participant_data$A_diagram_before
data$diff_B <- participant_data$B_diagram_after - participant_data$B_diagram_before

# Perform Shapiro-Wilk normality test
shapiro_test <- shapiro.test(data$diff_A)
cat("Shapiro-Wilk test for A:", shapiro_test$p.value, "\n")

shapiro_test <- shapiro.test(data$diff_B)
cat("Shapiro-Wilk test for B:", shapiro_test$p.value, "\n")

# Visualisation des différences
# Histogrammes
hist(data$diff_A, main="Histogramme des différences (Session A)", xlab="Différences")
hist(data$diff_B, main="Histogramme des différences (Session B)", xlab="Différences")

# Diagrammes de boîte
boxplot(data$diff_A, main="Boxplot des différences (Session A)", xlab="Différences")
boxplot(data$diff_B, main="Boxplot des différences (Session B)", xlab="Différences")