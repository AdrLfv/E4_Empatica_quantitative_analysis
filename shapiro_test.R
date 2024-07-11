base_path <- "D:/MIT project/2024_06 E4 Data/Cleaned data"
# Lister tous les dossiers dans le chemin de base
stream_folders <- list.dirs(base_path, recursive = FALSE)

shapirowilk_test <- function(file_path) {
  data_file <- read.csv(file_path, header = FALSE, stringsAsFactors = FALSE, sep = ";")
  
  column_names <- c("V2", "V3")
  for (column_name in column_names) {
    data <- as.numeric(data_file[[column_name]][2:nrow(data_file)])
    # Perform Shapiro-Wilk normality test
    shapiro_test <- shapiro.test(data)
    # Print the results
    print(shapiro_test)
    # Check the p-value
    if (shapiro_test$p.value > 0.05) {
      cat(paste("The data in", file_path, "follows a normal distribution."))
    } 
    else {
      print("The data does not follow a normal distribution.")
    }
  }
}

for (participant_folder in stream_folders) {
    shapirowilk_test(file_path <- file.path(participant_folder, "HR/A_HR.csv"))
    shapirowilk_test(file_path <- file.path(participant_folder, "HR/B_HR.csv"))
    shapirowilk_test(file_path <- file.path(participant_folder, "HR/C_HR.csv"))
    shapirowilk_test(file_path <- file.path(participant_folder, "HR/D_HR.csv"))
    
  }
