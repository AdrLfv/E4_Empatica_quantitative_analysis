# This script performs Shapiro-Wilk normality tests on heart rate (HR) and delta heart rate (ΔHR) data to determine 
# if the data follows a normal distribution, and tracks the minimum and maximum p-values across all tests.

base_path <- "D:/path_to_folder/Cleaned data"

# List all the files in the basic path
stream_folders <- list.dirs(base_path, recursive = FALSE)

min_p_value_HR <- 10000
max_p_value_HR <- 0
min_p_value_DHR <- 10000
max_p_value_DHR <- 0

shapirowilk_test <- function(file_path) {
  data_file <- read.csv(file_path, header = FALSE, stringsAsFactors = FALSE, sep = ";")
  
  column_names <- c("V2", "V3")
  for (column_name in column_names) {
    data <- as.numeric(data_file[[column_name]][2:nrow(data_file)])
    # Perform Shapiro-Wilk normality test
    shapiro_test <- shapiro.test(data)
    # Print the results
    # print(shapiro_test)
    if (column_name == "V2") {
      min_p_value_HR <- min(shapiro_test$p.value, min_p_value_HR)
      max_p_value_HR <- max(shapiro_test$p.value, max_p_value_HR)
    } else {
      min_p_value_DHR <- min(shapiro_test$p.value, min_p_value_DHR)
      max_p_value_DHR <- max(shapiro_test$p.value, max_p_value_DHR)
    }
    # Check the p-value
    if (shapiro_test$p.value > 0.05) {
      # cat(paste("The data in", file_path, "follows a normal distribution."))
    } 
    else {
      # print("The data does not follow a normal distribution.")
    }
  }
  return (c(min_p_value_HR, max_p_value_HR, min_p_value_DHR, max_p_value_DHR))
}

for (participant_folder in stream_folders) {
  test_A <- shapirowilk_test(file_path <- file.path(participant_folder, "HR/A_HR.csv"))
  test_B <- shapirowilk_test(file_path <- file.path(participant_folder, "HR/B_HR.csv"))
  test_C <- shapirowilk_test(file_path <- file.path(participant_folder, "HR/C_HR.csv"))
  test_D <- shapirowilk_test(file_path <- file.path(participant_folder, "HR/D_HR.csv"))

  min_p_value_HR <- min(min_p_value_HR, test_A[1], test_B[1], test_C[1], test_D[1])
  max_p_value_HR <- max(max_p_value_HR, test_A[2], test_B[2], test_C[2], test_D[2])
  min_p_value_DHR <- min(min_p_value_DHR, test_A[3], test_B[3], test_C[3], test_D[3])
  max_p_value_DHR <- max(max_p_value_DHR, test_A[4], test_B[4], test_C[4], test_D[4])
}

cat(paste("Minimum p-value for HR:", min_p_value_HR, "\n"))
cat(paste("Maximum p-value for HR:", max_p_value_HR, "\n"))
cat(paste("Minimum p-value for ΔHR:", min_p_value_DHR, "\n"))
cat(paste("Maximum p-value for ΔHR:", max_p_value_DHR, "\n"))
