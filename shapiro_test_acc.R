base_path <- "D:/path_to_folder/Cleaned data"
# List all the files in the basic path
stream_folders <- list.dirs(base_path, recursive = FALSE)

# This script performs Shapiro-Wilk normality tests on accelerometer (ACC) data to determine if the data follows a normal distribution, 
# and tracks the minimum and maximum p-values across all tests.

min_p_value_ACC <- 10000
max_p_value_ACC <- 0

source("D:/path_to_folder/E4_quantitative_analysis/standardize_ACC.R")

shapirowilk_test <- function(data_file) {  
    # Perform Shapiro-Wilk normality test
    shapiro_test <- shapiro.test(data_file$ACC)
    min_p_value_ACC <- min(shapiro_test$p.value, min_p_value_ACC)
    max_p_value_ACC <- max(shapiro_test$p.value, max_p_value_ACC)

    # Check the p-value
    if (shapiro_test$p.value > 0.05) {
        cat(paste("The data in", file_path, "follows a normal distribution."))
    } 
    else {
        print("The data does not follow a normal distribution.")
    }
  
  return (c(min_p_value_ACC, max_p_value_ACC))
}

for (participant_folder in stream_folders) {
    data_A = normalise_ACC(participant_folder, "A")
    data_B = normalise_ACC(participant_folder, "B")
    data_C = normalise_ACC(participant_folder, "C")
    data_D = normalise_ACC(participant_folder, "D")
    test_A <- shapirowilk_test(data_A)
    test_B <- shapirowilk_test(data_B)
    test_C <- shapirowilk_test(data_C)
    test_D <- shapirowilk_test(data_D)

    min_p_value_ACC <- min(min_p_value_ACC, test_A[1], test_B[1], test_C[1], test_D[1])
    max_p_value_ACC <- max(max_p_value_ACC, test_A[2], test_B[2], test_C[2], test_D[2])
}

cat(paste("Minimum p-value for HR:", min_p_value_ACC, "\n"))
cat(paste("Maximum p-value for HR:", max_p_value_ACC, "\n"))
