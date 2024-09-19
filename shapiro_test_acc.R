base_path <- "cleaned_data"
# List all the files in the basic path
stream_folders <- list.dirs(base_path, recursive = FALSE)

# This script performs Shapiro-Wilk normality tests on accelerometer (ACC) data to determine if the data follows a normal distribution, 
# and tracks the minimum and maximum p-values across all tests.

min_p_value_ACC <- 10000
max_p_value_ACC <- 0

source("center_ACC.R")

shapirowilk_test <- function(data_file, session) {  
    # Perform Shapiro-Wilk normality test
    shapiro_test <- shapiro.test(data_file$ACC)
    min_p_value_ACC <- min(shapiro_test$p.value, min_p_value_ACC)
    max_p_value_ACC <- max(shapiro_test$p.value, max_p_value_ACC)

    # Check the p-value
    if (shapiro_test$p.value > 0.05) {
        cat(paste("The data in", file_path, "follows a normal distribution."))
    } 
    else {
        cat(paste("The data does not follow a normal distribution for session", session, "(p.value <= 0.05)\n"))
    }
  
  return (c(min_p_value_ACC, max_p_value_ACC))
}

for (participant_folder in stream_folders) {
    participant_ID <- basename(participant_folder)
    print(paste("Participant", participant_ID, ":"))
    data_A = center_ACC(participant_folder, "A") %>% head(60*32) *10
    data_B = center_ACC(participant_folder, "B") %>% head(60*32) *10
    data_C = center_ACC(participant_folder, "C") %>% head(60*32) *10
    data_D = center_ACC(participant_folder, "D") %>% head(60*32) *10
    test_A <- shapirowilk_test(data_A, "A")
    test_B <- shapirowilk_test(data_B, "B")
    test_C <- shapirowilk_test(data_C, "C")
    test_D <- shapirowilk_test(data_D, "D")

    min_p_value_ACC <- min(min_p_value_ACC, test_A[1], test_B[1], test_C[1], test_D[1])
    max_p_value_ACC <- max(max_p_value_ACC, test_A[2], test_B[2], test_C[2], test_D[2])
}

cat(paste("Minimum p-value for HR:", min_p_value_ACC, "\n"))
cat(paste("Maximum p-value for HR:", max_p_value_ACC, "\n"))
