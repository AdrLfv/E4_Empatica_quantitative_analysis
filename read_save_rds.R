# Read the raw e4 streams from csv files and back up data in an RDS file

# Create a folder for the data in the current folder if it does not exist
if (!dir.exists("data_rds")) {
    dir.create("data_rds")
}
if (!dir.exists("data_rds/E4_streams")) {
    dir.create("data_rds/E4_streams")
}


# Load the data
base_path <- "data_csv/E4 streams"
participant_path <- "data_csv/participants.csv"
videos_timecodes_path <- "data_csv/videos timecodes.csv"

# List all files in the basic path
stream_folders <- list.dirs(base_path, recursive = FALSE)

participants_data <- read.csv(participant_path, header = TRUE, stringsAsFactors = FALSE, sep = ";")
videos_timecodes <- read.csv(videos_timecodes_path, header = FALSE, stringsAsFactors = FALSE, sep = ";")

# Load all the ACC.csv, EDA.csv, HR.csv, tags.csv in each folder and save them in an RDS file
for (folder in stream_folders) {
    participant_ID <- substr(basename(folder), 1, 3)
    # Load the data
    acc_data <- read.csv(file.path(folder, "ACC.csv"), header = FALSE, stringsAsFactors = FALSE, sep = ",")
    eda_data <- read.csv(file.path(folder, "EDA.csv"), header = FALSE, stringsAsFactors = FALSE, sep = ";")
    hr_data <- read.csv(file.path(folder, "HR.csv"), header = FALSE, stringsAsFactors = FALSE, sep = ";")
    tags_data <- read.csv(file.path(folder, "tags.csv"), header = FALSE, stringsAsFactors = FALSE, sep = ";")

    # Save the all data in one RDS file
    saveRDS(list(acc_data = acc_data, eda_data = eda_data, hr_data = hr_data, tags_data = tags_data), file.path(paste("data_rds/E4_streams/data_", participant_ID, ".rds", sep = "")))
}

saveRDS(participants_data, "data_rds/participants.rds")
saveRDS(videos_timecodes, "data_rds/videos_timecodes.rds")

print("Data saved in RDS files")