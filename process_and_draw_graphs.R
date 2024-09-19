# Install the necessary packages if they are not already
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")

library(dplyr)
library(ggplot2)

# This script processes and synchronizes E4 data streams (HR, EDA, ACC) with video recordings, 
# generates visualizations using ggplot2, and saves the cleaned_data for further analysis.
# The script assumes that the raw data is already loaded and saved in RDS files.

# Define the basic path
base_path <- "data_rds/E4_streams"

# Check and create the "cleaned_data" folder if there is no
if (!dir.exists("cleaned_data")) {
  dir.create("cleaned_data", recursive = TRUE)
}

videos_timecodes <- readRDS("data_rds/videos_timecodes.rds")
participants_data <- readRDS("data_rds/participants.rds")
stream_files <- list.files(base_path, full.names = TRUE, recursive = TRUE)

time_to_seconds <- function(time_str) {
  hms <- as.numeric(strsplit(time_str, ":")[[1]])
  return(hms[1] * 3600 + hms[2] * 60 + hms[3])
}

# Function to convert seconds to "HH: MM: SS"
seconds_to_time <- function(seconds) {
  hours <- floor(seconds / 3600)
  minutes <- floor((seconds %% 3600) / 60)
  seconds <- seconds %% 60
  return(sprintf("%02d:%02d:%02d", hours, minutes, seconds))
}

# Function to process a CSV file (HR or EDA)
process_HR_EDA <- function(data, tags_data, participant, file_name) {

  if (nrow(data) > 0) {
    data$V2 <- rep(NA, nrow(data))
    data$V3 <- rep(NA, nrow(data))
    data$V4 <- rep(NA, nrow(data))
    data$V5 <- rep(NA, nrow(data))
    
    data$V2[3:nrow(data)] <- seq(0, (nrow(data) - 3))
    data$V3[3:nrow(data)] <- data[1, 1] + data$V2[3:nrow(data)] / data[2, 1]

    data$V4 <- as.POSIXct(data$V3, origin = "1970-01-01", tz = "GMT") + (-4 * 3600)
    data$V4 <- format(data$V4, format = "%H:%M:%S")
    
    # tags_path <- file.path(folder, "tags.csv")
    
    # tags_data <- read.csv(tags_path, header = FALSE, stringsAsFactors = FALSE, sep = ";")
    timestamp_unix <- as.numeric(tags_data[1, 1])
    
    # Find the participant's index in videos_timecodes
    participant_index <- which(videos_timecodes[, 1] == participant)
    
    if (length(participant_index) > 0) {
      timestamp_video <- as.POSIXct(videos_timecodes[participant_index, 10], format = "%H:%M:%S", tz = "GMT")
      
      data$V2[is.na(data$V2)] <- ""
      data$V3[is.na(data$V3)] <- ""
      data$V4[is.na(data$V4)] <- ""
      data$V5[is.na(data$V5)] <- ""
      
      data[1, 2] <- "Timecode ID"
      data[1, 3] <- "Timecode UNIX"
      data[1, 4] <- "Timecode"
      data[1, 5] <- "In video timecode"
      
      # Create a file for each participant in "cleaned_data"
      participant_folder <- file.path("cleaned_data", participant, file_name)  # Subdossier file_name for each participant
      dir.create(participant_folder, showWarnings = FALSE, recursive = TRUE)
      
      # Create 4 CSV files for each participant (A, B, C, D)
      
      columns <- list(c(2, 3), c(4, 5), c(6, 7), c(8, 9))  # Utiliser des indices de colonne
      
      # =============================================================
      # TO MODIFY
      sessions <- c("A", "B", "C", "D")
      EDA_y_lim_min = 0
      EDA_y_lim_max = 10 # To modify regarding the EDA max value
      HR_y_lim_min = 50 # To modify regarding the HR min value
      HR_y_lim_max = 170 # To modify regarding the HR max value      
      HR_var_y_lim_min = -120 # To modify regarding the Delta Heart Rate min value
      HR_var_y_lim_max = 120 # To modify regarding the Delta Heart Rate max value
      # Temporary values ​​for Edr_min and Edr_max
      EDR_min = EDA_y_lim_max
      EDR_max = EDA_y_lim_min
      # =============================================================

      #We create a list of dataframas for each session
      EDR_passage_data_list  <- list()
      participant_order <- participants_data[participants_data$ID == participant, "Order"]
      
      for (i in 1:length(sessions)) {
        session <- sessions[i]
        print(session)
        start_col <- columns[[i]][1]
        end_col <- columns[[i]][2]
        
        video_start_time <- videos_timecodes[which(videos_timecodes[, 1] == participant), start_col]
        video_end_time <- videos_timecodes[which(videos_timecodes[, 1] == participant), end_col]
        
        #convert TimestampUnix to Hh:mm:ss
        timestamp <- format(as.POSIXct(timestamp_unix, origin = "1970-01-01", tz = "GMT") + (-4 * 3600), format = "%H:%M:%S")
        time_stamp_video_format <- format(as.POSIXct(timestamp_video, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), "%H:%M:%S")
        
        timestamp_sec <- time_to_seconds(timestamp)
        video_start_time_sec <- time_to_seconds(video_start_time)
        video_end_time_sec <- time_to_seconds(video_end_time)
        timestamp_video_sec <- time_to_seconds(time_stamp_video_format)

        # We add Timetamp and Video Start_Time and subtract Timetamp Video
        timecode_start_passage <- seconds_to_time(timestamp_sec + video_start_time_sec - timestamp_video_sec)
        timecode_end_passage <- seconds_to_time(timestamp_sec + video_end_time_sec - timestamp_video_sec)
        
        matching_start_index <- which(data$V4 == timecode_start_passage)[1]
        matching_end_index <- which(data$V4 == timecode_end_passage)[1]
        
        time_sequence <- seq(from = timestamp_video, by = 1/data[2, 1], length.out = matching_end_index - matching_start_index + 1)
        time_sequence_formatted <- format(time_sequence, "%H:%M:%S")
        
        # V1 = Time, V2 = HR, V3 = Delta Heart Rate
        if (length(matching_start_index) > 0 & length(matching_end_index) > 0) {
          passage_data <- data.frame(
            V1 = time_sequence_formatted,
            V2 = data$V1[matching_start_index:matching_end_index],
            #Calculation of the Delta Heart Rate
            V3 = data$V1[matching_start_index:matching_end_index] - data$V1[matching_start_index]
          )
          passage_data_rds <- passage_data
          
          passage_data$V1 <- as.POSIXct(passage_data$V1, format = "%H:%M:%S", tz = "GMT")
          if (any(is.na(passage_data$V1))) {
            passage_data$V1 <- as.POSIXct(strptime(passage_data$V1, format = "%H:%M:%S", tz = "GMT"))
          }
          
          plot_data <- data.frame(
            Time = passage_data$V1,
            Pulse = passage_data$V2
          )
          
          if (file_name == "HR") {
            title = "Pulse over Time"
            y_lim <- c(HR_y_lim_min, HR_y_lim_max)
            y_unit <- "Pulse (bpm)"
            y_breaks <- seq(HR_y_lim_min, HR_y_lim_max, by = 10)  # Intervals every 10 BPM
            y_lim_variation <- c(HR_var_y_lim_min, HR_var_y_lim_max, by = 10)
            y_breaks_variation <- seq(HR_var_y_lim_min, HR_var_y_lim_max, by = 10)  # Intervals every 10 BPM
          }
          else if (file_name == "EDA") {
            title = "Electrodermal activity over Time"
            y_lim <- c(EDA_y_lim_min, EDA_y_lim_max)
            y_unit <- "EDA (μS)"
            y_breaks <- seq(EDA_y_lim_min, EDA_y_lim_max, by = 1)  # Intervals every 1 μs
          }
          # If the value in participants.
          if (participants_data[participants_data$ID == participant, session] == "A") {
            plot_color <- "#cf0000"
          } else if (participants_data[participants_data$ID == participant, session] == "B"){
            plot_color <- "#009c0b"
          } else if (participants_data[participants_data$ID == participant, session] == "C"){
            plot_color <- "#1d00cf"
          } else if (participants_data[participants_data$ID == participant, session] == "D"){
            plot_color <- "#beb300"
          } else if (participants_data[participants_data$ID == participant, session] == "A-P05"){
            plot_color <- "#cf00b9"
          } else if (participants_data[participants_data$ID == participant, session] == "A-P07"){
            plot_color <- "#d87f00"
          } else if (participants_data[participants_data$ID == participant, session] == "A-P11"){
            plot_color <- "#00b4cf"
          }
          else {
            print("Participant not found in Participants.csv")
            print(participant)
            print(session)
          }
          # Calculation of the session number
          char_list <- strsplit(participant_order, split = "")[[1]]
          session_number <- which(char_list == session)
          if (session_number == 1) {
            session_number_in_letters <- "first"
          } else if (session_number == 2) {
            session_number_in_letters <- "second"
          } else if (session_number == 3) {
            session_number_in_letters <- "third"
          } else if (session_number == 4) {
            session_number_in_letters <- "fourth"
          }
          substr(participant_order, i, i)
          # Creation of a graph for the session concerned
          p <- ggplot(plot_data, aes(x = Time, y = Pulse)) +
            geom_line(color=plot_color) +
            labs(title = paste(title, "- participant", participant, "-", session_number_in_letters, "session", "-", session),
                  x = "Time (hh:mm:ss)",
                  y = y_unit) +
            scale_x_datetime(date_labels = "%H:%M:%S", date_breaks = "10 sec") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            scale_y_continuous(limits = y_lim, breaks = y_breaks)
          
          # Add a red dotted line to 0.1 for EDA
          if (file_name == "EDA") {
            p <- p + geom_hline(yintercept = 0.1, linetype = "dashed", color = "black")
          }
          
          ggsave(filename = file.path(participant_folder, paste0(session, "_pulse_plot.png")),
                  plot = p, width = 10, height = 5, units = "in")
          if (file_name == "HR") {
            # Addition of a variable for the Delta Heart Rate
            plot_data_variation <- data.frame(
              Time = passage_data$V1,
              DeltaHR = passage_data$V3
            )
            
            # Creation of a graph for the Delta Heart Rate
            p_variation <- ggplot(plot_data_variation, aes(x = Time, y = DeltaHR)) +
              geom_line(color = plot_color) +
              geom_hline(yintercept = 0, color = "red", linetype = "dashed") +  # Ligne rouge en pointillés à y=0
              geom_ribbon(aes(ymin = pmin(DeltaHR, 0), ymax = 0), fill = "red", alpha = 0.2) +  # Zone rouge pour les valeurs négatives
              geom_ribbon(aes(ymin = 0, ymax = pmax(DeltaHR, 0)), fill = "green", alpha = 0.2) +  # Zone verte pour les valeurs positives
              labs(title = paste("Delta Heart Rate over Time - participant", participant, "-", session_number_in_letters, "session", "-", session),
                  x = "Time (hh:mm:ss)",
                  y = "Delta Heart Rate") +
              scale_x_datetime(date_labels = "%H:%M:%S", date_breaks = "10 sec") +
              theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
              scale_y_continuous(limits = y_lim_variation, breaks = y_breaks_variation)

            
            # Graphic backup
            ggsave(filename = file.path(participant_folder, paste0(session, "_delta_hr_plot.png")),
                    plot = p_variation, width = 10, height = 5, units = "in")
          }
          # We update Edr_min and Edr_max
          if (file_name == "EDA") {
            EDR_min = min(EDR_min, min(passage_data$V2))
            EDR_max = max(EDR_max, max(passage_data$V2))
            EDR_passage_data_list[[i]] <- passage_data_rds
          }
          else if (file_name == "HR") {
            names(passage_data_rds)[names(passage_data) == 'V1'] <- 'Time'
            names(passage_data_rds)[names(passage_data) == 'V2'] <- 'HR'
            names(passage_data_rds)[names(passage_data) == 'V3'] <- 'Delta_Heart_Rate'

            # save in rds file
            saveRDS(passage_data_rds, file.path(participant_folder, paste0(session, "_HR.rds")))
            
            # write.table(passage_data_csv, file = file.path(participant_folder, paste0(session, "_HR.csv")), row.names = FALSE, sep = ";", col.names = TRUE)
          } 
          
          #cat(paste("\nSaved participant", participant, file_name, "data for", "session", session))
        } else {
          warning(paste("No correspondence found for the sessions", session, "in the file"))
        }
      }
      
      # We apply the formula edr_x2 = (edr_x1 - edr_min) / (edr_max - edr_min) with each element of V2 and it is recorded in a CSV file
      for (i in 1:4) {
        session <- sessions[i]
        if (file_name == "EDA") {
          EDR_passage_data <- EDR_passage_data_list[[i]]
          EDR_passage_data$V4 <- (EDR_passage_data$V2 - EDR_min) / (EDR_max - EDR_min)
          
          names(EDR_passage_data)[names(EDR_passage_data) == 'V1'] <- 'Time'
          names(EDR_passage_data)[names(EDR_passage_data) == 'V2'] <- 'EDA'
          names(EDR_passage_data)[names(EDR_passage_data) == 'V3'] <- 'Delta_Heart_Rate'
          names(EDR_passage_data)[names(EDR_passage_data) == 'V4'] <- 'Standardized EDA'
          # save in rds file
          saveRDS(EDR_passage_data, file.path(participant_folder, paste0(session, "_EDA.rds")))

          # write.table(EDR_passage_data, file = file.path(participant_folder, paste0(session, "_EDA.csv")), row.names = FALSE, sep = ";", col.names = TRUE)
          
        }
      }
    } else {
      warning("Participant not found in videos_timecodes.")
    }
  }
} 

# Function to process CSV files (ACC)
process_ACC <- function(data, tags_data, participant) {
  file_name = "ACC"
  # file_path <- file.path(folder, paste(file_name, ".csv", sep=""))
  # if (file.exists(file_path)) {
  # data <- read.csv(file_path, header = FALSE, stringsAsFactors = FALSE, sep = ",")
  if (nrow(data) > 0) {
    data$V4 <- rep(NA, nrow(data))
    data$V5 <- rep(NA, nrow(data))
    data$V6 <- rep(NA, nrow(data))
    data$V7 <- rep(NA, nrow(data))
    
    data$V4[3:nrow(data)] <- seq(0, (nrow(data) - 3))
    data$V5[3:nrow(data)] <- data[1, 1] + data$V4[3:nrow(data)] / data[2, 1]
    data$V6 <- as.POSIXct(data$V5, origin = "1970-01-01", tz = "GMT") + (-4 * 3600)
    data$V6 <- format(data$V6, format = "%H:%M:%S")
    
    # tags_path <- file.path(folder, "tags.csv")
    # tags_data <- read.csv(tags_path, header = FALSE, stringsAsFactors = FALSE, sep = ";")
    timestamp_unix <- as.numeric(tags_data[1, 1])
    
    # Extract the participant from the name of the file
    # participant <- substr(basename(folder), 1, 3)
    
    # Find the participant's index in videos_timecodes
    participant_index <- which(videos_timecodes[, 1] == participant)
    
    if (length(participant_index) > 0) {
      timestamp_video <- as.POSIXct(videos_timecodes[participant_index, 10], format = "%H:%M:%S", tz = "GMT")
      
      data$V4[is.na(data$V4)] <- ""
      data$V5[is.na(data$V5)] <- ""
      data$V6[is.na(data$V6)] <- ""
      data$V7[is.na(data$V7)] <- ""
      
      data[1, 4] <- "Timecode ID"
      data[1, 5] <- "Timecode UNIX"
      data[1, 6] <- "Timecode"
      data[1, 7] <- "In video timecode"
      
      # Create a file for each participant in "cleaned_data"
      participant_folder <- file.path("cleaned_data", participant, file_name)  # Subdossier file_name for each participant
      dir.create(participant_folder, showWarnings = FALSE, recursive = TRUE)
      
      # =============================================================
      # TO MODIFY
      
      sessions <- c("A", "B", "C", "D")
      columns <- list(c(2, 3), c(4, 5), c(6, 7), c(8, 9))
      
      ACC_y_lim_min = 0
      ACC_y_lim_max = 120 # To modify regarding the EDA max value
      
      # =============================================================

      #We create a list of dataframas for each session
      participant_order <- participants_data[participants_data$ID == participant, "Order"]
      
      for (i in 1:length(sessions)) {
        session <- sessions[i]
        print(session)
        
        start_col <- columns[[i]][1]
        end_col <- columns[[i]][2]
        
        video_start_time <- videos_timecodes[which(videos_timecodes[, 1] == participant), start_col]
        video_end_time <- videos_timecodes[which(videos_timecodes[, 1] == participant), end_col]
        #convertirTimestampUnixEnHh:mm:ss
        timestamp <- format(as.POSIXct(timestamp_unix, origin = "1970-01-01", tz = "GMT") + (-4 * 3600), format = "%H:%M:%S")
        time_stamp_video_format <- format(as.POSIXct(timestamp_video, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), "%H:%M:%S")
        
        timestamp_sec <- time_to_seconds(timestamp)
        video_start_time_sec <- time_to_seconds(video_start_time)
        video_end_time_sec <- time_to_seconds(video_end_time)
        timestamp_video_sec <- time_to_seconds(time_stamp_video_format)
        # We add Timetamp and Video Start_Time and subtract Timetamp Video
        timecode_start_passage <- seconds_to_time(timestamp_sec + video_start_time_sec - timestamp_video_sec)
        timecode_end_passage <- seconds_to_time(timestamp_sec + video_end_time_sec - timestamp_video_sec)
        
        matching_start_index <- which(data$V6 == timecode_start_passage)[1]
        matching_end_index <- which(data$V6 == timecode_end_passage)[1]
        time_sequence <- seq(from = timestamp_video, by = 1/data[2, 1], length.out = matching_end_index - matching_start_index + 1)
        time_sequence_formatted <- format(time_sequence, "%H:%M:%S")

        # V1 = Time, V2 = HR
        if (length(matching_start_index) > 0 & length(matching_end_index) > 0) {
          passage_data <- data.frame(
            V1 = time_sequence_formatted,
            V2 = data$V1[matching_start_index:matching_end_index],
            V3 = data$V2[matching_start_index:matching_end_index],
            V4 = data$V3[matching_start_index:matching_end_index]
          )
          passage_data_rds <- passage_data
          
          passage_data$V1 <- as.POSIXct(passage_data$V1, format = "%H:%M:%S", tz = "GMT")
          if (any(is.na(passage_data$V1))) {
            passage_data$V1 <- as.POSIXct(strptime(passage_data$V1, format = "%H:%M:%S", tz = "GMT"))
          }

          mean_ACC <- (sqrt(passage_data$V2^2 + passage_data$V3^2 + passage_data$V4^2))
          
          plot_data <- data.frame(
            Time = passage_data$V1,
            Acceleration = mean_ACC  # Acceleration 'Acceleration' Correction '
          )

          if (file_name == "ACC") {
            title = "Acceleration over Time"
            y_lim <- c(ACC_y_lim_min, ACC_y_lim_max)
            y_unit <- "ACC"
            y_breaks <- seq(ACC_y_lim_min, ACC_y_lim_max, by = 10)  # Intervals every 1 μs
          }
          
          # If the value in participants.
          if (participants_data[participants_data$ID == participant, session] == "A") {
            plot_color <- "#cf0000"
          } else if (participants_data[participants_data$ID == participant, session] == "B"){
            plot_color <- "#009c0b"
          } else if (participants_data[participants_data$ID == participant, session] == "C"){
            plot_color <- "#1d00cf"
          } else if (participants_data[participants_data$ID == participant, session] == "D"){
            plot_color <- "#beb300"
          } else if (participants_data[participants_data$ID == participant, session] == "A-P05"){
            plot_color <- "#cf00b9"
          } else if (participants_data[participants_data$ID == participant, session] == "A-P07"){
            plot_color <- "#d87f00"
          } else if (participants_data[participants_data$ID == participant, session] == "A-P11"){
            plot_color <- "#00b4cf"
          }
          else {
            print("Participant not found in participants.csv")
            print(participant)
            print(session)
          }#
          # Calcul du numero de session#
          char_list <- strsplit(participant_order, split = "")[[1]]
          session_number <- which(char_list == session)
          if (session_number == 1) {
            session_number_in_letters <- "first"
          } else if (session_number == 2) {
            session_number_in_letters <- "second"
          } else if (session_number == 3) {
            session_number_in_letters <- "third"
          } else if (session_number == 4) {
            session_number_in_letters <- "fourth"
          }
          #substr(participant_order, i, i)
          # Création d'un graphique pour le session concerné
          p <- ggplot(plot_data, aes(x = Time, y = Acceleration)) +
            geom_line(color=plot_color) +
            labs(title = paste(title, "- participant", participant, "-", session_number_in_letters, "session", "-", session),
                  x = "Time (hh:mm:ss)",
                  y = y_unit) +
            scale_x_datetime(date_labels = "%H:%M:%S", date_breaks = "10 sec") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            scale_y_continuous(limits = y_lim, breaks = y_breaks)
          
          ggsave(filename = file.path(participant_folder, paste0(session, "_ACC_plot.png")),
                  plot = p, width = 10, height = 5, units = "in")
          
          names(passage_data_rds)[names(passage_data) == 'V1'] <- 'Time'
          names(passage_data_rds)[names(passage_data) == 'V2'] <- 'ACC_x'
          names(passage_data_rds)[names(passage_data) == 'V3'] <- 'ACC_y'
          names(passage_data_rds)[names(passage_data) == 'V4'] <- 'ACC_z'

          # write.table(passage_data_csv, file = file.path(participant_folder, paste0(session, "_ACC.csv")), row.names = FALSE, sep = ";", col.names = TRUE)
          
          # save in rds file
          saveRDS(passage_data_rds, file.path(participant_folder, paste0(session, "_ACC.rds")))
          #cat(paste("\nSaved participant", participant, file_name, "data for", "session", session))
        } else {
          # warning(paste("No correspondence found for the sessions", session, "in the file", folder))
        }
      }
    } else {
      warning("Participant not found in videos_timecodes.")
    }
  } else {
    # warning("The file", file_name, "in the file", folder, "is empty.")
  }
} 

# Browse
for (file in stream_files) {
  file_name <- basename(file)
  participant <- substr(file_name, 6, 8)

  # Load the data
  acc_data <- readRDS(file)$acc_data
  eda_data <- readRDS(file)$eda_data
  hr_data <- readRDS(file)$hr_data
  tags_data <- readRDS(file)$tags_data
  
  cat(paste("\nProcessing", participant, "HR data"))
  process_HR_EDA(hr_data, tags_data, participant, "HR")
  
  cat(paste("\nProcessing", participant, "EDA data"))
  eda_data <- process_HR_EDA(eda_data, tags_data, participant, "EDA")
  
  cat(paste("\nProcessing", participant, "accelerometer data"))
  eda_data <- process_ACC(acc_data, tags_data, participant)
}

cat("\nDone!")