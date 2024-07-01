# Installer les packages nécessaires s'ils ne le sont pas déjà
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")

# Charger les packages
library(dplyr)
library(ggplot2)

# Définir le chemin de base
base_path <- "D:/MIT project/2024_06 E4 Data/E4 streams"
clear_data_path <- "D:/MIT project/2024_06 E4 Data/Cleaned data"

# Vérifier et créer le dossier "Cleared data" s'il n'existe pas
if (!dir.exists(clear_data_path)) {
    dir.create(clear_data_path, recursive = TRUE)
}

video_timecodes_path <- "D:/MIT project/2024_06 E4 Data/videos timecodes.csv"
videos_timecodes <- read.csv(video_timecodes_path, header = FALSE, stringsAsFactors = FALSE, sep = ";")

# Lister tous les dossiers dans le chemin de base
stream_folders <- list.dirs(base_path, recursive = FALSE)

time_to_seconds <- function(time_str) {
  hms <- as.numeric(strsplit(time_str, ":")[[1]])
  return(hms[1] * 3600 + hms[2] * 60 + hms[3])
}

# Fonction pour convertir secondes en "hh:mm:ss"
seconds_to_time <- function(seconds) {
  hours <- floor(seconds / 3600)
  minutes <- floor((seconds %% 3600) / 60)
  seconds <- seconds %% 60
  return(sprintf("%02d:%02d:%02d", hours, minutes, seconds))
}

# Fonction pour traiter un fichier CSV (HR ou EDA)
process_csv <- function(folder, file_name) {
    file_path <- file.path(folder, paste(file_name, ".csv", sep=""))
    
    if (file.exists(file_path)) {
        data <- read.csv(file_path, header = FALSE, stringsAsFactors = FALSE, sep = ";")
        if (nrow(data) > 0) {
          data$V2 <- rep(NA, nrow(data))
          data$V3 <- rep(NA, nrow(data))
          data$V4 <- rep(NA, nrow(data))
          data$V5 <- rep(NA, nrow(data))
          
          data$V2[3:nrow(data)] <- seq(0, (nrow(data) - 3))
          data$V3[3:nrow(data)] <- data[1, 1] + data$V2[3:nrow(data)] / data[2, 1]
          data$V4 <- as.POSIXct(data$V3, origin = "1970-01-01", tz = "GMT") + (-4 * 3600)
          data$V4 <- format(data$V4, format = "%H:%M:%S")
          
          tags_path <- file.path(folder, "tags.csv")
          
  
          if (file.exists(tags_path)) {
              tags_data <- read.csv(tags_path, header = FALSE, stringsAsFactors = FALSE, sep = ";")
              timestamp_unix <- as.numeric(tags_data[1, 1])
              
              # Extraire le participant du nom du dossier
              participant <- substr(basename(folder), 1, 3)
              
              # Trouver l'index du participant dans videos_timecodes
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
                
                # Créer un dossier pour chaque participant dans "D:/MIT project/2024_06 E4 Data/Clear data"
                participant <- substr(basename(folder), 1, 3)  # Extrait les 3 premiers caractères du nom du dossier
                participant_folder <- file.path(clear_data_path, participant, file_name)  # Sous-dossier file_name pour chaque participant
                dir.create(participant_folder, showWarnings = FALSE, recursive = TRUE)
                
                # Créer 4 fichiers CSV pour chaque participant (A, B, C, D)
                passages <- c("A", "B", "C", "D")
                columns <- list(c(2, 3), c(4, 5), c(6, 7), c(8, 9))  # Utiliser des indices de colonne
                
                
                #on crée une liste de dataframes pour chaque passage
                EDR_passage_data_list  <- list()
                HR_passage_data_list  <- list()
                
                # Temporairement on met EDR_min, EDR_max, HR_min et HR_max à des valeurs extrêmes
                EDR_min = 10
                EDR_max = 0.1
                HR_min = 200
                HR_max = 50

                for (i in 1:4) {
                  passage <- passages[i]
                  print(passage)
                  start_col <- columns[[i]][1]
                  end_col <- columns[[i]][2]
                  
                  video_start_time <- videos_timecodes[which(videos_timecodes[, 1] == participant), start_col]
                  video_end_time <- videos_timecodes[which(videos_timecodes[, 1] == participant), end_col]

                  #convertir timestamp_unix en hh:mm:ss
                  timestamp <- format(as.POSIXct(timestamp_unix, origin = "1970-01-01", tz = "GMT") + (-4 * 3600), format = "%H:%M:%S")
                  time_stamp_video_format <- format(as.POSIXct(timestamp_video, format = "%Y-%m-%d %H:%M:%S", tz = "GMT"), "%H:%M:%S")
                  
                  timestamp_sec <- time_to_seconds(timestamp)
                  video_start_time_sec <- time_to_seconds(video_start_time)
                  video_end_time_sec <- time_to_seconds(video_end_time)
                  timestamp_video_sec <- time_to_seconds(time_stamp_video_format)

                  # on additionne time_stamp et video_start_time et on soustrait timestamp_video
                  timecode_start_passage <- seconds_to_time(timestamp_sec + video_start_time_sec - timestamp_video_sec)
                  timecode_end_passage <- seconds_to_time(timestamp_sec + video_end_time_sec - timestamp_video_sec)
                  
                  matching_start_index <- which(data$V4 == timecode_start_passage)[1]
                  matching_end_index <- which(data$V4 == timecode_end_passage)[1]

                  time_sequence <- seq(from = timestamp_video, by = 1/data[2, 1], length.out = matching_end_index - matching_start_index + 1)
                  time_sequence_formatted <- format(time_sequence, "%H:%M:%S")
                  
                  # V1 = Time, V2 = HR, V3 = Variation coefficient
                  if (length(matching_start_index) > 0 & length(matching_end_index) > 0) {
                    passage_data <- data.frame(
                      V1 = time_sequence_formatted,
                      V2 = data$V1[matching_start_index:matching_end_index],
                      V3 = data$V1[matching_start_index:matching_end_index] - data$V1[matching_start_index]
                    )
                    passage_data_csv <- passage_data
                    
                    passage_data$V1 <- as.POSIXct(passage_data$V1, format = "%H:%M:%S", tz = "GMT")
                    if (any(is.na(passage_data$V1))) {
                      passage_data$V1 <- as.POSIXct(strptime(passage_data$V1, format = "%H:%M:%S", tz = "GMT"))
                    }
                    
                    # Créer un graphique pour chaque passage

                    plot_data <- data.frame(
                      Time = passage_data$V1,
                      Pulse = passage_data$V2
                    )
                    
                    if (file_name == "HR") {
                      title = "Pulse over Time"
                      y_lim <- c(50,205)
                      y_unit <- "Pulse (bpm)"
                      y_breaks <- seq(0, 200, by = 10)  # Intervalles tous les 10 bpm
                    }
                    else if (file_name == "EDA") {
                      title = "Electrodermal activity over Time"
                      #y_lim <- ylim(min(data$V1[3:nrow(data)]),max(data$V1[3:nrow(data)]))
                      y_lim <- c(0,10)
                      y_unit <- "EDA (μS)"
                      y_breaks <- seq(0, 10, by = 1)  # Intervalles tous les 1 μS
                    }
                    
                    p <- ggplot(plot_data, aes(x = Time, y = Pulse)) +
                      geom_line() +
                      labs(title = paste(title, "- Participant", participant, "Passage", passage),
                           x = "Time (hh:mm:ss)",
                           y = y_unit) +
                      scale_x_datetime(date_labels = "%H:%M:%S", date_breaks = "10 sec") +
                      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                      scale_y_continuous(limits = y_lim, breaks = y_breaks)
                    
                    # Ajouter une ligne rouge en pointillés à 0.1 pour EDA
                    if (file_name == "EDA") {
                      p <- p + geom_hline(yintercept = 0.1, linetype = "dashed", color = "red")
                    }
                    
                    #ggsave(filename = file.path(participant_folder, paste0(passage, file_name, "plot.png")),
                    ggsave(filename = file.path(participant_folder, paste0(passage, "_pulse_plot.png")),
                           plot = p, width = 10, height = 5, units = "in")
                    
                    # On met à jour EDR_min, EDR_max, HR_min et HR_max
                    if (file_name == "EDA") {
                      EDR_min = min(EDR_min, min(passage_data$V2))
                      EDR_max = max(EDR_max, max(passage_data$V2))
                      EDR_passage_data_list[[i]] <- passage_data_csv
                    }
                    else if (file_name == "HR") {
                      HR_min = min(HR_min, min(passage_data$V2))
                      HR_max = max(HR_max, max(passage_data$V2))
                      HR_passage_data_list[[i]] <- passage_data_csv
                    }
                    
                    #cat(paste("\nSaved participant", participant, file_name, "data for", "passage", passage))
                  } else {
                    warning(paste("Aucune correspondance trouvée pour les passages", passage, "dans le dossier", folder))
                  }
                }
                
                # on applique la formule EDR_x2 = (EDR_x1 - EDR_min) / (EDR_max - EDR_min) à chaque élément de V2 et on l'enregistre dans un fichier csv
                for (i in 1:4) {
                  passage <- passages[i]
                  if (file_name == "EDA") {
                    EDR_passage_data <- EDR_passage_data_list[[i]]
                    EDR_passage_data$V4 <- (EDR_passage_data$V2 - EDR_min) / (EDR_max - EDR_min)
                    
                    names(EDR_passage_data)[names(EDR_passage_data) == 'V1'] <- 'Time'
                    names(EDR_passage_data)[names(EDR_passage_data) == 'V2'] <- 'EDA'
                    names(EDR_passage_data)[names(EDR_passage_data) == 'V3'] <- 'Variation coefficient'
                    names(EDR_passage_data)[names(EDR_passage_data) == 'V4'] <- 'Standardized EDA'
                    
                    write.table(EDR_passage_data, file = file.path(participant_folder, paste0(passage, "_EDA.csv")), row.names = FALSE, sep = ";", col.names = TRUE)
                  }
                  else if (file_name == "HR") {
                    HR_passage_data <- HR_passage_data_list[[i]]
                    HR_passage_data$V4 <- (HR_passage_data$V2 - HR_min) / (HR_max - HR_min)
                    print(HR_min)
                    print(HR_max)
                    
                    names(HR_passage_data)[names(HR_passage_data) == 'V1'] <- 'Time'
                    names(HR_passage_data)[names(HR_passage_data) == 'V2'] <- 'HR'
                    names(HR_passage_data)[names(HR_passage_data) == 'V3'] <- 'Variation coefficient'
                    names(HR_passage_data)[names(HR_passage_data) == 'V4'] <- 'Standardized HR'
                      
                    write.table(HR_passage_data, file = file.path(participant_folder, paste0(passage, "_HR.csv")), row.names = FALSE, sep = ";", col.names = TRUE)
                  }
                }
              } else {
              warning("Participant non trouvé dans videos_timecodes.")
              }
          } else {
              warning("Le fichier tags.csv n'existe pas dans le dossier", folder)
          }
        } else {
          warning("Le fichier", file_name, "dans le dossier", folder, "est vide.")
        }
    } else {
        warning("Le fichier", file_name, "n'existe pas dans le dossier", folder)
    }
    
    
}


# Parcourir chaque dossier
for (folder in stream_folders) {
    participant <- substr(basename(folder), 1, 3)
    
    # if (participant != "P20") {
    #   next
    # }
    
    cat(paste("\nProcessing", participant, "HR data"))
    process_csv(folder, "HR")
    
    cat(paste("\nProcessing", participant, "EDA data"))
    eda_data <- process_csv(folder, "EDA")
}

cat("\nDone!")

