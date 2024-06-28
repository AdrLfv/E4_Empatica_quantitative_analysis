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

# Fonction pour traiter un fichier CSV (HR ou EDA)
process_csv <- function(folder, file_name, seq_start) {
    file_path <- file.path(folder, paste(file_name, ".csv", sep=""))
    
    if (file.exists(file_path)) {
        data <- read.csv(file_path, header = FALSE, stringsAsFactors = FALSE, sep = ";")
        
        if (nrow(data) > 0) {
        data$V2 <- rep(NA, nrow(data))
        data$V3 <- rep(NA, nrow(data))
        data$V4 <- rep(NA, nrow(data))
        data$V5 <- rep(NA, nrow(data))
        
        data$V2[3:nrow(data)] <- seq(seq_start, seq_start + (nrow(data) - 3))
        data$V3[3:nrow(data)] <- data[1, 1] + data$V2[3:nrow(data)] / data[2, 1]
        data$V4 <- as.POSIXct(data$V3, origin = "1970-01-01", tz = "GMT") + (-4 * 3600)
        data$V4 <- format(data$V4, format = "%H:%M:%S")
        
        tags_path <- file.path(folder, "tags.csv")
        if (file.exists(tags_path)) {
            tags_data <- read.csv(tags_path, header = FALSE, stringsAsFactors = FALSE, sep = ";")
            
            # Extraire le participant du nom du dossier
            participant <- substr(basename(folder), 1, 3)
            
            # Trouver l'index du participant dans videos_timecodes
            participant_index <- which(videos_timecodes[, 1] == participant)
            
            if (length(participant_index) > 0) {
            timestamp <- videos_timecodes[participant_index, 10]
            start_time <- as.POSIXct(timestamp, format = "%H:%M:%S", tz = "GMT")
            time_sequence <- seq(from = start_time, by = 1/data[2, 1], length.out = nrow(data) - 2)
            time_sequence_formatted <- format(time_sequence, "%H:%M:%S")
            data$V5[3:nrow(data)] <- time_sequence_formatted
            
            data$V2[is.na(data$V2)] <- ""
            data$V3[is.na(data$V3)] <- ""
            data$V4[is.na(data$V4)] <- ""
            data$V5[is.na(data$V5)] <- ""
            
            data[1, 2] <- "Timecode ID"
            data[1, 3] <- "Timecode UNIX"
            data[1, 4] <- "Timecode"
            data[1, 5] <- "In video timecode"
            
            return(data)
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
    return(NULL)
}


draw_graphs <- function(data, file_name, participant) {
    if (is.null(data)) {
        warning("Les données sont nulles pour le participant", participant)
        return(NULL)
    }
    
    # Créer un dossier pour chaque participant dans "D:/MIT project/2024_06 E4 Data/Clear data"
    participant <- substr(basename(folder), 1, 3)  # Extrait les 3 premiers caractères du nom du dossier
    participant_folder <- file.path(clear_data_path, participant, file_name)  # Sous-dossier file_name pour chaque participant
    dir.create(participant_folder, showWarnings = FALSE, recursive = TRUE)
    
    # Créer 4 fichiers CSV pour chaque participant (A, B, C, D)
    passages <- c("A", "B", "C", "D")
    columns <- list(c(2, 3), c(4, 5), c(6, 7), c(8, 9))  # Utiliser des indices de colonne

    EDR_min = 0.1
    EDR_max = 10

    #on crée une liste de dataframes pour chaque passage
    EDR_passage_data_list  <- list()
    
    for (i in 1:4) {
        passage <- passages[i]
        start_col <- columns[[i]][1]
        end_col <- columns[[i]][2]
        
        start_time <- videos_timecodes[which(videos_timecodes[, 1] == participant), start_col]
        end_time <- videos_timecodes[which(videos_timecodes[, 1] == participant), end_col]
        
        matching_start_index <- which(data$V5 == start_time)[1]
        matching_end_index <- which(data$V5 == end_time)[1]
        
        # V1 = Time, V2 = HR, V3 = Variation coefficient
        if (length(matching_start_index) > 0 & length(matching_end_index) > 0) {
            passage_data <- data.frame(
                V1 = data$V5[matching_start_index:matching_end_index],
                V2 = data$V1[matching_start_index:matching_end_index],
                V3 = data$V1[matching_start_index:matching_end_index] - data$V1[matching_start_index]
            )
            passage_data_csv <- passage_data
            
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
                y_lim <- ylim(50,205)
                y_unit <- "Pulse (bpm)"
            }
            else if (file_name == "EDA") {
                title = "Electrodermal activity over Time"
                #y_lim <- ylim(min(data$V1[3:nrow(data)]),max(data$V1[3:nrow(data)]))
                y_lim <- ylim(0,10)
                y_unit <- "EDA (μS)"
            }
            
            p <- ggplot(plot_data, aes(x = Time, y = Pulse)) +
                geom_line() +
                labs(title = paste(title, "- Participant", participant, "Passage", passage),
                    x = "Time (hh:mm:ss)",
                    y = y_unit) +
                scale_x_datetime(date_labels = "%H:%M:%S", date_breaks = "10 sec") +
                theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                y_lim
            
            if (file_name == "EDA") {
                p <- p + geom_hline(yintercept = 0.1, linetype = "dashed", color = "red")
            }
            
            #ggsave(filename = file.path(participant_folder, paste0(passage, file_name, "plot.png")),
            ggsave(filename = file.path(participant_folder, paste0(passage, "_pulse_plot.png")),
                    plot = p, width = 10, height = 5, units = "in")
            
            # On met à jour EDR_min et EDR_max
            if (file_name == "EDA") {
                EDR_min = min(EDR_min, min(passage_data$V2))
                EDR_max = max(EDR_max, max(passage_data$V2))
                EDR_passage_data_list[[i]] <- passage_data_csv
            }
            else {
                names(passage_data_csv)[names(passage_data) == 'V1'] <- 'Time'
                names(passage_data_csv)[names(passage_data) == 'V2'] <- 'HR'
                names(passage_data_csv)[names(passage_data) == 'V3'] <- 'Variation coefficient'

                write.table(passage_data_csv, file = file.path(participant_folder, paste0(passage, "_HR.csv")), row.names = FALSE, sep = ";", col.names = TRUE)
            }

            cat(paste("\nSaved participant", participant, file_name, "data for", "passage", passage))
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
    }
}


# Parcourir chaque dossier
for (folder in stream_folders) {
  
    participant <- substr(basename(folder), 1, 3)
    cat(paste("\nProcessing", participant,"HR data"))
    hr_data <- process_csv(folder, "HR", 5)
    draw_graphs(hr_data,"HR", participant)
    
    cat(paste("\nProcessing", participant,"EDA data"))
    eda_data <- process_csv(folder, "EDA", 0)
    draw_graphs(eda_data, "EDA", participant)
}

cat("\nDone!")

