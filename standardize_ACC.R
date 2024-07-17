# Installer les packages nécessaires si ce n'est pas déjà fait
# install.packages("signal")
# install.packages("pracma")
# install.packages("dplyr")
#install.packages("FSA")
#install.packages("ggsignif")

library(signal)
library(pracma)
library(dplyr)
library(FSA)
library(ggsignif)



base_path <- "D:/MIT project/2024_06 E4 Data/Cleaned data"
stream_folders <- list.dirs(base_path, recursive = FALSE)
participant_path <- "D:/MIT project/2024_06 E4 Data/participants.csv"
participant_data <- read.table(participant_path, header = TRUE, sep = ";", stringsAsFactors = FALSE)

normalise_ACC <- function(participant_folder) {
  file_path <- file.path(participant_folder, "ACC", paste(sessions_order[i], "_ACC.csv", sep=""))
  data_file <- read.table(file_path, header = TRUE, stringsAsFactors = FALSE, sep = ";") 
  acc_data <- data_file %>% head(60*32)
  acc_data$time_seconds <- as.numeric(as.POSIXct(acc_data$Time, format = "%H:%M:%S"))

  # # Paramètres du filtre
  # fs <- 32 # fréquence d'échantillonnage en Hz (à ajuster selon vos données)
  # fc <- 5  # fréquence de coupure en Hz (à ajuster selon vos besoins)
  # order <- 4 # ordre du filtre

  # # Création du filtre passe-bas
  # bf <- butter(order, fc/(fs/2), type = "low", plane = "z")

  # # Application du filtre
  # # le filtre sert à enlever les bruits de haute fréquence
  # acc_data <- acc_data %>%
  #   mutate(
  #     x_filt = filtfilt(bf, acc_data$ACC_x),
  #     y_filt = filtfilt(bf, acc_data$ACC_y),
  #     z_filt = filtfilt(bf, acc_data$ACC_z)
  #   )

  # # Calcul des seuils basés sur l'IQR
  # calculate_threshold <- function(data, factor = 1.5) {
  #   Q1 <- quantile(data, 0.25, na.rm = TRUE)
  #   Q3 <- quantile(data, 0.75, na.rm = TRUE)
  #   IQR <- Q3 - Q1
  #   lower_bound <- Q1 - factor * IQR
  #   upper_bound <- Q3 + factor * IQR
  #   return(c(lower_bound, upper_bound))
  # }

  # # Calcul des seuils pour chaque axe
  # x_thresholds <- calculate_threshold(acc_data$x_filt)
  # y_thresholds <- calculate_threshold(acc_data$y_filt)
  # z_thresholds <- calculate_threshold(acc_data$z_filt)

  # #print(paste("Nombre de lignes après filtrage:", nrow(acc_data)))

  # # Rejeter les valeurs au-delà des seuils calculés
  # acc_data <- acc_data %>%
  #   dplyr::filter(
  #     x_filt > x_thresholds[1] & x_filt < x_thresholds[2] &
  #     y_filt > y_thresholds[1] & y_filt < y_thresholds[2] &
  #     z_filt > z_thresholds[1] & z_filt < z_thresholds[2]
  #   )

  # # Vérification des données après rejet des artefacts
  # print(paste("Nombre de lignes après rejet des artefacts:", nrow(acc_data)))

  # # Vérification des données restantes après filtrage des artefacts
  # if (nrow(acc_data) == 0) {
  #   stop("Toutes les données ont été rejetées après le filtrage des artefacts.")
  # }

  # # Calibration - Méthode de Ferraris et al.

  # # Calcul des moyennes pour chaque axe
  # mean_x <- mean(acc_data$x_filt)
  # mean_y <- mean(acc_data$y_filt)
  # mean_z <- mean(acc_data$z_filt)

  # # Calibration en soustrayant les biais
  # acc_data <- acc_data %>%
  #   mutate(
  #     x_calibrated = x_filt - mean_x,
  #     y_calibrated = y_filt - mean_y,
  #     z_calibrated = z_filt - mean_z
  #   )

  # # Return une moyenne de l'accélération des trois axes
  # acc_data <- acc_data %>%
  #   mutate(
  #     ACC = sqrt(x_calibrated^2 + y_calibrated^2 + z_calibrated^2)
  #   )

  acc_data <- acc_data %>%
  mutate(
    ACC = sqrt(acc_data$ACC_x^2 + acc_data$ACC_y^2 + acc_data$ACC_z^2)
  )

  # log_scale = log(acc_data$ACC)
  # acc_data$ACC = (log_scale - min(log_scale)) / (max(log_scale) - min(log_scale))

  # Standardisation de l'accélération
  # acc_data$ACC = (acc_data$ACC - mean(acc_data$ACC)) / sd(acc_data$ACC)
  # acc_data$ACC = scale(acc_data$ACC)
  


  # # Génération du graphe de l'accélération totale
  # plot <- ggplot(acc_data, aes(x = seq_along(ACC), y = ACC)) +
  #   geom_line() + # Utiliser geom_line pour tracer une ligne
  #   labs(title = "Total standardized acceleration",
  #       x = "Observation",
  #       y = "Standardized cceleration") +
  #   theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # # Enregistrement du graphe dans un fichier
  # ggsave(file.path(participant_folder, "ACC", paste(sessions_order[i], "_normalised_ACC.png", sep="")), plot, width = 10, height = 6, dpi = 300)

  # print(paste("Saved", participant_id, "session:", sessions_order[i]))

  acc_data <- acc_data %>% select(ACC)
  return(acc_data)
}
