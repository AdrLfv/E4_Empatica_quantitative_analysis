#install.packages("gridExtra")

# Charger les bibliothèques nécessaires
library(ggplot2)
library(gridExtra)

# Données pour le premier histogramme
session_order <- data.frame(
  Session = factor(1:4),
  Mean_Std_Dev = c(3.562123, 2.874289, 2.656499, 1.645414)
)

# Créer le premier histogramme
plot1 <- ggplot(session_order, aes(x = Session, y = Mean_Std_Dev)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Mean of standard deviations by session order") +
  xlab("Session Order") +
  ylab("Mean of Standard Deviations") +
  theme_minimal()

# Données pour le deuxième histogramme
familiarity <- data.frame(
  Familiarity = factor(c("Low", "Friend", "Relative", "Self"), 
                       levels = c("Low", "Friend", "Relative", "Self")),
  Mean_Std_Dev = c(8.565768, 1.349328, 0.5117608, 0.3114678)
)

# Créer le deuxième histogramme en spécifiant l'ordre des niveaux
plot2 <- ggplot(familiarity, aes(x = Familiarity, y = Mean_Std_Dev)) +
  geom_bar(stat = "identity", fill = "coral") +
  ggtitle("Mean of standard deviations by familiarity") +
  xlab("Familiarity") +
  ylab("Mean of Standard Deviations") +
  theme_minimal()

# Afficher les deux histogrammes côte à côte
grid.arrange(plot1, plot2, ncol = 2)

ggsave("D:/MIT project/2024_06 E4 Data/histograms.png", grid.arrange(plot1, plot2, ncol = 2), width = 12, height = 6)
