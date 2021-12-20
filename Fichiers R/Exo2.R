# Librairies --------------------------------------------------------------

library(readxl)
library(ggplot2)


# Chargement et gestion de la BDD --------------------------------------------------------------

data_vl <- read_excel("data_vl.xlsx", 
                      col_types = c("numeric", "numeric", "numeric", 
                                    "numeric"))

data_vl$id <- as.factor(data_vl$id)


Intercept <- data.frame(
  id = data_vl$id[which(duplicated(data_vl$id) == F)],
  Time_sympt = 0,
  Time_inf = 5,
  LVL = 1
) # Création d'une BDD qui est data_vl avec une valeur initiale (Time_sympt = 0) d'une LVL = 1.

# On ajoute cet intercept aux 13 patients
data_vl_intercept <- merge(data_vl, Intercept, all.x = T, all.y = T)

# Statistiques descriptives de la BDD ------------------------------------

# Paramètre pour les plot de ggplot
p <- theme(
  plot.title = element_text(hjust = 0.5, face = "bold"),
  axis.text = element_text(face = "bold"),
  axis.title = element_text(face = "bold")
)



summary(data_vl$id) # 13 patients
156/13 # 12 mesures par patient en moyenne (min = 7 ; max = 22)

data_vl$Time_inf-data_vl$Time_sympt # Egal à 5 pour tous les individus, la variable "Time_inf" a été correctement codée.

# Statistiques sur le temps depuis l'apparition des 1ers symtpômes
boxplot(data_vl$Time_sympt, main = "Temps depuis l'apparition des premiers symptômes", ylab = "Jours", col = "orange")
summary(data_vl$Time_sympt)
mean(data_vl$Time_sympt) ; min(data_vl$Time_sympt) ; max(data_vl$Time_sympt) ; sd(data_vl$Time_sympt)
# moyenne de 12,1 jours ; 1 jour min ; 28 jours max ; 6,4 jours Écart-type

# Les statistiques sont les mêmes pour chaque individu (avec +5 pour chaque valeur) pour la variable "Temps depuis infection"

# Statistiques sur la charge virale des patients
boxplot(data_vl$Time_sympt, main = "Temps depuis l'apparition des premiers symptômes", ylab = "Jours", col = "orange")

boxplot(data_vl$LVL, main = "Log10 de la charge virale en copies ARN/mL", ylab = "Log10(ARN/mL)", col = "orange")
summary(data_vl$LVL)
mean(data_vl$LVL) ; min(data_vl$LVL) ; max(data_vl$LVL) ; sd(data_vl$LVL)

# Regardons l'évolution de la charge virale chez les patients depuis l'infection

ggplot(data = data_vl_intercept, aes(x = Time_inf, y = LVL, col = id)) +
  geom_line() +
  theme(legend.position='none') +
  ylab("Log10(ARN/mL)") +
  xlab("Temps depuis l'infection") +
  p +
  ggtitle("Évolution de la charge virale chez les patients en fonction du temps depuis l'infection")

