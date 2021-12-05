# Librairies --------------------------------------------------------------

library(readr)
library(ggplot2)


# Données -----------------------------------------------------------------

tumeur <- read_table2("Exercice 1/PD_data_tumor_growth_no_NA.txt")

tumeur$ID <- as.factor(tumeur$ID) # Codage en variable qualitative
tumeur$BLIVER <- as.logical(tumeur$BLIVER) # Codage en indicatrice

covar_ind <- tumeur[which(duplicated(tumeur$ID) == F), 4:6] # Je récupère les covariables pour faciliter leur description

Dose <- data.frame(
  ID = rep(tumeur$ID[which(duplicated(tumeur$ID) == F)], 25),
  dose = 1200,
  time = rep(21*0:24,49),
  SLD = NA,
  AGE = rep(tumeur$AGE[which(duplicated(tumeur$ID) == F)],25),
  BLIVER = rep(tumeur$BLIVER[which(duplicated(tumeur$ID) == F)],25),
  BHBG = rep(tumeur$BHBG[which(duplicated(tumeur$ID) == F)],25)
) # Création d'un tableau de Doses administrées. Une Dose en 0 puis une tous les 21j jusqu'à ce que le temps t+21 d'administration soit
# supérieur au temps de dernière mesure (car dans ce cas on a aucune info).

Dose <- Dose[order(Dose$ID, Dose$time),]
Dose$suppr <- 0

for (j in 1:1225) {
  if (Dose$time[j] > max(tumeur$time[which(tumeur$ID == Dose$ID[j])])) {
    Dose$suppr[j] <- 1
  }
} 

Dose <- Dose[which(Dose$suppr == 0), -8]

tumeur_Dose <- merge(Dose, tumeur, all.x = T, all.y = T)

tumeur_Dose <- tumeur_Dose[order(tumeur_Dose$ID, tumeur_Dose$time),] 

write.csv(tumeur_Dose, row.names = F, na = ".", file = "tumeur_Dose.csv") # Tableau de données à utiliser sous MLXTRAN


# Résumé des données ------------------------------------------------------

summary(tumeur)

length(levels(tumeur$ID)) # 49 patients
length(tumeur$ID)/length(levels(tumeur$ID)) # 2.653061 mesures par patient en moyenne
sum(summary(tumeur$ID) == 1) # 15 patients n'ont été visité qu'une seule fois

boxplot(covar_ind$AGE, main = "Répartition de l'âge des patients", ylab = "Âge (années)", col = "orange")
mean(covar_ind$AGE) ; min(covar_ind$AGE) ; max(covar_ind$AGE) ; sd(covar_ind$AGE)
# 65.69388 ans en moyenne ; 45 age min ; 83 age max ; 9.427805 Écart-type


boxplot(covar_ind$BHBG, main = "Niveau d'hémoglobine au premier rendez-vous", ylab = "concentration (g/l)", col = "orange")
mean(covar_ind$BHBG) ; min(covar_ind$BHBG) ; max(covar_ind$BHBG) ; sd(covar_ind$BHBG)
# 120.9325 g/l d'hémoglobine à la baseline ; 88 min ; 156 max ; 17.75565 Écart-type

sum(covar_ind$BLIVER) # 11 patients avec métastases au foie

mean(summary(Dose$ID)) # 5.877551 Doses en moyenne
sum(summary(Dose$ID)==1) # 15 patients n'ont reçu qu'une dose


ggplot(data = tumeur, aes(x = time, y = SLD, col = ID)) +
  geom_line() +
  theme(legend.position='none') +
  ggtitle("Évolution de la taille de la tumeur chez\nles patients en fonction du temps")
