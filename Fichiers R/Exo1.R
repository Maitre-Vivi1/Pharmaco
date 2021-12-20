# Librairies --------------------------------------------------------------

library(readr)
library(ggplot2)


# Données -----------------------------------------------------------------

tumeur <- read_table2("PD_data_tumor_growth_no_NA.txt")

tumeur$ID <- as.factor(tumeur$ID) # Codage en variable qualitative
tumeur$BLIVER <- as.logical(tumeur$BLIVER) # Codage en indicatrice
tumeur$BSLD <- tumeur$SLD

for (i in levels(tumeur$ID)) {
  tumeur$BSLD[which(tumeur$ID == i)] <- tumeur$BSLD[which(tumeur$ID == i)][1]
}


covar_ind <- tumeur[which(duplicated(tumeur$ID) == F), 3:6] # Je récupère les covariables pour faciliter leur description
names(covar_ind)[1] <- "BSLD"


Dose <- data.frame(
  ID = rep(tumeur$ID[which(duplicated(tumeur$ID) == F)], 25),
  dose = 1200,
  time = rep(21*0:24,49),
  SLD = NA,
  AGE = rep(tumeur$AGE[which(duplicated(tumeur$ID) == F)],25),
  BLIVER = rep(tumeur$BLIVER[which(duplicated(tumeur$ID) == F)],25),
  BHBG = rep(tumeur$BHBG[which(duplicated(tumeur$ID) == F)],25),
  BSLD = rep(covar_ind$BSLD,25)
) # Création d'un tableau de Doses administrées. Une Dose en 0 puis une tous les 21j jusqu'à ce que le temps t+21 d'administration soit
# supérieur au temps de dernière mesure (car dans ce cas on a aucune info).

Dose <- Dose[order(Dose$ID, Dose$time),]
Dose$suppr <- 0

for (j in 1:1225) {
  if (Dose$time[j] > max(tumeur$time[which(tumeur$ID == Dose$ID[j])])) {
    Dose$suppr[j] <- 1
  }
} 

Dose <- Dose[which(Dose$suppr == 0), -9]

tumeur_Dose <- merge(Dose, tumeur, all.x = T, all.y = T)

tumeur_Dose <- tumeur_Dose[order(tumeur_Dose$ID, tumeur_Dose$time),] 

write.csv(tumeur_Dose, row.names = F, na = ".", file = "tumeur_Dose.csv") # Tableau de données à utiliser sous MLXTRAN


# Résumé des données ------------------------------------------------------
p <- theme(
  plot.title = element_text(hjust = 0.5, face = "bold"),
  axis.text = element_text(face = "bold"),
  axis.title = element_text(face = "bold")
)

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
  p +
  ggtitle("Évolution de la taille tumorale chez les patients en fonction du temps")


# Modele de wang ----------------------------------------------------------

# BSLD * exp(-d*t) + g*t

# BSLD serait le niveau initial moyen de SLD chez les patients

BSLD <- mean(tumeur$SLD[which(tumeur$time == 0)])
BSLD_sd <- sd(tumeur$SLD[which(tumeur$time == 0)])

# d est un paramètre représentant la vitesse de décroissance initiale de la tumeur
# g est un paramètre représentant la vitesse de croissance de la tumeur une fois que le traitement n'est plus efficace

# On pourrait estimer d et g en lançant l'analyse sur quelques sujets
# Par exemple sur 5 sujets

# Sinon on pourrait regarder dans la littérature

tumeur$indice <- 0
for (i in 2:130) {
  if (tumeur$SLD[i] > tumeur$SLD[i-1] & tumeur$ID[i] == tumeur$ID[i-1] ) {
    tumeur$indice[i] <- 1
  }
}




# Pour Wang ---------------------------------------------------------------
# g -----------------------------------------------------------------------



library(nlme)
reg_param_croiss_Wang <- lme(SLD~time, data = tumeur[which(tumeur$indice==1),], random = ~ 1 + time | ID, method = "ML")
summary(reg_param_croiss_Wang) # g estimé à 0.09255 et son écart-type à 0.045398
g_Wang <- 0.09255
g_sd_Wang <- 0.045398


# d -----------------------------------------------------------------------

reg_param_decroiss_Wang <- lme(SLD~time, data = tumeur[which(tumeur$indice==0),], random = ~ 1 + time | ID, method = "ML")
summary(reg_param_decroiss_Wang)  # Ne pas oublier qu'on est dans une exponentielle
log(0.23689) # d = 1.440159 
-exp(log(0.23689))
d_Wang <- 1.440159/BSLD
d_sd_Wang <- 0.060186/0.23689*d_Wang


# Pour Stein-Fojo --------------------------------------------------------
# SLD(t) = BSLD(exp(-d*t) + exp(g*t) - 1)


# Pour g ------------------------------------------------------------------

reg_param_croiss_SteinFojo <- lme(SLD/BSLD~time - 1, data = tumeur[which(tumeur$indice==1),], random = ~ 1 + time | ID, method = "ML")
summary(reg_param_croiss_SteinFojo)

# exp(g*t) -1 = 0.01384
# g*t = ln(1.01384)

g_SteinFojo <- log(1.01384)
g_sd_SteinFojo <- 0.001322597/0.01384131*g_SteinFojo


# Pour d ------------------------------------------------------------------

reg_param_decroiss_SteinFojo <- lme(SLD/BSLD~time , data = tumeur[which(tumeur$indice==0),], random = ~ 1 + time | ID, method = "ML",
                                    control =  lmeControl(maxIter = 500, niterEM = 500, msMaxEval = 500,msMaxIter = 500))
summary(reg_param_decroiss_SteinFojo)

# log(exp(-d*t)-1) = -0.004035
-log(exp(-0.004035) + 1)



