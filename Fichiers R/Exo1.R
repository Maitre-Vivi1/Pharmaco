# Librairies --------------------------------------------------------------

library(readr)
library(ggplot2)


# Données -----------------------------------------------------------------

tumeur <- read_table2("Exercice 1/PD_data_tumor_growth_no_NA.txt")

tumeur$ID <- as.factor(tumeur$ID)
tumeur$BLIVER <- as.logical(tumeur$BLIVER)

covar_ind <- tumeur[which(duplicated(tumeur$ID) == F), 4:6]

# Résumé des données ------------------------------------------------------

summary(tumeur)

length(levels(tumeur$ID)) # 49 patients
length(tumeur$ID)/length(levels(tumeur$ID)) # 2.653061 mesures par patient en moyenne
sum(summary(tumeur$ID) == 1) # 15 patients n'ont été visité qu'une seule fois

boxplot(covar_ind$AGE)
mean(covar_ind$AGE) ; min(covar_ind$AGE) ; max(covar_ind$AGE) ; sd(covar_ind$AGE)
# 65.69388 ans en moyenne ; 45 age min ; 83 age max ; 9.427805 Écart-type


boxplot(covar_ind$BHBG)
mean(covar_ind$BHBG) ; min(covar_ind$BHBG) ; max(covar_ind$BHBG) ; sd(covar_ind$BHBG)
# 120.9325 g/l d'hémoglobine à la baseline ; 88 min ; 156 max ; 17.75565 Écart-type


sum(covar_ind$BLIVER) # 11 patients avec métastases au foie



# Commentaires -------------------------------------------------------------

# Supprimer les patients suivient qu'une seule fois ?


# -------------------------------------------------------------------------




