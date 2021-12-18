# Librairies --------------------------------------------------------------

library(readxl)


# Chargement --------------------------------------------------------------

data_vl <- read_excel("C:/Users/vivi1/Desktop/Ensai 3A/Projet Pharma/Pharmaco/Exercice 2/data_vl.xlsx", 
                      col_types = c("numeric", "numeric", "numeric", 
                                    "numeric"))

data_vl$id <- as.factor(data_vl$id)

summary(data_vl$id) # 13 patients
156/13 # 12 mesures par patient en moyenne (min = 7 ; max = 22)


