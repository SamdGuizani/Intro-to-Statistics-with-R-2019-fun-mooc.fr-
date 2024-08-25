# Chargement données
data <- read.csv2('satisfaction_hopital.csv')

library(utils)
library(stats)

########################
# Question 1: Estimez le modèle de régression linéaire expliquant la variable « score.relation » 
# par les variables « age », « sexe », « score.information », « amelioration.sante », 
# « amelioration.moral », « profession »,  « service ». 
# (Le script doit inclure la vérification éventuelle des conditions de validité de la méthode utilisée.)

# Création d'un modèle de régression linéaire multiple
data$service <- factor(data$service) # Recodage en variable catégorielle 
data$sexe <- factor(data$sexe) # Recodage en variable catégorielle 
data$profession <- factor(data$profession) # Recodage en variable catégorielle 
model_lm <- lm(score.relation ~ age + sexe + score.information + amelioration.sante + amelioration.moral + profession + service, data = data)
summary(model_lm) # Affichage du modèle
 
# Conditions de validité du modèle de régression linéaire multiple : 
#   1) Normalité des résidus (= bruit)
#   2) Variance bruit indépendante de valeurs de la variable à expliquer et des des valeurs des variables explicatives
#   3) Bruit: Pas de structure de corrélation évidente, de structure de corrélation interne.
# En pratique les 2 dernières conditions sont difficiles à vérifier. 
# On se contente de vérifier la normalité de la distribution des résidus
hist(resid(model_lm))

# La distribution des résidus est proche d'une distribution normal à l'exception de quelques résidus situés à l'extrème gauche de l'histogramme.
# On peut donc conclure que le modèle linéaire est valide.


########################
# Question 2: Estimez le modèle de régression logistique expliquant la variable « recommander.b »
# par les variables « age », « sexe », « score.information », « amelioration.sante », 
# « amelioration.moral », « profession »,  « service ». 
# Notons que la variable « recommander.b » est une transformation de la variable « recommander » 
# en une variable binaire où « recommander.b » vaut 0 si « recommander » vaut 0 ou 1, 
# et 1 si « recommander » vaut 2. 
# (Le script doit inclure la vérification éventuelle des conditions de validité de la 
# méthode utilisée)

# Création d'un modèle de régression logistique
data$recommander.b <- factor(ifelse(data$recommander < 2, '0', '1')) # recodage variable "recommander" en variable binaire "recommander.b"
model_logistique <- glm(recommander.b ~ age + sexe + score.information + amelioration.sante + amelioration.moral + profession + service, data = data, family = 'binomial')
summary(model_logistique) # Affichage du modèle

# Conditions de validité du modèle de régression logistique :
# Au moins 5 à 10 évènements par variable explicative
table(data$recommander.b, useNA = 'always') # tableau d'effectifs
# On constate que 269 sujets ont donné le score recommander.b = 1
# Comptage du nombre de variables
N_var = 1 + (nlevels(data$sexe) - 1) + 1 + 1 + 1 + (nlevels(data$profession) - 1) + (nlevels(data$service) - 1)
N_var * 10 # Nombre minimum d'évènements en prenant comme règle 10 évènements/variable explicative
N_var * 5 # Nombre minimum d'évènements en prenant comme règle 5 évènements/variable explicative
# Dans notre cas, nous avons 269 évènements pour un minimum de 190. Le modèle peut donc être considéré comme valide.

