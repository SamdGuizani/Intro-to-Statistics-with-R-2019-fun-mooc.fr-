# Chargement donn�es
data <- read.csv2('satisfaction_hopital.csv')

library(utils)
library(stats)

########################
# Question 1: Estimez le mod�le de r�gression lin�aire expliquant la variable � score.relation � 
# par les variables � age �, � sexe �, � score.information �, � amelioration.sante �, 
# � amelioration.moral �, � profession �,  � service �. 
# (Le script doit inclure la v�rification �ventuelle des conditions de validit� de la m�thode utilis�e.)

# Cr�ation d'un mod�le de r�gression lin�aire multiple
data$service <- factor(data$service) # Recodage en variable cat�gorielle 
data$sexe <- factor(data$sexe) # Recodage en variable cat�gorielle 
data$profession <- factor(data$profession) # Recodage en variable cat�gorielle 
model_lm <- lm(score.relation ~ age + sexe + score.information + amelioration.sante + amelioration.moral + profession + service, data = data)
summary(model_lm) # Affichage du mod�le
 
# Conditions de validit� du mod�le de r�gression lin�aire multiple : 
#   1) Normalit� des r�sidus (= bruit)
#   2) Variance bruit ind�pendante de valeurs de la variable � expliquer et des des valeurs des variables explicatives
#   3) Bruit: Pas de structure de corr�lation �vidente, de structure de corr�lation interne.
# En pratique les 2 derni�res conditions sont difficiles � v�rifier. 
# On se contente de v�rifier la normalit� de la distribution des r�sidus
hist(resid(model_lm))

# La distribution des r�sidus est proche d'une distribution normal � l'exception de quelques r�sidus situ�s � l'extr�me gauche de l'histogramme.
# On peut donc conclure que le mod�le lin�aire est valide.


########################
# Question 2: Estimez le mod�le de r�gression logistique expliquant la variable � recommander.b �
# par les variables � age �, � sexe �, � score.information �, � amelioration.sante �, 
# � amelioration.moral �, � profession �,  � service �. 
# Notons que la variable � recommander.b � est une transformation de la variable � recommander � 
# en une variable binaire o� � recommander.b � vaut 0 si � recommander � vaut 0 ou 1, 
# et 1 si � recommander � vaut 2. 
# (Le script doit inclure la v�rification �ventuelle des conditions de validit� de la 
# m�thode utilis�e)

# Cr�ation d'un mod�le de r�gression logistique
data$recommander.b <- factor(ifelse(data$recommander < 2, '0', '1')) # recodage variable "recommander" en variable binaire "recommander.b"
model_logistique <- glm(recommander.b ~ age + sexe + score.information + amelioration.sante + amelioration.moral + profession + service, data = data, family = 'binomial')
summary(model_logistique) # Affichage du mod�le

# Conditions de validit� du mod�le de r�gression logistique :
# Au moins 5 � 10 �v�nements par variable explicative
table(data$recommander.b, useNA = 'always') # tableau d'effectifs
# On constate que 269 sujets ont donn� le score recommander.b = 1
# Comptage du nombre de variables
N_var = 1 + (nlevels(data$sexe) - 1) + 1 + 1 + 1 + (nlevels(data$profession) - 1) + (nlevels(data$service) - 1)
N_var * 10 # Nombre minimum d'�v�nements en prenant comme r�gle 10 �v�nements/variable explicative
N_var * 5 # Nombre minimum d'�v�nements en prenant comme r�gle 5 �v�nements/variable explicative
# Dans notre cas, nous avons 269 �v�nements pour un minimum de 190. Le mod�le peut donc �tre consid�r� comme valide.

