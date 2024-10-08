# Pr�alables : r�pertoire de travail et importation donn�es. Chargement librairies
setwd("X:/Documentation/07-Trainings and Conferences/2019-FunMOOC Intro R/Devoirs/Devoir 02")
data <- read.csv2('satisfaction_hopital.csv')
library(utils)
library(stats)
library(Epi)
library(stats)

# Question 1: tranformation variable recommander en variable binaire recommander.b

data$recommander.b <- factor(data$recommander) # Cr�ation variable recommander.b 
levels(data$recommander.b)[1:2] <- '0' # Recodage niveaux 0 et 1 en niveau 0
levels(data$recommander.b)[2] <- '1' # Recodage niveau 2 en niveau 1

# V�rification recodage
print('Q1: Tableau crois� recommander.b x recommander (v�rification recodage)')
print(xtabs(~data$recommander.b + data$recommander)) # Affichage tableau crois� (v�rification recodage) 

print('Conclusion Q1: Recodage termin�')

# Question 2: Odds-ratio - association recommander.b et sexe et intervalle de confiance

tab <- xtabs(~ data$sexe + data$recommander.b) # Tableau de contingence (pour info)

print('Q2: Tableau contingence recommander.b vs. sexe (pour info)')
print(tab) # affichage tableau contingence (pour info)

print('Q2: Affichage association Odds-ratio et intervalle de confiance :')
twoby2_tab <- twoby2(data$recommander.b, data$sexe) # Test Odds-ratio (association et intervalle de confiance)

print('Conclusion Q2: Intervalle de confiance lu en ligne Sample Odds Ratio du tableau 95% conf. interval')

# Question 3: Test corr�lation score.relation et age

# Evaluation de la distribution de la variable age
hist(data$age)
qqnorm(data$age)
qqline(data$age)

# Evaluation de la distribution de la variable score-relation
hist(data$score.relation)
qqnorm(data$score.relation)
qqline(data$score.relation)

# La variable age a une distribution proche de la la loi Normale.
# La variable score.relation ne suit pas une distribution Normale.
# Il est donc possible de r�aliser le test de corr�lation de Pearson (une des 2 variables suit une loi Noramale)
pearson_tab <- cor.test(data$age, data$score.relation)

print('Q3: Affichage tableau test de corr�lation de Pearson :')
print(pearson_tab)

print('Conclusion Q3 : p = 0.073 > 0.05. Corr�lation age et score relation pas significative.')

# Question 4: score.relation moyen femmes vs. hommes

# V�rification des effectifs par sexe
print('Q4: Effectifs par sexe:')
print(xtabs(~data$sexe))

# L'effectif par sexe est bien au-del� de 30.
# A la question 3, score.relation ne suit pas une loi Normale.
# Vu les effectifs tr�s sup�rieurs � 30, il est raisonnable d'utiliser le test de Student pour comparer les moyennes de score.relation dans les 2 groupes (hommes/femmes)

# Calcul et affichage des �carts-type dans les 2 groupes
print('Q4: Ecart-type par sexe:')
print(by(data$score.relation, data$sexe, sd, na.rm = TRUE))
# Ecarts-type proches, utilisation de l'option var.equal=TRUE

student_tab <- t.test(data$score.relation ~ data$sexe, var.equal=TRUE)
print('Q4: Comparaison de moyenne score.relation en fonction du sexe. Test de Student')
print(student_tab)

print('Conclusion Q4 : p = 0.26 > 0.05. Pas de diff�rence de la moyenne entre hommes et femmes')

# Alternative: si on ne souhaite pas faire le test de Student (non normalit� de score.relation)
# il est possible d'utiliser le test de Wilcoxson
print('Q4: M�thode alternative. Test Wilcoxon (non-normalit� score.relation)')
wilcox_tab <- wilcox.test(data$score.relation ~ data$sexe)
print(wilcox_tab)
print('Conclusion Q4 m�thode alternative : p = 0.35 > 0.05. Pas de diff�rence entre hommes et femmes, m�me conclusion que le test de Student')
