## Devoir 01: satisfaction hopital - importation donn�es
sh <- read.csv2("satisfaction_hopital.csv")

## Question 1 : Pourcentage sujets dans 3 variables cat�gorielles (service, sexe, profession)
# Conversion des variables service, sexe et profession en variables cat�gorielles
sh$service <- factor(sh$service)
sh$sexe <- factor(sh$sexe, levels = c(0,1), labels = c("Homme","Femme"))
sh$profession <- factor(sh$profession)
# Calcul des pourcentages
Pourcentage.service <- 100*prop.table(table(sh$service, useNA = "always")) # useNA = "always" utilis� pour donn�es la proportion de donn�es absentes
Pourcentage.sexe <- 100*prop.table(table(sh$sexe, useNA = "always"))
Pourcentage.profession <- 100*prop.table(table(sh$profession, useNA = "always"))

## Question 2 : donn�es synth�tiques des variables continues
tab.synth <- describe(sh,num.desc = c("mean","median","sd","min","max","valid.n"))

## Question 3 : histogramme score relation
hist(sh$score.relation, main = "Histogramme Score Relation", xlab = "Score Relation", ylab = "Freq.", las=1)

## Question 4 : boxplot score relation vs. sexe
boxplot(sh$score.relation~sh$sexe, main = "Boxplot Score Relation vs. Sexe", xlab = "Sexe", ylab = "Score Relation")
