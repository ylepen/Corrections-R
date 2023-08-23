# EStimation of a VAR model with vars package


#install.packages("vars")

library(vars)

# an object with class attribute ts and mts with 4 variables and 84 observations
Canada

plot(Canada)

#sink(file = "output.txt")

# Selection du nombre de retards optimal 
# lag.max = nombre maximal de retards
# type = forme de la composante déterministe "const", "trend","both" ou "none (1 seule spécification possible)
pselect<-VARselect(y=Canada,lag.max=10,type=c("const"))

# Affichage des valeurs des critère de sélection
pselect$criteria
# nombre de retards optimal selon les critères de sélection
pselect$selection

#sink()

# Estimation d'un modèle VAR avec 2 retards et une constante
var.2lag <-VAR(y = Canada,type="const",lag.max = 2)
#Affichage des résultats avec summary
summary(var.2lag)

# Estimation d'un modèle VAR avec 2 retards et une constante

var.HQlag <-VAR(y = endog_data,type="const",lag.max = 10,ic=c("HQ"))

summary(var.HQlag)

# Affichage des racines du polynome caractéristique
roots(var.HQlag)

# liste des matrices des coefficients estimés du VAR
A<-Acoef(var.HQlag)

plot(var.HQlag)



causality(var.HQlag,cause=c('GDP_gap'))

causality(var.HQlag,cause=c('Infl'))

causality(var.HQlag,cause=c('FF'))

var.aiclag$varresult$e$residuals

# Calcul des Impulse response functions

irf<-irf(var.HQlag,n.ahead = 12,ortho = TRUE,ci=0.95)
 plot(irf)



 # Decomposition de la variance
dec_var <-fevd(var.HQlag,n.ahead=20) 
 
plot(dec_var) 
 
## Calcul des prévisions
forecast<-predict(var.HQlag,n.ahead = 10, ci=0.95)

summary(forecast)
plot(forecast)
