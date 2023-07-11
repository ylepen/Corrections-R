#Master 104

#fiche 1 creation de processus 

# generation d'un bruit blanc

n=1000
e = rnorm(n = n, 0,1)
plot(x = e, type="l",col="blue",main = "Bruit blanc gaussien",ylab = "")

acf(e, main="Autocorrelogramme d'un bruit blanc") 
pacf(e, main ="Autocorrelation partielle d'un bruit blanc")


# generation d'une marche aleatoire sans derive
# option 1 : une boucle
rw1 = rep(0,n)

for(i in 2:n ){
  rw1[i]=rw1[i-1]+e[i]
}

# option 2 : utilisation de la fonction cumsum
rw2 =cumsum(e[2:n])
plot(x = rw2, type="l",col="blue",main = "Marche aléatoire ",ylab = "")

acf(rw2) 
pacf(rw)

# generation d'une marche al?atoire avec d?rive
mu=1
rw = cumsum(e+mu)
plot(x = rw, type="l",col="blue",main = "Marche al?atoire ",ylab = "")

 acf(rw) 
pacf(rw)


# g
time_trend = 1:n
plot(time_trend,type ="l")
a =1
b=0.2
TS = a + b*time_trend+10*e
plot(TS,type ="l",col="red")
lines(a+b*time_trend,col="blue")


acf(TS)
pacf(TS)


# AR(1) process
phi1 = 0.9
x1 = rep(0,n)
for(i in 2:n){
x1[i]=phi1*x1[i-1]+e[i] 
}
plot(x = x1, type="l",col="blue",main = "AR(1)",ylab = "")

acf(x1)
pacf(x1)

# MA(1) process
theta1 = 0.9
x2 = rep(0,n)
for(i in 2:n){
  x2[i]=theta1*e[i-1]+e[i] 
}
plot(x = x2, type="l",col="blue",main = "AR(1)",ylab = "")

acf(x2)
pacf(x2)
