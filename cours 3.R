
# séquences sur les variables
txt = "au revoir"
txt = "bonjour"
txt

# fonction bonjour
bonjour <- function() {
  print("bonjour")
}

bonjour()


# fonction bonjour avec un paramètre
bonjour <- function( txt ) {
  a = 2
  print(txt)
}

txt = "test"
bonjour("bonjour")
print(a)

a = 2
print(a)


# travaux avec quantmod
install.packages("quantmod")

library(quantmod)
getSymbols("GOOGL")
print(GOOGL)
plot(GOOGL)
head(GOOGL)
tail(GOOGL)

x = getSymbols("GOOGL",auto.assign=F) 
print(x)
head(x)

a = x[,"GOOGL.Adjusted"]
plot(a)

g = a / lag(a,1) - 1
plot(g)

sum( g, na.rm=T ) * 100
plot(a)

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

Return.annualized(g)*100
Return.cumulative(g)*100
Return.cumulative(g, geometric=F)*100

summary(g)

# calcul de moyennes mobiles 25 et 200 jours
m25  = rollapply( a,  25, mean, align="right")
m200 = rollapply( a, 200, mean, align="right")
signaux = m25 / m200

plot(m25)
lines(m200,col="red")

plot(signaux)


# operateur de comparaison "global"

for( i in 1:length(m25) ) {
  print( m25[i] )
}


signaux = m25 / m200
signaux = na.omit( signaux )
length(signaux)

for( k in 1:length(signaux) ) {
  if( signaux[k] > 1 ) {
    signaux[k] = 1
  } else {
    signaux[k] = -1
  }
}

print(signaux)
plot(signaux)


# on cree une matrice xts: price, return, signal


m = cbind( x[,"GOOGL.Adjusted"], g, signaux, lag(signaux,1) )
tail(m)
colnames(m) = c("price","return","signal", "signalprev")
tail(m)
m

Return.annualized( m[,"return"] )*100
Return.annualized( m[,"return"] * m[,"signalprev"] )*100
Return.annualized( m[,"return"] * m[,"signal"] )*100

# mise sous forme d'une fonction générique

M1IEFstrat <- function( p, n1, n2 ) {

  # return
  r = p / lag(p,1) - 1
  
  # calcul du macd
  m1 = rollapply( p, n1, mean, align="right" )
  m2 = rollapply( p, n2, mean, align="right" )
  sg = m1 / m2
  
  # calcul des positions
  sg = na.omit( sg )
  for( k in 1:length(sg) ) {
    if( sg[k] > 1 ) {
      sg[k] = 1
    } else {
      sg[k] = -1
    }
  }
  
  # preparation des resultats
  m = cbind( p, r, sg, lag(sg,1) )
  colnames(m) = c("price","return","signal", "signalprev")
  
  # calcul du return de la strategie
  s = Return.annualized( m[,"return"] * m[,"signalprev"] )*100
  
  # return de la valeur
  return(s)
}

M1IEFstrat( x[,"GOOGL.Adjusted"], 25, 200 )
M1IEFstrat( x[,"GOOGL.Adjusted"], 5, 25 )

x = getSymbols("GOOGL",auto.assign=F) 

perfArray = NULL
for( i in 5:10 ) {
  for( j in 25:30 ) {
    r = M1IEFstrat( x[,"GOOGL.Adjusted"], i, j )
    p = c( i, j, r )
    perfArray = rbind( perfArray, p )
  }
}
print(perfArray)



# generer 10 points et calculer une moyenne


# créer un vecteur contenant les valeurs "un" "deux" "trois"

# créer une séquence de 1 à 10


# créer une matrice 2x2


# fonction hello()