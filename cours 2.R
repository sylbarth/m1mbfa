

# génération d'une variable pseudo aléatoire de 40 points
set.seed(1)
x = rnorm(40)
x

x[1]
x[2]
length(x)





y = x
for( i in 1:length(x) ) {
  print(i)
  y[i] = 2 * x[i]
}





y = x
for( i in 2:length(x) ) {
  y[i] = y[i-1] + x[i]
}


# création d'une série temporelle
z = ts( y, start=2000, frequency=4 )
plot(z)

# conditions
if( x[2] > 0 ) {
  msg = paste( 2, " => ", x[2], " positif" )
  print(msg)
} else {
  msg = paste( 2, " => ", x[2], " negatif" )
  print(msg)
  print("not ok")
}

for( i in 1:length(x) ) {
  if( x[i] > 0 ) {
    msg = paste( i, " => ", x[i], " positif" )
    print(msg)
  } else {
    msg = paste( i, " => ", x[i], " negatif" )
    print(msg)
  }
}

# regression linéaire
set.seed(1)
x = rnorm(100)
y = rnorm(100)
ols <- lm( y ~ x )
summary(ols)

z = rnorm(100)
ols <- lm( y ~ x + z )
summary(ols)

# travail avec fichier excel
install.packages("xlsx")
library(xlsx)
weo <- read.xlsx(file="weo.xls", sheetName="usa")
weo
class(weo)

write.csv(weo,"weo.csv")

# identifier des colonnes
weo[,2]
weo[,"gdp"]
weo$gdp

gdp = ts( weo[,"gdp"], start=1980, frequency=1 )
plot(gdp)

diff(gdp)
lag(gdp,-1)

tgdp1 = ((gdp / lag(gdp,-1)) - 1)*100
tgdp2 = ((diff(gdp) / lag(gdp,-1)))*100
cbind(tgdp1,tgdp2)

plot(tgdp)

cbind( weo, tgdp1 )

cbind( weo, tgdp=c(NA,tgdp1) )

# meilleure facon de faire => matrice temporelle
weo
class(weo)

weots = ts( weo[,-1], start=1980, frequency=1 )
plot(weots)
plot(weo)

weots = cbind( weots, tgdp1 )
colnames(weots) <- c("gdp","infl","curracc","tgdp")
colnames(weots)
plot(weots)

# regression inflation = a * tx croiss PIB + b
?lm
ols <- lm( infl ~ tgdp, data=weots )
summary(ols)

plot  ( weo[,"infl"] )
lines ( weo[,"infl"] )
lines ( predict(ols), col="red" )

window( weots, 1980, 1990)
summary( lm( infl ~ tgdp, data=window( weots, 1980, 1990) ) )
summary( lm( infl ~ tgdp, data=window( weots, 1990, 2000) ) )
summary( lm( infl ~ tgdp, data=window( weots, 2000, 2010) ) )
summary( lm( infl ~ tgdp, data=window( weots, 2010, 2018) ) )




# fonctions

test <- function( x ) {
  print(10*x)
}

test(2)


# estimation d'output gap
library(zoo)
?rollapply

gdp = weots[,"gdp"]
gdppot = rollapply( gdp, 5, mean, align="right" )
plot(gdp)
lines(gdppot,col="red")

ogap = (gdp / gdppot - 1) * 100
plot(ogap)

weots = ts( weo[,-1], start=1980, frequency=1 )
weots = cbind(weots, ogap)
colnames(weots) <- c("gdp","infl","curracc","ogap")
weots


# ogap sur taux de croissance du pib

tgdppot = rollapply( tgdp1, 5, mean, align="right" )
ogap    = tgdp1 - tgdppot

plot(tgdp1)
lines(tgdppot,col="red")

weots = ts( weo[,-1], start=1980, frequency=1 )
weots = cbind(weots, ogap)
colnames(weots) <- c("gdp","infl","curracc","ogap")
weots

summary( ols <- lm( infl ~ ogap, data=weots ) )

