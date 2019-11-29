


x = 10
x

y = 10:50
y

z = seq(10,50,5)
z

# générer des 100 nombre aléatoires suivant
# une loi normale
x = rnorm(100)
plot(x)

set.seed(123)
x = rnorm(100)
y = rnorm(100)
head(x)

plot(x)
lines(x,col="red")

x = ts( x, start=1970, frequency=12 )
y = ts( y, start=1970, frequency=12 )

plot(x)

m = cbind( x, y )
m

df = data.frame( x, y )
df

matligne = rbind( x, y )
matligne

library(zoo)
#?rollapply

xm = rollapply( data=x, width=3, FUN=mean, align = "right")
#rollapply( x, 3, mean, align = "right")

plot(x)
lines(xm,col="red")

# calculer l'écart entre x et xm

z = x - xm
plot(z)

library(quantmod)
sp500 = getSymbols( "^GSPC", auto.assign=FALSE )
sp500 = sp500[,4]
head(sp500)

plot(sp500)

sp500_m05 = rollapply( sp500, 5, mean, align = "right")
sp500_m25 = rollapply( sp500,25, mean, align = "right")

# calculer un indicateur MACD
macd = sp500_m05 - sp500_m25
plot(macd)

# vérifier le type de MACD
class(macd)

# afficher le graph depuis le 1er janvier 2019
plot( macd["2019-01-01/"] )
plot( macd["/2019-06-15"] )
plot( macd["2019-01-01/2019-01-15"] )

#window(macd,2019)

# écrire une fonction qui calcule le MACD

test <- function() {
  print("hello")
  return("bye bye!")
}

k = test()
k

MACD <- function( cours ) {
  m05  = rollapply( cours, 5, mean, align = "right")
  m25  = rollapply( cours,25, mean, align = "right")
  macd = m05 - m25
  return(macd)
}

plot( MACD( sp500 ) )

# modifier la fonction, pour qu'elle prenne
# en argument les tailles des moyennes

MACD <- function( cours, m1, m2 ) {
  m05  = rollapply( cours, m1, mean, align = "right")
  m25  = rollapply( cours, m2, mean, align = "right")
  macd = m05 - m25
  return(macd)
}

plot( MACD( sp500["2019/"], 5, 25 ) )
lines( MACD( sp500, 10, 50 ), col="red")

# calculer un rendement en pourcentage (daily)
ret = sp500 / lag(sp500,1) - 1

model <- lm( ret ~ macd )
summary(model)

round( ret*100, 2 )


# 1 -> return en moyenne sur 5 jours
# 2 -> ecart type sur 30 jours
# 3 -> tout dans la même matrice ret, ret5, sd30 et macd
ret5  = rollapply( ret,  5, mean, align = "right")
sd30  = rollapply( ret, 30,   sd, align = "right")

mx = cbind( ret, ret5, sd30, macd )
tail(mx)

# je renomme mes colones ret, ret5, sd30 et macd
colnames(mx) = c("ret","ret5","sd30","macd")
tail(mx)

# je lance une PCA sur mx et j'interprète
library(FactoMineR)

PCA(mx)

# nombre de lignes de mx
nrow(mx)
ncol(mx)

# écrire une fonction qui calculer un ratio sharpe

mz = mx[,-2]

head(mz)

mz = na.omit(mx)
head(mz)

HCPC( mx["2019-10/"] )

ret30 = rollapply( ret, 30, mean, align = "right")
sd30  = rollapply( ret, 30,   sd, align = "right")

par(mfrow=c(1,1))
sharpe = ret30 / sd30
plot(sharpe)

