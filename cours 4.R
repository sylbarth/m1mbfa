
#install.packages("quantmod")

library(quantmod)

sp500 = getSymbols("^GSPC",auto.assign=F) 
tail(sp500)

# on récupère le adjusted
sp500 = sp500[,"GSPC.Adjusted"]

# on dessine le graphique
plot(sp500)


#
cac40 = getSymbols( "^FCHI",  auto.assign=F ) 
dax30 = getSymbols( "^GDAXI", auto.assign=F ) 

cac40 = cac40[,6]
dax30 = dax30[,6]

# matrice globale
prices = cbind( sp500, cac40, dax30 )
tail(prices)

colnames(prices) = c( "sp500", "cac40", "dax30" )
plot(prices)

ret = diff(log(prices))
tail(ret)
plot(ret)

ret = na.omit( ret )
class( ret )

ret["2018-10"]

plot( ret["2018-10"] )
plot( ret["2018-01/"] )
plot( ret["/2018-01"] )
plot( ret["2017-12-10/2018-01-05"] )


cor( ret )
cor( ret["2017"])
cor( ret["2018"])

plot(cor(ret))

install.packages("FactoMineR")
library(FactoMineR)

x <- PCA(ret)

library(FactoMineR)
PCA(ret)


# travaux sur Iris dataset
nrow(iris)
ncol(iris)
dim(iris)

head(iris)
iris[,"Species"]
iris$Species
table( iris$Species )

plot( iris )
plot(data.frame(ret))

pie( table(iris$Species) )
hist(iris$Petal.Length)
hist(iris[,2])

iris[,1:4]
iris[,c(1,2,3,4)]
1:4

cor(iris)
cor(iris[,1:4])

round( cor(iris[,1:4])*100, 1 )

PCA(iris[,1:4])
