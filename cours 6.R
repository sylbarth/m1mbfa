
library(xlsx)

db = read.xlsx(file="cours 6 - data.xlsx", sheetName="data")

# spécifique pour ceux qui ont des problèmes Java
write.csv(db, "cours 6 - data.csv")
db = read.csv("cours 6 - data.csv")
db = db[,-1]

plot(db)

# plot
head(db)
plot(db[,"gdp_fra"])
lines(db[,"gdp_fra"])

# convertir la série en ts
gdp_fra = ts( db[,"gdp_fra"], start = 1980, frequency = 1 )
plot(gdp_fra)

# datasets par pays

fra = ts( db[,c("gdp_fra","unemp_fra")], start = 1980, frequency = 1 )
esp = ts( db[,c("gdp_esp","unemp_esp")], start = 1980, frequency = 1 )
usa = ts( db[,c("usa_gdp","usa_unemp")], start = 1980, frequency = 1 )
jpn = ts( db[,c("gdp_jpn","unemp_jpn")], start = 1980, frequency = 1 )

plot(fra)
plot(esp)
plot(usa)
plot(jpn)

colnames(fra) = c("gdp","unemp")
colnames(esp) = c("gdp","unemp")
colnames(usa) = c("gdp","unemp")
colnames(jpn) = c("gdp","unemp")

# récupérer les gdp: plot et corrélations

gdp = cbind( fra[,"gdp"], esp[,"gdp"], usa[,"gdp"], jpn[,"gdp"])
plot(gdp)

# corrélation depuis 1980 et depuis 2000 et entre 2006 et 2009
round( cor( window( gdp, 1980) )*100, 1 )
round( cor( window( gdp, 2000) )*100, 1 )
round( cor( window( gdp, start=2006,end=2009) )*100, 1 )


# estimations
#install.packages("dynlm")

library(dynlm)

?dynlm
summary( dynlm( unemp ~ gdp, data=fra ) )

# meme chose en ajoutant des retards
summary( dynlm( unemp ~ L(gdp,1) + L(gdp,2), data=fra ) )

# prévisions
model <- dynlm( unemp ~ L(gdp,1) + L(gdp,2), data=fra )

plot(fra[,"unemp"])
lines( fitted(model), col="red" )

# estimations des modèles
model1 <- dynlm( unemp ~ gdp, data=fra )
model2 <- dynlm( unemp ~ L(gdp,1), data=fra )
model3 <- dynlm( unemp ~ L(gdp,1) + L(gdp,2), data=fra )

plot(fra[,"unemp"])
lines( fitted(model1), col="green" )
lines( fitted(model2), col="blue" )
lines( fitted(model3), col="red" )

# ajouter le GDP US avec un lag

fra = cbind( fra, usa[,"gdp"] )
colnames(fra) = c("gdp","unemp","gdpus")
head(fra)

# autre méthode

fra = ts( db[,c("gdp_fra","unemp_fra","usa_gdp")], start = 1980, frequency = 1 )
colnames(fra) = c("gdp","unemp","gdpus")

model4 <- dynlm( unemp ~ L(gdp,1) + L(gdpus,1), data=fra )
summary( model4 )

plot(fra[,"unemp"])
lines( fitted(model1), col="green" )
lines( fitted(model2), col="blue" )
lines( fitted(model3), col="red" )
lines( fitted(model4), col="violet" )

# output gap
gdp5  = rollapply( fra[,"gdp"], 5, mean, align="right" )
plot(fra[,"gdp"])
lines(gdp5,col="red")

ogap = fra[,"gdp"] - gdp5
plot(ogap)

dbgap = cbind( fra, ogap )
head(dbgap)
plot(dbgap)

# regression avec output gap
model5 <- dynlm( fra.unemp ~ L(ogap,1), data=dbgap )
summary( model5 )

# estimation d'une loi d'Okun
du = diff( dbgap[,"fra.unemp"] )
dbokun = cbind( dbgap[,"fra.gdp"], du )
colnames(dbokun) = c("gdp","du")

model6 <- dynlm( du ~ gdp, data=dbokun )
summary( model6 )

x = coef(model6)
-x[1]/x[2]

