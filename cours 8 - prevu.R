


# connecteur Banque Mondiale


# scrap
library(RJSONIO)

# expliquer JSON
x = RJSONIO::fromJSON("https://api.worldbank.org/v2/countries/?format=json")
x[[1]]
x = x[[2]]
x[[1]]$id
x[[1]]$name
dat = lapply(x, function(j) cbind(j$id, j$name ))
dat = data.frame(do.call('rbind', dat))


#

# expliquer JSON
x = RJSONIO::fromJSON("https://api.worldbank.org/v2/countries/?format=json&per_page=25000")
x = x[[2]]
dat = lapply(x, function(j) cbind(j$id, j$name, j$capitalCity ))
dat = data.frame(do.call('rbind', dat))





#
library(xts)




getWB <- function(country, indicator) {
  url = paste("https://api.worldbank.org/v2/countries/", country, "/indicators/", indicator,"?date=1960:2018&per_page=25000", "&format=json", sep = "")
  dat = tryCatch(RJSONIO::fromJSON(url, nullValue=NA), error=function(cond){return(NA)}   )
  if(length(dat)==2){
    if(length(dat[[2]])<2) return(NA)
    dat = dat[[2]]
    dat = lapply(dat, function(j) cbind(j$country[[1]], j$date, j$value ))
    dat = data.frame(do.call('rbind', dat))
    dat[,1] = as.character(dat[,1])
    dat[,2] = as.character(dat[,2])
    dat[,3] = as.numeric(as.character(dat[,3]))
    data = xts( as.numeric(dat[,3]), order.by=as.Date( paste( dat[,2], "-01-01", sep="" ) ) )
    names(data) = paste(country,indicator,sep="/")
    return(data)
  }
  return(NULL)
}

gdp   = getWB("USA","NY.GDP.MKTP.KD.ZG")
infl  = getWB("USA","FP.CPI.TOTL.ZG")
pop   = getWB("USA","SP.POP.GROW")
inv   = getWB("USA","NE.GDI.TOTL.KD.ZG")
cons  = getWB("USA","NE.CON.TOTL.KD.ZG")
gdppc = getWB("USA","NY.GDP.PCAP.PP.CD")


# dataset
db = cbind(gdp,infl,pop,inv,cons,gdppc)
plot(ts(db))
plot(ts(db,start=1960))

#
cor(db)
plot(data.frame(db))


# rappel
dbx = na.omit(db)
dbs      = scale( dbx, center=T, scale=T )
dbs.d    = dist(dbs)
cah.ward = hclust( dbs.d, method="ward.D2" )
cah.ward
plot(cah.ward)
rect.hclust(cah.ward,k=4)



library(FactoMineR)
res.pca = PCA(dbx, scale.unit=TRUE)
res.hcpc = HCPC( res.pca, nb.clust=-1 )
res.hcpc$desc.var
res.hcpc$desc.axes
res.hcpc$desc.ind


# idem sur des pib de plusieurs pays (trouver les codes iso)
countries = c("USA","FRA","DEU","BRA","RUS","IND","CHN","NGA","ZAF","ARG","THA","MYS","VNM","AUS")
gdpArray = NULL
# boucle
for(i in 1:length(countries)) {
  x = getWB(countries[i],"NY.GDP.MKTP.KD.ZG")
  # ajouter une colonne
  gdpArray = cbind(gdpArray,x)
}
gdpArray

# noms
colnames(gdpArray) = countries

plot(data.frame(gdpArray))

dbz = gdpArray
pairs( dbz, pch = 21)
#pairs( dbz, pch = 21, bg = c("green3", "red")[dbx$CRISIS+1])


# comparaison des pays (pas des années)

dbx = na.omit(gdpArray) # on supprime d'abord les lignes
dbx = t(dbx)
dbs.d    = dist(dbx)
cah.ward = hclust( dbs.d, method="ward.D2" )
cah.ward
plot(cah.ward)
rect.hclust(cah.ward,k=3)

# puis dans une fonction


# pca
res.pca = PCA(dbx, scale.unit=TRUE)
res.hcpc = HCPC( res.pca, nb.clust=-1 )
res.hcpc$desc.var
res.hcpc$desc.axes
res.hcpc$desc.ind

# cor
# puis analyse de données
#

# puis 5 pays
# idem
# interprétation


# recursive partitionning
library(rpart)
library(rpart.plot)

# révisions du reste

# charger une librairie ?
# lire un fichier csv ?
# ecrire un fichier csv ?
# lire un fichier excel ?
# 