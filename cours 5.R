
install.packages("FactoMinerR")
library(FactoMineR)

data( decathlon )
head( decathlon )

cor( decathlon[,1:10] )
round( cor(decathlon[,1:10])*100, 1 )

class(decathlon)
decathlon[,1]
decathlon[,"100m"]
decathlon$Rank
decathlon[,"Competition"]
decathlon[,"Competition"] == "OlympicG"

decathlon[ decathlon[,"Competition"] == "OlympicG", ]
colnames(decathlon)

round( cor( decathlon[ decathlon[,"Competition"] == "OlympicG", 1:10] )*100, 1 )
round( cor( decathlon[ decathlon[,"Competition"] == "Decastar", 1:10] )*100, 1 )

res.pca = PCA( decathlon[,1:10], scale.unit=FALSE )
res.pca = PCA( decathlon[,1:10], scale.unit=TRUE )
res.pca = PCA( decathlon[,1:13], scale.unit=TRUE, quanti.sup=c(11,12), quali.sup=13 )

plot.PCA(res.pca, axes=c(1, 2), choix="ind", habillage=13)

dimdesc( res.pca, axes=c(1,2) )



# dataset données pays
library(xlsx)

db = read.xlsx("cours 5 - data.xlsx", sheetName="data")
cn = read.xlsx("cours 5 - data.xlsx", sheetName="countries")

head(db)

rownames(db) = db[,1]
dbx = db[,-1]

res.pca = PCA( dbx )

# fusion
dbz <- merge( db, cn, by.x='code', by.y='ISO3' )

# mettre en nom de ligne le code iso3 des pays
rownames(dbz) = dbz[,"code"]

# supprimer les colonnes Code et country
dbz = dbz[,c(-1,-10)]

# enlever les valeurs manquantes
dbz = na.omit( dbz )

# ACP sur col 1 à 8
res.pca <- PCA( dbz, quali.sup=9)

rownames(dbz)=='VEN'
which( rownames(dbz) == 'VEN' )

suplin <- c(
  which(rownames(dbz)=='VEN'),
  which(rownames(dbz)=='YEM'),
  which(rownames(dbz)=='IRL'),
  which(rownames(dbz)=='UKR'),
  which(rownames(dbz)=='SLE'),
  which(rownames(dbz)=='LBY')
)

#which( rownames(dbz)=='VEN' | rownames(dbz)=='YEM' | rownames(dbz)=='IRL' )

dbk <- dbz[-suplin,]
res.pca <- PCA( dbk[,1:8] )

res.pca <- PCA( dbk[,1:8], ind.sup=suplin )




res.pca <- PCA( dbz, quali.sup=9, ind.sup=suplin )


# mettre en nom de ligne le code iso3 des pays
rownames(dbreg) <- dbreg[,"code"] 
