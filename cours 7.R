
library(xlsx)
db = read.xlsx(file="cours 7 - data.xls", sheetName="data")

#write.csv(db,"cours 7 - data.csv",row.names=F)
#db = read.csv("cours 7 - data.csv")

arg = db[ db$COUNTRY=="ARGENTINA", ]
arg = ts( arg, start=1993, frequency=1 )
plot( arg[,"GDPG"])
plot( arg[,"REALRATE"])

# on reformate
dbx = db[,-c(1,2)]
dbx = db[,3:ncol(db)]
3:ncol(db)
3:17
c(3,4,5,6,7,9)
rownames(dbx) = db$ID
rownames(dbx) = db[,"ID"]

# plot
head(dbx)
plot(dbx)
pairs(dbx)

dbz = dbx[,-c(1,2)]
pairs( dbz, pch = 21, bg = c("green3", "red")[dbx$CRISIS+1])

pairs( dbx[,-c(1,2)], pch = 21, bg = c("green3", "red")[dbx$CRISIS+1])

# corrélation
round(cor( dbx )*100,1)
round(cor( na.omit(dbx) )*100,1)
dim(dbx)


# calculer les distances entre tous les points
d.dbs <- dist(dbx)


dbs = scale( dbx, center=T, scale=T )
mean(dbs,na.rm=T)
sd(dbs,na.rm=T)

#CAH - critère de Ward
#method = « ward.D2 » correspond au vrai critère de Ward
#utilisant le carré de la distance

dbs      = scale( dbx, center=T, scale=T )
d.dbs    = dist(dbs)
cah.ward = hclust( d.dbs, method="ward.D2" )
cah.ward
plot(cah.ward)
rect.hclust(cah.ward,k=2)
rect.hclust(cah.ward,k=4)


cbind( dbx$CRISIS, g )
table( g, dbx$CRISIS )

g = cutree( cah.ward, k=2 )
m = table( g, dbx$CRISIS )
sum(diag(m))/sum(m)*100
m

A = m[1,1]
B = m[1,2]
C = m[2,1]
D = m[2,2]

Se = A/(A+C) * 100
Sp = D/(B+D) * 100
PPV = A/(A+B) * 100
Se
Sp
PPV

# calculer la sensibilité et la spécificité



#d.dbs <- dist(dbx)
#cah.ward <- hclust(d.dbs,method="ward.D2")
inertie <- sort( cah.ward$height, decreasing = TRUE)
plot( inertie, type = "s")
plot( inertie[1:10], type = "s")

#best.cutree(cah.ward, min = 2, graph = TRUE, xlab = "Nombre de classes", ylab = "Inertie relative")

# K-Means

groupes.kmeans <- kmeans( na.omit(dbs), centers=4 )

g.ward  = cutree( cah.ward, k=4 )
g.kmeans = groupes.kmeans$cluster
cbind( g.ward, g.kmeans )

#affichage des résultats
print(groupes.kmeans)


# mettre la CAH sous forme de fonction

cah <- function( x ) {
  s = scale( na.omit(x), center=T, scale=T )
  d = dist(s)
  h = hclust( d, method="ward.D2" )
  h
}

cah.ward    = cah( dbx )
cah.groupes = cutree( cah.ward, k=4 )

set.seed(1)
km = kmeans( na.omit(dbs), centers=4 )
km.groupes = km$cluster

table( cah.groupes, km.groupes )

# --- ACP
head(dbs)
rownames( dbx ) = db$ID

library(FactoMineR)
res.pca = PCA(dbx, scale.unit=TRUE)

res.hcpc = HCPC( res.pca, nb.clust=-1 )

res.hcpc$desc.var
res.hcpc$desc.axes
res.hcpc$desc.ind
