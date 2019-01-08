

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
