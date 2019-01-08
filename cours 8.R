
#install.packages("RJSONIO")


# scrap
library(RJSONIO)
library(xts)

x = fromJSON("https://api.worldbank.org/v2/countries/?format=json")
dat = lapply( x[[2]], function(j) c( j$id, j$name ) )
dat = data.frame( do.call('rbind', dat) )

wbReadJSON <- function( url, fields ) {
  x = fromJSON( url )
  dat = lapply( x[[2]], function(j) j[fields] )
  dat = data.frame( do.call('rbind', dat) )

    dat$date  = as.character(dat$date)
  dat$value = as.numeric  (dat$value)

  dat = xts( dat$value, order.by=as.Date( dat$date, format="%Y" ) )
  
  dat
}

wbReadJSON( "https://api.worldbank.org/v2/countries/?format=json", c("id","name","iso2Code")  )

library(xts)
test = wbReadJSON( "https://api.worldbank.org/v2/countries/bra/indicators/SP.POP.TOTL?format=json", c("date","value") )
wbReadJSON( "https://api.worldbank.org/v2/countries/usa/indicators/NY.GDP.PCAP.PP.CD?format=json", c("date","value") )

country   = "BRA"
indicator = "SP.POP.TOTL"
urlBase   = "https://api.worldbank.org/v2/countries/"
paste( urlBase, country, "/indicators/", indicator, "/?format=json", sep="" )


wbReadData <- function( country, indicator ) {
  urlBase   = "https://api.worldbank.org/v2/countries/"
  url = paste( urlBase, country, "/indicators/", indicator, "/?format=json", sep="" )
  dat = wbReadJSON( url, c("date","value") )
  dat
}

# real gdp, gdp per capita ppp, 
# inflation, croissance de l'investissement, 
# croissance de la consommation

pop = wbReadData( "BRA", "SP.POP.TOTL" )
plot(pop)

gdp    = wbReadData("USA", "NY.GDP.MKTP.KD.ZG")
infl   = wbReadData("USA", "FP.CPI.TOTL.ZG")
gdppc  = wbReadData("USA", "NY.GDP.PCAP.PP.CD")
invest = wbReadData("USA", "NE.GDI.TOTL.ZS")
cons   = wbReadData("USA", "NE.CON.TOTL.KD.ZG")

db = 




