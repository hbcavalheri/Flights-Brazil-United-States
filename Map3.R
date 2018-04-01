library(leaflet)
library(geosphere)


# Airports ----
airp<-read.table("airports.dat.txt",sep=",",header=TRUE)
colnames(airp)<-c("Airport","Name","City","Country","IATA","ICAO","Latitude",
                  "Longitude","Altitude","Timezone","DST","Tz","Type","Source")
#Altitude in feet
airp<-airp[,c(2:5,7:8)]
br.air<-subset(airp,Country=="Brazil")

# Flight routes ----
route<-read.table("routes.dat.txt",sep=",",header=TRUE)
colnames(route)<-c("Airline","Air_ID","from","from.id","to","to.id","Codeshare",
                  "Stops","Equipament")
airp<-airp[,c(2:5,7:8)]
head(route)

# Subsetting flights from Brazil ----
flight<-route[route$from%in%br.air$IATA,]
flight

fly1<-merge(flight,airp,by.x='to',by.y='IATA')
fly2<-fly1[,c(1,4,11:14)]
colnames(fly2)<-c('to','from','city','country','lat.to','long.to')
fly3<-subset(fly2,country!='Brazil')
fly<-merge(fly3,airp,by.x='from',by.y='IATA')
fly<-fly[,c(1:6,8,10:11)]
colnames(fly)<-c('from','to','city.to','country','lat.to','long.to','city.from',
                 'lat.from','long.from')
path<-data.frame('airport'=c(fly$to,fly$from),'lat'=c(fly$lat.to,fly$lat.from),
                 'long'=c(fly$long.to,fly$long.from),group=rep(1:nrow(fly),2))
fly$cor<-c(rep('#FF0000FF',3),rep('#FF6600FF',8),rep('#FFCC00FF',6),
           rep('#CCFF00FF',3),rep('#66FF00FF',2),rep('#00FF00FF',4),
           rep('#00FF66FF',41),rep('#00FFCCFF',100),rep('#00CCFFFF',2),
           rep('#0066FFFF',5),'#0000FFFF',rep('#6600FFFF',10),
           rep('#CC00FFFF',6),rep('#FF00CCFF',7),'#FF0066FF')

# Map using Leaflet ----

gcIntermediate(fly[,c('long.to','lat.to')],fly[,c('long.from','lat.from')],
               200,breakAtDateLine=FALSE, addStartEnd=TRUE, sp=TRUE)  %>% 
    leaflet() %>% 
    addTiles() %>% 
    setView(lng=-75,lat=0,zoom=2) %>% 
    addProviderTiles("NASAGIBS.ViirsEarthAtNight2012") %>%
    addPolylines(color=fly$cor,weight=1) %>%
    addLegend("bottomright",colors=rainbow(15),
              labels=c('Belem','Brasilia','Belo Horizonte','Curitiba','Florianopolis',
                       'Fortaleza','Rio de Janeiro','Sao Paulo','Foz do Iguacu',
                       'Manaus','Natal','Porto Alegre','Recife','Salvador','Campinas'),
              title='Departure',opacity=1)
