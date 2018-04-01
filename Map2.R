library(plotly)
library(dplyr)


# Data collected from different Internet sources
cities<-data.frame("city"=c("Sao Paulo","Rio de Janeiro","Campinas","Recife",
                            "Belem","Belo Horizonte","Brasilia","Manaus","Orlando",
                            "Fort Lauderdale","New York","Miami","Los Angeles",
                            "Atlanta","Chicago","Dallas","Houston","Washington"),
                   "country"=c(rep("Brazil",8),rep("United States",10)),
                   "lat"=c(-23.5505,-22.9068,-22.9099,-8.0476,-1.4558,-19.9245,
                           -15.7942,-3.1190,28.5383,26.1224,40.7128,25.7617,
                           34.0522,33.7490,41.8781,32.7767,29.7604,38.9072),
                   "long"=c(-46.6333,-43.1729,-47.0626,-34.8770,-48.4902,-43.9352,
                            -47.8822,-60.0217,-81.3792,-80.1373,-74.0060,-80.1918,
                            -118.2437,-84.3880,-87.6298,-96.7970,-95.3698,-77.0369))
cities$cor<-c("aquamarine1","coral1","cyan","darkgreen","red","darkgoldenrod1",
              "darkviolet","blue",rep("black",10))

flights<-data.frame("city-start"=c(rep("Sao Paulo",9),rep("Rio de Janeiro",5),
                                   rep("Campinas",2),"Recife",rep("Belem",2),
                                   rep("Belo Horizonte",2),"Brasilia","Manaus"),
                    "city-end"=c("New York","Miami","Los Angeles","Orlando",
                                 "Atlanta","Chicago","Dallas","Houston","Washington",
                                 "Miami","New York","Atlanta","Houston","Orlando",
                                 "Orlando","Fort Lauderdale","Miami","Fort Lauderdale",
                                 "Miami","Miami","Orlando","Miami","Miami"),
                    "start-lat"=rep(NA,23),
                    "start-long"=rep(NA,23),
                    "end-lat"=rep(NA,23),
                    "end-long"=rep(NA,23))
oi<-c("Sao Paulo","Rio de Janeiro","Campinas","Recife","Belem","Belo Horizonte",
      "Brasilia","Manaus")
for(i in 1:length(oi)){
    flights[which(flights$city.start==oi[i]),3]<-cities[which(cities==oi[i]),3]
    flights[which(flights$city.start==oi[i]),4]<-cities[which(cities==oi[i]),4]
}
oi1<-c("Orlando","Fort Lauderdale","New York","Miami","Los Angeles","Atlanta",
       "Chicago","Dallas","Houston","Washington")
for(u in 1:length(oi1)){
    flights[which(flights$city.end==oi1[u]),5]<-cities[which(cities==oi1[u]),3]
    flights[which(flights$city.end==oi1[u]),6]<-cities[which(cities==oi1[u]),4]
}

flights$id <- seq_len(nrow(flights))
flights$cor<-c(rep("aquamarine1",9),rep("coral1",5),rep("cyan",2),"darkgreen",
               rep("red",2),rep("darkgoldenrod1",2),"darkviolet","blue")

###############################################################

g<- list(
    scope = 'world',
    showland = TRUE,
    showframe = FALSE,
    showcountries=TRUE,
    landcolor = toRGB("gray90"))

g1 <- c(
    g,
    resolution = 50,
    showcoastlines = T,
    showcountries = F,
    countrycolor = toRGB("white"),
    coastlinecolor = toRGB("white"),
    projection = list(type = 'mercator'),
    list(lonaxis = list(range = c(-135, -30))),
    list(lataxis = list(range = c(-35,55))),
    list(domain = list(x = c(0, 1), y = c(0, 1)))
)


p<-plot_geo(locationmode='country names',data=cities) %>%
    add_segments(
        data=group_by(flights,city.start),
        x= ~start.long,xend= ~end.long,
        y= ~start.lat, yend= ~end.lat,
        alpha=0.8, color=I(flights$cor),split=~city.start) %>%
    add_markers(
        data =cities,x=~long, y = ~lat,
        alpha=0.7,showlegend=F,
        text = ~city,color=I(cities$cor)) %>%
    add_trace(
        data = cities, z = ~cor, locations = ~city,
        showscale = F, geo = "g1",text=~city) %>%
    add_trace(
        data=cities,z = "",locations = ~country,
        showscale = F, geo = "g1") %>% 
    layout(
        title='Non-stop flights from Brazil to United States',geo=g1)

ggplotly(p)

