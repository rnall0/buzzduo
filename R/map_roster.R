library(httr)
library(rjson)
library(maps)
library(mapdata)
library(ggplot2)
library(ggsn)

roster<-read.csv(“C:\\path\\to\\data\\Springroster2017.csv”)

#geocode with google
counter<-0
total<-nrow(roster)
roster.geocode=NULL
for (i in 1:nrow(roster)){
 require(httr)
 require(rjson)
 url<-”http://maps.googleapis.com/maps/api/geocode/json"
 addr<-paste(roster[i, 8], roster[i,9], sep=”, “)
 response <- GET(url,query=list(sensor=”FALSE”,address=addr))
 json <- fromJSON(content(response,type=”text”))
 cat(paste (‘“‘, addr, ‘“\n’, sep = “”))
 counter <<- counter + 1;
 message(paste(counter, “of”, total))
 loc <- (json[‘results’][[1]][[1]]$geometry$location)
 ID<-roster[i,1]
 Number<-roster[i,2]
 Name<-roster[i,3]
 Position<-roster[i,4]
 Height<-roster[i,5]
 Weight<-roster[i,6]
 Class<-roster[i,7]
 City<-roster[i,8]
 State<-roster[i,9]
 Lat<-loc$lat
 Lon<-loc$lng
 df<-data.frame(ID, Number, Name, Position, Height, Weight, Class,       City, State, Lat, Lon)
 roster.geocode<-rbind(roster.geocode, df)
}

#map points
states <- map_data("state")
roster.map<-ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), color = "white") +
  geom_point(data = roster.geocode, aes(x = roster.geocode$Lon, y = roster.geocode$Lat), color = " dark orange", size = 2) +
  coord_fixed(1.3) +
  ggtitle("2017 Auburn Spring Football Roster Hometowns")
  
#map point density
roster.map<-ggplot(data = states) + 
 geom_polygon(aes(x = long, y = lat, group = group), color = “white”) + 
 stat_density2d(data=roster.geocode,aes(roster.geocode$Lon,roster.geocode$Lat, fill= ..level..), alpha =0.5, geom=”polygon”)+
 geom_point(data=roster.geocode, aes(roster.geocode$Lon, roster.geocode$Lat), colour=”dark orange”, alpha = 1, size=1)+
 coord_fixed(1.3) +
 ggtitle(“2017 Auburn Spring Football Roster Hometowns”)+
 guides(fill=FALSE)
 
 #scale bar
 scale.bar <- roster.map + scalebar(location=”bottomright”,y.min=27, y.max=32, x.min=-120, x.max=-110, dist=1000, 
                                    dd2km= TRUE, model=’WGS84', st.dist=.3)
                                    
  #north arrow
  north2(scale.bar, x=.2, y=.3, symbol=8)
