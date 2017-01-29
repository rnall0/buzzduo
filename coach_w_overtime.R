library(RSQLite)
library(ggplot2)
library(directlabels)

db<-dbConnect(SQLite(), dbname = "cfbdata.sqlite")
rs<-dbSendQuery(db, "select * from sec_yt where School = 'auburn'")
aub<- dbFetch(rs, n = -1)

aub$Year<-as.integer(aub$Year)
aub$Pct<-as.numeric(aub$Pct)

modern<-subset(aub, aub$Year>=1951)
graph<-ggplot(modern, aes(x=Year, y=Pct, color=Coach)) + geom_point(size=2) +
    scale_colour_hue(l=50) + 
    geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3),se = FALSE)+ 
	  ggtitle("AU Coach Winning Pct Over Time (1951 to Present)")+
	  theme(plot.title = element_text(lineheight=.8, face="bold"))
direct.label(graph, list("visualcenter",fontsize=5))
	

	

-----------------------------------

#subset individual coaches
shug<-subset(AUtotals, AUtotals$Year >= 1951 & AUtotals$Year <= 1975)
barfield<-subset(AUtotals, AUtotals$Year >= 1976 & AUtotals$Year <= 1980)
dye<-subset(AUtotals, AUtotals$Year >= 1981 & AUtotals$Year <= 1992)
bowden<-subset(AUtotals, AUtotals$Year >= 1993 & AUtotals$Year <= 1998)
tubs<-subset(AUtotals, AUtotals$Year >= 1999 & AUtotals$Year <= 2008)
chiz<-subset(AUtotals, AUtotals$Year >= 2009 & AUtotals$Year <= 2012)
gus<-subset(AUtotals, AUtotals$Year >= 2013 & AUtotals$Year <= 2015)

ggplot(shug,aes(Year,Pct))+
  geom_line(aes(color="Shug Jordan"))+
  geom_line(data=barfield,aes(color="Doug Barfield"))+
  geom_line(data=dye,aes(color="Pat Dye"))+
  geom_line(data=bowden,aes(color="Terry Bowden"))+
  geom_line(data=tubs,aes(color="Tommy Tuberville"))+
  geom_line(data=chiz,aes(color="Gene Chizik"))+
  geom_line(data=gus,aes(color="Gus Malzahn"))+
  labs(color="Coaches")


ggplot(AUtotals, aes(x=Year, y=Pct, group = Coach)) +
  geom_point() +
  geom_smooth(se=FALSE)
--------------------------------------

> plot(time, series1, type='l', xlim=c(0.0,20.0), 
+ ylim=c(0.0,1.0), xlab='t /s', ylab='s1')
> par(new=T)
> plot(time, series2, type='l', xlim=c(0.0,20.0), 
+ ylim=c(0.0,1.0), xlab='', ylab='', axes=F)
> par(new=F)




