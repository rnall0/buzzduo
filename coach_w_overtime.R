library(RSQLite)
library(ggplot2)
library(directlabels)

db<-dbConnect(SQLite(), dbname = "cfbdata.sqlite")
rs<-dbSendQuery(db, "select * from sec_yt where School = 'auburn'")
aub<- dbFetch(rs, n = -1)
dbDisconnect(db)

aub$Year<-as.integer(aub$Year)
aub$Pct<-as.numeric(aub$Pct)

modern<-subset(aub, aub$Year>=1951)
graph<-ggplot(modern, aes(x=Year, y=Pct, color=Coach)) + geom_point(size=2) +
    scale_colour_hue(l=50) + 
    geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3),se = FALSE)+ 
	  ggtitle("AU Coach Winning Pct Over Time (1951 to Present)")+
	  theme(plot.title = element_text(lineheight=.8, face="bold"))
direct.label(graph, list("visualcenter",fontsize=5))




