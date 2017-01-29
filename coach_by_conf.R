#Specify conference and collect yearly totals for each time in that conference and output to a SQLite database.
#Currently leaves out Independents. 

library(rvest)
library(stringr)
library(dplyr)
library(RSQLite)

setwd("C:\\path\\to\\cfb\\data\\")

conf<-"sec" #aac, acc, big10, big12, conferenceusa, mac, mountainwest,
            #pac10, sec, sunbelt
weblink <- paste("http://www.cfbdatawarehouse.com/data/div_ia/", conf, "/index.php", sep="")
webpage <- read_html(weblink)
nodeslinks<- html_nodes(webpage, xpath = "//a")
needlinks<-nodeslinks[27:40]
links<-bind_rows(lapply(xml_attrs(needlinks), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
links$href<-gsub("../../", "http://www.cfbdatawarehouse.com/data/", links$href)
links$href<-gsub("index", "yearly_totals", links$href)

get_yearly_totals<-function(webaddress){
  url<-paste(webaddress)
  print(strsplit(url, "/")[[1]][7])
  webaddy <- read_html(url)
  yearlytotals <- html_nodes(webaddy, 'table')
  yt <- html_table(yearlytotals, fill = TRUE)[[15]]
  yt.clean<-yt[,c(1:10)]
  colnames(yt.clean) <- c("Year","Coach","Win","Loss","Tie","Pct","PF","PA", "Delta", "School")
  yt.clean$School<-paste(as.character(strsplit(url, "/")[[1]][7]))
  yt.clean <- yt.clean[-nrow(yt.clean),] #last row contains totals
  yt.clean <- yt.clean[-c(1:5), ] #remove first 5 rows  
}

yearly.totals.matrix<- do.call(rbind,lapply(as.character(links$href),get_yearly_totals))
yearly.totals <- data.frame(yearly.totals.matrix)

#import to sqlite database
db<-dbConnect(SQLite(), dbname = "cfbdata.sqlite")
dbWriteTable(conn = db, name = paste(conf, "_yt", sep=""), value = yearly.totals)


#test to make sure it's there
#dbListTables(db)
#dbListFields(db, paste(conf, "_yt", sep=""))
#head(dbReadTable(db, paste(conf, "_yt", sep="")))
dbDisconnect(db)


