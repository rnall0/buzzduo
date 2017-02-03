library(rvest)
library(stringr)
library(dplyr)
library(RSQLite)

setwd("C:\\to\\output")

weblink <- paste("http://www.cfbdatawarehouse.com/data/div_ia_conf_index.php")
webpage <- read_html(weblink)
nodeslinks<- html_nodes(webpage, xpath = "//a")
needlinks<-nodeslinks[33:169]
links<-bind_rows(lapply(xml_attrs(needlinks), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
links<-as.data.frame(links[!grepl("div_ia", links$href),])
colnames(links)[1]<-"href"
links$href<-paste("http://www.cfbdatawarehouse.com/data/", links$href, sep="")
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

system.time(yearly.totals.matrix<- do.call(rbind,lapply(as.character(links$href),get_yearly_totals)))
yearly.totals <- data.frame(yearly.totals.matrix)

#import to sqlite database
db<-dbConnect(SQLite(), dbname = "cfbdata.sqlite")
dbWriteTable(conn = db, name = "yearly_totals", value = yearly.totals)


#test to make sure it's there
#dbListTables(db)
#dbListFields(db, "yearly_totals")
#head(dbReadTable(db, "yearly_totals"))
dbDisconnect(db)


