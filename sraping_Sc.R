if(F){
  setwd("/Users/juanpabloalvarezvega/Dropbox/Desktop2021/Masters/Thesis/FRC")
  frg<-read.csv(paste("Data/splc-hate-groups-", 2019,".csv",sep=""))
  
  ###
  State_levels_scrape<-levels(frg$State)
  remDr <- remoteDriver(browserName = "chrome",port = 4444)
  Sys.sleep(.5)
  remDr$open(silent=T)
  Sys.sleep(.5)
  remDr$navigate("https://trac.syr.edu/phptools/immigration/secure/")
  Sys.sleep(.5)
  drop_down<-remDr$findElement("css", "#col2head2 #dimension_pick_col1")
  Sys.sleep(.5)
  drop_down$sendKeysToElement(list("Fiscal Year"))
  Sys.sleep(.5)
  year_button<-remDr$findElement("css", "div div:nth-child(1) tr~ tr+ tr input")
  Sys.sleep(.5)
  year_button$clickElement()
  Sys.sleep(.5)
  
  for(i in 1:51){
    state<-remDr$findElement(using = "link text", State_levels_scrape[i])
    Sys.sleep(.5)
    state$clickElement()
    Sys.sleep(.5)
    remDr$executeScript("javascript:m.tableClick('refused', 'All');")
    Sys.sleep(.5)
    title<-"All"
    webElems <- remDr$findElements(using = "class", "bar")
    Sys.sleep(.5)
    values<-sapply(webElems, function(x){x$getElementAttribute("height")})
    Sys.sleep(.5)
    values<-as.numeric(unlist(values))
    highest_value <- remDr$findElements(using = "class", "tick")
    Sys.sleep(.5)
    highest_value <- highest_value[[length(highest_value)]]
    highest_value<-gsub(",", "",highest_value$getElementText())
    highest_value<-as.numeric(highest_value)
    highest_value<-highest_value/280
    values<-highest_value*values
    year <- remDr$findElements(using = "class", "tick")
    Sys.sleep(.5)
    years<-sapply(year, function(y){y$getElementText()})
    Sys.sleep(.5)
    years<-as.numeric(unlist(years))
    year_range<-2009:2019
    years<-years[which(years%in%year_range)]
    secure_communities<-data.frame(rep(State_levels_scrape[i], length(values)), years, values)
    names(secure_communities)<-c("State", "Year", title)
    if(length(year_range[which(!year_range%in%years)])!=0){
      year_range<-year_range[which(!year_range%in%years)]
      added_rows<-data.frame(rep(State_levels_scrape[i], length(year_range)), year_range,
                             rep(NA, length(year_range)))
      names(added_rows)<-c("State", "Year", title)
      secure_communities<-rbind(secure_communities,added_rows)
      secure_communities<-secure_communities[order(secure_communities$Year),]
    }
    assign(paste(State_levels_scrape[i], "sc", title, sep = "_"), secure_communities)
  }
  
  for(i in 1:51){
    if(i==1){
      secure_communities<-Alabama_sc_All
    }else{
      sc_rows<-get(paste(State_levels_scrape[i],"sc","All", sep = "_"))
      secure_communities<-rbind(secure_communities, sc_rows)
    }
  }
  
  sapply(secure_communities, class)
  secure_communities$State<-as.character(factor(secure_communities$State))
  secure_communities$State<-gsub(" ","_",secure_communities$State)
  secure_communities<-secure_communities[order(secure_communities$Year),]
  a<-secure_communities
  for(i in 1:nrow(secure_communities)){
    if(is.na(secure_communities$All[i])){
      secure_communities$All[i]<-0
    }else{
      next
    }
  }
}
write.csv(secure_communities, "Data/secure_communities.csv")

