library(RSelenium)
redr<-remoteDriver(port = 4444, browserName = "chrome")
redr$open()
# https://cran.r-project.org/web/packages/RSelenium/vignettes/basics.html
redr$navigate("https://www.ncsl.org/research/immigration/immigration-laws-database.aspx")
year<-redr$findElement(using = 'id', value ='dnn_ctr71247_StateNetDB_ddlYear')
year$sendKeysToElement(list("All"))
state<-redr$findElement(using = 'id', value ='dnn_ctr71247_StateNetDB_ckBxAllStates')
state$clickElement()


for (i in 1:5){
  assign(paste("state", i, sep = ""), redr$findElement(using = 'id', value =paste("dnn_ctr71247_StateNetDB_ckBxStates_5", i, sep = "")))
}
state1$clickElement()
state2$clickElement()
state3$clickElement()
state4$clickElement()
state5$clickElement()

topics<-redr$findElement(using = 'id', value ='dnn_ctr71247_StateNetDB_ckBxAllTopics')
topics$clickElement()

search<-redr$findElement(using = 'name', value ='dnn$ctr71247$StateNetDB$btnSearch')
search$clickElement()


