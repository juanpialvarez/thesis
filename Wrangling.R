R.Version() #"R version 3.6.3 (2020-02-29)"
RStudio.Version() #‘1.3.1093’
#Clear
rm(list = ls())

#Library
library(glmmADMB)
library(glmmTMB)
library(maxLik)
library(miscTools)
library(pglm)
library(grid)
library(lmtest)
library(xts)
library(dygraphs)
library(RColorBrewer)
library(car)
library(tidycensus)
library(network)
library(sna)
library(tidyverse)
library(igraph)
library(ggplot2)
library(tidygraph)
library(readxl)
library(stargazer)
library(rvest)
library(stringr)
library(pdftools)
library(sandwich)
library(NLP)
library(tm)
library(RSelenium)
library(plyr)
library(plm)
library(haven)
library(rnoaa)
library(moments)
library(xlsx)
library(ggraph)

#Set workind direcotry, read data into R and name the file FRG
v15 <- load_variables(2005, "acs1", cache = TRUE)
v16 <- load_variables(2019, "acs5", cache = TRUE)
v17<-load_variables(2010, "sf1", cache = FALSE)
v18<-load_variables(2014, "acs1", cache = FALSE)
v19<-load_variables(2015, "acs1", cache = FALSE)

setwd("/Users/juanpabloalvarezvega/Dropbox/Desktop2021/Masters/Thesis/FRC")

frg<-read.csv(paste("Data/splc-hate-groups-", 2019,".csv",sep=""))
State_levels<-levels(frg$State)
State_levels_scrape<-levels(frg$State)
State_levels<-gsub(" ","_", State_levels)
`%notin%` <- Negate(`%in%`)
state_abb<-read_csv("Data/State_abbs")

President_votes<- read_csv("Data/President_Data/1976-2020-president.csv")
state_abb<-unique(President_votes$state_po)
names_abb<-data_frame(State_levels, state_abb)
write_csv(names_abb, "Data/State_abbs")

for (i in 2005:2016){
  frg<-read.csv(paste("Data/Network_Data/splc-hate-groups-", i,".csv",sep=""))
  frg$Title     <- as.character(factor(frg$Title))
  frg$Title     <- gsub(" ","_", frg$Title)
  frg$State     <- as.character(factor(frg$State)) 
  frg$State     <- gsub(" ","_", frg$State)
  frg$Group     <- as.character(factor(frg$Group))
  frg$Group     <- gsub(" ","_", frg$Group)
  frg$City      <- as.character(factor(frg$City))
  frg$City      <- gsub(" ","_", frg$City)
  frg$Ideology  <- as.character(factor(frg$Ideology))
  frg$Ideology  <- gsub(" ","_", frg$Ideology)
  titles<-unique(frg$Title)
  pre_edges<-list()
  for (x in 1:length(titles)){
    title_text<-paste("title_", x, "<- subset(frg, Title==",  deparse(titles[x]) , ", select = State)", sep = "")
    eval(parse(text = title_text))
    pre_edges[[x]]<-get(paste("title_", x ,sep=""))
    remove_text<-paste("rm(", paste("title_", x, sep = ""), ")", sep = "")
    eval(parse(text = remove_text))
  }
  pre_edges_2<-list()
  for (x in 1:length(State_levels)){
    assign(paste(State_levels[x], x, sep = "_"), list())
    for (k in 1:length(pre_edges)){
      if(State_levels[x]%in%pre_edges[[k]][[1]]){
        to<-subset(pre_edges[[k]], pre_edges[[k]][[1]]!=State_levels[x])
        to<-to[,1]
        from<-rep(State_levels[x],length(to))
        assign(paste(State_levels[x], k, sep = ""), data.frame(from, to))
        text_list<-paste(paste(State_levels[x], "_", x, "[[", k ,"]]", sep = ""), "<-",paste(State_levels[x], k, sep = ""), sep = "")
        eval(parse(text = text_list))
        remove_text<-paste("rm(",paste(State_levels[x], k, sep = ""), ")", sep = "")
        eval(parse(text = remove_text))
      }else{
        next
      }
    }
    pre_edges_2[[x]]<-get(paste(State_levels[x], x, sep = "_"))
    remove_text<-paste("rm(",paste(State_levels[x], x, sep = "_"), ")", sep = "")
    eval(parse(text = remove_text))
  }
  
  zero_obs<-c()
  for (x in 1:length(pre_edges_2)){
    if(length(pre_edges_2[[x]])==0){
      if(is.null(zero_obs)){
        zero_obs[1]<-x
      }else{
        zero_obs[length(zero_obs)+1]<-x
      }
    }else{
      pre_edges_2[[x]] <- pre_edges_2[[x]][-which(sapply(pre_edges_2[[x]], is.null))]
    }
  }
  
  if(!is.null(zero_obs)){
    pre_edges_2<-pre_edges_2[-zero_obs]
  }
  
  edge_list<-data.frame(from=NULL, to=NULL)
  for (x in 1:length(pre_edges_2)){
    for(k in 1:length(pre_edges_2[[x]])){
      if(length(pre_edges_2[[x]][[k]][[1]])==0){
        next
      }else{
        if(nrow(edge_list)==0){
          edge_list<-pre_edges_2[[x]][[k]]
        }else{
          edge_list<-rbind(edge_list, pre_edges_2[[x]][[k]])
        }
      }
    }
  }  
  
  edge_list <- edge_list %>%  
    group_by(from, to) %>%
    #must use dplyr because if you load plyr it defaults to that package's summarise function 
    dplyr::summarise(weight = n()) %>% 
    ungroup()
  
  edge_list$from<-as.character(factor(edge_list$from))
  edge_list$to<-as.character(factor(edge_list$to))
  
  assign(paste("edge_list",i, sep = "_"), edge_list)
  assign(paste("rwe_graph",i , sep = "_"),graph_from_data_frame(get(paste("edge_list",i, sep = "_")), directed = F, vertices = NULL))
  
  Degree<-degree(get(paste('rwe_graph_', i , sep = '')))
  State<-State_levels[State_levels%in%unique(edge_list$to)]
  degree_data<-data.frame(State, Degree)
  State<-State_levels[!State_levels%in%unique(edge_list$to)]
  Degree<-rep(NA, length(State))
  added_row<-data.frame(State, Degree)
  degree_data<-rbind(degree_data, added_row)
  degree_data$State<-as.character(factor(degree_data$State))
  degree_data<-degree_data[order(degree_data$State),]
  assign(paste("degree", i, sep = ""), degree_data)
  
  betweenness<-betweenness(get(paste('rwe_graph_', i , sep = '')))
  State<-State_levels[State_levels%in%unique(edge_list$to)]
  betweenness_data<-data.frame(State, betweenness)
  State<-State_levels[!State_levels%in%unique(edge_list$to)]
  betweenness<-rep(NA, length(State))
  added_row<-data.frame(State, betweenness)
  betweenness_data<-rbind(betweenness_data, added_row)
  betweenness_data$State<-as.character(factor(betweenness_data$State))
  betweenness_data<-betweenness_data[order(betweenness_data$State),]
  assign(paste("betweenness", i, sep = ""), betweenness_data)
  
  closeness<-closeness(get(paste('rwe_graph_', i , sep = '')))
  State<-State_levels[State_levels%in%unique(edge_list$to)]
  closeness_data<-data.frame(State, closeness)
  State<-State_levels[!State_levels%in%unique(edge_list$to)]
  closeness<-rep(NA, length(State))
  added_row<-data.frame(State, closeness)
  closeness_data<-rbind(closeness_data, added_row)
  closeness_data$State<-as.character(factor(closeness_data$State))
  closeness_data<-closeness_data[order(closeness_data$State),]
  assign(paste("closeness", i, sep = ""), closeness_data)
  
  eigen_vector<-eigen_centrality(get(paste('rwe_graph_', i , sep = '')))$vector
  State<-State_levels[State_levels%in%unique(edge_list$to)]
  eigen_vector_data<-data.frame(State, eigen_vector)
  State<-State_levels[!State_levels%in%unique(edge_list$to)]
  eigen_vector<-rep(NA, length(State))
  added_row<-data.frame(State, eigen_vector)
  eigen_vector_data<-rbind(eigen_vector_data, added_row)
  eigen_vector_data$State<-as.character(factor(eigen_vector_data$State))
  eigen_vector_data<-eigen_vector_data[order(eigen_vector_data$State),]
  assign(paste("eigen_vector", i, sep = ""), eigen_vector_data)
  
  plot(get(paste("rwe_graph",i, sep = "_")),
       layout=layout_with_graphopt,
       vertex.color="gold",
       vertex.size=10,
       vertex.frame.color="gray", 
       vertex.label.color="black",
       vertex.label.cex=0.8, 
       vertex.label.dist=2,
       edge.curved=0.2)

  
  order_size<-data.frame(size= gsize(get(paste("rwe_graph",i, sep = "_"))),
                         order=gorder(get(paste("rwe_graph",i, sep = "_"))),
                         degree=max(degree(get(paste("rwe_graph",i, sep = "_")))))
  write.xlsx2(order_size, paste0("Output/order_size", i,".xlsx") )
}

Pirus<- read_excel("Data/PIRUS_May2020/PIRUS_Public_May2020.xlsx")
Pirus[Pirus == -99 | Pirus == -88]<- NA

for (i in 2005:2016){
  if(i==2019){
    next
  }else{
    if(i==2005){
      instrument<-subset(Pirus, Pirus$Date_Exposure>as.Date("2005-01-01") & Pirus$Date_Exposure<as.Date("2001-01-01"))
    }else{
      instrument<-subset(Pirus, Pirus$Date_Exposure>as.Date(paste(i, "-01-01", sep = "")) & Pirus$Date_Exposure<as.Date(paste(i+1, "-01-01", sep = "")))
    }
  }
  N_individuals <- instrument %>%  
    group_by(Loc_Habitation_State1) %>%
    dplyr::summarise(extremists = n()) %>% 
    ungroup() 
  N_individuals <- subset(N_individuals, !is.na(N_individuals[1]))
  N_individuals$Loc_Habitation_State1<-gsub(" ","_", N_individuals$Loc_Habitation_State1)
  Loc_Habitation_State1<-as.character(State_levels[which(!State_levels%in%N_individuals$Loc_Habitation_State1)])
  zereo_extremist<-rep(0.0, length(Loc_Habitation_State1))
  added_states<-data.frame(Loc_Habitation_State1, extremists=0)
  added_states$Loc_Habitation_State1<-as.character(factor(added_states$Loc_Habitation_State1))
  N_individuals<-rbind(N_individuals, added_states)
  N_individuals<-N_individuals[order(N_individuals$Loc_Habitation_State1),]
  N_individuals<-subset(N_individuals, Loc_Habitation_State1!="Puerto_Rico")
  assign(paste("N_individuals", i, sep = "_"), N_individuals)
}

for (i in 2005:2016){
  text_analysis<-paste(paste("analysis_data", i, sep = "_"), "<-", paste("N_individuals_", i, sep = ""))
  eval(parse(text = text_analysis))
  text_analysis<-paste(paste("analysis_data_", i, "$degree",sep = ""), "<-", paste("degree", i, sep = ""), "$Degree", sep = "")
  eval(parse(text = text_analysis))
  text_analysis<-paste(paste("analysis_data_", i, "$betweeness", sep = ""), "<-",paste("betweenness", i, sep = ""), "$betweenness", sep = "")
  eval(parse(text = text_analysis))
  text_analysis<-paste(paste("analysis_data_", i, "$closeness", sep = ""), "<-",paste("closeness", i, sep = ""), "$closeness", sep = "")
  eval(parse(text = text_analysis))
  text_analysis<-paste(paste("analysis_data_", i, "$eigen_vector", sep = ""), "<-",paste("eigen_vector", i, sep = ""), "$eigen_vector", sep = "")
  eval(parse(text = text_analysis))
  year<-rep(i, length(get(paste("analysis_data", i, sep = "_"))$Loc_Habitation_State1))
  text_analysis<-paste(paste("analysis_data_", i, "$Year", sep = ""), "<-","year", sep = "")
  eval(parse(text = text_analysis))
}

if(F){
state_noaa<-ncdc_locs(locationcategoryid='ST', limit=52, token = "hvsYpwxfeUHEHFTbbRwslBNwXebjSLTh")
state_noaa<-state_noaa[[2]]
noaa_variables<-ncdc_datatypes(datasetid="GSOY",
                               locationid = 'FIPS:01',
                               token = "hvsYpwxfeUHEHFTbbRwslBNwXebjSLTh")
noaa_variables<-noaa_variables[[2]]
for(x in 2005:2016){
  for(i in 1:nrow(state_noaa)){
    hot_temperature<-ncdc(datasetid="GSOY",
                          datatypeid = "DX70",
                          locationid = state_noaa$id[i],
                          startdate = paste(x, "-01-01", sep = ""),
                          enddate = paste(x, "-12-31", sep = ""),
                          token = "hvsYpwxfeUHEHFTbbRwslBNwXebjSLTh")
    hot_temperature<-hot_temperature[[2]]
    if(i==1){
      temp_data<-data.frame(State_levels[i], mean(hot_temperature$value)) 
      names(temp_data)<-c("State", "value")
    }else{
      hot_temperature<-data.frame(State_levels[i], mean(hot_temperature$value)) 
      names(hot_temperature)<-c("State", "value")
      temp_data<-rbind(temp_data, hot_temperature)
    }
  }
  text_analysis<-paste(paste("analysis_data_", x, "$temperature",sep = ""),
                       "<-","temp_data$value", sep = "")
  eval(parse(text = text_analysis))
}

for(x in 2005:2016){
  for(i in 1:nrow(state_noaa)){
    hot_temperature<-ncdc(datasetid="GSOY",
                          datatypeid = "DP01",
                          locationid = state_noaa$id[i],
                          startdate = paste(x, "-01-01", sep = ""),
                          enddate = paste(x, "-12-31", sep = ""),
                          token = "hvsYpwxfeUHEHFTbbRwslBNwXebjSLTh")
    hot_temperature<-hot_temperature[[2]]
    if(i==1){
      temp_data<-data.frame(State_levels[i], mean(hot_temperature$value)) 
      names(temp_data)<-c("State", "value")
    }else{
      hot_temperature<-data.frame(State_levels[i], mean(hot_temperature$value)) 
      names(hot_temperature)<-c("State", "value")
      temp_data<-rbind(temp_data, hot_temperature)
    }
  }
  text_analysis<-paste(paste("analysis_data_", x, "$rain",sep = ""),
                       "<-","temp_data$value", sep = "")
  eval(parse(text = text_analysis))
}

for(x in 2005:2016){
  for(i in 1:nrow(state_noaa)){
    hot_temperature<-ncdc(datasetid="GSOY",
                          datatypeid = "DP10",
                          locationid = state_noaa$id[i],
                          startdate = paste(x, "-01-01", sep = ""),
                          enddate = paste(x, "-12-31", sep = ""),
                          token = "hvsYpwxfeUHEHFTbbRwslBNwXebjSLTh")
    hot_temperature<-hot_temperature[[2]]
    if(i==1){
      temp_data<-data.frame(State_levels[i], mean(hot_temperature$value)) 
      names(temp_data)<-c("State", "value")
    }else{
      hot_temperature<-data.frame(State_levels[i], mean(hot_temperature$value)) 
      names(hot_temperature)<-c("State", "value")
      temp_data<-rbind(temp_data, hot_temperature)
    }
  }
  text_analysis<-paste(paste("analysis_data_", x, "$rain_high",sep = ""),
                       "<-","temp_data$value", sep = "")
  eval(parse(text = text_analysis))
}

for(x in 2005:2016){
  for(i in 1:nrow(state_noaa)){
    hot_temperature<-ncdc(datasetid="GSOY",
                          datatypeid = "DSND",
                          locationid = state_noaa$id[i],
                          startdate = paste(x, "-01-01", sep = ""),
                          enddate = paste(x, "-12-31", sep = ""),
                          token = "hvsYpwxfeUHEHFTbbRwslBNwXebjSLTh")
    hot_temperature<-hot_temperature[[2]]
    if(i==1){
      temp_data<-data.frame(State_levels[i], mean(hot_temperature$value)) 
      names(temp_data)<-c("State", "value")
    }else{
      hot_temperature<-data.frame(State_levels[i], mean(hot_temperature$value)) 
      names(hot_temperature)<-c("State", "value")
      temp_data<-rbind(temp_data, hot_temperature)
    }
  }
  text_analysis<-paste(paste("analysis_data_", x, "$snow",sep = ""),
                       "<-","temp_data$value", sep = "")
  eval(parse(text = text_analysis))
}

for(x in 2005:2016){
  for(i in 1:nrow(state_noaa)){
    hot_temperature<-ncdc(datasetid="GSOY",
                          datatypeid = "DT00",
                          locationid = state_noaa$id[i],
                          startdate = paste(x, "-01-01", sep = ""),
                          enddate = paste(x, "-12-31", sep = ""),
                          token = "hvsYpwxfeUHEHFTbbRwslBNwXebjSLTh")
    hot_temperature<-hot_temperature[[2]]
    if(i==1){
      temp_data<-data.frame(State_levels[i], mean(hot_temperature$value)) 
      names(temp_data)<-c("State", "value")
    }else{
      hot_temperature<-data.frame(State_levels[i], mean(hot_temperature$value)) 
      names(hot_temperature)<-c("State", "value")
      temp_data<-rbind(temp_data, hot_temperature)
    }
  }
  text_analysis<-paste(paste("analysis_data_", x, "$cold",sep = ""),
                       "<-","temp_data$value", sep = "")
  eval(parse(text = text_analysis))
}
}


for(i in 2005:2016){
  if(i<2013){
    lgbtq_pop<-read_xls(paste("Data/LGBTQ/ssex-tables-", i,".xls", sep = ""), range = "A2:D53")
    text_analysis<-paste(paste("analysis_data_", i, "$lgbtq_pop",sep = ""),
                         "<-","lgbtq_pop$Number", sep = "")
    eval(parse(text = text_analysis))
  }else{
    lgbtq_pop<-read_xlsx(paste("Data/LGBTQ/ssex-tables-", i,".xlsx", sep = ""), range = "A2:D53")
    text_analysis<-paste(paste("analysis_data_", i, "$lgbtq_pop",sep = ""),
                         "<-","lgbtq_pop$Number", sep = "")
    eval(parse(text = text_analysis))
  }
}

for(i in 2005:2016){
  patent_data<-read_xlsx("Data/Patents/patents-per-1000-se-occupation-holders.xlsx")
  patent_data<-patent_data[c("State",paste(i))]
  names(patent_data)<-c("State", "value")
  text_analysis<-paste(paste("analysis_data_", i, "$patents",sep = ""),
                       "<-","patent_data$value", sep = "")
  eval(parse(text = text_analysis))
}

for (i in 2005:2016){
  if(i==2005){
    for(x in 15:25){
      if(x==15){
        age<-get_acs(geography = "State", variables = paste("B01001_0",x, sep = ""), survey = "acs1", year = i)
      }else{
        age<-rbind(age, get_acs(geography = "State", variables = paste("B01001_0",x, sep = ""), survey = "acs1", year = i))
      }
    } 
    age<-age%>%
      group_by(NAME)%>%
      dplyr::summarise(total=sum(estimate))%>%
      ungroup()
    age<-subset(age, NAME!="Puerto Rico")
    text_analysis<-paste(paste("analysis_data_", i, "$old_age",sep = ""),
                         "<-","age$total", sep = "")
    eval(parse(text = text_analysis))
  }else{
    for(x in 15:25){
      if(x==15){
        age<-get_acs(geography = "State", variables = paste("B01001_0",x, sep = ""), survey = "acs1", year = i)
      }else{
        age<-rbind(age, get_acs(geography = "State", variables = paste("B01001_0",x, sep = ""), survey = "acs1", year = i))
      }
    } 
    age<-age%>%
      group_by(NAME)%>%
      dplyr::summarise(total=sum(estimate))%>%
      ungroup()
    age<-subset(age, NAME!="Puerto Rico")
    text_analysis<-paste(paste("analysis_data_", i, "$old_age",sep = ""),
                         "<-","age$total", sep = "")
    eval(parse(text = text_analysis))
  }
}

for(i in 2005:2016){
  crime_data<-read_csv("Data/Crime/estimated_crimes_1979_2019.csv", skip_empty_rows=F)
  crime_data<-crime_data[-length(crime_data)]
  crime_data<-subset(crime_data, year==i)
  crime_data$state_name<-gsub(" ", "_", crime_data$state_name)
  crime_data<-crime_data[order(crime_data$state_name),]
  crime_data<-subset(crime_data, !is.na(state_name))
  text_analysis<-paste(paste("analysis_data_", i, "$violent_crime_rate",sep = ""),
                       "<-","crime_data$violent_crime/crime_data$population", sep = "")
  eval(parse(text = text_analysis))
}

for(i in 2005:2016){
  consumption<-read_xls("Data/Consumption/consumption.xls")
  consumption<-consumption[c("GeoName","Description", as.character(i))]
  consumption_dg<-subset(consumption, Description=="Durable goods")
  text_analysis<-paste(paste("analysis_data_", i, "$consumption_dg",sep = ""),
                       "<-","consumption_dg$`", i, "`", sep = "")
  eval(parse(text = text_analysis))
  consumption_nfp<-subset(consumption, Description=="Final consumption expenditures of nonprofit institutions serving households (NPISHs)")
  text_analysis<-paste(paste("analysis_data_", i, "$consumption_nfp",sep = ""),
                       "<-","consumption_nfp$`", i, "`", sep = "")
  eval(parse(text = text_analysis))
  consumption_fsi<-subset(consumption, Description=="Financial services and insurance")
  text_analysis<-paste(paste("analysis_data_", i, "$consumption_fsi",sep = ""),
                       "<-","consumption_fsi$`", i, "`", sep = "")
  eval(parse(text = text_analysis))
}

for(i in 2005:2016){
  Drug_Data<-read_sas("Data/Drug_Use/NSDUH_99_19_state_saes_final.sas7bdat")
  Drug_Data<-subset(Drug_Data, outcome=="MDE")
  for(x in 1:5){
    subse_values<--1:-5
    Drug_Data<-subset(Drug_Data, state!=subse_values[x])
  }
  if(i==2010){
    Drug_Data<-subset(Drug_Data, pyearnm=="2010-2011 (updated)")
    Drug_Data <- Drug_Data %>%  
      group_by(stname,outcome) %>%
      dplyr::summarise(total = sum(est_total)) %>% 
      group_by(stname)%>%
      dplyr::summarise(total = sum(total))%>%
      ungroup()
  }else{
    if(i==2014){
      Drug_Data<-data.frame(stname=State_levels, total=rep(NA, length(State_levels)))
    }else{
      Drug_Data<-subset(Drug_Data, pyearnm==paste(i, i+1, sep = "-"))
      Drug_Data <- Drug_Data %>%  
        group_by(stname,outcome) %>%
        dplyr::summarise(total = sum(est_total)) %>% 
        group_by(stname)%>%
        dplyr::summarise(total = sum(total))%>%
        ungroup()
    }
  }
  text_analysis<-paste(paste("analysis_data_", i, "$depression",sep = ""),
                       "<-","Drug_Data$total", sep = "")
  eval(parse(text = text_analysis)) 
}

for(i in 2005:2016){
  Drug_Data<-read_sas("Data/Drug_Use/NSDUH_99_19_state_saes_final.sas7bdat")
  Drug_Data<-subset(Drug_Data, outcome=="ABODALC")
  for(x in 1:5){
    subse_values<--1:-5
    Drug_Data<-subset(Drug_Data, state!=subse_values[x])
  }
  if(i==2010){
    Drug_Data<-subset(Drug_Data, pyearnm=="2010-2011 (updated)")
    Drug_Data <- Drug_Data %>%  
      group_by(stname,outcome) %>%
      dplyr::summarise(total = sum(est_total)) %>% 
      group_by(stname)%>%
      dplyr::summarise(total = sum(total))%>%
      ungroup()
  }else{
    Drug_Data<-subset(Drug_Data, pyearnm==paste(i, i+1, sep = "-"))
    Drug_Data <- Drug_Data %>%  
      group_by(stname,outcome) %>%
      dplyr::summarise(total = sum(est_total)) %>% 
      group_by(stname)%>%
      dplyr::summarise(total = sum(total))%>%
      ungroup()
  }
  text_analysis<-paste(paste("analysis_data_", i, "$alcohol_use",sep = ""),
                       "<-","Drug_Data$total", sep = "")
  eval(parse(text = text_analysis)) 
}

for(i in 2005:2016){
  art_employment<-read_xls("Data/Arts/art_employment.xls")
  art_employment<-art_employment[c("GeoName", as.character(i))]
  text_analysis<-paste(paste("analysis_data_", i, "$art_employment",sep = ""),
                       "<-","art_employment$`", i, "`", sep = "")
  eval(parse(text = text_analysis))
}
for(i in 2005:2016){
  crime_data<-read_csv("Data/Crime/estimated_crimes_1979_2019.csv", skip_empty_rows=F)
  crime_data<-crime_data[-length(crime_data)]
  crime_data<-subset(crime_data, year==i)
  crime_data$state_name<-gsub(" ", "_", crime_data$state_name)
  crime_data<-crime_data[order(crime_data$state_name),]
  crime_data<-subset(crime_data, !is.na(state_name))
  text_analysis<-paste(paste("analysis_data_", i, "$motor_vehicle_theft",sep = ""),
                       "<-","crime_data$motor_vehicle_theft/crime_data$population", sep = "")
  eval(parse(text = text_analysis))
}

for (i in 2005:2016){
  frg<-read.csv(paste("Data/Network_Data/splc-hate-groups-", i,".csv",sep=""))
  frg<- frg%>%
    group_by(State)%>%
    dplyr::summarise(titles=n())%>%
    ungroup()
  frg$State<-gsub(" ", "_", frg$State)
  if(length(which(!State_levels%in%frg$State))!=0){
    added_rows<-State_levels[which(!State_levels%in%frg$State)]
    added_rows<-data.frame(added_rows, rep(NA, length(added_rows)))
    names(added_rows)<-names(frg)
    frg<-rbind(frg, added_rows)
    frg<-frg[order(frg$State),]
  }
  text_analysis<-paste(paste("analysis_data_", i, "$titles",sep = ""),
                       "<-","frg$titles", sep = "")
  eval(parse(text = text_analysis))
}

if(F){
  frg<-read.csv(paste("Data/splc-hate-groups-", 2019,".csv",sep=""))
  
  State_levels_scrape<-levels(frg$State)
  
  for(i in 1:51){
    remDr <- remoteDriver(browserName = "chrome",port = 5556)
    Sys.sleep(.5)
    remDr$open(silent=T)
    Sys.sleep(.5)
    remDr$navigate("https://trac.syr.edu/phptools/immigration/detain/")
    Sys.sleep(.5)
    drop_down<-remDr$findElement("css", "#col2head2 #dimension_pick_col1")
    Sys.sleep(.5)
    drop_down$sendKeysToElement(list("Detainer Refused"))
    Sys.sleep(.5)
    year_button<-remDr$findElement("css", "div div:nth-child(1) tr~ tr+ tr input")
    Sys.sleep(.5)
    year_button$clickElement()
    Sys.sleep(.5)
    button_detain<-c("All", "No", "Not Known")
    state<-remDr$findElement(using = "link text", State_levels_scrape[i])
    Sys.sleep(.5)
    state$clickElement()
    Sys.sleep(.5)
    links <- remDr$findElements(using = "css", "[href]")
    Sys.sleep(.5)
    links<-unlist(sapply(links, function(y) {y$getElementAttribute("href")}))
    Sys.sleep(.5)
    links_all<- c("javascript:m.tableClick('refused', 'All');",                      
                  "javascript:m.tableClick('refused', '1');",                       
                  "javascript:m.tableClick('refused', '2');",                        
                  "javascript:m.tableClick('refused', '3');")
    links<-links[which(links%in%links_all)]
    for (x in 1:length(links)){
      remDr$executeScript(links_all[x])
      Sys.sleep(.5)
      title<-remDr$findElements(using = "class", "label")
      Sys.sleep(.5)
      title<-sapply(title, function(y){y$getElementText()})
      Sys.sleep(.5)
      title<-unlist(title[[1]])
      title<-gsub(State_levels_scrape[i], "", title)
      title<-gsub("-", "", title)
      title<-gsub(" ", "", title)
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
      highest_value<-highest_value/320
      values<-highest_value*values
      year <- remDr$findElements(using = "class", "tick")
      Sys.sleep(.5)
      years<-sapply(year, function(y){y$getElementText()})
      Sys.sleep(.5)
      years<-as.numeric(unlist(years))
      year_range<-2003:2020
      years<-years[which(years%in%year_range)]
      detainers<-data.frame(rep(State_levels_scrape[i], length(values)), years, values)
      names(detainers)<-c("State", "Year", title)
      if(length(year_range[which(!year_range%in%years)])!=0){
        year_range<-year_range[which(!year_range%in%years)]
        added_rows<-data.frame(rep(State_levels_scrape[i], length(year_range)), year_range,
                               rep(NA, length(year_range)))
        names(added_rows)<-c("State", "Year", title)
        detainers<-rbind(detainers,added_rows)
        detainers<-detainers[order(detainers$Year),]
      }
      assign(paste(State_levels_scrape[i], "det", title, sep = "_"), detainers)
    }
    remDr$close()
  }
  
  for(i in 1:51){
    if(i==1){
      detainer_data<-Alabama_det_All
      detainer_data$NotKnown<-Alabama_det_NotKnown$NotKnown
      detainer_data$No<-Alabama_det_No$No
      detainer_data$Yes<-Alabama_det_Yes$Yes
    }else{
      if(!exists(paste(State_levels_scrape[i],"det","Yes", sep = "_"))){
        detainer_rows_1<-get(paste(State_levels_scrape[i],"det","All", sep = "_"))
        detainer_rows_2<-get(paste(State_levels_scrape[i],"det","NotKnown", sep = "_"))
        detainer_rows_3<-get(paste(State_levels_scrape[i],"det","No", sep = "_"))
        detainer_rows_4<-data.frame("State"=rep(State_levels_scrape[i], 18),
                                    "Year"= 2003:2020, 
                                    "Yes"= rep(NA, 18))
        detainer_rows<-detainer_rows_1
        detainer_rows$NotKnown<-detainer_rows_2$NotKnown
        detainer_rows$No<-detainer_rows_3$No
        detainer_rows$Yes<-detainer_rows_4$Yes
        detainer_data<-rbind(detainer_data, detainer_rows)
      }else{
        detainer_rows_1<-get(paste(State_levels_scrape[i],"det","All", sep = "_"))
        detainer_rows_2<-get(paste(State_levels_scrape[i],"det","NotKnown", sep = "_"))
        detainer_rows_3<-get(paste(State_levels_scrape[i],"det","No", sep = "_"))
        detainer_rows_4<-get(paste(State_levels_scrape[i],"det","Yes", sep = "_"))
        detainer_rows<-detainer_rows_1
        detainer_rows$NotKnown<-detainer_rows_2$NotKnown
        detainer_rows$No<-detainer_rows_3$No
        detainer_rows$Yes<-detainer_rows_4$Yes
        detainer_data<-rbind(detainer_data, detainer_rows)
      }
    }
  }
  detainer_data$State<-as.character(factor(detainer_data$State))
  detainer_data$State<-gsub(" ","_",detainer_data$State)
  detainer_data<-detainer_data[order(detainer_data$Year),]
  write.csv(detainer_data, "Data/Detainer_data.csv")
}
detainer_data<-read_csv("Data/Detainer_data.csv")

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
  write.csv(secure_communities, "Data/secure_communities.csv")
}

secure_communities<-read_csv("Data/secure_communities.csv")

for(i in 2005:2016){
  detainer_subset<-subset(detainer_data, Year==i)
  detainer_subset<-detainer_subset[order(detainer_subset$State),]
  text_analysis<-paste(paste("analysis_data_", i, "$detainer_all",sep = ""),
                       "<-","detainer_subset$All", sep = "")
  eval(parse(text = text_analysis))
  text_analysis<-paste(paste("analysis_data_", i, "$detainer_notknown",sep = ""),
                       "<-","detainer_subset$NotKnown", sep = "")
  eval(parse(text = text_analysis))
  text_analysis<-paste(paste("analysis_data_", i, "$detainer_no",sep = ""),
                       "<-","detainer_subset$No", sep = "")
  eval(parse(text = text_analysis))
  text_analysis<-paste(paste("analysis_data_", i, "$detainer_yes",sep = ""),
                       "<-","detainer_subset$Yes", sep = "")
  eval(parse(text = text_analysis))
}

for(i in 2005:2008){
  party<-rep("Republican", 51)
  text_analysis<-paste(paste("analysis_data_", i, "$president_party",sep = ""),
                       "<-","party", sep = "")
  eval(parse(text = text_analysis))
}
for(i in 2009:2016){
  party<-rep("Democrat", 51)
  text_analysis<-paste(paste("analysis_data_", i, "$president_party",sep = ""),
                       "<-","party", sep = "")
  eval(parse(text = text_analysis))
}
for(i in 2017:2016){
  party<-rep("Republican", 51)
  text_analysis<-paste(paste("analysis_data_", i, "$president_party",sep = ""),
                       "<-","party", sep = "")
  eval(parse(text = text_analysis))
}

for (i in 2005:2016) {
  cars<-get_acs(geography = "State", variables = "B08006_002", survey = "acs1", year = i)
  cars<-subset(cars, NAME!="Puerto Rico")
  cars$NAME<-gsub(" ","_", cars$NAME)
  population<-get_acs(geography = "State", variables = "B01003_001", survey = "acs1", year = i)
  population<-subset(population, NAME!="Puerto Rico")
  population$NAME<-gsub(" ","_", population$NAME)
  cars$estimate<-cars$estimate/population$estimate
  text_analysis<-paste(paste("analysis_data_", i, "$worker_cars",sep = ""),
                       "<-", "cars$estimate", sep = "")
  eval(parse(text = text_analysis))
}


for (i in 2005:2016) {
  male_white<-get_acs(geography = "State", variables = "B01001A_017", survey = "acs1", year = i)
  male_white<-subset(male_white, NAME!="Puerto Rico")
  male_white$NAME<-gsub(" ","_", male_white$NAME)
  female_white<-get_acs(geography = "State", variables = "B01001A_017", survey = "acs1", year = i)
  female_white<-subset(female_white, NAME!="Puerto Rico")
  female_white$NAME<-gsub(" ","_", female_white$NAME)  
  white_pop<-male_white
  white_pop$estimate<-male_white$estimate+female_white$estimate
  population<-get_acs(geography = "State", variables = "B01003_001", survey = "acs1", year = i)
  population<-subset(population, NAME!="Puerto Rico")
  population$NAME<-gsub(" ","_", population$NAME)
  white_pop$estimate<-white_pop$estimate/population$estimate
  text_analysis<-paste(paste("analysis_data_", i, "$white_pop",sep = ""),
                       "<-", "white_pop$estimate", sep = "")
  eval(parse(text = text_analysis))
}

for( i in 5:18){
  if(nchar(i)==1){
    women_work<-read_xlsx(paste("Data/Unemployment/table14full0",i,".xlsx",sep=""))
    women_work<-women_work[-c(1:7),]
    women_work<-women_work[,-13]
    names(women_work)<-c("flip_code", "group_code", "State", "group", "population", "labourforce",
                         "percent_labourforce", "employed", "percent_employed", "unemployed", "unemployment_rate",
                         "low_ninetieth_per_error", "high_ninetieth_per_error")
    women_work<-subset(women_work, group_code=="03")
    women_work<-women_work[c("State","labourforce")]
    Year<-paste(200, i, sep = "")%>%
      as.numeric()%>%
      rep(nrow(women_work))
    women_work$Year<-Year
    women_work$labourforce<-as.numeric(women_work$labourforce)
    women_work$labourforce<-women_work$labourforce*1000
    assign(paste("women_work", paste(200, i, sep = ""), sep = ""), women_work)
    text_analysis<-paste(paste("analysis_data_",paste(200, i, sep = ""), "$women_employment",sep = ""),
                         "<-", paste("women_work", paste(200, i, sep = ""), "$labourforce",
                                     sep =""), sep = "")
    eval(parse(text = text_analysis))
  }else{
    if(i>=15 & i<17){
      women_work<-read_xlsx(paste("Data/Unemployment/table14afull",i,".xlsx",sep=""))
      women_work<-women_work[-c(1:7),]
      women_work<-women_work[,-13]
      names(women_work)<-c("flip_code", "group_code", "State", "group", "population", "labourforce",
                           "percent_labourforce", "employed", "percent_employed", "unemployed", "unemployment_rate",
                           "low_ninetieth_per_error", "high_ninetieth_per_error")
      women_work<-subset(women_work, group_code=="09")
      women_work<-women_work[c("State","labourforce")]
      Year<-paste(200, i, sep = "")%>%
        as.numeric()%>%
        rep(nrow(women_work))
      women_work$Year<-Year
      women_work$labourforce<-as.numeric(women_work$labourforce)
      women_work$labourforce<-women_work$labourforce*1000
      assign(paste("women_work", paste(20, i, sep = ""), sep = ""), women_work)
      text_analysis<-paste(paste("analysis_data_",paste(20, i, sep = ""), "$women_employment",sep = ""),
                           "<-", paste("women_work", paste(20, i, sep = ""), "$labourforce",
                                       sep =""), sep = "")
      eval(parse(text = text_analysis))
    } else{
      if(i>=17){
        women_work<-read_xlsx(paste("Data/Unemployment/table14afull",i,".xlsx",sep=""))
        women_work<-women_work[-c(1:6),]
        names(women_work)<-c("flip_code", "group_code", "State", "group", "population", "labourforce",
                             "percent_labourforce", "employed", "percent_employed", "unemployed", "unemployment_rate",
                             "margin")
        women_work<-subset(women_work, group_code=="09")
        women_work<-women_work[c("State","labourforce")]
        Year<-paste(200, i, sep = "")%>%
          as.numeric()%>%
          rep(nrow(women_work))
        women_work$Year<-Year
        women_work$labourforce<-as.numeric(women_work$labourforce)
        women_work$labourforce<-women_work$labourforce*1000
        assign(paste("women_work", paste(20, i, sep = ""), sep = ""), women_work)
        text_analysis<-paste(paste("analysis_data_",paste(20, i, sep = ""), "$women_employment",sep = ""),
                             "<-", paste("women_work", paste(20, i, sep = ""), "$labourforce",
                                         sep =""), sep = "")
        eval(parse(text = text_analysis))
      }else{
        women_work<-read_xlsx(paste("Data/Unemployment/table14full",i,".xlsx",sep=""))
        women_work<-women_work[-c(1:7),]
        women_work<-women_work[,-13]
        names(women_work)<-c("flip_code", "group_code", "State", "group", "population", "labourforce",
                             "percent_labourforce", "employed", "percent_employed", "unemployed", "unemployment_rate",
                             "low_ninetieth_per_error", "high_ninetieth_per_error")
        women_work<-subset(women_work, group_code=="03")
        women_work<-women_work[c("State","labourforce")]
        Year<-paste(200, i, sep = "")%>%
          as.numeric()%>%
          rep(nrow(women_work))
        women_work$Year<-Year
        women_work$labourforce<-as.numeric(women_work$labourforce)
        women_work$labourforce<-women_work$labourforce*1000
        assign(paste("women_work", paste(20, i, sep = ""), sep = ""), women_work)
        text_analysis<-paste(paste("analysis_data_",paste(20, i, sep = ""), "$women_employment",sep = ""),
                             "<-", paste("women_work", paste(20, i, sep = ""), "$labourforce",
                                         sep =""), sep = "")
        eval(parse(text = text_analysis))
      }
    }
  }
}



for(i in 2013:2016){
  state_control<-read_xlsx(paste("Data/Legislature/Legis_Control_", i, ".xlsx", sep = ""))
  names(state_control)<-c("State", "Leg_Control", "Gov_Party", "State_Control")
  state_control$State<-gsub(" ","_", state_control$State)
  State<-State_levels[which(!State_levels%in%state_control$State)]
  Leg_Control<-NA
  Gov_Party<-NA
  State_Control<-NA
  added_row<-data.frame(State, Leg_Control, Gov_Party, State_Control)
  state_control<-rbind(state_control, added_row)
  state_control[which(state_control$State_Control=="N/ A" | state_control$State_Control=="N/A"), 4]<-NA
  state_control<-state_control[order(state_control$State),]
  state_control$State_Control
  state_control$State_Control<-as.factor(state_control$State_Control)
  text_analysis<-paste(paste("analysis_data_",i, "$state_control",sep = ""),
                       "<-", paste("state_control$State_Control",
                                   sep =""), sep = "")
  eval(parse(text = text_analysis))
}

for(i in 2005:2012){
  if(i%in%c(2005, 2007, 2009, 2011)){
    state_control<-read_xlsx("Data/Legislature/Legis_Control_2002_2014.xlsx")
    state_control$State<-gsub(" ","_", state_control$State)
    split_values<-which(state_control[paste(i+1)]=="Split")
    for(x in 1:length(split_values)){
      state_control[split_values[x],paste(i+1)]<-"Divided"
    }
    names(state_control)<-c("State", "values_2002", "values_2004", "values_2006",
                            "values_2008", "values_2010", "values_2012")
    State<-State_levels[which(!State_levels%in%state_control$State)]
    added_row<-data.frame(State, NA)
    names(added_row)[2]<-paste("values_", i+1, sep = "")
    state_control<-state_control[c("State", paste("values_", i+1, sep = ""))]
    state_control<-rbind(state_control, added_row)
    state_control<-state_control[order(state_control$State),]
    text_analysis<-paste(paste("analysis_data_", i, "$state_control",sep = ""),
                         "<-", paste("state_control$", paste("values_", i+1, sep = ""),
                                     sep =""), sep = "")
    eval(parse(text = text_analysis))
  }else{
    state_control<-read_xlsx("Data/Legislature/Legis_Control_2002_2014.xlsx")
    state_control$State<-gsub(" ","_", state_control$State)
    split_values<-which(state_control[paste(i)]=="Split")
    for(x in 1:length(split_values)){
      state_control[split_values[x],paste(i)]<-"Divided"
    }
    names(state_control)<-c("State", "values_2002", "values_2004", "values_2006",
                            "values_2008", "values_2010", "values_2012")
    State<-State_levels[which(!State_levels%in%state_control$State)]
    added_row<-data.frame(State, NA)
    names(added_row)[2]<-paste("values_", i, sep = "")
    state_control<-state_control[c("State", paste("values_", i, sep = ""))]
    state_control<-rbind(state_control, added_row)
    state_control<-state_control[order(state_control$State),]
    text_analysis<-paste(paste("analysis_data_", i, "$state_control",sep = ""),
                         "<-", paste("state_control$", paste("values_", i, sep = ""),
                                     sep =""), sep = "")
    eval(parse(text = text_analysis))
  }
}

for (i in 2005:2016) {
  if(i==2005){
    gini_data<-get_acs(geography = "State", variables = "B19083_001", survey = "acs1", year = 2006)
    gini_data$estimate<-rep(NA, nrow(gini_data))
    gini_data<-subset(gini_data, NAME!="Puerto Rico")
    gini_data$NAME<-gsub(" ","_",gini_data$NAME)
  }else{
    gini_data<-get_acs(geography = "State", variables = "B19083_001", survey = "acs1", year = i)
    gini_data<-subset(gini_data, NAME!="Puerto Rico")
    gini_data$NAME<-gsub(" ","_", gini_data$NAME)
  }
  text_analysis<-paste(paste("analysis_data_", i, "$gini_index",sep = ""),
                       "<-", "gini_data$estimate", sep = "")
  eval(parse(text = text_analysis))
}

for (i in 2005:2016){
  if(i<2008){
    President_votes<- read_csv("Data/President_Data/1976-2020-president.csv")
    President_votes<-subset(President_votes, year==2004)
    President_votes$total_votes <- ave(President_votes$candidatevotes,
                                       President_votes$state,
                                       President_votes$year,
                                       FUN=sum)
    President_votes$party_votes <- ave(President_votes$candidatevotes,
                                       President_votes$state,
                                       President_votes$year,
                                       President_votes$party_detailed,
                                       FUN=sum)
    President_votes<-subset(President_votes, party_detailed=="DEMOCRAT")
    President_votes<-President_votes %>%
      group_by(state,state_po, year) %>%
      dplyr::summarize(total_votes=mean(total_votes), party_votes=mean(party_votes)) %>%
      ungroup()
    President_votes$vote_proportion<- President_votes$party_votes/President_votes$total_votes
    for (x in 1:length(President_votes$state_po)){
      President_votes$state_po[x]<-names_abb$State_levels[which(names_abb$state_abb%in%President_votes$state_po[x])]
    }
    President_votes<-President_votes[-c(1, 4, 5)]
    names(President_votes)<-c("Loc_Habitation_State1", "Year", "vote_proportion_president")
    if (nrow(President_votes)!=0 & nrow(President_votes)<51){
      Loc_Habitation_State1<-State_levels[which(!State_levels%in%President_votes$Loc_Habitation_State1)]
      Year<-i
      vote_proportion_president<-NA
      added_row<-data.frame(Loc_Habitation_State1,
                            Year,
                            vote_proportion_president)
      added_row$Loc_Habitation_State1<-as.character(factor(added_row$Loc_Habitation_State1))
      President_votes<-rbind(President_votes, added_row) 
      President_votes[order(President_votes$Loc_Habitation_State1),]
    }
  }else{
    if(i<2012){
      President_votes<- read_csv("Data/President_Data/1976-2020-president.csv")
      President_votes<-subset(President_votes, year==2008)
      President_votes$total_votes <- ave(President_votes$candidatevotes,
                                         President_votes$state,
                                         President_votes$year,
                                         FUN=sum)
      President_votes$party_votes <- ave(President_votes$candidatevotes,
                                         President_votes$state,
                                         President_votes$year,
                                         President_votes$party_detailed,
                                         FUN=sum)
      President_votes<-subset(President_votes, party_detailed=="DEMOCRAT")
      President_votes<-President_votes %>%
        group_by(state,state_po, year) %>%
        dplyr::summarize(total_votes=mean(total_votes), party_votes=mean(party_votes)) %>%
        ungroup()
      President_votes$vote_proportion<- President_votes$party_votes/President_votes$total_votes
      for (x in 1:length(President_votes$state_po)){
        President_votes$state_po[x]<-names_abb$State_levels[which(names_abb$state_abb%in%President_votes$state_po[x])]
      }
      President_votes<-President_votes[-c(1, 4, 5)]
      names(President_votes)<-c("Loc_Habitation_State1", "Year", "vote_proportion_president")
      if (nrow(President_votes)!=0 & nrow(President_votes)<51){
        Loc_Habitation_State1<-State_levels[which(!State_levels%in%President_votes$Loc_Habitation_State1)]
        Year<-i
        vote_proportion_president<-NA
        added_row<-data.frame(Loc_Habitation_State1,
                              Year,
                              vote_proportion_president)
        added_row$Loc_Habitation_State1<-as.character(factor(added_row$Loc_Habitation_State1))
        President_votes<-rbind(President_votes, added_row) 
        President_votes[order(President_votes$Loc_Habitation_State1),]
      }
    }else{
      if(i<2016){
        President_votes<- read_csv("Data/President_Data/1976-2020-president.csv")
        President_votes<-subset(President_votes, year==2012)
        President_votes$total_votes <- ave(President_votes$candidatevotes,
                                           President_votes$state,
                                           President_votes$year,
                                           FUN=sum)
        President_votes$party_votes <- ave(President_votes$candidatevotes,
                                           President_votes$state,
                                           President_votes$year,
                                           President_votes$party_detailed,
                                           FUN=sum)
        President_votes<-subset(President_votes, party_detailed=="DEMOCRAT")
        President_votes<-President_votes %>%
          group_by(state,state_po, year) %>%
          dplyr::summarize(total_votes=mean(total_votes), party_votes=mean(party_votes)) %>%
          ungroup()
        President_votes$vote_proportion<- President_votes$party_votes/President_votes$total_votes
        for (x in 1:length(President_votes$state_po)){
          President_votes$state_po[x]<-names_abb$State_levels[which(names_abb$state_abb%in%President_votes$state_po[x])]
        }
        President_votes<-President_votes[-c(1, 4, 5)]
        names(President_votes)<-c("Loc_Habitation_State1", "Year", "vote_proportion_president")
        if (nrow(President_votes)!=0 & nrow(President_votes)<51){
          Loc_Habitation_State1<-State_levels[which(!State_levels%in%President_votes$Loc_Habitation_State1)]
          Year<-i
          vote_proportion_president<-NA
          added_row<-data.frame(Loc_Habitation_State1,
                                Year,
                                vote_proportion_president)
          added_row$Loc_Habitation_State1<-as.character(factor(added_row$Loc_Habitation_State1))
          President_votes<-rbind(President_votes, added_row) 
          President_votes[order(President_votes$Loc_Habitation_State1),]
        }
      }else{
        if(i<2020){
          President_votes<- read_csv("Data/President_Data/1976-2020-president.csv")
          President_votes<-subset(President_votes, year==2016)
          President_votes$total_votes <- ave(President_votes$candidatevotes,
                                             President_votes$state,
                                             President_votes$year,
                                             FUN=sum)
          President_votes$party_votes <- ave(President_votes$candidatevotes,
                                             President_votes$state,
                                             President_votes$year,
                                             President_votes$party_detailed,
                                             FUN=sum)
          President_votes<-subset(President_votes, party_detailed=="DEMOCRAT")
          President_votes<-President_votes %>%
            group_by(state,state_po, year) %>%
            dplyr::summarize(total_votes=mean(total_votes), party_votes=mean(party_votes)) %>%
            ungroup()
          President_votes$vote_proportion<- President_votes$party_votes/President_votes$total_votes
          for (x in 1:length(President_votes$state_po)){
            President_votes$state_po[x]<-names_abb$State_levels[which(names_abb$state_abb%in%President_votes$state_po[x])]
          }
          President_votes<-President_votes[-c(1, 4, 5)]
          names(President_votes)<-c("Loc_Habitation_State1", "Year", "vote_proportion_president")
          if (nrow(President_votes)!=0 & nrow(President_votes)<51){
            Loc_Habitation_State1<-State_levels[which(!State_levels%in%President_votes$Loc_Habitation_State1)]
            Year<-i
            vote_proportion_president<-NA
            added_row<-data.frame(Loc_Habitation_State1,
                                  Year,
                                  vote_proportion_president)
            added_row$Loc_Habitation_State1<-as.character(factor(added_row$Loc_Habitation_State1))
            President_votes<-rbind(President_votes, added_row) 
            President_votes[order(President_votes$Loc_Habitation_State1),]
          }
        }
      }
    }
  }
  for(x in 1:nrow(President_votes)){
    if(is.na(President_votes$vote_proportion_president[x])){
      next
    }else{
      if(President_votes$vote_proportion_president[x]>0.5){
        President_votes$vote_proportion_president[x]<-"Democrat"
      }else{
        President_votes$vote_proportion_president[x]<-"Republican"
      }
    }
  }
  President_votes<-President_votes[order(President_votes$Loc_Habitation_State1),]
  assign(paste("President_votes", i, sep ="_"), President_votes)
  text_analysis<-paste(paste("analysis_data_", i, "$president_choice",sep = ""), "<-", paste("President_votes_", i, "$vote_proportion_president", sep =""), sep = "")
  eval(parse(text = text_analysis))
}

for (i in 2005:2016){
  House_votes<- read_csv("Data/House_Data/1976-2020-house.csv")
  House_votes<-subset(House_votes, year>i-1 & year<i+1)
  House_votes$total_votes <- ave(House_votes$candidatevotes,
                                 House_votes$state,
                                 House_votes$year,
                                 FUN=sum)
  House_votes$party_votes <- ave(House_votes$candidatevotes,
                                 House_votes$state,
                                 House_votes$year,
                                 House_votes$party,
                                 FUN=sum)
  House_votes<-subset(House_votes, party=="DEMOCRAT")
  House_votes<-House_votes %>%
    group_by(state,state_po, year) %>%
    dplyr::summarize(total_votes=mean(total_votes), party_votes=mean(party_votes)) %>%
    ungroup()
  House_votes$vote_proportion<- House_votes$party_votes/House_votes$total_votes
  for (x in 1:length(House_votes$state_po)){
    House_votes$state_po[x]<-names_abb$State_levels[which(names_abb$state_abb%in%House_votes$state_po[x])]
  }
  House_votes<-House_votes[-c(1, 4, 5)]
  names(House_votes)<-c("Loc_Habitation_State1", "Year", "vote_proportion_house")
  if(nrow(House_votes)==0){
    Loc_Habitation_State1<-State_levels
    Year<-rep(i, length(State_levels))
    vote_proportion_house<-rep(NA, length(State_levels))
    House_votes<-data.frame(Loc_Habitation_State1,
                            Year,
                            vote_proportion_house= rep(NA, length(State_levels)))
    House_votes$Loc_Habitation_State1<-as.character(factor(House_votes$Loc_Habitation_State1))
  }
  if (nrow(House_votes)!=0 & nrow(House_votes)<51){
    Loc_Habitation_State1<-State_levels[which(!State_levels%in%House_votes$Loc_Habitation_State1)]
    Year<-i
    vote_proportion_house<-NA
    added_row<-data.frame(Loc_Habitation_State1,
                          Year,
                          vote_proportion_house)
    added_row$Loc_Habitation_State1<-as.character(factor(added_row$Loc_Habitation_State1))
    House_votes<-rbind(House_votes, added_row) 
    House_votes[order(House_votes$Loc_Habitation_State1),]
  }
  assign(paste("House_votes", i, sep ="_"), House_votes)
  text_analysis<-paste(paste("analysis_data_", i, "$House_votes",sep = ""), "<-", paste("House_votes_", i, "$vote_proportion_house", sep =""), sep = "")
  eval(parse(text = text_analysis))
}

for (i in 2005:2016){
  total_migration<-get_acs(geography = "state", variables = "B05002_013", year = i, survey = "acs1")
  total_migration$NAME<- gsub(" ","_", total_migration$NAME)
  total_migration<-subset(total_migration, NAME!="Puerto_Rico")
  total_migration$Year<-rep(i, length(total_migration$NAME))
  text_analysis<-paste(paste("analysis_data_", i, "$total_migration",sep = ""), "<- unlist(total_migration['estimate'])", sep = "")
  eval(parse(text = text_analysis))
  
  total_citizen<-get_acs(geography = "state", variables = "B05002_014", year = i, survey = "acs1")
  total_citizen$NAME<- gsub(" ","_", total_citizen$NAME)
  total_citizen<-subset(total_citizen, NAME!="Puerto_Rico")
  total_citizen$Year<-rep(i, length(total_citizen$NAME))
  text_analysis<-paste(paste("analysis_data_", i, "$total_citizen",sep = ""), "<- unlist(total_citizen['estimate'])", sep = "")
  eval(parse(text = text_analysis))
  
  total_noncitizen<-get_acs(geography = "state", variables = "B05002_015", year = i, survey = "acs1")
  total_noncitizen$NAME<- gsub(" ","_", total_noncitizen$NAME)
  total_noncitizen<-subset(total_noncitizen, NAME!="Puerto_Rico")
  total_noncitizen$Year<-rep(i, length(total_noncitizen$NAME))
  text_analysis<-paste(paste("analysis_data_", i, "$total_noncitizen",sep = ""), "<- unlist(total_noncitizen['estimate'])", sep = "")
  eval(parse(text = text_analysis))
}

President_votes<- read_csv("Data/President_Data/1976-2020-president.csv")
state_abb<-unique(President_votes$state_po)
names_abb<-data_frame(State_levels, state_abb)
write_csv(names_abb, "Data/State_abbs")

for (i in 2005:2016){
  President_votes<- read_csv("Data/President_Data/1976-2020-president.csv")
  President_votes<-subset(President_votes, year>i-1 & year<i+1)
  President_votes$total_votes <- ave(President_votes$candidatevotes,
                                     President_votes$state,
                                     President_votes$year,
                                     FUN=sum)
  President_votes$party_votes <- ave(President_votes$candidatevotes,
                                     President_votes$state,
                                     President_votes$year,
                                     President_votes$party_detailed,
                                     FUN=sum)
  President_votes<-subset(President_votes, party_detailed=="DEMOCRAT")
  President_votes<-President_votes %>%
    group_by(state,state_po, year) %>%
    dplyr::summarize(total_votes=mean(total_votes), party_votes=mean(party_votes)) %>%
    ungroup()
  President_votes$vote_proportion<- President_votes$party_votes/President_votes$total_votes
  for (x in 1:length(President_votes$state_po)){
    President_votes$state_po[x]<-names_abb$State_levels[which(names_abb$state_abb%in%President_votes$state_po[x])]
  }
  President_votes<-President_votes[-c(1, 4, 5)]
  names(President_votes)<-c("Loc_Habitation_State1", "Year", "vote_proportion_president")
  if(nrow(President_votes)==0){
    Loc_Habitation_State1<-State_levels
    Year<-rep(i, length(State_levels))
    vote_proportion_president<-rep(NA, length(State_levels))
    President_votes<-data.frame(Loc_Habitation_State1,
                                Year,
                                vote_proportion_president= rep(NA, length(State_levels)))
    President_votes$Loc_Habitation_State1<-as.character(factor(President_votes$Loc_Habitation_State1))
  }
  if (nrow(President_votes)!=0 & nrow(President_votes)<51){
    Loc_Habitation_State1<-State_levels[which(!State_levels%in%President_votes$Loc_Habitation_State1)]
    Year<-i
    vote_proportion_president<-NA
    added_row<-data.frame(Loc_Habitation_State1,
                          Year,
                          vote_proportion_president)
    added_row$Loc_Habitation_State1<-as.character(factor(added_row$Loc_Habitation_State1))
    President_votes<-rbind(President_votes, added_row) 
    President_votes[order(President_votes$Loc_Habitation_State1),]
  }
  assign(paste("President_votes", i, sep ="_"), President_votes)
  text_analysis<-paste(paste("analysis_data_", i, "$President_votes",sep = ""), "<-", paste("President_votes_", i, "$vote_proportion_president", sep =""), sep = "")
  eval(parse(text = text_analysis))
}

year<-2005:2016
for (i in 1:14){
  manufacturing<-read_xls("Data/Manufac/manufac_data.xls", skip = 4)
  manufacturing<-manufacturing[c(2, 10:23)]
  manufacturing<-manufacturing[c(1,i+1)]
  names(manufacturing)<-c("State", "value")
  manufacturing$State<-manufacturing$State<-gsub(" ","_",manufacturing$State)
  subsetter<-manufacturing$State[which(!manufacturing$State%in%State_levels)]
  manufacturing<-subset(manufacturing,State!="New_England")
  manufacturing$value<-as.numeric(manufacturing$value)
  text_analysis<-paste(paste("analysis_data_", year[i], "$manufacturing",sep = ""),
                       "<-","manufacturing$value", sep = "")
  eval(parse(text = text_analysis)) 
}


for(i in 2005:2016){
  Drug_Data<-read_sas("Data/Drug_Use/NSDUH_99_19_state_saes_final.sas7bdat")
  Drug_Data<-subset(Drug_Data, outcome=="UDPYILAL"|outcome=="ABODILAL" )
  for(x in 1:5){
    subse_values<--1:-5
    Drug_Data<-subset(Drug_Data, state!=subse_values[x])
  }
  if(i==2010){
    Drug_Data<-subset(Drug_Data, pyearnm=="2010-2011 (updated)")
    Drug_Data <- Drug_Data %>%  
      group_by(stname,outcome) %>%
      dplyr::summarise(total = sum(est_total)) %>% 
      group_by(stname)%>%
      dplyr::summarise(total = sum(total))%>%
      ungroup()
  }else{
    if(i==2014){
      Drug_Data<-data.frame(stname=State_levels, total=rep(NA, length(State_levels)))
    }else{
      Drug_Data<-subset(Drug_Data, pyearnm==paste(i, i+1, sep = "-"))
      Drug_Data <- Drug_Data %>%  
        group_by(stname,outcome) %>%
        dplyr::summarise(total = sum(est_total)) %>% 
        group_by(stname)%>%
        dplyr::summarise(total = sum(total))%>%
        ungroup()
    }
  }
  text_analysis<-paste(paste("analysis_data_", i, "$drug_use",sep = ""),
                       "<-","Drug_Data$total", sep = "")
  eval(parse(text = text_analysis)) 
}


for(i in 2005:2016){
  if(i<2009){
    sc_subset<-data.frame(State_levels, All=rep(NA, length(State_levels)))
  }else{
    sc_subset<-subset(secure_communities, Year==i)
    sc_subset<-sc_subset[order(sc_subset$State),]
  }
  text_analysis<-paste(paste("analysis_data_", i, "$sc_removals",sep = ""),
                       "<-","sc_subset$All", sep = "")
  eval(parse(text = text_analysis))
}

for (i in 2005:2016){
  if(i==2005){
    new_data<-analysis_data_2005
  }else{
    new_data<-rbind(new_data, get(paste("analysis_data_", i, sep = "")))
  }
}

for(i in 2005:2016){
  lib_index<-read_csv("Data/Liberalism_index/summary_17.csv",  skip = 1)
    if(i==2005){
      lib_cl<-subset(lib_index, Year==i)
      lib_cl$State<-gsub(" ","_", lib_cl$State)
      state<-State_levels[which(!State_levels%in%lib_cl$State)]
      state_add<-data.frame(matrix(ncol = length(lib_cl), nrow = 1))
      names(state_add)<-names(lib_cl)
      state_add$State<-state
      lib_cl<-rbind(lib_cl, state_add)
      lib_cl<-lib_cl[order(lib_cl$State),]
    }else{
      added_row<-subset(lib_index, Year==i)
      added_row$State<-gsub(" ","_", added_row$State)
      state<-State_levels[which(!State_levels%in%added_row$State)]
      state_add<-data.frame(matrix(ncol = length(added_row), nrow = 1))
      names(state_add)<-names(added_row)
      state_add$State<-state
      added_row<-rbind(added_row, state_add)
      added_row<-added_row[order(added_row$State),]
      lib_cl<-rbind(lib_cl, added_row)
    }
}
lib_cl<-lib_cl[order(lib_cl$Year),]
new_data<-new_data[order(new_data$Year),]
new_data$liberal<-lib_cl$liberal
new_data$civlib<-lib_cl$civlib

welfare_data<-read_xlsx("Data/Welfare_data/UKCPR_National_Welfare_Data_Update_042821.xlsx", sheet = 2)

for(i in 1:nrow(welfare_data)){
  welfare_data$state_name[i]<-names_abb$State_levels[which(names_abb$state_abb%in%welfare_data$state_name[i])]
}
welfare_data<-subset(welfare_data, year>2004& year<2017)
welfare_data<-welfare_data[,-c(1:4)]

ideology_data<-read_dta("Data/Political_variables/ideology_data.dta")
for(i in 1:nrow(ideology_data)){
  ideology_data$st[i]<-names_abb$State_levels[which(names_abb$state_abb%in%ideology_data$st[i])]
}
ideology_data<-subset(ideology_data, year>2004 & year<2017)
added_rows<-data.frame(matrix(nrow = length(2005:2016), ncol = length(ideology_data)))
names(added_rows)<-names(ideology_data)
added_rows$year<-2005:2016
added_rows$st<-State_levels[which(!State_levels%in%ideology_data$st)]
ideology_data<-rbind(ideology_data, added_rows)
ideology_data<-ideology_data[order(ideology_data$year),]
ideology_data[-c(1:4)]

tax_rate<-read_xlsx("Data/Tax_Rate/Historical-Federal-Individual-Income-Tax-Rates-and-Brackets.xlsx", range = "A1:E192")
tax_rate$av_tax_type<-rowSums(tax_rate[,2:5])/4
tax_rate <- tax_rate %>%  
  group_by(Year) %>%
  dplyr::summarise(av_tax = mean(av_tax_type)) %>% 
  ungroup()



for(i in 2005:2016){
  tax_rate_year<-data.frame(Year = rep(i, 51),
                            av_tax_rate = rep(tax_rate$av_tax[which(tax_rate$Year==i)], 51))
  if(i==2005){
    av_tr<-tax_rate_year
  }else{
    av_tr<-rbind(av_tr, tax_rate_year)
  }
}



various_vars<-read_csv("Data/Varied_sd/cspp_june_2021.csv")
#various_vars<-various_vars[c( "year", "st", "general_expenditure", "general_revenue",
                             #"taxes","total_debt_outstanding", "total_expenditure",
                             #"total_revenue", "aprivemp", "boci")]
various_vars<-subset(various_vars, year>1999 & year<2017)                         
NA_list<-sapply(various_vars, function(x){sum(is.na(x))})
subset<-names(which(!NA_list>51))
various_vars<-various_vars[subset]
for(i in 2005:2016){
  extract<-subset(various_vars, year==i)
  if(TRUE%in%duplicated(extract$state)){
    extract<-extract[-which(duplicated(extract$state)),]
  }
  if(i==2005){
    var_extract<-extract
  }else{
    var_extract<-rbind(var_extract, extract)
  }
}
var_extract<-var_extract[order(var_extract$year),]
codebook<-read_csv("Data/Varied_sd/codebook.csv")
var_names_extract<-codebook[which(codebook$variable%in%names(var_extract)),]
subset<-var_names_extract$variable[which(!var_names_extract$sources=="Sorens, Jason, Fait Muedini, and William P. Ruger. 'State and Local Public Policies in 2006: A New Database.' State Politics & Policy Quarterly 8.3 (2008): 309-26." &
                                           !var_names_extract$sources=="University of Kentucky Center for Poverty Research. 2019. 'UKCPR National Welfare Data, 1980-2017.' Lexington, KY. http://ukcpr.org/resources/national-welfare-data")]
var_extract<-var_extract[subset]
var_extract<-var_extract[-c(1:4)]
NA_list<-sapply(var_extract, function(x){sum(is.na(x))})
subset<-names(which(!NA_list>60))
var_extract<-var_extract[subset]

read_sheet<-c("Mandatory", "Matching", "Discretionary", "Moe")
territory<-c("American Samoa", "Guam", "Northern Mariana", "Northern Mariana Islands", "Puerto Rico", "Virgin Islands")
for (x in 2005:2011){
  for(i in 1:4){
    if(x<2009){
      tanf_data<-read_xls(paste("Data/TANF/funding_", x ,".xls",sep = ""), sheet = i)
    }else{
      tanf_data<-read_xlsx(paste("Data/TANF/funding_", x ,".xlsx",sep = ""), sheet = i)
    }
    tanf_data<-tanf_data[which(!tanf_data$State%in%territory),]
    tanf_data<-tanf_data[order(tanf_data$State),]
    if(i==1){
      count<-1
      year_data<-as.numeric(tanf_data$`Total Expenditures`)
    }else{
      count<-count+1
      year_data<-as_tibble(cbind(year_data, as.numeric(tanf_data$`Total Expenditures`)))
      names(year_data)[count]<-paste(read_sheet[i])
    }
  }
  if(x==2005){
    year_data$total<-rowSums(year_data)
    tanf_2<-as_tibble(cbind(State=State_levels, Year=rep(x, length(State_levels)),total=year_data$total))
  }else{
    year_data$total<-rowSums(year_data)
    row_add<-as_tibble(cbind(State=State_levels, Year=rep(x, length(State_levels)),total=year_data$total))
    tanf_2<-rbind(tanf_2, row_add)
  }
}

territory<-c("American Samoa", "Guam", "Northern Mariana", "Northern Mariana Islands", "Puerto Rico", "Virgin Islands")
for (x in 2012:2016){
    tanf_data<-read_xlsx(paste("Data/TANF/funding_", x ,".xlsx",sep = ""), skip = 4, range = "A5:G61")
    tanf_data<-tanf_data[which(!tanf_data$State%in%territory),]
    tanf_data<-tanf_data[order(tanf_data$State),]
    if(x==2012){
      tanf_3<-as_tibble(cbind(State=State_levels,
                                 Year=rep(x, length(State_levels)),
                                 total=as.numeric(tanf_data$Total)))
    }else{
      add_row<-as_tibble(cbind(State=State_levels,
                                 Year=rep(x, length(State_levels)),
                                 total=as.numeric(tanf_data$Total)))
      tanf_3<-rbind(tanf_3, add_row)
    }
}

tanf_exp<-rbind(tanf_2, tanf_3)
tanf_exp<-tanf_exp[order(tanf_exp$Year),]

territory_regions<-c("American_Samoa", "Guam", "Northern_Mariana", "Northern_Mariana Islands",
                     "Puerto_Rico", "Virgin_Islands", "MARO", "NERO", "SERO", "MWRO", "SWRO", "MPRO", "WRO",
                     "Outlying_Areas", "Commonwealth_of_Northern_Mariana_Islands")
states_territories<-c(State_levels, territory_regions)


for(x in 5:14){
  for (i in 1:7){
    if(x<10){
      snap<-read_xls(paste("Data/Food_programs/SNAP/FY0", x,".xls", sep = ""), sheet = i, skip = 3)
    }else{
      snap<-read_xls(paste("Data/Food_programs/SNAP/FY", x,".xls", sep = ""), sheet = i, skip = 3)
    }
    names(snap)[1]<-"totals"
    snap$totals<-gsub(" ", "_",snap$totals )
    states<-snap$totals[which(snap$totals%in%states_territories| snap$totals=="No_data_found.")]
    obs_delete<-which(states=="No_data_found.")
    if(length(obs_delete)>0){
      obs_delete_2<-obs_delete-1
      obs_delete<-c(obs_delete, obs_delete_2)
      states<-states[-c(obs_delete)]
    }
    totals<-snap$Cost[which(snap$totals=="Total")]
    if(i==1){
      data_snap<-data.frame(states, totals)
    }else{
      add_row<-data.frame(states, totals)
      data_snap<-rbind(data_snap, add_row)
    }
  }
  data_snap<-data_snap[which(data_snap$states%in%State_levels),]
  data_snap$states<-as.character(factor(data_snap$states))
  data_snap<-data_snap[order(data_snap$states),]
  data_snap<-as_tibble(data_snap)
  if(x<10){
    data_snap$year<-paste("200", x, sep = "")
    data_snap$year<-as.numeric(data_snap$year)
  }else{
    data_snap$year<-paste("20", x, sep = "")
    data_snap$year<-as.numeric(data_snap$year)
  }
  if(x==5){
    fs_data<-data_snap
  }else{
    fs_data<-rbind(fs_data, data_snap)
  }
}

for(x in 15:16){
  for (i in 1:7){
    snap<-read_xls(paste("Data/Food_programs/SNAP/FY", x,".xls", sep = ""), sheet = i, skip = 5)
    names(snap)[1]<-"totals"
    snap$totals<-gsub(" ", "_",snap$totals )
    states<-snap$totals[which(snap$totals%in%states_territories| snap$totals=="No_data_found.")]
    obs_delete<-which(states=="No_data_found.")
    if(length(obs_delete)>0){
      obs_delete_2<-obs_delete-1
      obs_delete<-c(obs_delete, obs_delete_2)
      states<-states[-c(obs_delete)]
    }
    totals<-snap$Cost[which(snap$totals=="Total")]
    if(i==1){
      data_snap<-data.frame(states, totals)
    }else{
      add_row<-data.frame(states, totals)
      data_snap<-rbind(data_snap, add_row)
    }
  }
  data_snap<-data_snap[which(data_snap$states%in%State_levels),]
  data_snap$states<-as.character(factor(data_snap$states))
  data_snap<-data_snap[order(data_snap$states),]
  data_snap<-as_tibble(data_snap)
  data_snap$year<-paste("20", x, sep = "")
  data_snap$year<-as.numeric(data_snap$year)
  
  if(x==15){
    fs_data_2<-data_snap
  }else{
    fs_data_2<-rbind(fs_data_2, data_snap)
  }
}

fs_data<-rbind(fs_data, fs_data_2)
fs_data<-fs_data[order(fs_data$year),]

med_exp<-read_xlsx("Data/Medicaid/NHE2019.xlsx", skip = 1)
for(i in 2005:2016){
  exp<-unlist(med_exp[8,paste(i)]*1000000)
  names(exp)<-""
  exp_2<-unlist(med_exp[7,paste(i)]*1000000)
  names(exp_2)<-""
  if(i==2005){
    med_exp_data<-data.frame(state= State_levels, state_exp=exp, fed_exp=exp_2, year=i)
  }else{
    add_row<-data.frame(state= State_levels, state_exp=exp, fed_exp=exp_2, year=i)
    med_exp_data<-rbind(med_exp_data, add_row)
  }
}
med_exp_agg<-as_tibble(med_exp_data)
med_exp_agg<-med_exp_agg[order(med_exp_agg$year),]

school_fp<-read_xlsx("Data/Food_programs/cncost-9.xlsx", skip = 5)
school_fp<-school_fp[-1,]
for(i in 2005:2016){
  school_fp_ss<-subset(school_fp, `Year `==i, select = Cost)
  if(i==2005){
    school_fp_data<-data.frame(state= State_levels, fed_exp=school_fp_ss, year=i)
  }else{
    add_row<-data.frame(state= State_levels, fed_exp=school_fp_ss, year=i)
    school_fp_data<-rbind(school_fp_data, add_row)
  }
}
school_fp_data<-as_tibble(school_fp_data)
school_fp_data$Cost<-school_fp_data$Cost*1000000

territory_regions<-c("Amer._Samoa", "Guam", "N._Mariana_Islands", "Virgin_Islands",
                     "Puerto_Rico", "Dist._Of_Col.", "National_Totals", "Mass._Blind")
states_territories<-c(State_levels, territory_regions)
year_val<-2005:2011
for(i in 1:7){
  med_exp<-read_xlsx("Data/Medicaid/State_exp/NetExpenditure02through11.xlsx", sheet = i)
  names(med_exp)<-unlist(unique(med_exp[7,]))
  if(i>=5){
    names(med_exp)[7]<-"Service Category 2"
    names(med_exp)[10]<-"State Share 2"
  }else{
    names(med_exp)[5]<-"Service Category 2"
    names(med_exp)[8]<-"State Share 2"
  }
  med_exp$`Service Category`<-gsub(" ","_", med_exp$`Service Category`)
  med_exp$`Service Category`<-gsub("-","", med_exp$`Service Category`)
  med_exp$`Service Category 2`<-gsub(" ","_", med_exp$`Service Category 2`)
  med_exp$`Service Category 2`<-gsub("-","", med_exp$`Service Category 2`)
  if(i==7){
    vars<-med_exp$`Service Category`[which(med_exp$`Service Category`%in%states_territories)]
    vars<-vars[-which(vars=="Mass._Blind")]
  }else{
    vars<-med_exp$`Service Category`[which(med_exp$`Service Category`%in%states_territories)]
  }
  totals<-med_exp$`State Share`[which(med_exp$`Service Category`=="Total_Net_Expenditures")]
  totals2<-med_exp$`State Share 2`[which(med_exp$`Service Category 2`=="Total_Net_Expenditures")]
  if(i==1){
    health_exp_2<-data.frame(year=year_val[i], states=vars, exp=totals, admin=totals2)
  }else{
    add_row<-data.frame(year=year_val[i], states=vars, exp=totals, admin=totals2)
    health_exp_2<-rbind(health_exp_2, add_row)
  }
}


health_exp_2<-as_tibble(health_exp_2)

territory_regions<-c("Amer._Samoa", "Guam", "N._Mariana_Islands", "Virgin_Islands",
                     "Puerto_Rico", "National_Totals")
states_territories<-c(State_levels, territory_regions)
states_territories[order(states_territories)]
names_vars<-c("Service_Category", "Total_Comp", "Federal_Share", "Federal_Share_Med",
              "Federal_Share_Arra", "Federal_Share_BIPP", "State_Share")

for(x in 2012:2016){
  for(i in 1:57){
    med_exp<-read_xlsx(paste("Data/Medicaid/State_exp/Loopfiles/FY",x,"/FMR_ FY",x,".xlsx", sep = ""), sheet = i, skip=6)
    names(med_exp)<-names_vars
    if(x==2012){
      names(med_exp)[8]<-"Service_Category_2"
      names(med_exp)[11]<-"State_Share_2"
      exp_med<-med_exp$State_Share[which(med_exp$Service_Category=="Total Net Expenditures")]
      exp_med_2<-med_exp$State_Share_2[which(med_exp$Service_Category_2=="Total Net Expenditures")]
    }else{
      med_exp_admin<-read_xlsx(paste("Data/Medicaid/State_exp/Loopfiles/FY",x,"/FMR_ FY",x,".xlsx", sep = ""), sheet = i+57, skip=6)
      exp_med<-med_exp$State_Share[which(med_exp$Service_Category=="Total Net Expenditures")]
      exp_med_2<-med_exp_admin$`State Share`[which(med_exp_admin$`Service Category`=="Total Net Expenditures")]
    }
    if(i==1){
      med_exp_data<-as_tibble(cbind(year=x, states=states_territories[i], exp=exp_med, admin=exp_med_2))
    }else{
      add_row<-as_tibble(cbind(year=x, states=states_territories[i], exp=exp_med, admin=exp_med_2))
      med_exp_data<-rbind(med_exp_data, add_row)
    }
  }
  if(x==2012){
    health_exp_3<-med_exp_data
  }else{
    health_exp_3<-rbind(health_exp_3, med_exp_data)
  }
}

health_exp_st<-rbind(health_exp_2, health_exp_3)


territory_state<-c("Amer._Samoa", "Guam", "N._Mariana_Islands", "Virgin_Islands",
                   "Puerto_Rico", "Dist._Of_Col.", "National_Totals", "Mass._Blind")
health_exp_st$states[which(health_exp_st$states=="Dist._Of_Col.")]<-"District_of_Columbia"
health_exp_st<-health_exp_st[which(!health_exp_st$states%in%territory_state),]
health_exp_st$exp<-as.character(factor(health_exp_st$exp))
health_exp_st$exp<-as.numeric(health_exp_st$exp)
health_exp_st$admin<-as.character(factor(health_exp_st$admin))
health_exp_st$admin<-as.numeric(health_exp_st$admin)
health_exp_st$total<-health_exp_st$exp+health_exp_st$admin

for(i in 2005:2016){
  pop_25<-get_acs(geography = "State", variables = "B06009_001", survey = "acs1", year = i)
  pop_25<-subset(pop_25, NAME!="Puerto Rico")
  highschool<-get_acs(geography = "State", variables = "B06009_003", survey = "acs1", year = i)
  highschool<-subset(highschool, NAME!="Puerto Rico")
  highschool$estimate<-highschool$estimate/pop_25$estimate
  if(i==2005){
    educ_dat<-data.frame(state=highschool$NAME,
                         year=rep(i, nrow(highschool)),
                         edu_prop=highschool$estimate)
  }else{
    add_year<-data.frame(state=highschool$NAME,
                         year=rep(i, nrow(highschool)),
                         edu_prop=highschool$estimate)
    educ_dat<-rbind(educ_dat, add_year)
  }
}

for(x in 2:6){
  for(i in 2005:2016){
    pop_25<-get_acs(geography = "State", variables = "B06009_001", survey = "acs1", year = i)
    pop_25<-subset(pop_25, NAME!="Puerto Rico")
    highschool<-get_acs(geography = "State", variables = paste0("B06009_00", x), survey = "acs1", year = i)
    highschool<-subset(highschool, NAME!="Puerto Rico")
    highschool$estimate<-highschool$estimate/pop_25$estimate
    if(i==2005){
      educ_dat<-data.frame(state=highschool$NAME,
                           year=rep(i, nrow(highschool)),
                           edu_prop=highschool$estimate)
    }else{
      add_year<-data.frame(state=highschool$NAME,
                           year=rep(i, nrow(highschool)),
                           edu_prop=highschool$estimate)
      educ_dat<-rbind(educ_dat, add_year)
    }
  }
  assign(paste0("educ_dat_", x), educ_dat)
}

educ_dat<-as_tibble(educ_dat)
educ_dat$state<-gsub(" ", "_", educ_dat$state)
educ_dat<-educ_dat[which(educ_dat$state%in%State_levels),]
educ_dat<-educ_dat[order(educ_dat$year, educ_dat$state),]

new_data<-as_tibble(new_data)


new_panel<-cbind(new_data, welfare_data, ideology_data, av_tax=av_tr$av_tax_rate, less_hs=educ_dat_2$edu_prop,
                 high_s=educ_dat_3$edu_prop, var_extract, tanf_st_exp=tanf_exp$total, snap_st_exp=fs_data$totals,
                 more_hs=educ_dat_4$edu_prop, med_state_exp=health_exp_st$total, med_fed_exp=med_exp_agg$fed_exp,
                 bach=educ_dat_5$edu_prop, school_fp_exp=school_fp_data$Cost, edu_prop=educ_dat$edu_prop,
                 grad_prof=educ_dat_6$edu_prop)%>%
  as_tibble()

new_panel$tanf_st_exp<-as.character(factor(new_panel$tanf_st_exp))
new_panel$tanf_st_exp<-as.numeric(new_panel$tanf_st_exp)


new_panel$taxes_pc<-new_panel$taxes/new_panel$Population
new_panel$gunbckcheck<-new_panel$gunbckcheck/new_panel$Population
new_panel$gsproduct_pc<-new_panel$gsproduct/new_panel$Population
new_panel$afcd_tanf_pc<-new_panel$`AFDC/TANF Recipients`/new_panel$Population
new_panel$afcd_tanf<-new_panel$`AFDC/TANF Recipients`
new_panel$fs_pc<-new_panel$`Food Stamp/SNAP Recipients`/new_panel$Population
new_panel$fs<-new_panel$`Food Stamp/SNAP Recipients`
new_panel$ssi_pc<-new_panel$`SSI recipients`/new_panel$Population
new_panel$maid_pc<-new_panel$`Medicaid beneficiaries`/new_panel$Population
new_panel$maid<-new_panel$`Medicaid beneficiaries`
new_panel$wic_pc<-new_panel$`WIC participation`/new_panel$Population
new_panel$nslp_pc<-new_panel$`NSLP Total Participation`/new_panel$Population
new_panel$sbp_pc<-new_panel$`SBP Total Participation`/new_panel$Population
new_panel$unemploy<-new_panel$`Unemployment rate` 
new_panel$pov_rate<-new_panel$`Poverty Rate`
new_panel$frac_dem<-new_panel$`Fraction of State House that is Democrat`
new_panel$min_wage<-new_panel$`State Minimum Wage`
new_panel$total_debt_pop<-new_panel$total_debt_outstanding/new_panel$Population
new_panel$total_migration<-new_panel$total_migration/new_panel$Population
new_panel$taxes_pc<-new_panel$taxes/new_panel$Population
new_panel$gsproduct_pc<-new_panel$gsproduct/new_panel$Population

cpi_data<-read_xlsx("Data/CPI.xlsx")
cpi_data<-subset(cpi_data, Year>1999 & Year<2017)
new_panel$gsproduct<-new_panel$gsptotal*1000000
for(i in 2000:2015){
  new_panel$med_state_exp[which(new_panel$Year==i)]<-new_panel$med_state_exp[which(new_panel$Year==i)]*
    (cpi_data$`Annual Average`[which(cpi_data$Year==2016)]/cpi_data$`Annual Average`[which(cpi_data$Year==i)])
}
new_panel$med_weight<-new_panel$med_state_exp/new_panel$gsproduct

for(i in 2000:2015){
  new_panel$tanf_st_exp[which(new_panel$Year==i)]<-new_panel$tanf_st_exp[which(new_panel$Year==i)]*
    (cpi_data$`Annual Average`[which(cpi_data$Year==2016)]/cpi_data$`Annual Average`[which(cpi_data$Year==i)])
}

new_panel$tanf_weight<-new_panel$tanf_st_exp/new_panel$gsproduct


for(i in 2000:2015){
  new_panel$snap_st_exp[which(new_panel$Year==i)]<-new_panel$snap_st_exp[which(new_panel$Year==i)]*
    (cpi_data$`Annual Average`[which(cpi_data$Year==2016)]/cpi_data$`Annual Average`[which(cpi_data$Year==i)])
}

new_panel$snap_weight<-new_panel$snap_st_exp/new_panel$gsproduct

new_panel$sp_index<-((new_panel$afcd_tanf * new_panel$tanf_weight) + (new_panel$fs * new_panel$snap_weight) + (new_panel$maid * new_panel$med_weight))/3
new_panel$sum_exp<-(new_panel$med_state_exp + new_panel$snap_st_exp + new_panel$tanf_st_exp)

new_panel$log_liberal<-log1p(abs(new_panel$liberal))
new_panel$log_liberal[which(new_panel$liberal<0)]<-new_panel$log_liberal[which(new_panel$liberal<0)]*-1
a_factor<-1-min(na.omit(new_panel$liberal))

new_panel$tanf_exp_pu<-new_panel$tanf_st_exp/new_panel$`AFDC/TANF Recipients`
new_panel$fs_exp_pu<-new_panel$snap_st_exp/new_panel$`Food Stamp/SNAP Recipients`
new_panel$med_exp_pu<-new_panel$med_state_exp/new_panel$`Medicaid beneficiaries`
lib_a_factor<-1-min(na.omit(new_panel$liberal))
new_panel$extremists_binary<-ifelse(new_panel$extremists==0, 1, 0)

write_csv(new_panel, "Data/New_Panel.csv")

write.xlsx2(new_panel, "Data/regs_panel.xlsx")







