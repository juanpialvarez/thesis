var_vector<- c("log_mirror_degree")
measures<-c("log_mirror_degree")
count<-0
for(x in 1:length(var_vector)){
  count<-count+1
  pannel_data<-read_csv("Data/pannel_data.csv" )
  pannel_data<-pannel_data[,-1]
  pannel_data$sc_removals[which(pannel_data$sc_removals==0)]<-NA
  pannel_data$patents<-pannel_data$patents/pannel_data$population
  pannel_data$detainer_prop<-pannel_data$detainer_no/pannel_data$detainer_all
  pannel_data$sc_removals<-pannel_data$sc_removals/pannel_data$population
  pannel_data$alcohol_use<-pannel_data$alcohol_use/pannel_data$population
  pannel_data$lgbtq_pop<-pannel_data$lgbtq_pop/pannel_data$population
  pannel_data$total_migration<-pannel_data$total_migration/pannel_data$population
  pannel_data$drug_use<-pannel_data$drug_use/pannel_data$population
  pannel_data$total_noncitizen<-pannel_data$total_noncitizen/pannel_data$population
  pannel_data$total_citizen<-pannel_data$total_citizen/pannel_data$population
  pannel_data$income_tax<-pannel_data$income_tax/pannel_data$population
  pannel_data$corporate_tax<-pannel_data$corporate_tax/pannel_data$population
  pannel_data$titles<-pannel_data$titles/pannel_data$population
  pannel_data$sc_removals<-pannel_data$sc_removals/pannel_data$population
  pannel_data$population<-pannel_data$population/100000
  pannel_data$date<-pannel_data$Year
  pannel_data$Year<-as.Date(ISOdate(pannel_data$Year, 1, 1))
  pannel_data$pov_binary<-rep(NA, nrow(pannel_data))
  pannel_data$state_control[which(pannel_data$state_control=="Dem*")]<-"Dem"
  pannel_data$state_control[which(pannel_data$state_control=="Divided")]<-NA
  for(i in 1:nrow(pannel_data)){
    ifelse(pannel_data$poverty_rate[i]<median(pannel_data$poverty_rate), pannel_data$pov_binary[i]<-1, pannel_data$pov_binary[i]<-0)
  }
  
  pannel_data$pov_binary<-as.factor(pannel_data$pov_binary)
  subset_obs<-which(pannel_data_2009$log_farm_change<0)
  mirror_degree<-pannel_data$degree%>%
    max(na.rm = T)+1%>%
    -pannel_data$degree
  pannel_data$mirror_degree<-mirror_degree
  pannel_data$log_mirror_degree<-log(pannel_data$mirror_degree)
  mirror_wp<-pannel_data$white_pop%>%
    max(na.rm = T)+1%>%
    -pannel_data$white_pop
  pannel_data$mirror_wp<-mirror_wp
  pannel_data$log_mirror_wp<-log(pannel_data$mirror_wp)
  data_ivtest<-pannel_data
  reg_text<-paste("relevance_1<- plm(log(sc_removals)~
                          pov_binary:av_tax +
                           ", var_vector[x]," +
                           diff(log(unemployment)) +
                           gini_index +
                           poverty_rate +
                           education +
                           log(total_migration) +
                           log(population) +
                           president_choice:president_party +
                  rain_high:",var_vector[x]," +
                  rain_high +
                  worker_cars,
                  data = data_ivtest,
                  index = c('Loc_Habitation_State1', 'Year'),
                  model = 'within',
                  effect = 'twoway',
                subset = date>2008)", sep = "")
  
  eval(parse(text = reg_text))
  summary(relevance_1)
  
  sc_all<-linearHypothesis(relevance_1, 
                           c(paste(var_vector[x],":rain_high","=0", sep=""),
                             "rain_high=0",
                             "worker_cars=0"), 
                           vcov = vcovHC, type = "HC1")
  
  residuals<-data.frame(number =as.numeric(names(residuals(relevance_1))),
                        residuals=residuals(relevance_1))
  residuals$number<-(nrow(data_ivtest)-max(residuals$number))+residuals$number
  data_ivtest$residuals<-NA
  data_ivtest$residuals[residuals$number]<-residuals$residuals
  
  reg_text<-paste("jtest_sc <- plm(residuals ~
                           pov_binary:av_tax +
                           ", var_vector[x]," +
                           diff(log(unemployment)) +
                           gini_index +
                           poverty_rate +
                           education +
                           log(total_migration) +
                           log(population) +
                           president_choice:president_party +
                  rain_high:",var_vector[x]," +
                  rain_high +
                  worker_cars,
                  data = data_ivtest,
                 index = c('Loc_Habitation_State1', 'Year'),
                 model = 'within',
                 effect = 'twoway',
                subset = date>2008)", sep = "")
  eval(parse(text = reg_text))
  summary(jtest_sc)
  jtest_sc_sig <- linearHypothesis(jtest_sc, 
                                   c( paste(var_vector[x],":rain_high","=0", sep=""),
                                      "rain_high=0",
                                      "worker_cars=0"), 
                                   test = "Chisq")
  
  jtest_sc<-pchisq(jtest_sc_sig[2, 3], df = 1, lower.tail = FALSE)
  
  assign(paste(measures[count], "df", sep = ""),
         data.frame(centrality_meeasure=rep(measures[count], 2),
                    test=c("relevance", "over_r"),
                    chi_squared=c(sc_all$Chisq[2], jtest_sc_sig$Chisq[2]),
                    P_value=c(sc_all$`Pr(>Chisq)`[2], jtest_sc))
  )
}

IV_relevance_table<-rbind(log_mirror_degreedf)
IV_relevance_table$standard<-0
IV_relevance_table$standard[which(IV_relevance_table$P_value<0.1)]<-"*"
IV_relevance_table$standard[which(IV_relevance_table$P_value<0.05)]<-"**"
IV_relevance_table$standard[which(IV_relevance_table$P_value<0.01)]<-"***"
IV_relevance_table$standard[which(IV_relevance_table$P_value>0.1)]<-"."
write_csv(IV_relevance_table, "Output/instrument_relevance_3.csv")
