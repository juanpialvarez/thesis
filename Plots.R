#Use Main analysis first

for(x in 1:length(pannel_data_2009)){
  if(is.numeric(unlist(pannel_data_2009[,x]))){
    skew<-skewness(pannel_data_2009[,x], na.rm = T)
  }else{
    next
  }
  if(skew>0.5|skew< -0.5){
    print(names(pannel_data_2009[,x]))
    print(skew)
  }
}
skewness(log(pannel_data_2009$degree), na.rm=T)
#Extremists
ggplot(pannel_data_2009, aes(x=extremists)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

log_extremists<-log(pannel_data_2009$extremists)
ggplot(pannel_data_2009, aes(x=log_extremists)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

#Degree
ggplot(pannel_data_2009, aes(x=degree)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

mirror_degree<-pannel_data_2009$degree%>%
  max(na.rm = T)+1%>%
  -pannel_data_2009$degree
pannel_data_2009$mirror_degree<-mirror_degree
log_mirror_degree<-log(pannel_data_2009$mirror_degree)
ggplot(pannel_data_2009, aes(x=log_mirror_degree)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

#Betweeness
ggplot(pannel_data_2009, aes(x=betweeness)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

log_betweeness<-log1p(pannel_data_2009$betweeness)
skewness(log_betweeness, na.rm = T)
ggplot(pannel_data_2009, aes(x=log_betweeness)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")
#White pop
ggplot(pannel_data_2009, aes(x=white_pop)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

mirror_white_pop<-pannel_data_2009$white_pop%>%
  max(na.rm = T)+1%>%
  -pannel_data_2009$white_pop
pannel_data_2009$mirror_white_pop<-mirror_white_pop
log_mirror_white_pop<-log(pannel_data_2009$mirror_white_pop)
ggplot(pannel_data_2009, aes(x=log_mirror_white_pop)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")
#closseness
ggplot(pannel_data_2009, aes(x=closeness)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

skewness(pannel_data_2009$closeness, na.rm = T)
log_closeness<-log(pannel_data_2009$closeness)
skewness(log_closeness, na.rm = T)
ggplot(pannel_data_2009, aes(x=log_closeness)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

#population
ggplot(pannel_data_2009, aes(x=population)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

skewness(pannel_data_2009$population, na.rm = T)
log_population<-log(pannel_data_2009$population)
skewness(log_population, na.rm = T)
ggplot(pannel_data_2009, aes(x=log_population)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

#total_migration
ggplot(pannel_data_2009, aes(x=total_migration)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

skewness(pannel_data_2009$total_migration, na.rm = T)
log_total_migration<-log(pannel_data_2009$total_migration)
skewness(log_total_migration, na.rm = T)
ggplot(pannel_data_2009, aes(x=log_total_migration)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

#unemployment
ggplot(pannel_data_2009, aes(x=unemployment)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

skewness(pannel_data_2009$unemployment, na.rm = T)
log_unemployment<-log(pannel_data_2009$unemployment)
skewness(log_unemployment, na.rm = T)
ggplot(pannel_data_2009, aes(x=log_unemployment)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

#white_pop
ggplot(pannel_data_2009, aes(x=white_pop)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

mirror_white_pop<-pannel_data_2009$white_pop%>%
  max(na.rm = T)+1%>%
  -pannel_data_2009$white_pop
pannel_data_2009$mirror_white_pop<-mirror_white_pop
skewness(pannel_data_2009$mirror_white_pop, na.rm = T)
log_mirror_white_pop<-log(pannel_data_2009$mirror_white_pop)
skewness(log_mirror_white_pop, na.rm = T)
ggplot(pannel_data_2009, aes(x=log_mirror_white_pop)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

#farm_change
ggplot(pannel_data_2009, aes(x=farm_change)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")
skewness(pannel_data_2009$farm_change, na.rm = T)

pannel_data_2009$log_farm_change<-pannel_data_2009$farm_change
subset_obs<-which(pannel_data_2009$log_farm_change<0)
pannel_data_2009$log_farm_change[subset_obs]<-abs(pannel_data_2009$log_farm_change[subset_obs])
pannel_data_2009$log_farm_change<-log1p(pannel_data_2009$log_farm_change)
pannel_data_2009$log_farm_change[subset_obs]<-pannel_data_2009$log_farm_change[subset_obs]*-1
skewness(pannel_data_2009$log_farm_change, na.rm = T)
ggplot(pannel_data_2009, aes(x=log_farm_change)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

#income_tax
ggplot(pannel_data_2009, aes(x=income_tax)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

skewness(pannel_data_2009$income_tax, na.rm = T)
log_income_tax<-log(pannel_data_2009$income_tax)
skewness(log_income_tax, na.rm = T)
ggplot(pannel_data_2009, aes(x=log_income_tax)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

#sc_removals
ggplot(pannel_data_2009, aes(x=sc_removals)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")
skewness(pannel_data_2009$sc_removals, na.rm = T)

log_sc_removals<-log(pannel_data_2009$sc_removals)
skewness(log_sc_removals, na.rm = T)
ggplot(pannel_data_2009, aes(x=log_sc_removals)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")


#titles
ggplot(pannel_data_2009, aes(x=titles)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

skewness(pannel_data_2009$titles, na.rm = T)
log_titles<-log(pannel_data_2009$titles)
skewness(log_titles, na.rm = T)
ggplot(pannel_data_2009, aes(x=log_titles)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

#temperature
ggplot(pannel_data_2009, aes(x=temperature)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

skewness(pannel_data_2009$temperature, na.rm = T)
sqrt_temperature<-sqrt(pannel_data_2009$temperature)
skewness(sqrt_temperature, na.rm = T)
ggplot(pannel_data_2009, aes(x=sqrt_temperature)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

#snow
ggplot(pannel_data_2009, aes(x=snow)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

skewness(pannel_data_2009$snow, na.rm = T)
sqrt_temperature<-sqrt(pannel_data_2009$snow)
skewness(sqrt_temperature, na.rm = T)
ggplot(pannel_data_2009, aes(x=sqrt_temperature)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

#cold
ggplot(pannel_data_2009, aes(x=cold)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

skewness(pannel_data_2009$cold, na.rm = T)
log_cold<-log1p(pannel_data_2009$cold)
skewness(log_cold, na.rm = T)
ggplot(pannel_data_2009, aes(x=log_cold)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

#extremists, degree, betweeness, closeness, population, total_migration, total_citizen, total_noncitizen,
#President_votes, unemployment, white_pop, farm_change, drug_use, abortion_providers, abortion_rate,
#corporate_tax, income_tax, detainer_all, detainer_notknown, detainer_no, detainer_yes, sc_removals, titles,
# violent_crime_rate, consumption_dg, consumption_fsi, depression, alcohol_use, art_employment, motor_vehicle_theft,
#lgbtq_pop, temperature, snow, cold, patents

for(i in 2009:2018){
  plot<-subset(pannel_data_2009, Year==paste(i, "-01-01", sep = ""))
  assign(paste(i,"plot", sep = ""), ggplot(plot, aes(x=titles, y=population)) +
           geom_point(aes(colour=titles)))
}

plot<-subset(pannel_data_2009, Year==paste(2018, "-01-01", sep = ""))
ggplot(plot, aes(x=titles, y=population)) +
  geom_point(aes(colour=titles)) +
  geom_smooth(method=lm)
plot<-subset(pannel_data_2009, Year==paste(2015, "-01-01", sep = ""))
ggplot(plot, aes(x=titles, y=detainer_prop)) +
  geom_point(aes(colour=titles)) +
  geom_smooth(method=lm)
plot<-subset(pannel_data_2009, Year==paste(2015, "-01-01", sep = ""))
ggplot(plot, aes(x=sc_removals, y=detainer_prop)) +
  geom_point(aes(colour=titles)) +
  geom_smooth(method=lm)


for(i in 1:length(pannel_data_2009)){
  if(is.numeric(unlist(pannel_data_2009[,i])) & names(pannel_data_2009[i])!="Year" & names(pannel_data_2009[i])!="titles"){
    assign(paste(names(pannel_data_2009[i]), "_plots", sep = ""), list())
    for(x in 2011:2018){
      if(x==2011){
        count<-1
      }else{
        count<-count+1
      }
      plot<-subset(pannel_data_2009, Year==paste(x, "-01-01", sep = ""))
      text_plot<-paste("scatter<-ggplot(plot, aes(x=titles, y=",
      names(pannel_data_2009[i]),
      ")) + geom_point(aes(colour=titles)) + geom_smooth(method=lm)", sep = "")
      eval(parse(text = text_plot))
      text_plot<-paste(paste(names(pannel_data_2009[i]), "_plots[[", count, "]]", sep = ""),"<-scatter", sep = "")
      eval(parse(text = text_plot))
    }
  }else{
    next
  }
  
}


reg_vars<-c("Loc_Habitation_State1",
            "Year",
            "closeness",
            "farm_change",
            "education",
            "population",
            "old_age",
            "manufacturing",
            "gini_index",
            "poverty_rate",
            "women_employment",
            "unemployment",
            "white_pop",
            "president_choice",
            "president_party",
            "total_migration",
            "income_tax",
            "sc_removals",
            "rain_high",
            "rain",
            "snow",
            "cold",
            "temperature",
            "population",
            "titles")
#motor_vehicle_theft, depression, art_employment, lgbtq_pop, violent_crime_rate, titles
#corporate_tax, total_citizen, total_migration, population, betweeness, total_noncitizen
#income_tax, unemployment


reg_panel<-pannel_data_2009[c("Loc_Habitation_State1", "Year", "titles", "sc_removals", "total_migration",
                              "closeness", "farm_change", "manufacturing", "unemployment", "gini_index",
                              "poverty_rate", "education", "income_tax", "population", "total_migration",
                              "women_employment", "white_pop", "sc_removals", "rain_high", "rain", "temperature",
                              "snow", "cold", "old_age")]

reg_panel$residuals_ev<-0
reg_panel$residuals_ev[which(rowSums(is.na(reg_panel))==0)]<-residuals(title_closeness_log)
reg_panel$residuals_ev[which(reg_panel$residuals_ev==0)]<-NA
reg_panel<-reg_panel[!duplicated(as.list(reg_panel))]
cor(reg_panel$residuals_ev, reg_panel$poverty_rate,  use = "complete.obs")
ggplot(reg_panel, aes(x=residuals_ev)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")
ggplot(plot, aes(x=residuals_ev, y=poverty_rate)) +
  geom_point(aes(colour=titles)) +
  geom_smooth(method=lm)

plot<-plm(log(titles) ~ sc_removals,
          data = pannel_data_2009,
          index = c('Loc_Habitation_State1', 'Year'),
          model = 'within',
          effect = 'time')
reg_panel<-pannel_data_2009[c("Loc_Habitation_State1", "Year", "titles", "sc_removals", "education")]
reg_panel$residuals_ev<-0
reg_panel$residuals_ev[which(rowSums(is.na(reg_panel))==0)]<-residuals(plot)
reg_panel$residuals_ev[which(reg_panel$residuals_ev==0)]<-NA
reg_panel<-reg_panel[!duplicated(as.list(reg_panel))]
cor(reg_panel$residuals_ev, reg_panel$sc_removals,  use = "complete.obs")
ggplot(reg_panel, aes(x=residuals_ev)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")
plot<-subset(reg_panel, Year==paste(2018, "-01-01", sep = ""))
ggplot(plot, aes(x=residuals_ev, y=sc_removals)) +
  geom_point(aes(colour=titles)) +
  geom_smooth(method=lm)


#motor_vehicle_theft, depression, art_employment, lgbtq_pop, violent_crime_rate, titles
#corporate_tax, total_citizen, total_migration, population, betweeness, total_noncitizen
#income_tax, unemployment

sapply(abortion_providers_plots, print)
sapply(abortion_rate_plots, print)
sapply(alcohol_use_plots, print)
sapply(art_employment_plots, print)
sapply(betweeness_plots, print)
sapply(closeness_plots, print)
sapply(cold_plots, print)
sapply(consumption_dg_plots, print)
sapply(consumption_fsi_plots, print)
sapply(consumption_nfp_plots, print)
sapply(corporate_tax_plots, print)


#motor_vehicle_theft, depression, art_employment, lgbtq_pop, violent_crime_rate, titles
#corporate_tax, total_citizen, total_migration, population, betweeness, total_noncitizen
#income_tax, unemployment

#correlated variables titles
for(x in 1:length(pannel_data_2009)){
  if(is.numeric(unlist(pannel_data_2009[,x]))){
    correlation<-cor(pannel_data_2009$titles, unlist(pannel_data_2009[,x]), use = "complete.obs")
  }else{
    next
  }
  print(names(pannel_data_2009[,x]))
  print(correlation)
}

#correlated variables sc_removals
for(x in 1:length(pannel_data_2009)){
  if(is.numeric(unlist(pannel_data_2009[,x]))){
    correlation<-cor(pannel_data_2009$sc_removals, unlist(pannel_data_2009[,x]), use = "complete.obs")
  }else{
    next
  }
  print(names(pannel_data_2009[,x]))
  print(correlation)
}

for(i in 1:length(pannel_data_2009)){
  if(is.numeric(unlist(pannel_data_2009[,i])) & names(pannel_data_2009[i])!="Year" & names(pannel_data_2009[i])!="titles" & names(pannel_data_2009[i])!="date"){
    reg_text<-paste("IV<-plm(log(titles) ~", names(pannel_data_2009[i]),"+ sc_removals,
          data = pannel_data_2009,
          index = c('Loc_Habitation_State1', 'Year'),
          model = 'within',
          effect = 'time')")
    eval(parse(text = reg_text))
    summary(IV)
    hypothesis<-linearHypothesis(IV, 
                                 paste(names(pannel_data_2009[i]), "=0", sep = ""), 
                                 vcov = vcovHC, type = "HC1")
    if(hypothesis$`Pr(>Chisq)`[2]<0.1){
      print(names(pannel_data_2009[,i]))
    }
  }else{
    next
  }
}

for(i in 1:length(pannel_data_2009)){
  if(is.numeric(unlist(pannel_data_2009[,i])) & names(pannel_data_2009[i])!="Year" & names(pannel_data_2009[i])!="titles" & names(pannel_data_2009[i])!="sc_removals" & names(pannel_data_2009[i])!="date"){
    reg_text<-paste("IV<-plm(sc_removals ~", names(pannel_data_2009[i]),",
          data = pannel_data_2009,
          index = c('Loc_Habitation_State1', 'Year'),
          model = 'within',
          effect = 'time')")
    eval(parse(text = reg_text))
    summary(IV)
    hypothesis<-linearHypothesis(IV, 
                                 paste(names(pannel_data_2009[i]), "=0", sep = ""), 
                                 vcov = vcovHC, type = "HC1")
    if(hypothesis$`Pr(>Chisq)`[2]<0.1){
      print(names(pannel_data_2009[,i]))
    }
  }else{
    next
  }
}

