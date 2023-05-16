#Analysis 

library(huxtable)
library(ggpubr)
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
setwd("/Users/juanpabloalvarezvega/Dropbox/Desktop2021/Masters/Thesis/FRC")

new_panel<-read_csv("Data/New_Panel.csv" )


#gsptotal, incomepcap, total_debt_outstanding, total_expenditure, total_revenue, med_spending_own
new_panel$gunbckcheck<-new_panel$gunbckcheck/new_panel$Population
new_panel$taxes_pc<-new_panel$taxes/new_panel$Population
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

#cor matrix, and a cor table
cor(new_panel$hou_chamber, new_panel$titles, use= "complete.obs")
cov(new_panel$hou_chamber, new_panel$titles, use= "complete.obs")


title_eigen_vector_log<- plm(log(titles) ~ eigen_vector +
                               lag(log(titles)) +
                               lag(log(titles), 2) +
                               manufacturing +
                               log(taxes):av_tax +
                               gini_index +
                               log(unemploy) +
                               log(pov_rate) +
                               edu_prop +
                               log(women_employment) +
                               diff(log(total_migration)) +
                               log(Population) +                               
                               frac_dem +
                               hou_chamber +
                               liberal +
                               log(tanf_exp_pu) +
                               fs_exp_pu +
                               log(med_exp_pu)|
                               .-hou_chamber +
                               eigen_vector +
                               temperature +
                               art_employment +
                               cold+
                               manufacturing +
                               log(taxes):av_tax +
                               gini_index +
                               log(unemploy) +
                               log(pov_rate) +
                               edu_prop +
                               log(women_employment) +
                               diff(log(total_migration)) +
                               log(Population) +                               
                               log(tanf_exp_pu) +
                               fs_exp_pu + 
                               log(med_exp_pu),
                             data = new_panel,
                             index = c("Loc_Habitation_State1", "Year"),
                             model = "within",
                             effect = "twoway",
                             na.action = na.omit)
summary(title_eigen_vector_log)
res_ev_t<-title_eigen_vector_log$residuals
res_ev_t<-data.frame(n=1:length(res_ev_t), res=res_ev_t)

ggqqplot(res_ev_t$res)
ggplot(res_ev_t, aes(res)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  xlab("Liberal") +
  ylab("Density") 
skewness(res_ev_t$res)
shapiro.test(res_ev_t$res)

title_eigen_vector_log_sc<-pbgtest(title_eigen_vector_log,
                                   data = pannel_data,
                                   type ="Chisq")
title_eigen_vector_log_r<-coeftest(title_eigen_vector_log, function(x) vcovHC(x, type = 'sss'))




title_degree_log<- plm(log(titles) ~ degree +
                         lag(log(titles)) +
                         lag(log(titles), 2) +
                         manufacturing +
                         log(taxes):av_tax +
                         gini_index +
                         log(unemploy) +
                         log(pov_rate) +
                         edu_prop +
                         log(women_employment) +
                         diff(log(total_migration)) +
                         log(Population) +                               
                         frac_dem +
                         hou_chamber +
                         liberal +
                         log(tanf_exp_pu) +
                         fs_exp_pu +
                         log(med_exp_pu)|
                         .-hou_chamber +
                         degree +
                         temperature +
                         art_employment +
                         cold+
                         manufacturing +
                         log(taxes):av_tax +
                         gini_index +
                         log(unemploy) +
                         log(pov_rate) +
                         edu_prop +
                         log(women_employment) +
                         diff(log(total_migration)) +
                         log(Population) +                               
                         log(tanf_exp_pu) +
                         fs_exp_pu + 
                         log(med_exp_pu),
                       data = new_panel,
                       index = c("Loc_Habitation_State1", "Year"),
                       model = "within",
                       effect = "twoway",
                       na.action = na.omit)
summary(title_degree_log)

res_ev_t<-title_degree_log$residuals
res_ev_t<-data.frame(n=1:length(res_ev_t), res=res_ev_t)

ggqqplot(res_ev_t$res)
ggplot(res_ev_t, aes(res)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  xlab("Liberal") +
  ylab("Density") 
skewness(res_ev_t$res)
shapiro.test(res_ev_t$res)

title_degree_log_sc<-pbgtest(title_degree_log,
                             data = pannel_data,
                             type ="Chisq")
title_degree_log_r<-coeftest(title_degree_log, function(x) vcovHC(x, type = 'sss'))

title_betweeness_log<- plm(log(titles) ~ betweeness +
                             lag(log(titles)) +
                             manufacturing +
                             log(taxes):av_tax +
                             gini_index +
                             log(unemploy) +
                             log(pov_rate) +
                             edu_prop +
                             log(women_employment) +
                             diff(log(total_migration)) +
                             log(Population) +                               
                             frac_dem +
                             hou_chamber +
                             liberal +
                             log(tanf_exp_pu) +
                             fs_exp_pu +
                             log(med_exp_pu)|
                             .-hou_chamber +
                             betweeness +
                             temperature +
                             art_employment +
                             cold+
                             manufacturing +
                             log(taxes):av_tax +
                             gini_index +
                             log(unemploy) +
                             log(pov_rate) +
                             edu_prop +
                             log(women_employment) +
                             diff(log(total_migration)) +
                             log(Population) +                               
                             log(tanf_exp_pu) +
                             fs_exp_pu + 
                             log(med_exp_pu),
                           data = new_panel,
                           index = c("Loc_Habitation_State1", "Year"),
                           model = "within",
                           effect = "twoway",
                           na.action = na.omit)

summary(title_betweeness_log)


res_ev_t<-title_betweeness_log$residuals
res_ev_t<-data.frame(n=1:length(res_ev_t), res=res_ev_t)

ggqqplot(res_ev_t$res)
ggplot(res_ev_t, aes(res)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  xlab("Liberal") +
  ylab("Density") 
skewness(res_ev_t$res)
shapiro.test(res_ev_t$res)

title_betweeness_log_sc<-pbgtest(title_betweeness_log,
                                 data = pannel_data,
                                 type ="Chisq")
title_betweeness_log_r<-coeftest(title_betweeness_log, function(x) vcovHC(x, type = 'sss'))


title_closeness_log<- plm(log(titles) ~ closeness +
                            lag(log(titles)) +
                            manufacturing +
                            log(taxes):av_tax +
                            gini_index +
                            log(unemploy) +
                            log(pov_rate) +
                            edu_prop +
                            log(women_employment) +
                            diff(log(total_migration)) +
                            log(Population) +                               
                            frac_dem +
                            hou_chamber +
                            liberal +
                            log(tanf_exp_pu) +
                            fs_exp_pu +
                            log(med_exp_pu)|
                            .-hou_chamber +
                            closeness +
                            temperature +
                            art_employment +
                            cold+
                            lag(log(titles)) +
                            lag(log(titles), 2) +
                            manufacturing +
                            log(taxes):av_tax +
                            gini_index +
                            log(unemploy) +
                            log(pov_rate) +
                            edu_prop +
                            log(women_employment) +
                            diff(log(total_migration)) +
                            log(Population) +                               
                            log(tanf_exp_pu) +
                            fs_exp_pu + 
                            log(med_exp_pu),
                          data = new_panel,
                          index = c("Loc_Habitation_State1", "Year"),
                          model = "within",
                          effect = "twoway",
                          na.action = na.omit)

summary(title_closeness_log)

res_ev_t<-title_closeness_log$residuals
res_ev_t<-data.frame(n=1:length(res_ev_t), res=res_ev_t)

ggqqplot(res_ev_t$res)
ggplot(res_ev_t, aes(res)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  xlab("Liberal") +
  ylab("Density") 
skewness(res_ev_t$res)
shapiro.test(res_ev_t$res)

title_closeness_log_sc<-pbgtest(title_closeness_log,
                                data = pannel_data,
                                type ="Chisq")
title_closeness_log_r<-coeftest(title_closeness_log, function(x) vcovHC(x, type = 'sss'))

stargazer(title_eigen_vector_log_r,
          title_degree_log_r,
          title_betweeness_log_r,
          title_closeness_log_r, title="Results", align=TRUE, type="latex", out = "Output/Regressions/Regression_iv.txt")
stargazer(title_eigen_vector_log_r,
          title_degree_log_r,
          title_betweeness_log_r,
          title_closeness_log_r, title="Results", align=TRUE, type="html", out = "Output/Regressions/Regression_iv.html")

### two way no IV
title_eigen_vector_nv<- plm(log(titles) ~ eigen_vector +
                              lag(log(titles)) +
                              lag(log(titles), 2) +
                              manufacturing +
                              log(taxes):av_tax +
                              gini_index +
                              log(unemploy) +
                              log(pov_rate) +
                              edu_prop +
                              log(women_employment) +
                              diff(log(total_migration)) +
                              log(Population) +                               
                              frac_dem +
                              hou_chamber +
                              liberal +
                              log(tanf_exp_pu) +
                              fs_exp_pu +
                              log(med_exp_pu),
                            data = new_panel,
                            index = c("Loc_Habitation_State1", "Year"),
                            model = "within",
                            effect = "twoway",
                            na.action = na.omit)

summary(title_eigen_vector_nv)

res_ev_t<-title_eigen_vector_nv$residuals
res_ev_t<-data.frame(n=1:length(res_ev_t), res=res_ev_t)
ggqqplot(res_ev_t$res)
ggplot(res_ev_t, aes(res)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  xlab("Liberal") +
  ylab("Density") 
skewness(res_ev_t$res)
shapiro.test(res_ev_t$res)

title_eigen_vector_nv_sc<-pbgtest(title_eigen_vector_nv,
                                  data = pannel_data,
                                  type ="Chisq")
title_eigen_vector_nv_r<-coeftest(title_eigen_vector_nv, function(x) vcovHC(x, type = 'sss'))

title_degree_nv<- plm(log(titles) ~ degree +
                        lag(log(titles)) +
                        lag(log(titles), 2) +
                        manufacturing +
                        log(taxes):av_tax +
                        gini_index +
                        log(unemploy) +
                        log(pov_rate) +
                        edu_prop +
                        log(women_employment) +
                        diff(log(total_migration)) +
                        log(Population) +                               
                        frac_dem +
                        hou_chamber +
                        liberal +
                        log(tanf_exp_pu) +
                        fs_exp_pu +
                        log(med_exp_pu),
                      data = new_panel,
                      index = c("Loc_Habitation_State1", "Year"),
                      model = "within",
                      effect = "twoway",
                      na.action = na.omit)
summary(title_degree_nv)

res_ev_t<-title_degree_nv$residuals
res_ev_t<-data.frame(n=1:length(res_ev_t), res=res_ev_t)
ggqqplot(res_ev_t$res)
ggplot(res_ev_t, aes(res)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  xlab("Liberal") +
  ylab("Density") 
skewness(res_ev_t$res)
shapiro.test(res_ev_t$res)
title_degree_nv_sc<-pbgtest(title_degree_nv,
                            data = pannel_data,
                            type ="Chisq")
title_degree_nv_r<-coeftest(title_degree_nv, function(x) vcovHC(x, type = 'sss'))


title_betweeness_nv<- plm(log(titles) ~ betweeness +
                            lag(log(titles)) +
                            manufacturing +
                            log(taxes):av_tax +
                            gini_index +
                            log(unemploy) +
                            log(pov_rate) +
                            edu_prop +
                            log(women_employment) +
                            diff(log(total_migration)) +
                            log(Population) +                               
                            frac_dem +
                            hou_chamber +
                            liberal +
                            log(tanf_exp_pu) +
                            fs_exp_pu +
                            log(med_exp_pu),
                          data = new_panel,
                          index = c("Loc_Habitation_State1", "Year"),
                          model = "within",
                          effect = "twoway",
                          na.action = na.omit)

summary(title_betweeness_nv)

res_ev_t<-title_betweeness_nv$residuals
res_ev_t<-data.frame(n=1:length(res_ev_t), res=res_ev_t)
ggqqplot(res_ev_t$res)
ggplot(res_ev_t, aes(res)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  xlab("Liberal") +
  ylab("Density") 
skewness(res_ev_t$res)
shapiro.test(res_ev_t$res)

title_betweeness_sc<-pbgtest(title_betweeness_nv,
                             data = pannel_data_2009,
                             type ="Chisq")
title_betweeness_nv_r<-coeftest(title_betweeness_nv, function(x) vcovHC(x, type = 'sss'))

title_closeness_nv<- plm(log(titles) ~ closeness +
                           lag(log(titles)) +
                           manufacturing +
                           log(taxes):av_tax +
                           gini_index +
                           log(unemploy) +
                           log(pov_rate) +
                           edu_prop +
                           log(women_employment) +
                           diff(log(total_migration)) +
                           log(Population) +                               
                           frac_dem +
                           hou_chamber +
                           liberal +
                           log(tanf_exp_pu) +
                           fs_exp_pu +
                           log(med_exp_pu),
                         data = new_panel,
                         index = c("Loc_Habitation_State1", "Year"),
                         model = "within",
                         effect = "twoway",
                         na.action = na.omit)

summary(title_closeness_nv)

res_ev_t<-title_closeness_nv$residuals
res_ev_t<-data.frame(n=1:length(res_ev_t), res=res_ev_t)
ggqqplot(res_ev_t$res)
ggplot(res_ev_t, aes(res)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  xlab("Liberal") +
  ylab("Density") 
skewness(res_ev_t$res)
shapiro.test(res_ev_t$res)

title_closeness_nv_sc<-pbgtest(title_closeness_nv,
                               data = pannel_data_2009,
                               type ="Chisq")
title_closeness_nv_r<-coeftest(title_closeness_nv, function(x) vcovHC(x, type = 'sss'))


stargazer(title_eigen_vector_nv_r,
          title_degree_nv_r,
          title_betweeness_nv_r,
          title_closeness_nv_r, title="Results", align=TRUE, type="latex", out = "Output/Regressions/Regression_niv.txt")
stargazer(title_eigen_vector_nv_r,
          title_degree_nv_r,
          title_betweeness_nv_r,
          title_closeness_nv_r, title="Results", align=TRUE, type="html", out = "Output/Regressions/Regression_niv.html")

hausman_test_ev<-phtest(title_eigen_vector_log,
                        title_eigen_vector_nv,
                        data = new_panel,
                        index = c("Loc_Habitation_State1", "Year"),
                        model = "within",
                        effect = "twoway",
                        method = c("chisq", "aux"),
                        vcov = vcovHC,
                        na.action = na.omit)
hausman_test_deg<-phtest(title_degree_log,
                         title_degree_nv,
                         data = new_panel,
                         index = c("Loc_Habitation_State1", "Year"),
                         model = "within",
                         effect = "twoway",
                         method = c("chisq", "aux"),
                         vcov = vcovHC,
                         na.action = na.omit)
hausman_test_bet<-phtest(title_betweeness_nv,
                         title_betweeness_log,
                         data = new_panel,
                         index = c("Loc_Habitation_State1", "Year"),
                         model = "within",
                         effect = "twoway",
                         method = c("chisq", "aux"),
                         vcov = vcovHC,
                         na.action = na.omit)
hausman_test_cls<-phtest(title_closeness_log,
                         title_closeness_nv,
                         data = new_panel,
                         index = c("Loc_Habitation_State1", "Year"),
                         model = "within",
                         effect = "twoway",
                         method = c("chisq", "aux"),
                         vcov = vcovHC,
                         na.action = na.omit)


measures<-c("closeness", "degree", "betweeness", "eigen_vector")
data_endog<-new_panel
for(i in measures){
  residuals<-data.frame(number =as.numeric(names(residuals(get(paste0("title_", i, "_nv"))))),
                        residuals= residuals(get(paste0("title_", i , "_nv"))))
  residuals$number<-(nrow(data_endog)-max(residuals$number))+residuals$number
  data_endog[paste0("residuals_",i,"_nv")]<-NA
  data_endog[residuals$number, paste0("residuals_",i,"_nv")]<-residuals$residuals
  jpeg(paste("Output/Endog/Endog_Tables/",i,"/plo_residuals_niv.jpg", sep = ""), width = 500, height = 500)
  ggplot(data_endog, aes(x=sc_removals, y=get(paste0("residuals_",i,"_nv")))) +
    geom_point(aes(colour=titles)) +
    geom_smooth(method=lm) +
    xlab("Secure Communities") +
    ylab("Residuals")
  dev.off()
  jpeg(paste("Output/Endog/Endog_Tables/",i,"/hist_residuals_noiv.jpg", sep = ""), width = 500, height = 500)
  ggplot(data_endog, aes(x=get(paste0("residuals_",i,"_nv")))) +
    geom_histogram(aes(y=..density..),color="black", fill="white") +
    geom_density(alpha=.2, fill="#FF6666") +
    xlab("Residuals") +
    ylab("Density")
  dev.off()
  non_instrument_sk<-skewness(data_endog[paste0("residuals_",i,"_nv")], na.rm = T)
  
  residuals<-data.frame(number =as.numeric(names(residuals(get(paste0("title_", i, "_log"))))),
                        residuals= residuals(get(paste0("title_", i , "_log"))))
  data_endog[paste0("residuals_",i)]<-NA
  data_endog[residuals$number, paste0("residuals_",i)]<-residuals$residuals
  jpeg(paste("Output/Endog/Endog_Tables/",i,"/plot_residuals.jpg", sep = ""), width = 500, height = 500)
  ggplot(data_endog, aes(x=sc_removals, y=get(paste0("residuals_",i)))) +
    geom_point(aes(colour=titles)) +
    geom_smooth(method=lm) +
    xlab("Secure Communities") +
    ylab("Residuals")
  dev.off()
  jpeg(paste("Output/Endog/Endog_Tables/",i,"/hist_residuals.jpg", sep = ""), width = 500, height = 500)
  ggplot(data_endog, aes(get(paste0("residuals_",i)))) +
    geom_histogram(aes(y=..density..),color="black", fill="white") +
    geom_density(alpha=.2, fill="#FF6666") +
    xlab("Residuals") +
    ylab("Density")
  dev.off()
  instrument_sk<-skewness(data_endog[paste0("residuals_",i)], na.rm = T)
  hausman_test<-phtest(get(paste0("title_", i, "_log")),
                       get(paste0("title_", i, "_nv")),
                       data = pannel_data,
                       subset = date>2008,
                       index = c("Loc_Habitation_State1", "Year"),
                       model = "within",
                       effect = "twoway",
                       method = c("chisq", "aux"),
                       vcov = vcovHC)
  
  sh_nv<-shapiro.test(as.numeric(unlist(data_endog[paste0("residuals_",i,"_nv")])))
  sh_iv<-shapiro.test(as.numeric(unlist(data_endog[paste0("residuals_",i)])))
  assign(paste(i, "_df", sep = ""), data.frame(measure=i,
                                               h_p.value=c(hausman_test$p.value),
                                               h_statistic=c(hausman_test$statistic),
                                               inst_sk=instrument_sk,
                                               non_inst_sk=non_instrument_sk,
                                               sh_p.value=sh_nv$p.value,
                                               sh_statistic=sh_nv$statistic,
                                               sh_iv_p.value=sh_iv$p.value,
                                               sh_iv_statistic=sh_iv$statistic))
}




endogeniety_table<-rbind(degree_df, eigen_vector_df, closeness_df, betweeness_df)
write_csv(endogeniety_table, "Output/Endog/endogeniety_table.csv")



library(huxtable)
library(ggpubr)
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
setwd("/Users/juanpabloalvarezvega/Dropbox/Desktop2021/Masters/Thesis/FRC")

new_panel<-read_csv("Data/New_Panel.csv" )


#gsptotal, incomepcap, total_debt_outstanding, total_expenditure, total_revenue, med_spending_own
new_panel$gunbckcheck<-new_panel$gunbckcheck/new_panel$Population
new_panel$taxes_pc<-new_panel$taxes/new_panel$Population
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

#cor matrix, and a cor table
cor(new_panel$hou_chamber, new_panel$titles, use= "complete.obs")
cov(new_panel$hou_chamber, new_panel$titles, use= "complete.obs")


title_eigen_vector_log<- plm(log(titles) ~ eigen_vector +
                               lag(log(titles)) +
                               lag(log(titles), 2) +
                               manufacturing +
                               log(taxes):av_tax +
                               gini_index +
                               log(unemploy) +
                               log(pov_rate) +
                               edu_prop +
                               log(women_employment) +
                               diff(log(total_migration)) +
                               log(Population) +                               
                               frac_dem +
                               hou_chamber +
                               liberal +
                               log(tanf_exp_pu) +
                               fs_exp_pu +
                               log(med_exp_pu)|
                               .-hou_chamber +
                               eigen_vector +
                               temperature +
                               art_employment +
                               cold+
                               manufacturing +
                               log(taxes):av_tax +
                               gini_index +
                               log(unemploy) +
                               log(pov_rate) +
                               edu_prop +
                               log(women_employment) +
                               diff(log(total_migration)) +
                               log(Population) +                               
                               log(tanf_exp_pu) +
                               fs_exp_pu + 
                               log(med_exp_pu),
                             data = new_panel,
                             index = c("Loc_Habitation_State1", "Year"),
                             model = "within",
                             effect = "twoway",
                             na.action = na.omit)
summary(title_eigen_vector_log)
res_ev_t<-title_eigen_vector_log$residuals
res_ev_t<-data.frame(n=1:length(res_ev_t), res=res_ev_t)

ggqqplot(res_ev_t$res)
ggplot(res_ev_t, aes(res)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  xlab("Liberal") +
  ylab("Density") 
skewness(res_ev_t$res)
shapiro.test(res_ev_t$res)

title_eigen_vector_log_sc<-pbgtest(title_eigen_vector_log,
                                   data = pannel_data,
                                   type ="Chisq")
title_eigen_vector_log_r<-coeftest(title_eigen_vector_log, function(x) vcovHC(x, type = 'sss'))




title_degree_log<- plm(log(titles) ~ degree +
                         lag(log(titles)) +
                         lag(log(titles), 2) +
                         manufacturing +
                         log(taxes):av_tax +
                         gini_index +
                         log(unemploy) +
                         log(pov_rate) +
                         edu_prop +
                         log(women_employment) +
                         diff(log(total_migration)) +
                         log(Population) +                               
                         frac_dem +
                         hou_chamber +
                         liberal +
                         log(tanf_exp_pu) +
                         fs_exp_pu +
                         log(med_exp_pu)|
                         .-hou_chamber +
                         degree +
                         temperature +
                         art_employment +
                         cold+
                         manufacturing +
                         log(taxes):av_tax +
                         gini_index +
                         log(unemploy) +
                         log(pov_rate) +
                         edu_prop +
                         log(women_employment) +
                         diff(log(total_migration)) +
                         log(Population) +                               
                         log(tanf_exp_pu) +
                         fs_exp_pu + 
                         log(med_exp_pu),
                       data = new_panel,
                       index = c("Loc_Habitation_State1", "Year"),
                       model = "within",
                       effect = "twoway",
                       na.action = na.omit)
summary(title_degree_log)

res_ev_t<-title_degree_log$residuals
res_ev_t<-data.frame(n=1:length(res_ev_t), res=res_ev_t)

ggqqplot(res_ev_t$res)
ggplot(res_ev_t, aes(res)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  xlab("Liberal") +
  ylab("Density") 
skewness(res_ev_t$res)
shapiro.test(res_ev_t$res)

title_degree_log_sc<-pbgtest(title_degree_log,
                             data = pannel_data,
                             type ="Chisq")
title_degree_log_r<-coeftest(title_degree_log, function(x) vcovHC(x, type = 'sss'))

title_betweeness_log<- plm(log(titles) ~ betweeness +
                             lag(log(titles)) +
                             manufacturing +
                             log(taxes):av_tax +
                             gini_index +
                             log(unemploy) +
                             log(pov_rate) +
                             edu_prop +
                             log(women_employment) +
                             diff(log(total_migration)) +
                             log(Population) +                               
                             frac_dem +
                             hou_chamber +
                             liberal +
                             log(tanf_exp_pu) +
                             fs_exp_pu +
                             log(med_exp_pu)|
                             .-hou_chamber +
                             betweeness +
                             temperature +
                             art_employment +
                             cold+
                             manufacturing +
                             log(taxes):av_tax +
                             gini_index +
                             log(unemploy) +
                             log(pov_rate) +
                             edu_prop +
                             log(women_employment) +
                             diff(log(total_migration)) +
                             log(Population) +                               
                             log(tanf_exp_pu) +
                             fs_exp_pu + 
                             log(med_exp_pu),
                           data = new_panel,
                           index = c("Loc_Habitation_State1", "Year"),
                           model = "within",
                           effect = "twoway",
                           na.action = na.omit)

summary(title_betweeness_log)


res_ev_t<-title_betweeness_log$residuals
res_ev_t<-data.frame(n=1:length(res_ev_t), res=res_ev_t)

ggqqplot(res_ev_t$res)
ggplot(res_ev_t, aes(res)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  xlab("Liberal") +
  ylab("Density") 
skewness(res_ev_t$res)
shapiro.test(res_ev_t$res)

title_betweeness_log_sc<-pbgtest(title_betweeness_log,
                                 data = pannel_data,
                                 type ="Chisq")
title_betweeness_log_r<-coeftest(title_betweeness_log, function(x) vcovHC(x, type = 'sss'))


title_closeness_log<- plm(log(titles) ~ closeness +
                            lag(log(titles)) +
                            manufacturing +
                            log(taxes):av_tax +
                            gini_index +
                            log(unemploy) +
                            log(pov_rate) +
                            edu_prop +
                            log(women_employment) +
                            diff(log(total_migration)) +
                            log(Population) +                               
                            frac_dem +
                            hou_chamber +
                            liberal +
                            log(tanf_exp_pu) +
                            fs_exp_pu +
                            log(med_exp_pu)|
                            .-hou_chamber +
                            closeness +
                            temperature +
                            art_employment +
                            cold+
                            lag(log(titles)) +
                            lag(log(titles), 2) +
                            manufacturing +
                            log(taxes):av_tax +
                            gini_index +
                            log(unemploy) +
                            log(pov_rate) +
                            edu_prop +
                            log(women_employment) +
                            diff(log(total_migration)) +
                            log(Population) +                               
                            log(tanf_exp_pu) +
                            fs_exp_pu + 
                            log(med_exp_pu),
                          data = new_panel,
                          index = c("Loc_Habitation_State1", "Year"),
                          model = "within",
                          effect = "twoway",
                          na.action = na.omit)

summary(title_closeness_log)

res_ev_t<-title_closeness_log$residuals
res_ev_t<-data.frame(n=1:length(res_ev_t), res=res_ev_t)

ggqqplot(res_ev_t$res)
ggplot(res_ev_t, aes(res)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  xlab("Liberal") +
  ylab("Density") 
skewness(res_ev_t$res)
shapiro.test(res_ev_t$res)

title_closeness_log_sc<-pbgtest(title_closeness_log,
                                data = pannel_data,
                                type ="Chisq")
title_closeness_log_r<-coeftest(title_closeness_log, function(x) vcovHC(x, type = 'sss'))

stargazer(title_eigen_vector_log_r,
          title_degree_log_r,
          title_betweeness_log_r,
          title_closeness_log_r, title="Results", align=TRUE, type="latex", out = "Output/Regressions/Regression_iv.txt")
stargazer(title_eigen_vector_log_r,
          title_degree_log_r,
          title_betweeness_log_r,
          title_closeness_log_r, title="Results", align=TRUE, type="html", out = "Output/Regressions/Regression_iv.html")

### two way no IV
title_eigen_vector_nv<- plm(log(titles) ~ eigen_vector +
                              lag(log(titles)) +
                              lag(log(titles), 2) +
                              manufacturing +
                              log(taxes):av_tax +
                              gini_index +
                              log(unemploy) +
                              log(pov_rate) +
                              edu_prop +
                              log(women_employment) +
                              diff(log(total_migration)) +
                              log(Population) +                               
                              frac_dem +
                              hou_chamber +
                              liberal +
                              log(tanf_exp_pu) +
                              fs_exp_pu +
                              log(med_exp_pu),
                            data = new_panel,
                            index = c("Loc_Habitation_State1", "Year"),
                            model = "within",
                            effect = "twoway",
                            na.action = na.omit)

summary(title_eigen_vector_nv)

res_ev_t<-title_eigen_vector_nv$residuals
res_ev_t<-data.frame(n=1:length(res_ev_t), res=res_ev_t)
ggqqplot(res_ev_t$res)
ggplot(res_ev_t, aes(res)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  xlab("Liberal") +
  ylab("Density") 
skewness(res_ev_t$res)
shapiro.test(res_ev_t$res)

title_eigen_vector_nv_sc<-pbgtest(title_eigen_vector_nv,
                                  data = pannel_data,
                                  type ="Chisq")
title_eigen_vector_nv_r<-coeftest(title_eigen_vector_nv, function(x) vcovHC(x, type = 'sss'))

title_degree_nv<- plm(log(titles) ~ degree +
                        lag(log(titles)) +
                        lag(log(titles), 2) +
                        manufacturing +
                        log(taxes):av_tax +
                        gini_index +
                        log(unemploy) +
                        log(pov_rate) +
                        edu_prop +
                        log(women_employment) +
                        diff(log(total_migration)) +
                        log(Population) +                               
                        frac_dem +
                        hou_chamber +
                        liberal +
                        log(tanf_exp_pu) +
                        fs_exp_pu +
                        log(med_exp_pu),
                      data = new_panel,
                      index = c("Loc_Habitation_State1", "Year"),
                      model = "within",
                      effect = "twoway",
                      na.action = na.omit)
summary(title_degree_nv)

res_ev_t<-title_degree_nv$residuals
res_ev_t<-data.frame(n=1:length(res_ev_t), res=res_ev_t)
ggqqplot(res_ev_t$res)
ggplot(res_ev_t, aes(res)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  xlab("Liberal") +
  ylab("Density") 
skewness(res_ev_t$res)
shapiro.test(res_ev_t$res)
title_degree_nv_sc<-pbgtest(title_degree_nv,
                            data = pannel_data,
                            type ="Chisq")
title_degree_nv_r<-coeftest(title_degree_nv, function(x) vcovHC(x, type = 'sss'))


title_betweeness_nv<- plm(log(titles) ~ betweeness +
                            lag(log(titles)) +
                            manufacturing +
                            log(taxes):av_tax +
                            gini_index +
                            log(unemploy) +
                            log(pov_rate) +
                            edu_prop +
                            log(women_employment) +
                            diff(log(total_migration)) +
                            log(Population) +                               
                            frac_dem +
                            hou_chamber +
                            liberal +
                            log(tanf_exp_pu) +
                            fs_exp_pu +
                            log(med_exp_pu),
                          data = new_panel,
                          index = c("Loc_Habitation_State1", "Year"),
                          model = "within",
                          effect = "twoway",
                          na.action = na.omit)

summary(title_betweeness_nv)

res_ev_t<-title_betweeness_nv$residuals
res_ev_t<-data.frame(n=1:length(res_ev_t), res=res_ev_t)
ggqqplot(res_ev_t$res)
ggplot(res_ev_t, aes(res)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  xlab("Liberal") +
  ylab("Density") 
skewness(res_ev_t$res)
shapiro.test(res_ev_t$res)

title_betweeness_sc<-pbgtest(title_betweeness_nv,
                             data = pannel_data_2009,
                             type ="Chisq")
title_betweeness_nv_r<-coeftest(title_betweeness_nv, function(x) vcovHC(x, type = 'sss'))

title_closeness_nv<- plm(log(titles) ~ closeness +
                           lag(log(titles)) +
                           manufacturing +
                           log(taxes):av_tax +
                           gini_index +
                           log(unemploy) +
                           log(pov_rate) +
                           edu_prop +
                           log(women_employment) +
                           diff(log(total_migration)) +
                           log(Population) +                               
                           frac_dem +
                           hou_chamber +
                           liberal +
                           log(tanf_exp_pu) +
                           fs_exp_pu +
                           log(med_exp_pu),
                         data = new_panel,
                         index = c("Loc_Habitation_State1", "Year"),
                         model = "within",
                         effect = "twoway",
                         na.action = na.omit)

summary(title_closeness_nv)

res_ev_t<-title_closeness_nv$residuals
res_ev_t<-data.frame(n=1:length(res_ev_t), res=res_ev_t)
ggqqplot(res_ev_t$res)
ggplot(res_ev_t, aes(res)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  xlab("Liberal") +
  ylab("Density") 
skewness(res_ev_t$res)
shapiro.test(res_ev_t$res)

title_closeness_nv_sc<-pbgtest(title_closeness_nv,
                               data = pannel_data_2009,
                               type ="Chisq")
title_closeness_nv_r<-coeftest(title_closeness_nv, function(x) vcovHC(x, type = 'sss'))


stargazer(title_eigen_vector_nv_r,
          title_degree_nv_r,
          title_betweeness_nv_r,
          title_closeness_nv_r, title="Results", align=TRUE, type="latex", out = "Output/Regressions/Regression_niv.txt")
stargazer(title_eigen_vector_nv_r,
          title_degree_nv_r,
          title_betweeness_nv_r,
          title_closeness_nv_r, title="Results", align=TRUE, type="html", out = "Output/Regressions/Regression_niv.html")

hausman_test_ev<-phtest(title_eigen_vector_log,
                        title_eigen_vector_nv,
                        data = new_panel,
                        index = c("Loc_Habitation_State1", "Year"),
                        model = "within",
                        effect = "twoway",
                        method = c("chisq", "aux"),
                        vcov = vcovHC,
                        na.action = na.omit)
hausman_test_deg<-phtest(title_degree_log,
                         title_degree_nv,
                         data = new_panel,
                         index = c("Loc_Habitation_State1", "Year"),
                         model = "within",
                         effect = "twoway",
                         method = c("chisq", "aux"),
                         vcov = vcovHC,
                         na.action = na.omit)
hausman_test_bet<-phtest(title_betweeness_nv,
                         title_betweeness_log,
                         data = new_panel,
                         index = c("Loc_Habitation_State1", "Year"),
                         model = "within",
                         effect = "twoway",
                         method = c("chisq", "aux"),
                         vcov = vcovHC,
                         na.action = na.omit)
hausman_test_cls<-phtest(title_closeness_log,
                         title_closeness_nv,
                         data = new_panel,
                         index = c("Loc_Habitation_State1", "Year"),
                         model = "within",
                         effect = "twoway",
                         method = c("chisq", "aux"),
                         vcov = vcovHC,
                         na.action = na.omit)


measures<-c("closeness", "degree", "betweeness", "eigen_vector")
data_endog<-new_panel
for(i in measures){
  residuals<-data.frame(number =as.numeric(names(residuals(get(paste0("title_", i, "_nv"))))),
                        residuals= residuals(get(paste0("title_", i , "_nv"))))
  residuals$number<-(nrow(data_endog)-max(residuals$number))+residuals$number
  data_endog[paste0("residuals_",i,"_nv")]<-NA
  data_endog[residuals$number, paste0("residuals_",i,"_nv")]<-residuals$residuals
  jpeg(paste("Output/Endog/Endog_Tables/",i,"/plo_residuals_niv.jpg", sep = ""), width = 500, height = 500)
  ggplot(data_endog, aes(x=sc_removals, y=get(paste0("residuals_",i,"_nv")))) +
    geom_point(aes(colour=titles)) +
    geom_smooth(method=lm) +
    xlab("Secure Communities") +
    ylab("Residuals")
  dev.off()
  jpeg(paste("Output/Endog/Endog_Tables/",i,"/hist_residuals_noiv.jpg", sep = ""), width = 500, height = 500)
  ggplot(data_endog, aes(x=get(paste0("residuals_",i,"_nv")))) +
    geom_histogram(aes(y=..density..),color="black", fill="white") +
    geom_density(alpha=.2, fill="#FF6666") +
    xlab("Residuals") +
    ylab("Density")
  dev.off()
  non_instrument_sk<-skewness(data_endog[paste0("residuals_",i,"_nv")], na.rm = T)
  
  residuals<-data.frame(number =as.numeric(names(residuals(get(paste0("title_", i, "_log"))))),
                        residuals= residuals(get(paste0("title_", i , "_log"))))
  data_endog[paste0("residuals_",i)]<-NA
  data_endog[residuals$number, paste0("residuals_",i)]<-residuals$residuals
  jpeg(paste("Output/Endog/Endog_Tables/",i,"/plot_residuals.jpg", sep = ""), width = 500, height = 500)
  ggplot(data_endog, aes(x=sc_removals, y=get(paste0("residuals_",i)))) +
    geom_point(aes(colour=titles)) +
    geom_smooth(method=lm) +
    xlab("Secure Communities") +
    ylab("Residuals")
  dev.off()
  jpeg(paste("Output/Endog/Endog_Tables/",i,"/hist_residuals.jpg", sep = ""), width = 500, height = 500)
  ggplot(data_endog, aes(get(paste0("residuals_",i)))) +
    geom_histogram(aes(y=..density..),color="black", fill="white") +
    geom_density(alpha=.2, fill="#FF6666") +
    xlab("Residuals") +
    ylab("Density")
  dev.off()
  instrument_sk<-skewness(data_endog[paste0("residuals_",i)], na.rm = T)
  hausman_test<-phtest(get(paste0("title_", i, "_log")),
                       get(paste0("title_", i, "_nv")),
                       data = pannel_data,
                       subset = date>2008,
                       index = c("Loc_Habitation_State1", "Year"),
                       model = "within",
                       effect = "twoway",
                       method = c("chisq", "aux"),
                       vcov = vcovHC)
  
  sh_nv<-shapiro.test(as.numeric(unlist(data_endog[paste0("residuals_",i,"_nv")])))
  sh_iv<-shapiro.test(as.numeric(unlist(data_endog[paste0("residuals_",i)])))
  assign(paste(i, "_df", sep = ""), data.frame(measure=i,
                                               h_p.value=c(hausman_test$p.value),
                                               h_statistic=c(hausman_test$statistic),
                                               inst_sk=instrument_sk,
                                               non_inst_sk=non_instrument_sk,
                                               sh_p.value=sh_nv$p.value,
                                               sh_statistic=sh_nv$statistic,
                                               sh_iv_p.value=sh_iv$p.value,
                                               sh_iv_statistic=sh_iv$statistic))
}




endogeniety_table<-rbind(degree_df, eigen_vector_df, closeness_df, betweeness_df)
write_csv(endogeniety_table, "Output/Endog/endogeniety_table.csv")



library(huxtable)
library(ggpubr)
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
setwd("/Users/juanpabloalvarezvega/Dropbox/Desktop2021/Masters/Thesis/FRC")

new_panel<-read_csv("Data/New_Panel.csv" )


#gsptotal, incomepcap, total_debt_outstanding, total_expenditure, total_revenue, med_spending_own
new_panel$gunbckcheck<-new_panel$gunbckcheck/new_panel$Population
new_panel$taxes_pc<-new_panel$taxes/new_panel$Population
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

#cor matrix, and a cor table
cor(new_panel$hou_chamber, new_panel$titles, use= "complete.obs")
cov(new_panel$hou_chamber, new_panel$titles, use= "complete.obs")


title_eigen_vector_log<- plm(log(titles) ~ eigen_vector +
                               lag(log(titles)) +
                               lag(log(titles), 2) +
                               manufacturing +
                               log(taxes):av_tax +
                               gini_index +
                               log(unemploy) +
                               log(pov_rate) +
                               edu_prop +
                               log(women_employment) +
                               diff(log(total_migration)) +
                               log(Population) +                               
                               frac_dem +
                               hou_chamber +
                               liberal +
                               log(tanf_exp_pu) +
                               fs_exp_pu +
                               log(med_exp_pu)|
                               .-hou_chamber +
                               eigen_vector +
                               temperature +
                               art_employment +
                               cold+
                               manufacturing +
                               log(taxes):av_tax +
                               gini_index +
                               log(unemploy) +
                               log(pov_rate) +
                               edu_prop +
                               log(women_employment) +
                               diff(log(total_migration)) +
                               log(Population) +                               
                               log(tanf_exp_pu) +
                               fs_exp_pu + 
                               log(med_exp_pu),
                             data = new_panel,
                             index = c("Loc_Habitation_State1", "Year"),
                             model = "within",
                             effect = "twoway",
                             na.action = na.omit)
summary(title_eigen_vector_log)
res_ev_t<-title_eigen_vector_log$residuals
res_ev_t<-data.frame(n=1:length(res_ev_t), res=res_ev_t)

ggqqplot(res_ev_t$res)
ggplot(res_ev_t, aes(res)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  xlab("Liberal") +
  ylab("Density") 
skewness(res_ev_t$res)
shapiro.test(res_ev_t$res)

title_eigen_vector_log_sc<-pbgtest(title_eigen_vector_log,
                                   data = pannel_data,
                                   type ="Chisq")
title_eigen_vector_log_r<-coeftest(title_eigen_vector_log, function(x) vcovHC(x, type = 'sss'))




title_degree_log<- plm(log(titles) ~ degree +
                         lag(log(titles)) +
                         lag(log(titles), 2) +
                         manufacturing +
                         log(taxes):av_tax +
                         gini_index +
                         log(unemploy) +
                         log(pov_rate) +
                         edu_prop +
                         log(women_employment) +
                         diff(log(total_migration)) +
                         log(Population) +                               
                         frac_dem +
                         hou_chamber +
                         liberal +
                         log(tanf_exp_pu) +
                         fs_exp_pu +
                         log(med_exp_pu)|
                         .-hou_chamber +
                         degree +
                         temperature +
                         art_employment +
                         cold+
                         manufacturing +
                         log(taxes):av_tax +
                         gini_index +
                         log(unemploy) +
                         log(pov_rate) +
                         edu_prop +
                         log(women_employment) +
                         diff(log(total_migration)) +
                         log(Population) +                               
                         log(tanf_exp_pu) +
                         fs_exp_pu + 
                         log(med_exp_pu),
                       data = new_panel,
                       index = c("Loc_Habitation_State1", "Year"),
                       model = "within",
                       effect = "twoway",
                       na.action = na.omit)
summary(title_degree_log)

res_ev_t<-title_degree_log$residuals
res_ev_t<-data.frame(n=1:length(res_ev_t), res=res_ev_t)

ggqqplot(res_ev_t$res)
ggplot(res_ev_t, aes(res)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  xlab("Liberal") +
  ylab("Density") 
skewness(res_ev_t$res)
shapiro.test(res_ev_t$res)

title_degree_log_sc<-pbgtest(title_degree_log,
                             data = pannel_data,
                             type ="Chisq")
title_degree_log_r<-coeftest(title_degree_log, function(x) vcovHC(x, type = 'sss'))

title_betweeness_log<- plm(log(titles) ~ betweeness +
                             lag(log(titles)) +
                             manufacturing +
                             log(taxes):av_tax +
                             gini_index +
                             log(unemploy) +
                             log(pov_rate) +
                             edu_prop +
                             log(women_employment) +
                             diff(log(total_migration)) +
                             log(Population) +                               
                             frac_dem +
                             hou_chamber +
                             liberal +
                             log(tanf_exp_pu) +
                             fs_exp_pu +
                             log(med_exp_pu)|
                             .-hou_chamber +
                             betweeness +
                             temperature +
                             art_employment +
                             cold+
                             manufacturing +
                             log(taxes):av_tax +
                             gini_index +
                             log(unemploy) +
                             log(pov_rate) +
                             edu_prop +
                             log(women_employment) +
                             diff(log(total_migration)) +
                             log(Population) +                               
                             log(tanf_exp_pu) +
                             fs_exp_pu + 
                             log(med_exp_pu),
                           data = new_panel,
                           index = c("Loc_Habitation_State1", "Year"),
                           model = "within",
                           effect = "twoway",
                           na.action = na.omit)

summary(title_betweeness_log)


res_ev_t<-title_betweeness_log$residuals
res_ev_t<-data.frame(n=1:length(res_ev_t), res=res_ev_t)

ggqqplot(res_ev_t$res)
ggplot(res_ev_t, aes(res)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  xlab("Liberal") +
  ylab("Density") 
skewness(res_ev_t$res)
shapiro.test(res_ev_t$res)

title_betweeness_log_sc<-pbgtest(title_betweeness_log,
                                 data = pannel_data,
                                 type ="Chisq")
title_betweeness_log_r<-coeftest(title_betweeness_log, function(x) vcovHC(x, type = 'sss'))


title_closeness_log<- plm(log(titles) ~ closeness +
                            lag(log(titles)) +
                            manufacturing +
                            log(taxes):av_tax +
                            gini_index +
                            log(unemploy) +
                            log(pov_rate) +
                            edu_prop +
                            log(women_employment) +
                            diff(log(total_migration)) +
                            log(Population) +                               
                            frac_dem +
                            hou_chamber +
                            liberal +
                            log(tanf_exp_pu) +
                            fs_exp_pu +
                            log(med_exp_pu)|
                            .-hou_chamber +
                            closeness +
                            temperature +
                            art_employment +
                            cold+
                            lag(log(titles)) +
                            lag(log(titles), 2) +
                            manufacturing +
                            log(taxes):av_tax +
                            gini_index +
                            log(unemploy) +
                            log(pov_rate) +
                            edu_prop +
                            log(women_employment) +
                            diff(log(total_migration)) +
                            log(Population) +                               
                            log(tanf_exp_pu) +
                            fs_exp_pu + 
                            log(med_exp_pu),
                          data = new_panel,
                          index = c("Loc_Habitation_State1", "Year"),
                          model = "within",
                          effect = "twoway",
                          na.action = na.omit)

summary(title_closeness_log)

res_ev_t<-title_closeness_log$residuals
res_ev_t<-data.frame(n=1:length(res_ev_t), res=res_ev_t)

ggqqplot(res_ev_t$res)
ggplot(res_ev_t, aes(res)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  xlab("Liberal") +
  ylab("Density") 
skewness(res_ev_t$res)
shapiro.test(res_ev_t$res)

title_closeness_log_sc<-pbgtest(title_closeness_log,
                                data = pannel_data,
                                type ="Chisq")
title_closeness_log_r<-coeftest(title_closeness_log, function(x) vcovHC(x, type = 'sss'))

stargazer(title_eigen_vector_log_r,
          title_degree_log_r,
          title_betweeness_log_r,
          title_closeness_log_r, title="Results", align=TRUE, type="latex", out = "Output/Regressions/Regression_iv.txt")
stargazer(title_eigen_vector_log_r,
          title_degree_log_r,
          title_betweeness_log_r,
          title_closeness_log_r, title="Results", align=TRUE, type="html", out = "Output/Regressions/Regression_iv.html")

### two way no IV
title_eigen_vector_nv<- plm(log(titles) ~ eigen_vector +
                              lag(log(titles)) +
                              lag(log(titles), 2) +
                              manufacturing +
                              log(taxes):av_tax +
                              gini_index +
                              log(unemploy) +
                              log(pov_rate) +
                              edu_prop +
                              log(women_employment) +
                              diff(log(total_migration)) +
                              log(Population) +                               
                              frac_dem +
                              hou_chamber +
                              liberal +
                              log(tanf_exp_pu) +
                              fs_exp_pu +
                              log(med_exp_pu),
                            data = new_panel,
                            index = c("Loc_Habitation_State1", "Year"),
                            model = "within",
                            effect = "twoway",
                            na.action = na.omit)

summary(title_eigen_vector_nv)

res_ev_t<-title_eigen_vector_nv$residuals
res_ev_t<-data.frame(n=1:length(res_ev_t), res=res_ev_t)
ggqqplot(res_ev_t$res)
ggplot(res_ev_t, aes(res)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  xlab("Liberal") +
  ylab("Density") 
skewness(res_ev_t$res)
shapiro.test(res_ev_t$res)

title_eigen_vector_nv_sc<-pbgtest(title_eigen_vector_nv,
                                  data = pannel_data,
                                  type ="Chisq")
title_eigen_vector_nv_r<-coeftest(title_eigen_vector_nv, function(x) vcovHC(x, type = 'sss'))

title_degree_nv<- plm(log(titles) ~ degree +
                        lag(log(titles)) +
                        lag(log(titles), 2) +
                        manufacturing +
                        log(taxes):av_tax +
                        gini_index +
                        log(unemploy) +
                        log(pov_rate) +
                        edu_prop +
                        log(women_employment) +
                        diff(log(total_migration)) +
                        log(Population) +                               
                        frac_dem +
                        hou_chamber +
                        liberal +
                        log(tanf_exp_pu) +
                        fs_exp_pu +
                        log(med_exp_pu),
                      data = new_panel,
                      index = c("Loc_Habitation_State1", "Year"),
                      model = "within",
                      effect = "twoway",
                      na.action = na.omit)
summary(title_degree_nv)

res_ev_t<-title_degree_nv$residuals
res_ev_t<-data.frame(n=1:length(res_ev_t), res=res_ev_t)
ggqqplot(res_ev_t$res)
ggplot(res_ev_t, aes(res)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  xlab("Liberal") +
  ylab("Density") 
skewness(res_ev_t$res)
shapiro.test(res_ev_t$res)
title_degree_nv_sc<-pbgtest(title_degree_nv,
                            data = pannel_data,
                            type ="Chisq")
title_degree_nv_r<-coeftest(title_degree_nv, function(x) vcovHC(x, type = 'sss'))


title_betweeness_nv<- plm(log(titles) ~ betweeness +
                            lag(log(titles)) +
                            manufacturing +
                            log(taxes):av_tax +
                            gini_index +
                            log(unemploy) +
                            log(pov_rate) +
                            edu_prop +
                            log(women_employment) +
                            diff(log(total_migration)) +
                            log(Population) +                               
                            frac_dem +
                            hou_chamber +
                            liberal +
                            log(tanf_exp_pu) +
                            fs_exp_pu +
                            log(med_exp_pu),
                          data = new_panel,
                          index = c("Loc_Habitation_State1", "Year"),
                          model = "within",
                          effect = "twoway",
                          na.action = na.omit)

summary(title_betweeness_nv)

res_ev_t<-title_betweeness_nv$residuals
res_ev_t<-data.frame(n=1:length(res_ev_t), res=res_ev_t)
ggqqplot(res_ev_t$res)
ggplot(res_ev_t, aes(res)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  xlab("Liberal") +
  ylab("Density") 
skewness(res_ev_t$res)
shapiro.test(res_ev_t$res)

title_betweeness_sc<-pbgtest(title_betweeness_nv,
                             data = pannel_data_2009,
                             type ="Chisq")
title_betweeness_nv_r<-coeftest(title_betweeness_nv, function(x) vcovHC(x, type = 'sss'))

title_closeness_nv<- plm(log(titles) ~ closeness +
                           lag(log(titles)) +
                           manufacturing +
                           log(taxes):av_tax +
                           gini_index +
                           log(unemploy) +
                           log(pov_rate) +
                           edu_prop +
                           log(women_employment) +
                           diff(log(total_migration)) +
                           log(Population) +                               
                           frac_dem +
                           hou_chamber +
                           liberal +
                           log(tanf_exp_pu) +
                           fs_exp_pu +
                           log(med_exp_pu),
                         data = new_panel,
                         index = c("Loc_Habitation_State1", "Year"),
                         model = "within",
                         effect = "twoway",
                         na.action = na.omit)

summary(title_closeness_nv)

res_ev_t<-title_closeness_nv$residuals
res_ev_t<-data.frame(n=1:length(res_ev_t), res=res_ev_t)
ggqqplot(res_ev_t$res)
ggplot(res_ev_t, aes(res)) +
  geom_histogram(aes(y=..density..),color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  xlab("Liberal") +
  ylab("Density") 
skewness(res_ev_t$res)
shapiro.test(res_ev_t$res)

title_closeness_nv_sc<-pbgtest(title_closeness_nv,
                               data = pannel_data_2009,
                               type ="Chisq")
title_closeness_nv_r<-coeftest(title_closeness_nv, function(x) vcovHC(x, type = 'sss'))


stargazer(title_eigen_vector_nv_r,
          title_degree_nv_r,
          title_betweeness_nv_r,
          title_closeness_nv_r, title="Results", align=TRUE, type="latex", out = "Output/Regressions/Regression_niv.txt")
stargazer(title_eigen_vector_nv_r,
          title_degree_nv_r,
          title_betweeness_nv_r,
          title_closeness_nv_r, title="Results", align=TRUE, type="html", out = "Output/Regressions/Regression_niv.html")

hausman_test_ev<-phtest(title_eigen_vector_log,
                        title_eigen_vector_nv,
                        data = new_panel,
                        index = c("Loc_Habitation_State1", "Year"),
                        model = "within",
                        effect = "twoway",
                        method = c("chisq", "aux"),
                        vcov = vcovHC,
                        na.action = na.omit)
hausman_test_deg<-phtest(title_degree_log,
                         title_degree_nv,
                         data = new_panel,
                         index = c("Loc_Habitation_State1", "Year"),
                         model = "within",
                         effect = "twoway",
                         method = c("chisq", "aux"),
                         vcov = vcovHC,
                         na.action = na.omit)
hausman_test_bet<-phtest(title_betweeness_nv,
                         title_betweeness_log,
                         data = new_panel,
                         index = c("Loc_Habitation_State1", "Year"),
                         model = "within",
                         effect = "twoway",
                         method = c("chisq", "aux"),
                         vcov = vcovHC,
                         na.action = na.omit)
hausman_test_cls<-phtest(title_closeness_log,
                         title_closeness_nv,
                         data = new_panel,
                         index = c("Loc_Habitation_State1", "Year"),
                         model = "within",
                         effect = "twoway",
                         method = c("chisq", "aux"),
                         vcov = vcovHC,
                         na.action = na.omit)


measures<-c("closeness", "degree", "betweeness", "eigen_vector")
data_endog<-new_panel
for(i in measures){
  residuals<-data.frame(number =as.numeric(names(residuals(get(paste0("title_", i, "_nv"))))),
                        residuals= residuals(get(paste0("title_", i , "_nv"))))
  residuals$number<-(nrow(data_endog)-max(residuals$number))+residuals$number
  data_endog[paste0("residuals_",i,"_nv")]<-NA
  data_endog[residuals$number, paste0("residuals_",i,"_nv")]<-residuals$residuals
  jpeg(paste("Output/Endog/Endog_Tables/",i,"/plo_residuals_niv.jpg", sep = ""), width = 500, height = 500)
  ggplot(data_endog, aes(x=sc_removals, y=get(paste0("residuals_",i,"_nv")))) +
    geom_point(aes(colour=titles)) +
    geom_smooth(method=lm) +
    xlab("Secure Communities") +
    ylab("Residuals")
  dev.off()
  jpeg(paste("Output/Endog/Endog_Tables/",i,"/hist_residuals_noiv.jpg", sep = ""), width = 500, height = 500)
  ggplot(data_endog, aes(x=get(paste0("residuals_",i,"_nv")))) +
    geom_histogram(aes(y=..density..),color="black", fill="white") +
    geom_density(alpha=.2, fill="#FF6666") +
    xlab("Residuals") +
    ylab("Density")
  dev.off()
  non_instrument_sk<-skewness(data_endog[paste0("residuals_",i,"_nv")], na.rm = T)
  
  residuals<-data.frame(number =as.numeric(names(residuals(get(paste0("title_", i, "_log"))))),
                        residuals= residuals(get(paste0("title_", i , "_log"))))
  data_endog[paste0("residuals_",i)]<-NA
  data_endog[residuals$number, paste0("residuals_",i)]<-residuals$residuals
  jpeg(paste("Output/Endog/Endog_Tables/",i,"/plot_residuals.jpg", sep = ""), width = 500, height = 500)
  ggplot(data_endog, aes(x=sc_removals, y=get(paste0("residuals_",i)))) +
    geom_point(aes(colour=titles)) +
    geom_smooth(method=lm) +
    xlab("Secure Communities") +
    ylab("Residuals")
  dev.off()
  jpeg(paste("Output/Endog/Endog_Tables/",i,"/hist_residuals.jpg", sep = ""), width = 500, height = 500)
  ggplot(data_endog, aes(get(paste0("residuals_",i)))) +
    geom_histogram(aes(y=..density..),color="black", fill="white") +
    geom_density(alpha=.2, fill="#FF6666") +
    xlab("Residuals") +
    ylab("Density")
  dev.off()
  instrument_sk<-skewness(data_endog[paste0("residuals_",i)], na.rm = T)
  hausman_test<-phtest(get(paste0("title_", i, "_log")),
                       get(paste0("title_", i, "_nv")),
                       data = pannel_data,
                       subset = date>2008,
                       index = c("Loc_Habitation_State1", "Year"),
                       model = "within",
                       effect = "twoway",
                       method = c("chisq", "aux"),
                       vcov = vcovHC)
  
  sh_nv<-shapiro.test(as.numeric(unlist(data_endog[paste0("residuals_",i,"_nv")])))
  sh_iv<-shapiro.test(as.numeric(unlist(data_endog[paste0("residuals_",i)])))
  assign(paste(i, "_df", sep = ""), data.frame(measure=i,
                                               h_p.value=c(hausman_test$p.value),
                                               h_statistic=c(hausman_test$statistic),
                                               inst_sk=instrument_sk,
                                               non_inst_sk=non_instrument_sk,
                                               sh_p.value=sh_nv$p.value,
                                               sh_statistic=sh_nv$statistic,
                                               sh_iv_p.value=sh_iv$p.value,
                                               sh_iv_statistic=sh_iv$statistic))
}




endogeniety_table<-rbind(degree_df, eigen_vector_df, closeness_df, betweeness_df)
write_csv(endogeniety_table, "Output/Endog/endogeniety_table.csv")


