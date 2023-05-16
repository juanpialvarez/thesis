

  data_ivtest<-new_panel
  relevance_1<- plm(hou_chamber ~
                      patents +
                      art_employment +
                      cold +
                     rain_high +
                      betweeness +
                      closeness +
                      eigen_vector +
                      degree +
                      log(taxes_pc):av_tax +
                      log(gsproduct_pc) +
                      gini_index +
                      log(unemploy) +
                      pov_rate +
                      less_hs +
                      high_s +
                      log(total_migration) +
                      log(Population) +   
                      frac_dem +
                      liberal +
                      log(fs_exp_pu) +
                      log(med_exp_pu),
                    data = new_panel,
                    index = c('Loc_Habitation_State1', 'Year'),
                    model = 'within',
                    effect = 'twoway')
                               
  
  summary(relevance_1)
  
  sc_all<-linearHypothesis(relevance_1, 
                           c(
                             "patents=0",
                             "art_employment=0",
                             "cold=0",
                             "rain_high=0"
                           ), 
                           vcov = vcovHC, type = "HC1")
  
  residuals<-data.frame(number =as.numeric(names(residuals(relevance_1))),
                        residuals=residuals(relevance_1))
  residuals$number<-(nrow(data_ivtest)-max(residuals$number))+residuals$number
  data_ivtest$residuals<-NA
  data_ivtest$residuals[residuals$number]<-residuals$residuals
  
  jtest_sc <- plm(residuals ~
                    patents +
                    art_employment +
                    cold +
                    rain_high +
                    betweeness +
                    closeness +
                    eigen_vector +
                    degree +
                    log(taxes_pc):av_tax +
                    log(gsproduct_pc) +
                    gini_index +
                    log(unemploy) +
                    pov_rate +
                    high_s +
                    less_hs +
                    log(total_migration) +
                    log(Population) +   
                    frac_dem +
                    liberal +
                    log(fs_exp_pu) +
                    log(med_exp_pu),
                  data = data_ivtest,
                  index = c('Loc_Habitation_State1', 'Year'),
                  model = 'within',
                  effect = 'twoway')
  summary(jtest_sc)
  jtest_sc_sig <- linearHypothesis(jtest_sc, 
                                   c(
                                     "patents=0",
                                     "art_employment=0",
                                     "cold=0",
                                     "rain=0"
                                   ), 
                                   test = "Chisq")
  
  jtest_sc<-pchisq(jtest_sc_sig[2, 3], df = 1, lower.tail = FALSE)
  
  assign(paste("all", "df", sep = ""),
         data.frame(centrality_meeasure=rep("all", 2),
                    test=c("relevance", "over_r"),
                    chi_squared=c(sc_all$Chisq[2], jtest_sc_sig$Chisq[2]),
                    P_value=c(sc_all$`Pr(>Chisq)`[2], jtest_sc))
  )

count_reg<-rowSums(is.na(regs_panel))
regs_panel$fitted_together<-NA
regs_panel$fitted_together[which(count_reg==0)]<-fitted(relevance_1)

IV_relevance_table<-rbind(alldf)
IV_relevance_table$standard<-0
IV_relevance_table$standard[which(IV_relevance_table$P_value<0.1)]<-"*"
IV_relevance_table$standard[which(IV_relevance_table$P_value<0.05)]<-"**"
IV_relevance_table$standard[which(IV_relevance_table$P_value<0.01)]<-"***"
IV_relevance_table$standard[which(IV_relevance_table$P_value>0.1)]<-"."
write_csv(IV_relevance_table, "Output/instrument_relevance_together.csv")
