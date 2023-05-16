iv_negbin_ev_ex<- plm(log1p(extremists) ~ log1p(eigen_vector) +
                     
                     log(lgbtq_pop) +
                     log_mirror_wp +
                     log(depression) +
                     state_control:president_party +
                     pov_binary:av_tax +
                     log_farm_change +
                     manufacturing +
                     log(unemployment) +
                     gini_index +
                     poverty_rate +
                     education +
                     log(total_migration) +
                     log(population) +
                     women_employment +
                     president_choice:president_party +
                     log(sc_removals) +
                     log(sc_removals):log1p(eigen_vector)|
                     .-log(sc_removals) 
                   - state_control +
                     
                     log(lgbtq_pop) +
                     log_mirror_wp +
                     pov_binary:av_tax +
                     log(depression) +
                     worker_cars +
                     worker_cars:log1p(eigen_vector) +
                     cold:log1p(eigen_vector) +
                     temperature:log1p(eigen_vector) +
                     president_party:art_employment +
                     art_employment +
                     temperature +
                     cold +
                     log_farm_change +
                     manufacturing +
                     log(unemployment) +
                     gini_index +
                     poverty_rate +
                     women_employment +
                     education +
                     log(population),
                   data = pannel_data_2009,
                   index = c("Loc_Habitation_State1", "Year"),
                   model = "within",
                   effect = "twoway",
                   family = negbin)
summary(iv_negbin_ev_ex)
iv_negbin_ev_sc_ex<-pdwtest(iv_negbin_ev_ex, pannel_data_2009)
iv_negbin_ev_ex<-coeftest(iv_negbin_ev_ex, function(x) vcovHC(x, type = 'sss'))

iv_negbin_deg_ex<- plm(log1p(extremists) ~ log_mirror_degree +
                      
                      log(lgbtq_pop) +
                      log_mirror_wp +
                      log(depression) +
                      state_control:president_party +
                      pov_binary:av_tax +
                      log_farm_change +
                      manufacturing +
                      log(unemployment) +
                      gini_index +
                      poverty_rate +
                      education +
                      log(total_migration) +
                      log(population) +
                      women_employment +
                      president_choice:president_party +
                      log(sc_removals) +
                      log(sc_removals):log_mirror_degree|
                      .-log(sc_removals) 
                    - state_control +
                      
                      log(lgbtq_pop) +
                      log_mirror_wp +
                      pov_binary:av_tax +
                      log(depression) +
                      worker_cars +
                      worker_cars:log_mirror_degree +
                      cold:log_mirror_degree +
                      temperature:log_mirror_degree +
                      president_party:art_employment +
                      art_employment +
                      temperature +
                      cold +
                      log_farm_change +
                      manufacturing +
                      log(unemployment) +
                      gini_index +
                      poverty_rate +
                      women_employment +
                      education +
                      log(population),
                    data = pannel_data_2009,
                    index = c("Loc_Habitation_State1", "Year"),
                    model = "within",
                    effect = "twoway",
                    family = negbin)
summary(iv_negbin_deg_ex)
iv_negbin_deg_sc_ex<-pdwtest(iv_negbin_deg_ex, pannel_data_2009)
iv_negbin_deg_ex<-coeftest(iv_negbin_deg_ex, function(x) vcovHC(x, type = 'sss'))


iv_negbin_bet_ex<- plm(log1p(extremists) ~ log1p(betweeness) +
                      
                      log(lgbtq_pop) +
                      log_mirror_wp +
                      log(depression) +
                      state_control:president_party +
                      pov_binary:av_tax +
                      log_farm_change +
                      manufacturing +
                      log(unemployment) +
                      gini_index +
                      poverty_rate +
                      education +
                      log(total_migration) +
                      log(population) +
                      women_employment +
                      president_choice:president_party +
                      log(sc_removals) +
                      log(sc_removals):log1p(betweeness)|
                      .-log(sc_removals) 
                    - state_control +
                      
                      log(lgbtq_pop) +
                      log_mirror_wp +
                      pov_binary:av_tax +
                      log(depression) +
                      worker_cars +
                      worker_cars:log1p(betweeness) +
                      cold:log1p(betweeness) +
                      temperature:log1p(betweeness) +
                      president_party:art_employment +
                      art_employment +
                      temperature +
                      cold +
                      log_farm_change +
                      manufacturing +
                      log(unemployment) +
                      gini_index +
                      poverty_rate +
                      women_employment +
                      education +
                      log(population),
                    data = pannel_data_2009,
                    index = c("Loc_Habitation_State1", "Year"),
                    model = "within",
                    effect = "twoway",
                    family = negbin)

summary(iv_negbin_bet_ex)
iv_negbin_bet_sc_ex<-pdwtest(iv_negbin_bet_ex, pannel_data_2009)
iv_negbin_bet_ex<-coeftest(iv_negbin_bet_ex, function(x) vcovHC(x, type = 'sss'))


iv_negbin_cls_ex<- plm(log1p(extremists) ~ log(closeness) +
                      
                      log(lgbtq_pop) +
                      log_mirror_wp +
                      log(depression) +
                      state_control:president_party +
                      pov_binary:av_tax +
                      log_farm_change +
                      manufacturing +
                      log(unemployment) +
                      gini_index +
                      poverty_rate +
                      education +
                      log(total_migration) +
                      log(population) +
                      women_employment +
                      president_choice:president_party +
                      log(sc_removals) +
                      log(sc_removals):log(closeness)|
                      .-log(sc_removals) 
                    - state_control +
                      
                      log(lgbtq_pop) +
                      log_mirror_wp +
                      pov_binary:av_tax +
                      log(depression) +
                      worker_cars +
                      worker_cars:log(closeness) +
                      cold:log(closeness) +
                      temperature:log(closeness) +
                      president_party:art_employment +
                      art_employment +
                      temperature +
                      cold +
                      log_farm_change +
                      manufacturing +
                      log(unemployment) +
                      gini_index +
                      poverty_rate +
                      women_employment +
                      education +
                      log(population),
                    data = pannel_data_2009,
                    index = c("Loc_Habitation_State1", "Year"),
                    model = "within",
                    effect = "twoway")
summary(iv_negbin_cls_ex)
iv_negbin_cls_sc_ex<-pdwtest(iv_negbin_cls_ex, pannel_data_2009)
iv_negbin_cls_ex<-coeftest(iv_negbin_cls_ex, function(x) vcovHC(x, type = 'sss'))

stargazer(iv_negbin_ev,
          iv_negbin_deg_ex,
          iv_negbin_bet_ex,
          iv_negbin_cls_ex, title="Results", align=TRUE, type="latex", out = "Output/Regressions/negbin_iv_ex.txt")
stargazer(iv_negbin_ev_ex,
          iv_negbin_deg_ex,
          iv_negbin_bet_ex,
          iv_negbin_cls_ex, title="Results", align=TRUE, type="html", out = "Output/Regressions/negbin_iv_ex.html")

### two way no IV
negbin_niv_ev_ex<- plm(log1p(extremists) ~ log1p(eigen_vector) +
                      
                      log(lgbtq_pop) +
                      log(depression) +
                      state_control:president_party +
                      pov_binary:av_tax +
                      log_mirror_wp +
                      log_farm_change +
                      manufacturing +
                      log(unemployment) +
                      gini_index +
                      poverty_rate +
                      education +
                      log(total_migration) +
                      log(population) +
                      women_employment +
                      president_choice:president_party +
                      log(sc_removals) +
                      log(sc_removals):log1p(eigen_vector),
                    data = pannel_data_2009,
                    index = c("Loc_Habitation_State1", "Year"),
                    model = "pooling",
                    effect = "twoway",
                    family = negbin)
summary(negbin_niv_ev_ex)
negbin_niv_ev_sc_ex<-pdwtest(negbin_niv_ev_ex, pannel_data_2009)
negbin_niv_ev_ex<-coeftest(negbin_niv_ev_ex, function(x) vcovHC(x, type = 'sss'))

negbin_niv_deg_ex<- plm(log1p(extremists) ~ log_mirror_degree +
                       
                       log(lgbtq_pop) +
                       log(depression) +
                       state_control:president_party +
                       pov_binary:av_tax +
                       log_mirror_wp +
                       log_farm_change +
                       manufacturing +
                       log(unemployment) +
                       gini_index +
                       poverty_rate +
                       education +
                       log(total_migration) +
                       log(population) +
                       women_employment +
                       president_choice:president_party +
                       log(sc_removals) +
                       log(sc_removals):log_mirror_degree,
                     data = pannel_data_2009,
                     index = c("Loc_Habitation_State1", "Year"),
                     model = "within",
                     effect = "twoway")

summary(negbin_niv_deg_ex)
negbin_niv_deg_sc_ex<-pdwtest(negbin_niv_deg_ex, pannel_data_2009)
negbin_niv_deg_ex<-coeftest(negbin_niv_deg_ex, function(x) vcovHC(x, type = 'sss'))


negbin_niv_bet_ex<- plm(log1p(extremists) ~ log1p(betweeness) +
                       
                       log(lgbtq_pop) +
                       log(depression) +
                       state_control:president_party +
                       pov_binary:av_tax +
                       log_mirror_wp +
                       log_farm_change +
                       manufacturing +
                       log(unemployment) +
                       gini_index +
                       poverty_rate +
                       education +
                       log(total_migration) +
                       log(population) +
                       women_employment +
                       president_choice:president_party +
                       log(sc_removals) +
                       log(sc_removals):log1p(betweeness),
                     data = pannel_data_2009,
                     index = c("Loc_Habitation_State1", "Year"),
                     model = "within",
                     effect = "twoway",
                     family = negbin)

summary(negbin_niv_bet_ex)
negbin_niv_bet_sc_ex<-pdwtest(negbin_niv_bet_ex, pannel_data_2009)
negbin_niv_bet_ex<-coeftest(negbin_niv_bet_ex, function(x) vcovHC(x, type = 'sss'))


negbin_niv_cls_ex<- plm(log1p(extremists) ~ log(closeness) +
                       
                       log(lgbtq_pop) +
                       log(depression) +
                       state_control:president_party +
                       pov_binary:av_tax +
                       log_mirror_wp +
                       log_farm_change +
                       manufacturing +
                       log(unemployment) +
                       gini_index +
                       poverty_rate +
                       education +
                       log(total_migration) +
                       log(population) +
                       women_employment +
                       president_choice:president_party +
                       log(sc_removals) +
                       log(sc_removals):log(closeness),
                     data = pannel_data_2009,
                     index = c("Loc_Habitation_State1", "Year"),
                     model = "within",
                     effect = "twoway")
summary(negbin_niv_cls_ex)
negbin_niv_cls_sc_ex<-pdwtest(negbin_niv_cls_ex, pannel_data_2009)
negbin_niv_cls_ex<-coeftest(negbin_niv_cls_ex, function(x) vcovHC(x, type = 'sss'))


stargazer(negbin_niv_ev_ex,
          negbin_niv_deg_ex,
          negbin_niv_bet_ex,
          negbin_niv_cls_ex, title="Results", align=TRUE, type="latex", out = "Output/Regressions/negbin_niv_ex.txt")
stargazer(negbin_niv_ev_ex,
          negbin_niv_deg_ex,
          negbin_niv_bet_ex,
          negbin_niv_cls_ex, title="Results", align=TRUE, type="html", out = "Output/Regressions/negbin_niv_ex.html")
