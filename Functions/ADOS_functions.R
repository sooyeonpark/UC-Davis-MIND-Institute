ados_indivicual_item_conversion = function(x){
  y=ifelse(is.na(x),NA,ifelse(x==8,0,ifelse(x==3,2,ifelse(x>=0 & x<=2,x,NA))))
}

ados_codes2scores = function(dt,col1,col2){
  #dt = data frame that you want to convert from codes to scores
  #col1 = starting column (characters)
  #col2 = ending column (in characters)
  for(j in which(names(dt)==col1):which(names(dt)==col2)){
    for(i in 1:nrow(dt)){
      dt[i,j] = ados_indivicual_item_conversion(dt[i,j])
    }
    dt[,j] = as.numeric(dt[,j])
  }
  return(dt)
}

ados_conditional_summation = function(dt,col_name,condition_col,item_list1,item_list2){
  #dt = data table which contains columns that you want to sum
  #condition_col = a column in dt to determine the condition (in characters)
  #col_name = name of the column created (in characters)
  #item_list1 = lists of indiv items in "if" condition (global variable)
  #item_list2 = lists of indiv items in "else" condition (global variable)
  for(i in 1:nrow(dt)){
    dt[i,col_name] = ifelse(dt[i,condition_col] == 1, sum(dt[i,item_list1],na.rm=F), sum(dt[i,item_list2],na.rm=F))
  }
  return(dt)
}

ados_severity_scores = function(dt){
  for(i in 1:nrow(dt)){
    dt$ados_age_corrected = ifelse(floor(dt$age)<24,24,ifelse(floor(dt$age)>168,168,dt$age))
    dt[i,"severity"] = ifelse(!is.na(dt[i,"sarrb_total"]),
                              ados_sev_tbl[which(dt$module[i]==ados_sev_tbl$module
                                          & dt$no_words[i] == ados_sev_tbl$no_words
                                          & dt$sarrb_total[i] >= ados_sev_tbl$total_min
                                          & dt$sarrb_total[i] <= ados_sev_tbl$total_max
                                          & floor(dt$ados_age_corrected[i]) >= ados_sev_tbl$age_min
                                          & floor(dt$ados_age_corrected[i]) <= ados_sev_tbl$age_max)
                                          ,"severity_score"],NA)
    dt[i,"sa_severity"] = ifelse(!is.na(dt[i,"sa_total"]),
                                 ados_sev_dom_tbl[which(dt$module[i]==ados_sev_dom_tbl$module
                                                  & dt$no_words[i] == ados_sev_dom_tbl$no_words
                                                  & dt$sa_total[i] >= ados_sev_dom_tbl$total_min
                                                  & dt$sa_total[i] <= ados_sev_dom_tbl$total_max
                                                  & floor(dt$ados_age_corrected[i]) >= ados_sev_dom_tbl$age_min
                                                  & floor(dt$ados_age_corrected[i]) <= ados_sev_dom_tbl$age_max
                                                  & ados_sev_dom_tbl$domain == "social affect")
                                                  ,"severity_score"],NA)
    dt[i,"rrb_severity"] = ifelse(!is.na(dt[i,"rrb_total"]),
                                  ados_sev_dom_tbl[which(dt$module[i]==ados_sev_dom_tbl$module
                                                   & dt$no_words[i] == ados_sev_dom_tbl$no_words
                                                   & dt$rrb_total[i] >= ados_sev_dom_tbl$total_min
                                                   & dt$rrb_total[i] <= ados_sev_dom_tbl$total_max
                                                   & floor(dt$ados_age_corrected[i]) >= ados_sev_dom_tbl$age_min
                                                   & floor(dt$ados_age_corrected[i]) <= ados_sev_dom_tbl$age_max
                                                   & ados_sev_dom_tbl$domain == "repetitive bx")
                                                   ,"severity_score"],NA)
  }
  return(dt)
}
