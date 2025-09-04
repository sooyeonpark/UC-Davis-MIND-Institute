options(stringsAsFactors = FALSE)
library(sqldf)

adi_young = totalB_calc(adi_young,adi_young$totalNVB,adi_young$totalVB)
adi_old = totalB_calc(adi_old,adi_old$totalNVB,adi_old$totalVB)
adi_score = totalB_calc(adi_score,adi_score$COMNVTCS,adi_score$COMVT_CS)

#young
adi_discrepancy_young_union = sqldf("SELECT t1.id, t1.visit, t1.adi_age, 'Total A' AS type, t1.totalA AS value_1, t2.SOCT_CS AS value_2 
                              FROM adi_young AS t1 
                              INNER JOIN adi_score AS t2 ON t1.id=t2.id AND t1.visit=t2.visit 
                              WHERE t1.totalA <> t2.SOCT_CS 
                              UNION 
                              SELECT t1.id, t1.visit, t1.adi_age, 'Total B' AS type, t1.totalB AS value_1, t2.totalB AS value_2 
                              FROM adi_young AS t1 
                              INNER JOIN adi_score AS t2 ON t1.id=t2.id AND t1.visit=t2.visit 
                              WHERE t1.totalB <> t2.totalB 
                              UNION 
                              SELECT t1.id, t1.visit, t1.adi_age, 'Total C' AS type, t1.totalC AS value_1, t2.BEHT_CS AS value_2 
                              FROM adi_young AS t1 
                              INNER JOIN adi_score AS t2 ON t1.id=t2.id AND t1.visit=t2.visit 
                              WHERE t1.totalC <> t2.BEHT_CS
                              UNION
                              SELECT t1.id, t1.visit, t1.adi_age, 'Total D' AS type, t1.totalD AS value_1, t2.DEVT_CS AS value_2 
                              FROM adi_young AS t1 
                              INNER JOIN adi_score AS t2 ON t1.id=t2.id AND t1.visit=t2.visit 
                              WHERE t1.totalD <> t2.DEVT_CS;")
adi_discrepancy_young_union[] = lapply(adi_discrepancy_young_union, function(x) if(is.factor(x)) factor(x) else x)

adi_young_discrepancy=sqldf("SELECT t1.id,t1.visit,t1.adi_age,t1.totalA,t1.totalB,t1.totalC,t1.totalD,
t2.SOCT_CS,t2.totalB,t2.BEHT_CS,t2.DEVT_CS
             from adi_young as t1
          INNER JOIN adi_score  as t2
          on t1.id = t2.id
          AND t1.visit = t2.visit 
          where 
            (
            t1.totalA <> t2.SOCT_CS OR
            t1.totalB <> t2.totalB OR
            t1.totalC <> t2.BEHT_CS OR
            t1.totalD <> t2.DEVT_CS
            )
          ")

adi_young_discrepancy$var_discrepancies <- ""
for(i in 1:nrow(adi_young_discrepancy)){
  if((adi_young_discrepancy[i,"totalA"] != adi_young_discrepancy[i,"SOCT_CS"])&(adi_young_discrepancy[i,"totalA"] != 0)&(!is.na(adi_young_discrepancy[i,"totalA"]))){
    adi_young_discrepancy[i,"var_discrepancies"] <- paste0("totalA; ")
  }
  if((adi_young_discrepancy[i,"totalB"] != adi_young_discrepancy[i,"totalB..9"])&(adi_young_discrepancy[i,"totalB"] != 0)){
    adi_young_discrepancy[i,"var_discrepancies"] <- paste0(adi_young_discrepancy[i,"var_discrepancies"],"totalB; ")
  }
  if((adi_young_discrepancy[i,"totalC"] != adi_young_discrepancy[i,"BEHT_CS"])&(adi_young_discrepancy[i,"totalC"] != 0)){
    adi_young_discrepancy[i,"var_discrepancies"] <-paste0(adi_young_discrepancy[i,"var_discrepancies"],"totalC; ")
  }
  if((adi_young_discrepancy[i,"totalD"] != adi_young_discrepancy[i,"DEVT_CS"])&(adi_young_discrepancy[i,"totalD"] != 0)){
    adi_young_discrepancy[i,"var_discrepancies"] <-paste0(adi_young_discrepancy[i,"var_discrepancies"],"totalD; ")
  }
}

#old
adi_discrepancy_old_union = sqldf("SELECT t1.id, t1.visit, t1.adi_age, 'Total A' AS type, t1.totalA AS value_1, t2.SOCT_CS AS value_2 
                              FROM adi_old AS t1 
                                    INNER JOIN adi_score AS t2 ON t1.id=t2.id AND t1.visit=t2.visit 
                                    WHERE t1.totalA <> t2.SOCT_CS 
                                    UNION 
                                    SELECT t1.id, t1.visit, t1.adi_age, 'Total B' AS type, t1.totalB AS value_1, t2.totalB AS value_2 
                                    FROM adi_old AS t1 
                                    INNER JOIN adi_score AS t2 ON t1.id=t2.id AND t1.visit=t2.visit 
                                    WHERE t1.totalB <> t2.totalB 
                                    UNION 
                                    SELECT t1.id, t1.visit, t1.adi_age, 'Total C' AS type, t1.totalC AS value_1, t2.BEHT_CS AS value_2 
                                    FROM adi_old AS t1 
                                    INNER JOIN adi_score AS t2 ON t1.id=t2.id AND t1.visit=t2.visit 
                                    WHERE t1.totalC <> t2.BEHT_CS
                                    UNION
                                    SELECT t1.id, t1.visit, t1.adi_age, 'Total D' AS type, t1.totalD AS value_1, t2.DEVT_CS AS value_2 
                                    FROM adi_old AS t1 
                                    INNER JOIN adi_score AS t2 ON t1.id=t2.id AND t1.visit=t2.visit 
                                    WHERE t1.totalD <> t2.DEVT_CS;")
adi_discrepancy_old_union[] = lapply(adi_discrepancy_old_union, function(x) if(is.factor(x)) factor(x) else x)

adi_old_discrepancy=sqldf("SELECT t1.id,t1.visit,t1.adi_age,t1.totalA,t1.totalB,t1.totalC,t1.totalD,
                            t2.SOCT_CS,t2.totalB,t2.BEHT_CS,t2.DEVT_CS
                            from adi_old as t1
                            INNER JOIN adi_score  as t2
                            on t1.id = t2.id
                            AND t1.visit = t2.visit 
                            where 
                            (
                            t1.totalA <> t2.SOCT_CS OR
                            t1.totalB <> t2.totalB OR
                            t1.totalC <> t2.BEHT_CS OR
                            t1.totalD <> t2.DEVT_CS
                            )
                            ")

adi_old_discrepancy$var_discrepancies <- ""
for(i in 1:nrow(adi_old_discrepancy)){
  if((adi_old_discrepancy[i,"totalA"] != adi_old_discrepancy[i,"SOCT_CS"])
     &(adi_old_discrepancy[i,"totalA"] != 0)&(!is.na(adi_old_discrepancy[i,"totalA"]))){
    adi_old_discrepancy[i,"var_discrepancies"] <- paste0("totalA; ")
  }
  if((adi_old_discrepancy[i,"totalB"] != adi_old_discrepancy[i,"totalB..9"])
     &(adi_old_discrepancy[i,"totalB"] != 0)&(!is.na(adi_old_discrepancy[i,"totalB"]))){
    adi_old_discrepancy[i,"var_discrepancies"] <- paste0(adi_old_discrepancy[i,"var_discrepancies"],"totalB; ")
  }
  if((adi_old_discrepancy[i,"totalC"] != adi_old_discrepancy[i,"BEHT_CS"])
     &(adi_old_discrepancy[i,"totalC"] != 0)&(!is.na(adi_old_discrepancy[i,"totalC"]))){
    adi_old_discrepancy[i,"var_discrepancies"] <-paste0(adi_old_discrepancy[i,"var_discrepancies"],"totalC; ")
  }
  if((adi_old_discrepancy[i,"totalD"] != adi_old_discrepancy[i,"DEVT_CS"])&(adi_old_discrepancy[i,"totalD"] != 0)){
    adi_old_discrepancy[i,"var_discrepancies"] <-paste0(adi_old_discrepancy[i,"var_discrepancies"],"totalD; ")
  }
}

#dfbeta diagnostic -> younger ones
adi_young_discrepancy$check = ""
adi_young_check = data.frame()
adi_young_check = dfbeta_diagnostic(adi_young_discrepancy,adi_young_discrepancy$totalA,
                              adi_young_discrepancy$SOCT_CS,adi_young_check,"total A")
adi_young_check = dfbeta_diagnostic(adi_young_discrepancy,adi_young_discrepancy$totalB,
                              adi_young_discrepancy$totalB..9,adi_young_check,"total B")
adi_young_check = dfbeta_diagnostic(adi_young_discrepancy,adi_young_discrepancy$totalC,
                              adi_young_discrepancy$BEHT_CS,adi_young_check,"total C")
adi_young_check = dfbeta_diagnostic(adi_young_discrepancy,adi_young_discrepancy$totalD,
                              adi_young_discrepancy$DEVT_CS,adi_young_check,"total D")

#dfbeta diagnostic -> older ones
adi_old_discrepancy$check = ""
adi_old_check = data.frame()
adi_old_check = dfbeta_diagnostic(adi_old_discrepancy,adi_old_discrepancy$totalA,
                              adi_old_discrepancy$SOCT_CS,adi_old_check,"total A")
adi_old_check = dfbeta_diagnostic(adi_old_discrepancy,adi_old_discrepancy$totalB,
                              adi_old_discrepancy$totalB..9,adi_old_check,"total B")
adi_old_check = dfbeta_diagnostic(adi_old_discrepancy,adi_old_discrepancy$totalC,
                              adi_old_discrepancy$BEHT_CS,adi_old_check,"total C")
adi_old_check = dfbeta_diagnostic(adi_old_discrepancy,adi_old_discrepancy$totalD,
                              adi_old_discrepancy$DEVT_CS,adi_old_check,"total D")

library(plyr)
names(adi_young_check)[8:11] = c("totalA_Orig","totalB_Orig","totalC_Orig","totalD_Orig")
adi_young_check = ddply(adi_young_check,.(id,visit,adi_age,totalA,totalB,totalC,totalD,
                                          totalA_Orig,totalB_Orig,totalC_Orig,totalD_Orig),summarize,check_list = paste(check,collapse="; "))
names(adi_old_check)[8:11] = c("totalA_Orig","totalB_Orig","totalC_Orig","totalD_Orig")
adi_old_check = ddply(adi_old_check,.(id,visit,adi_age,totalA,totalB,totalC,totalD,
                                      totalA_Orig,totalB_Orig,totalC_Orig,totalD_Orig),summarize,check_list = paste(check,collapse="; "))
adi_check = rbind(adi_young_check,adi_old_check)
write.csv(adi_check,"adi_checklist.csv")

##outlier and discrepancy consolidation
library(sqldf)
adi_outlier_n_discr = sqldf("select t1.id, t1.visit,t1.adi_age, 'ADI' as type, outlier_list, check_list
                        from adi_rescored as t1
                        left join adi_check as t2
                        on t1.id = t2.id")
adi_issue_list = subset(adi_outlier_n_discr, outlier_list != ""|check_list != "")
