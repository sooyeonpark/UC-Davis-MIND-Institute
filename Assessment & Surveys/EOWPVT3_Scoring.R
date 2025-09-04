eowpvt3 = sqlQuery(new_con,"select * from EOWPVT3;",stringsAsFactors=F)

#importing norm tables
con <- odbcConnectAccess2007("S:/MIND/RESEARCH/APP Behavior Data/Local Data Entry/APP Data entry Aug 8 2011.mdb")
eowpvt3_ae = sqlQuery(con,"select * from scr_eowpvt_age_eq;",stringsAsFactors=F)
eowpvt3_ss = sqlQuery(con,"select * from scr_eowpvt_standard_scores;",stringsAsFactors=F)

eowpvt3 = id_visit_changing_into_char(eowpvt3)
eowpvt3_entry_flag = entry_flag(eowpvt3,'eowpvt3')
eowpvt3 = subset(eowpvt3, entry_status == 2)
eowpvt3$entry_date = NULL
if(!is.null(eowpvt3_entry_flag)){
  eowpvt3 = rbind(eowpvt3,eowpvt3_entry_flag[,-ncol(eowpvt3_entry_flag)])
}
eowpvt3 = removing_prefix(eowpvt3,'eowpvt_')

#calculating age
eowpvt3 = fxage(eowpvt3,'id','interview_date')

#changing negative raw scores to NA's
eowpvt3$original_raw = eowpvt3$raw_scr
for(i in 1:nrow(eowpvt3)){
  eowpvt3[i,"raw_scr"] = cbraw(eowpvt3[i,"raw_scr"])
}

#missing data analysis
eowpvt3[,"missing_items_comment"] = ifelse(eowpvt3$original_raw == -1,'Raw score missing: got all practice items wrong',
                                           ifelse(eowpvt3$original_raw == -2,'Raw score missing: attended to task, but could not produce words',
                                                  ifelse(eowpvt3$original_raw == -8,'Raw score missing: did not attend to task',
                                                         ifelse(eowpvt3$original_raw == -9,'Raw score missing: did not attempt testing',''))))

#importing ae values
eowpvt3 = sqldf("select t1.*, age_eq as ae from eowpvt3 as t1 left join eowpvt3_ae on
                t1.raw_scr = eowpvt3_ae.`raw score`")

#obtaining ss
eowpvt3 = sqldf("select t1.*, `standard score` as ss from eowpvt3 as t1 left join eowpvt3_ss on
                t1.raw_scr = eowpvt3_ss.`raw score` and cast(t1.age as int) = eowpvt3_ss.age")

#putting back prefix
eowpvt3 = removing_prefix(eowpvt3,"EOWPVT_")
eowpvt3 = inserting_prefix_into_variables(eowpvt3,"eowpvt_")

#orphaned/duplicate data
eowpvt3_orphaned_data = orphaned_data_consolidate(eowpvt3)
eowpvt3 = orphaned_data_remove(eowpvt3)
eowpvt3_duplicate_data = duplicate_data_consolidate(eowpvt3,"eowpvt_age")
eowpvt3 = duplicate_data_remove(eowpvt3,"eowpvt_age")

#outliers
eowpvt3_scored = eowpvt3[,c(1:2,grep("^eowpvt_age$",names(eowpvt3)),grep("missing",names(eowpvt3)),
                            grep("raw_",names(eowpvt3)),which(names(eowpvt3)=="eowpvt_ae"):ncol(eowpvt3))]
names(eowpvt3_scored)[which(names(eowpvt3_scored)=="eowpvt_raw_scr")]="eowpvt_raw"
names(eowpvt3_scored)[ncol(eowpvt3_scored)] = "eowpvt_ss"
eowpvt3_scored = outlier_list(eowpvt3_scored)
#eowpvt3$outlier_list = eowpvt3_scored$outlier_list
# eowpvt3_outlier_table = sqldf("select id,visit, outlier_list from eowpvt3_scored where outlier_list != ''")
# eowpvt3_scored$outlier_list=NULL

rm(eowpvt3_ae,eowpvt3_ss)