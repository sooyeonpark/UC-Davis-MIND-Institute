#pulling the data table
rbs_t1 = exportRecords(ace_con,forms=c("participant_information","rbs"),events="t1_arm_1",labels=F,stringsAsFactors=F)
rbs_t1 = rbs_t1[grepl("Co",rbs_t1$rbs_complete)&!is.na(rbs_t1$rbs_timestamp),-(2:grep("participant_information_complete",names(rbs_t1)))]
rbs_t1$visit = 1
rbs_t3 = exportRecords(ace_con,forms=c("participant_information","rbs"),events="t3_arm_1",labels=F,stringsAsFactors=F)
rbs_t3 = rbs_t3[grepl("Co",rbs_t3$rbs_complete)&!is.na(rbs_t3$rbs_timestamp),-(2:grep("participant_information_complete",names(rbs_t3)))]
rbs_t3$visit = 3
rbs_t5 = exportRecords(ace_con,forms=c("participant_information","rbs"),events="t5_arm_1",labels=F,stringsAsFactors=F)
rbs_t5 = rbs_t5[grepl("Co",rbs_t5$rbs_complete)&!is.na(rbs_t5$rbs_timestamp),-(2:grep("participant_information_complete",names(rbs_t5)))]
rbs_t5$visit = 5
rbs_rc = rbind(rbs_t1,rbs_t3,rbs_t5)
rbs = sqlQuery(new_con,"select * from Repetitive_Behavior_Scale;",stringsAsFactors=F)
rbs_staar = sqlQuery(con4,"select * from Repetitive_Behavior_Scale;",stringsAsFactors=F)

#initial formatting
rbs = id_visit_changing_into_char(rbs)
rbs_entry_flag = entry_flag(rbs,'rbs')

#combining tables
names(rbs_staar)[5:6] = c("rbs_resp","rbs_date")
rbs = rbind.fill(rbs,rbs_staar)
rm(rbs_staar)

#dealing with single entries
rbs = subset(rbs,entry_status==2 & visit != '4P')
if(!is.null(rbs_entry_flag)){
  rbs = rbind(rbs,rbs_entry_flag[,-ncol(rbs_entry_flag)])
}
rbs = removing_prefix(rbs,"rbs_")
rbs_rc = study_id_to_id(rbs_rc,"rbs_")
rbs = identify_same_data(rbs_rc,rbs)
rbs = rbind.fill(rbs,rbs_rc)

#calculating age
rbs = fxage(rbs,'id','date')

#negative scores to NA's
for(j in which(names(rbs)=='1'):which(names(rbs)=='43')){ 
  rbs[,j] = cbraw(rbs[,j])
}

#overall missing data count
rbs = count_missing_items(rbs,'1','43')

#3-factor scoring
sr_items = paste0('',c(1:6,40:43))
si_items = paste0('',7:14)
crs_items = paste0('',15:39)
total = c(sr_items,si_items,crs_items)
rbs = summing_items_per_row(rbs,list(sr_items,si_items,crs_items,total),
                            list('3_factor_sr','3_factor_si','3_factor_crs','3_factor_total'),F)                        

#5-factor scoring
st_items = paste0('',c(1:6,22,42:43))
si_items = paste0('',7:14)
c_items = paste0('',15:20)
rs_items = paste0('',c(26:28,30:35,37:39))
r_items = paste0('',c(36,40,41))
total = c(st_items,si_items,c_items,rs_items,r_items)
rbs = summing_items_per_row(rbs,list(sr_items,si_items,c_items,rs_items,r_items,total),
                            list('5_factor_stereotypy','5_factor_self_injurious',
                                 '5_factor_compulsive','5_factor_ritsame',
                                 '5_factor_restricted','5_factor_total'),F)                

#6-factor scoring
st_items = paste0('',1:6)
si_items = paste0('',7:14)
c_items = paste0('',15:22)
rit_items = paste0('',23:28)
s_items = paste0('',29:39)
r_items = paste0('',40:43)
total = c(st_items,si_items,c_items,rit_items,s_items,r_items)
rbs = comment_missing_data(rbs,list(st_items,si_items,c_items,rit_items,s_items,r_items),
                           list('6_factor_stereotypy','6_factor_self_injurious','6_factor_compulsive',
                                '6_factor_ritualistic','6_factor_sameness','6_factor_restricted'))
rbs = summing_items_per_row(rbs,list(st_items,si_items,c_items,rit_items,s_items,r_items,total),
                            list('6_factor_stereotypy','6_factor_self_injurious','6_factor_compulsive',
                                 '6_factor_ritualistic','6_factor_sameness','6_factor_restricted','6_factor_total'),F)

#putting prefix back in
rbs = inserting_prefix_into_variables(rbs,"rbs_")

#orphaned/duplicate data
rbs_orphaned_data = orphaned_data_consolidate(rbs)
rbs = orphaned_data_remove(rbs)
rbs_duplicate_data = duplicate_data_consolidate(rbs,"rbs_age")
rbs = duplicate_data_remove(rbs,"rbs_age")

#taking out necessary columns
rbs_scored = rbs[grep("[0-9]{6}-[0-9]{3}",rbs$id),c(1:2,grep("_age$",names(rbs)),grep("missing",names(rbs)),grep("3_factor",names(rbs)),grep("5_factor",names(rbs)),grep("6_factor",names(rbs)))]
rbs_scored_staar = rbs[grep("^[0-9]{4}$",rbs$id),c(1:2,grep("_age$",names(rbs)),grep("missing",names(rbs)),grep("3_factor",names(rbs)),grep("5_factor",names(rbs)),grep("6_factor",names(rbs)))]

#outliers
rbs_scored = outlier_list(rbs_scored)
rbs_scored_staar = outlier_list(rbs_scored_staar)
names(rbs_scored)[ncol(rbs_scored)] = "rbs_outlier_list"
names(rbs_scored_staar)[ncol(rbs_scored_staar)] = "rbs_outlier_list"
# rbs_outlier_table = sqldf("select id,visit,outlier_list from rbs_scored where outlier_list != ''")
# rbs_outlier_table_staar = sqldf("select id,visit,outlier_list from rbs_scored_staar where outlier_list != ''")
# rbs = merge(rbs,rbind(rbs_outlier_table,rbs_outlier_table_staar),all.x=T)
# rbs_scored$outlier_list=NULL
# rbs_scored_staar$outlier_list = NULL

#archiving the data
write.csv(rbs_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/rbs_scored.csv",row.names = F)
write.csv(rbs_scored_staar,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived_staar/rbs_scored.csv",row.names = F)
write.csv(rbs[-grep("^[0-9]{4}$",rbs$id),c(1:2,grep("_resp$",names(rbs)),grep("_1$",names(rbs)):grep("_43$",names(rbs)))],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/rbs_items.csv",row.names = F)
write.csv(rbs[grep("^[0-9]{4}$",rbs$id),c(1:2,grep("_resp$",names(rbs)),grep("_1$",names(rbs)):grep("_43$",names(rbs)))],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived_staar/rbs_items.csv",row.names = F)

#cleaning up
rm(st_items,si_items,sr_items,crs_items,c_items,r_items,rit_items,rs_items,s_items,total,
   rbs_t1,rbs_t3,rbs_t5,rbs_rc)
