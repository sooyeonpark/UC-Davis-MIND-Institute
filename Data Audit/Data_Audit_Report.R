data_missing_certainNsuspected = subset(data,possible_solution %in% c("","NA","Inquire Brianna to check"))

# #proportion of participants engaging in missing data at least in one time point
# length(grep("[0-9]{6}-[0-9]{3}",subj$subj_id)) #total primary participant
# length(unique(data_missing_certainNsuspected$id))
# 
# #propotion of data from ineligible participants among missing data
# length(unique(data_missing_certainNsuspected$id))
# missing_ineligible = subset(data_missing_certainNsuspected,id %in% subj[subj$app_diagnosis=="Ineligible","subj_id"]==T)
# length(unique(missing_ineligible$id))
# 
# #proportion of available data among ineligible participants
# length(unique(missing_ineligible$id))
# length(which(unique(missing_ineligible$id) %in% master_visit2$id==T))

#spliting between for-sure-missing and suspected-missing
# missing_id_visit = ddply(data_missing_certainNsuspected[data_missing_certainNsuspected$possible_solution=="NA",2:4],.(id,visit),summarize,measure_list=paste0(measure,collapse="; "))
# missing_id_visit = subset(missing_id_visit,id %in% subj[subj$app_diagnosis=="Ineligible","subj_id"]==F)
# table(missing_id_visit$visit)
suspected_missing_id_visit = ddply(data_missing_certainNsuspected[data_missing_certainNsuspected$possible_solution!="NA"
                                                                  & !(data_missing_certainNsuspected$task_status %in% c("Not Given","N/A","Not Applicable","Not collected","Not Returned","Not Entered","refused","To Be Rescheduled","Scheduled")),2:4],.(id,visit),summarize,measure_list=paste0(measure,collapse="; "))
suspected_missing_id_visit = subset(suspected_missing_id_visit,id %in% subj[subj$app_diagnosis=="Ineligible","subj_id"]==F)
suspected_missing_id_visit = merge(suspected_missing_id_visit,subj[,c(1,2,9)],by.x="id",by.y="subj_id",all.x=T)
table(suspected_missing_id_visit$visit)

##fixing suspected missing data list
#1) getting rid of TD STI's
suspected_missing_id_visit$measure_list = ifelse(suspected_missing_id_visit$app_diagnosis=="TD" & grepl("STI",suspected_missing_id_visit$measure_list),
                                             gsub("STI","",suspected_missing_id_visit$measure_list),suspected_missing_id_visit$measure_list)
#2) getting rid of ASD SCQ's for T1 and T3 APP & TD APP T3
suspected_missing_id_visit$measure_list = ifelse(suspected_missing_id_visit$app_diagnosis=="ASD" & suspected_missing_id_visit$study_cohort == "APP" & grepl("SCQ",suspected_missing_id_visit$measure_list),
                                             gsub("SCQ","",suspected_missing_id_visit$measure_list),suspected_missing_id_visit$measure_list)
suspected_missing_id_visit$measure_list = ifelse(suspected_missing_id_visit$app_diagnosis=="TD" & suspected_missing_id_visit$study_cohort == "APP" & grepl("SCQ",suspected_missing_id_visit$measure_list) & suspected_missing_id_visit$visit=="3",
                                             gsub("SCQ","",suspected_missing_id_visit$measure_list),suspected_missing_id_visit$measure_list)
suspected_missing_id_visit = suspected_missing_id_visit[!grepl("^[[:space:]]*[;]*$",suspected_missing_id_visit$measure_list),]
suspected_missing_id_visit$measure_list = gsub("[[:space:]][;]{1}[[:space:]]"," ",suspected_missing_id_visit$measure_list)
suspected_missing_id_visit$measure_list = gsub("^[;]*[[:space:]]","",suspected_missing_id_visit$measure_list)
suspected_missing_id_visit$measure_list = gsub("[;]+[[:space:]]$","",suspected_missing_id_visit$measure_list)

#missing data by measure
char_lists_to_table(missing_id_visit[,"measure_list"],"; ")
write.csv(suspected_missing_id_visit,"S:/MIND/RESEARCH/APP/APP Database/7 - Data Auditing/suspected_missing_data.csv",row.names=F)
char_lists_to_table(suspected_missing_id_visit[,"measure_list"],"; ")

#orphaned data
char_lists_to_table(orphaned_data_list[,"measure_list"],", ")

#duplicate data
char_lists_to_table(duplicate_data_list[,"measure_list"],", ")

#write.csv(char_lists_to_table(missing_id_visit[,"measure_list"],"; "),"Data Audit/missing_data_measures.csv",row.names=F)
#write.csv(char_lists_to_table(suspected_missing_id_visit[,"measure_list"],"; "),"Data Audit/missing_data_measures.csv",row.names=F)
write.csv(char_lists_to_table(orphaned_data_list[,"measure_list"],", "),"Data Audit/missing_data_measures.csv",row.names=F)
write.table(char_lists_to_table(duplicate_data_list[,"measure_list"],", "),"Data Audit/missing_data_measures.csv",row.names=F,col.names=T,sep=",",append=T)

#clean up
#rm(missing_ineligible,measure_list_flagged)
