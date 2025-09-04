#importing table from ACCESS and REDCap
aqc_t5 = exportRecords(ace_con,forms=c("participant_information","alexithymia"),events="t5_arm_2",labels=F,stringsAsFactors=F)
aqc_t5 = aqc_t5[grepl("Co",aqc_t5$alexithymia_complete)&!is.na(aqc_t5$alexithymia_timestamp),-(2:grep("participant_information_complete",names(aqc_t5)))]
aqc_t5$visit = 5
aqc = sqlFetch(new_con,"Alexithymia",stringsAsFactors=F)

aqc = id_visit_changing_into_char(aqc)
aqc_entry_flag = entry_flag(aqc,'aqc')
aqc = subset(aqc,entry_status==2)
names(aqc) = gsub("acq_","aqc_",names(aqc))
names(aqc_t5) = gsub("acq_","aqc_",names(aqc_t5))
aqc = removing_prefix(aqc,"aqc_")
aqc_t5 = study_id_to_id(aqc_t5,"aqc_")
aqc = identify_same_data(aqc_t5,aqc)
aqc = rbind.fill(aqc,aqc_t5)

#calculating age
aqc = fxage(aqc,'id','date')

#changing the categories into numbers
for(j in paste0('',1:20)){
  aqc[,j] = ifelse(aqc[,j]=="Often True",2,
                   ifelse(aqc[,j]=="Sometimes True",1,
                          ifelse(aqc[,j]=="Not True",0,NA)))
  aqc[,j] = as.numeric(aqc[,j])
}

#dealing with reverse items
reverse_items = paste0('',c(4:5,10,18:19))
for(j in reverse_items){
  aqc[,j] = 3 - aqc[,j]
}

#list of items
dif_items = paste0('',c(1,3,6:7,9,13:14))
ddf_items = paste0('',c(2,4,11:12,17))
eot_items = paste0('',c(5,8,10,15:16,18:20))

#missing data analysis
aqc = count_missing_items(aqc,'1','20')
aqc = comment_missing_data(aqc,list(dif_items,ddf_items,eot_items),list('dif','ddf','eot'))

#obtaining raw total
aqc = summing_items_per_row(aqc,list(dif_items,ddf_items,eot_items),list('dif_total','ddf_total','eot_total'),F)

#putting back prefix
aqc = inserting_prefix_into_variables(aqc,'aqc_')

#orpahned/duplicate data
aqc_orphaned_data = orphaned_data_consolidate(aqc)
aqc = orphaned_data_remove(aqc)
aqc_duplicate_data = duplicate_data_consolidate(aqc,'aqc_age')
aqc = duplicate_data_remove(aqc,'aqc_age')

#outliers
aqc_outliers = aqc[,c(1:2,grep("_age$",names(aqc)),grep("_total$",names(aqc)))]
aqc_outliers = outlier_list(aqc_outliers)
aqc$aqc_outlier_list = aqc_outliers$outlier_list

#archiving the data
aqc_scored = aqc[,c(1:2,grep("age$",names(aqc)):ncol(aqc))]
write.csv(aqc_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/aqc_scored.csv",row.names=F)
write.csv(aqc[,c(1:2,grep("_[0-9]+$",names(aqc)))],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/aqc_items.csv",row.names=F)

#clean up
rm(dif_items,ddf_items,eot_items,aqc_outliers,reverse_items,aqc_t5)
