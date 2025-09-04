#pulling the data table
scq_t1 = exportRecords(ace_con,forms=c("participant_information","scq"),events="t1_arm_1",labels=F,stringsAsFactors=F)
scq_t1 = scq_t1[grepl("Co",scq_t1$scq_complete)&!is.na(scq_t1$scq_timestamp),-(2:grep("participant_information_complete",names(scq_t1)))]
scq_t1$visit = 1
scq_t3 = exportRecords(ace_con,forms=c("participant_information","scq"),events="t3_arm_1",labels=F,stringsAsFactors=F)
scq_t3 = scq_t3[grepl("Co",scq_t3$scq_complete)&!is.na(scq_t3$scq_timestamp),-(2:grep("participant_information_complete",names(scq_t3)))]
scq_t3$visit = 3
scq_t5 = exportRecords(ace_con,forms=c("participant_information","scq"),events="t5_arm_1",labels=F,stringsAsFactors=F)
scq_t5 = scq_t5[grepl("Co",scq_t5$scq_complete)&!is.na(scq_t5$scq_timestamp),-(2:grep("participant_information_complete",names(scq_t5)))]
scq_t5$visit = 5
scq_rc = rbind(scq_t1,scq_t3,scq_t5)
scq = sqlQuery(new_con,"select * from SCQ;",stringsAsFactors=F)
scq_staar = sqlFetch(con4,"SCQ",stringsAsFactors=F)
scq = rbind.fill(scq,scq_staar)
rm(scq_staar)

#id and visit into characters, removing single-entered rows and prefix
scq = id_visit_changing_into_char(scq)
scq_entry_flag = entry_flag(scq,'scq')
if(!is.null(scq_entry_flag)){
  scq = rbind(scq,scq_entry_flag[,-ncol(scq_entry_flag)])
}
scq = subset(scq,entry_status==2)
scq = removing_prefix(scq,"scq_")
scq_rc = study_id_to_id(scq_rc,"scq_")
scq = identify_same_data(scq_rc,scq)
scq = rbind.fill(scq,scq_rc)

#calculating age
scq = fxage(scq,'id','date')

#dealing with "Missing" response to NA
for(j in which(names(scq)=="1"):which(names(scq)=="40")){
  for(i in 1:nrow(scq)){
    scq[i,j] = ifelse(!is.na(scq[i,j]) & scq[i,j]=="Missing","",scq[i,j])
  }
}

##scoring
#converting yes's and no's into score
common_nonreverse_items = paste0('',c(8,10:18))
common_reverse_items = paste0('',c(9,19:40))
noncommon_nonreverse_items = paste0('',c(3:7))
noncommon_reverse_items = paste0('',2)

#summing the scores
scq$total = 0
for (i in 1:nrow(scq)){
  if(scq[i,"1"] == "No"){
    for(j in common_nonreverse_items){
      scq$total[i] = ifelse(scq[i,j]=="",NA,ifelse(scq[i,j] =="Yes",scq$total[i]+1,scq$total[i]))
    }
    for(j in common_reverse_items){
      scq$total[i] = ifelse(scq[i,j]=="",NA,ifelse(scq[i,j] =="No",scq$total[i]+1,scq$total[i]))
    }
  }
  else{
    for(j in c(common_nonreverse_items,noncommon_nonreverse_items)){
      scq$total[i] = ifelse(scq[i,j]=="",NA,ifelse(scq[i,j] =="Yes",scq$total[i]+1,scq$total[i]))
    }
    for(j in c(common_reverse_items,noncommon_reverse_items)){
      scq$total[i] = ifelse(scq[i,j]=="",NA,ifelse(scq[i,j] =="No",scq$total[i]+1,scq$total[i]))
    }
  }
}


#putting back prefix
scq = inserting_prefix_into_variables(scq,"scq_")

#orphaned/duplicate data
scq_orphaned_data = orphaned_data_consolidate(scq)
scq = orphaned_data_remove(scq)
scq_duplicate_data = duplicate_data_consolidate(scq,"scq_age")
scq = duplicate_data_remove(scq,"scq_age")

#obtaining outliers
scq_scored = scq[grep("[0-9]{6}-[0-9]{3}",scq$id),c(1:2,grep("_age$",names(scq)),grep("_total$",names(scq)))]
scq_scored_staar = scq[grep("^[0-9]{4}$",scq$id),c(1:2,grep("_age$",names(scq)),grep("_total$",names(scq)))]
scq_scored = outlier_list(scq_scored)
scq_scored_staar = outlier_list(scq_scored_staar)
names(scq_scored)[ncol(scq_scored)] = "scq_outlier_list"
names(scq_scored_staar)[ncol(scq_scored_staar)] = "scq_outlier_list"

#archiving the table
write.csv(scq_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/scq_scored.csv",row.names = F)
write.csv(scq_scored_staar,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived_staar/scq_scored.csv",row.names = F)
write.csv(scq[grep("^[0-9]{4}$",scq$id),c(1:2,grep("_resp$",names(scq)),grep("_1$",names(scq)):grep("_40$",names(scq)))],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived_staar/scq_items.csv",row.names = F)
write.csv(scq[-grep("^[0-9]{4}$",scq$id),c(1:2,grep("_resp$",names(scq)),grep("_1$",names(scq)):grep("_40$",names(scq)))],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/scq_items.csv",row.names = F)

#cleaning up
rm(common_nonreverse_items,common_reverse_items,noncommon_nonreverse_items,noncommon_reverse_items,
   scq_t1,scq_t3,scq_t5,scq_rc)
