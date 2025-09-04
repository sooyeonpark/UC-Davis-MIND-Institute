#querying the data
cbq_t1 = exportRecords(ace_con,forms=c("participant_information","cbq"),events="t1_arm_1",labels=F,stringsAsFactors=F)
cbq_t1 = cbq_t1[grepl("Co",cbq_t1$cbq_complete)&!is.na(cbq_t1$cbq_timestamp),-(2:grep("participant_information_complete",names(cbq_t1)))]
cbq_t1$visit = 1
cbq = sqlQuery(new_con,"select * from CBQ_item_entry;",stringsAsFactors=F)

#reformating data
cbq = id_visit_changing_into_char(cbq)
cbq_entry_flag = entry_flag(cbq,'cbq')
cbq = subset(cbq,entry_status==2)
if(!is.null(cbq_entry_flag)){
  cbq = rbind(cbq,cbq_entry_flag[,-ncol(cbq_entry_flag)])
}
#removing prefix for convenience
cbq = removing_prefix(cbq,"cbq_")
cbq_t1 = study_id_to_id(cbq_t1,"cbq_")
cbq = identify_same_data(cbq_t1,cbq)
cbq = rbind.fill(cbq,cbq_t1)

#changing all scores -9 and 17 into NA
for(j in which(names(cbq)=="1"):which(names(cbq)=="94")){
  cbq[,j] = cbraw(cbq[,j])
}

#dealing with reverse scores
inverse_item_index = paste0('',sort(c(18,50,93,61,90,92,16,21,84,3,49,91,25,34,75,35,68,78,36,43,82,53)))
for(j in inverse_item_index){
  cbq[,j] = 8 - cbq[,j]
}

#calculating age
cbq = fxage(cbq,'id','date')

#obtaining total scores for each scale
#creating indexes for each scale
activity_items = paste0('',c(1,12,18,22,50,85,93))
anger_items = paste0('',c(2,14,30,40,61,87))
motif_items = paste0('',c(6,15,46,58,90,92))
focus_items = paste0('',c(16,21,62,71,84,89))
discomfort_items = paste0('',c(3,9,29,49,64,91))
console_items = paste0('',c(25,34,44,59,66,75))
fear_items = paste0('',c(17,23,35,41,63,68))
pleasure_high_items = paste0('',c(4,10,33,69,78,88))
impulse_items = paste0('',c(7,28,36,43,51,82))
inhibitory_control_items = paste0('',c(38,45,53,67,73,81))
pleasure_low_items = paste0('',c(26,39,57,65,72,76,86,94))
sensitivity_items = paste0('',c(5,13,24,32,47,83))
sadness_items = paste0('',c(8,20,27,31,54,56,74))
shyness_items = paste0('',c(11,37,42,52,60,70))
laugh_items = paste0('',c(19,48,55,77,79,80))

#missing data analysis
cbq = count_missing_items(cbq,'1','94')
cbq = comment_missing_data(cbq,list(activity_items,anger_items,motif_items,focus_items,discomfort_items,
                                    console_items,fear_items,pleasure_high_items,pleasure_low_items,impulse_items,
                                    inhibitory_control_items,sensitivity_items,sadness_items,shyness_items,laugh_items),
                           list('activity','anger','motif','focus','discomfort','console','fear',
                                'high_pleasure','low_pleasure','impulse','inhibitory_control','sensitivity',
                                'sadness','shyness','laugh'))

#summing them up accordingly
cbq = summing_items_per_row(cbq,list(activity_items,anger_items,motif_items,focus_items,discomfort_items,
                                     console_items,fear_items,pleasure_high_items,impulse_items,inhibitory_control_items,
                                     pleasure_low_items,sensitivity_items,sadness_items,shyness_items,laugh_items),
                            list('activity_total','anger_total','motif_total','focus_total','discomfort_total',
                                 'console_total','fear_total','pleasure_high_total','impulse_total','inhibitory_control_total',
                                 'pleasure_low_total','sensitive_total','sadness_total','shyness_total','laugh_total'),F)

#obtaining average scores
for(i in 1:nrow(cbq)){
  #finding number of NA's for each scale in each row
  activity_na_count = length(which(is.na(cbq[i,activity_items])))
  anger_na_count = length(which(is.na(cbq[i,anger_items])))
  motif_na_count = length(which(is.na(cbq[i,motif_items])))
  focus_na_count = length(which(is.na(cbq[i,focus_items])))
  discomfort_na_count = length(which(is.na(cbq[i,discomfort_items])))
  console_na_count = length(which(is.na(cbq[i,console_items])))
  fear_na_count = length(which(is.na(cbq[i,fear_items])))
  pleasure_high_na_count = length(which(is.na(cbq[i,pleasure_high_items])))
  impulse_na_count = length(which(is.na(cbq[i,impulse_items])))
  inhibitory_control_na_count = length(which(is.na(cbq[i,inhibitory_control_items])))
  pleasure_low_na_count = length(which(is.na(cbq[i,pleasure_low_items])))
  sensitive_na_count = length(which(is.na(cbq[i,sensitivity_items])))
  sadness_na_count = length(which(is.na(cbq[i,sadness_items])))
  shyness_na_count = length(which(is.na(cbq[i,shyness_items])))
  laugh_na_count = length(which(is.na(cbq[i,laugh_items])))
  
  #obtaining average scores for each scale
  cbq$activity_avg[i] = ifelse(7-activity_na_count != 0,round(cbq$activity_total[i]/(7-activity_na_count),2),NA)
  cbq$anger_avg[i] = ifelse(6-anger_na_count != 0,round(cbq$anger_total[i]/(6-anger_na_count),2),NA)
  cbq$motif_avg[i] = ifelse(6-motif_na_count != 0,round(cbq$motif_total[i]/(6-motif_na_count),2),NA)
  cbq$focus_avg[i] = ifelse(6-focus_na_count != 0,round(cbq$focus_total[i]/(6-focus_na_count),2),NA)
  cbq$discomfort_avg[i] = ifelse(6-discomfort_na_count != 0,round(cbq$discomfort_total[i]/(6-discomfort_na_count),2),NA)
  cbq$console_avg[i] = ifelse(6-console_na_count != 0,round(cbq$console_total[i]/(6-console_na_count),2),NA)
  cbq$fear_avg[i] = ifelse(6-fear_na_count != 0,round(cbq$fear_total[i]/(6-fear_na_count),2),NA)
  cbq$pleasure_high_avg[i] = ifelse(6-pleasure_high_na_count != 0,round(cbq$pleasure_high_total[i]/(6-pleasure_high_na_count),2),NA)
  cbq$impulse_avg[i] = ifelse(6-impulse_na_count != 0,round(cbq$impulse_total[i]/(6-impulse_na_count),2),NA)
  cbq$inhibitory_control_avg[i] = ifelse(6-inhibitory_control_na_count != 0,round(cbq$inhibitory_control_total[i]/(6-inhibitory_control_na_count),2),NA)
  cbq$pleasure_low_avg[i] = ifelse(8-pleasure_low_na_count != 0,round(cbq$pleasure_low_total[i]/(8-pleasure_low_na_count),2),NA)
  cbq$sensitive_avg[i] = ifelse(6-sensitive_na_count != 0,round(cbq$sensitive_total[i]/(6-sensitive_na_count),2),NA)
  cbq$sadness_avg[i] = ifelse(7-sadness_na_count != 0,round(cbq$sadness_total[i]/(7-sadness_na_count),2),NA)
  cbq$shyness_avg[i] = ifelse(6-shyness_na_count != 0,round(cbq$shyness_total[i]/(6-shyness_na_count),2),NA)
  cbq$laugh_avg[i] = ifelse(6-laugh_na_count != 0,round(cbq$laugh_total[i]/(6-laugh_na_count),2),NA)
  
  #Big 3 scores
  cbq$surgency[i] = round(sum(cbq$activity_avg[i],cbq$pleasure_high_avg[i],cbq$impulse_avg[i],(8-cbq$shyness_avg[i]))/4,2)
  cbq$negative_affectivity[i] = round(sum(cbq$anger_avg[i],cbq$discomfort_avg[i],cbq$fear_avg[i],cbq$sadness_avg[i],(8-cbq$console_avg[i]))/5,2)
  cbq$effortful_control[i] = round(sum(cbq$focus_avg[i],cbq$inhibitory_control_avg[i],cbq$pleasure_low_avg[i],cbq$sensitive_avg[i])/4,2)
}

#putting prefix back in
cbq = inserting_prefix_into_variables(cbq,"cbq_")

#orphaned/duplicate data
cbq_orphaned_data = orphaned_data_consolidate(cbq)
cbq = orphaned_data_remove(cbq)
cbq_duplicate_data = duplicate_data_consolidate(cbq,"cbq_age")
cbq = duplicate_data_remove(cbq,"cbq_age")

#outliers
cbq_outliers = cbq[,c(1:2,101,grep("_avg$",names(cbq)),grep("surgency",names(cbq)),grep("negative_affect",names(cbq)),grep("effortful_control",names(cbq)))]
cbq_outliers = outlier_list(cbq_outliers)
cbq$cbq_outlier_list = cbq_outliers$outlier_list
#cbq_outlier_table = sqldf("select id, visit, outlier_list from cbq where outlier_list != ''")
rm(cbq_outliers)

#archiving data
cbq_scored = cbq[,c(1:2,grep("_age$",names(cbq)):ncol(cbq))]
write.csv(cbq_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/cbq_scored.csv",row.names = F)
write.csv(cbq[,c(1:2,grep("_[0-9]+$",names(cbq)))],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/cbq_items.csv",row.names = F)

rm(activity_items,activity_na_count,anger_items,anger_na_count,motif_items,motif_na_count,focus_items,focus_na_count,
   fear_items,fear_na_count,pleasure_high_items,pleasure_high_na_count,pleasure_low_items,pleasure_low_na_count,sadness_items,
   sadness_na_count,shyness_items,shyness_na_count,discomfort_items,discomfort_na_count,console_items,console_na_count,
   impulse_items,impulse_na_count,inhibitory_control_items,inhibitory_control_na_count,sensitive_na_count,sensitivity_items,
   laugh_items,laugh_na_count,inverse_item_index,cbq_t1)
