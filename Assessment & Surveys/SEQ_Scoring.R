#initial data importing, formatting, and flagging
seq_t1 = exportRecords(ace_con,forms=c("participant_information","seq"),events="t1_arm_1",labels=F,stringsAsFactors=F)
seq_t1 = seq_t1[grepl("Co",seq_t1$seq_complete)&!is.na(seq_t1$seq_timestamp),-(2:grep("participant_information_complete",names(seq_t1)))]
seq_t1$visit = 1
seq_t3 = exportRecords(ace_con,forms=c("participant_information","seq"),events="t3_arm_1",labels=F,stringsAsFactors=F)
seq_t3 = seq_t3[grepl("Co",seq_t3$seq_complete)&!is.na(seq_t3$seq_timestamp),-(2:grep("participant_information_complete",names(seq_t3)))]
seq_t3$visit = 3
seq_t5 = exportRecords(ace_con,forms=c("participant_information","seq"),events="t5_arm_1",labels=F,stringsAsFactors=F)
seq_t5 = seq_t5[grepl("Co",seq_t5$seq_complete)&!is.na(seq_t5$seq_timestamp),-(2:grep("participant_information_complete",names(seq_t5)))]
seq_t5$visit = 5
seq_rc = rbind(seq_t1,seq_t3,seq_t5)
seq = sqlQuery(new_con,"select * from SensoryExperienceQuestionnaire;",stringsAsFactors=F)
seq_entry_flag = entry_flag(seq,'seq')
seq = subset(seq,entry_status==2)
names(seq_rc)[1:3]=c("id","seq_date","seq_resp")

#checking the same data rows and combining
seq = identify_same_data(seq_rc,seq)
seq = rbind.fill(seq,seq_rc)
seq = id_visit_changing_into_char(seq)
seq = removing_prefix(seq,'seq_')

#calculate age
seq = fxage(seq,'id','date')

#convering items into numeric scores and counting NA's
for(j in which(names(seq)=='1'):which(names(seq)=='98')){
  for(i in 1:nrow(seq)){
    seq[i,j] = seq_factor2scores(seq[i,j])
  }
  seq[,j] = as.numeric(seq[,j])
}

#item lists
hyper_items = paste0('',c(1,2,9,10,15,18,36,38,40,42,44,46,47,49,51,52,54,59,61,63,72,73,77,81,83,87,89,92,95,96,97))
hypo_items = paste0('',c(4,8,14,22,23,31,34,43,53,56,58,69,74,82,84,86,91,94))
sirs_items = paste0('',c(7,11,16,17,19,21,25,27,28,29,30,32,37,39,41,45,50,55,57,60,62,64,65,67,68,71,76,78,79,80,85))
ep_items = paste0('',c(3,5,6,12,20,24,33,48,66,70,88,90))

#missing data analysis
seq = count_missing_items(seq,'1','98')
seq = comment_missing_data(seq,list(hyper_items,hypo_items,sirs_items,ep_items),
                           list('hyperresponsive','hyporesponsive','sensory_interests/repetition/seeking','enhanced_perception'))

#summing up scores
seq = summing_items_per_row(seq,list(hyper_items,hypo_items,sirs_items,ep_items),
                            list("hr_total",'ho_total','sirs_total','ep_total'),F)

#item means for sensory pattern scores
for(i in 1:nrow(seq)){
  hr_na_count = length(which(is.na(seq[i,hyper_items])))
  ho_na_count = length(which(is.na(seq[i,hypo_items])))
  sirs_na_count = length(which(is.na(seq[i,sirs_items])))
  ep_na_count = length(which(is.na(seq[i,ep_items])))
  seq$hr_item_mean[i] = round(seq$hr_total[i]/(length(hyper_items)-hr_na_count),2)
  seq$ho_item_mean[i] = round(seq$ho_total[i]/(length(hypo_items)-ho_na_count),2)
  seq$sirs_item_mean[i] = round(seq$sirs_total[i]/(length(sirs_items)-sirs_na_count),2)
  seq$ep_item_mean[i] = round(seq$ep_total[i]/(length(ep_items)-ep_na_count),2)
}

#context scores
social_items = paste0('',c(5,8,10,11,16,18,23,24,31,36,37,41,43,51,52,54,61,78,83,89,92))
nonsocial_items = paste0('',c(1,2,3,4,6,7,9,12,14,15,17,19,20,21,22,25,27,28,29,30,32,33,34,38,39,40,42,44,45,46,47,48,49,50,53,55,56,57,58,59,60,62,63,64,65,66,67,68,69,70,71,72,73,74,76,77,79,80,81,82,84,85,86,87,88, 90,91,94,95))
seq = comment_missing_data(seq,list(social_items,nonsocial_items),list('social','non-social'))
seq = summing_items_per_row(seq,list(social_items,nonsocial_items),
                            list("social_total",'nonsocial_total'),F)

#item means scores for context scores
for(i in 1:nrow(seq)){
  social_na_count = length(which(is.na(seq[i,social_items])))
  nonsocial_na_count = length(which(is.na(seq[i,nonsocial_items])))
  seq$social_item_mean[i] = round(seq$social_total[i]/(length(social_items)-social_na_count),2)
  seq$nonsocial_item_mean[i] = round(seq$nonsocial_total[i]/(length(nonsocial_items)-nonsocial_na_count),2)
}

#sensory modality scores
auditory_items = paste0('',c(1,2,3,4,5,6,7,8,9,10,11,12,14))
visual_items = paste0('',c(15,16,17,18,19,20,21,22,23,24,25,27,28,29,30,31,32,33,34))
texture_items = paste0('',c(36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58))
smell_items = paste0('',c(59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74))
movement_items = paste0('',c(76,77,78,79,80,81,82,83,84,85,86))
seq = comment_missing_data(seq,list(auditory_items,visual_items,texture_items,
                                    smell_items,movement_items),
                           list('auditory','visual','tactile','olfactory','vestibular/proprioception'))
seq = summing_items_per_row(seq,list(auditory_items,visual_items,texture_items,
                                     smell_items,movement_items),
                            list('auditory_total','visual_total','tactile_total',
                                 'olfactory_total','vestibular_total'),F)

#item means for sensory modality scores
for(i in 1:nrow(seq)){
  auditory_na_count = length(which(is.na(seq[i,auditory_items])))
  seq$auditory_item_mean[i] = round(seq$auditory_total[i]/(length(auditory_items)-auditory_na_count),2)
  visual_na_count = length(which(is.na(seq[i,visual_items])))
  seq$visual_item_mean[i] = round(seq$visual_total[i]/(length(visual_items)-visual_na_count),2)
  tactile_na_count = length(which(is.na(seq[i,texture_items])))
  seq$tactile_item_mean[i] = round(seq$tactile_total[i]/(length(texture_items)-tactile_na_count),2)
  smell_na_count = length(which(is.na(seq[i,smell_items])))
  seq$olfactory_item_mean[i] = round(seq$olfactory_total[i]/(length(smell_items)-smell_na_count),2)
  movement_na_count = length(which(is.na(seq[i,movement_items])))
  seq$vestibular_item_mean[i] = round(seq$vestibular_total[i]/(length(movement_items)-movement_na_count),2)
}

#combining subomains
visual_hyper_items = unique(c(hyper_items,visual_items))
visual_sirs_items = unique(c(visual_items,sirs_items))

seq = summing_items_per_row(seq,list(visual_hyper_items,visual_sirs_items),
                            list('visual_hyper_total','visual_sirs_total'),F)

for(i in 1:nrow(seq)){
  #visual_hyper_na_count = length(which(is.na(seq[i,visual_hyper_items])))
  seq$visual_hyper_item_mean[i] = round(seq$visual_hyper_total[i]/length(visual_hyper_items),2)
  #visual_sirs_na_count = length(which(is.na(seq[i,visual_sirs_items])))
  seq$visual_sirs_item_mean[i] = round(seq$visual_sirs_total[i]/length(visual_sirs_items),2)
}

#putting back prefixes
seq = inserting_prefix_into_variables(seq,"seq_")

#orphaned/duplicate data
seq_orphaned_data = orphaned_data_consolidate(seq)
seq = orphaned_data_remove(seq)
seq_duplicate_data = duplicate_data_consolidate(seq,"seq_age")
seq = duplicate_data_remove(seq,"seq_age")

#outliers?
seq_outliers = seq[,c(1:2,grep("_age$",names(seq)),grep("_mean$",names(seq)))]
seq_outliers = outlier_list(seq_outliers)
seq$seq_outlier_list = seq_outliers$outlier_list
#seq_outlier_table = sqldf("select id,visit,outlier_list from seq where outlier_list != ''")
rm(seq_outliers)

#archiving the data
seq_scored = seq[,c(1:2,grep("age",names(seq)):ncol(seq))]
write.csv(seq_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/seq_scored.csv",row.names=F)
write.csv(seq[,c(1:2,grep("_[0-9]+$",names(seq)))],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/seq_items.csv",row.names=F)

#cleaning up
rm(hyper_items,hypo_items,sirs_items,ep_items,hr_na_count,ho_na_count,sirs_na_count,
   ep_na_count,social_items,nonsocial_items,social_na_count,nonsocial_na_count,
   auditory_items,visual_items,auditory_na_count,visual_na_count,smell_items,
   smell_na_count,texture_items,tactile_na_count,movement_items,movement_na_count,
   visual_hyper_items,visual_sirs_items,seq_rc,seq_t1,seq_t3,seq_t5)
