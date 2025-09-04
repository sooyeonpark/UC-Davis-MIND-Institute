##For this measure, treat missing items as non-existing items
tmcq = sqlQuery(new_con,"select * from TMCQ;",stringsAsFactors=F)

#initial manipulation
tmcq = id_visit_changing_into_char(tmcq)
tmcq_entry_flag = entry_flag(tmcq,'tmcq')
tmcq = subset(tmcq,entry_status==2)
if(!is.null(tmcq_entry_flag)){
  tmcq = rbind(tmcq,tmcq_entry_flag[,-ncol(tmcq_entry_flag)])
}
tmcq = removing_prefix(tmcq,'tmcq_')

#calculate age
tmcq = fxage(tmcq,'id','date')

#dealing with negative scores
#dealing with "does not apply" items (-8) -> change the score to 0
for(j in grep("^1$",names(tmcq)):grep("157$",names(tmcq))){
  tmcq[,j] = ifelse(tmcq[,j]==-8,0,tmcq[,j])
}
#dealing with truly missing items -> change to NA
for(j in grep("^1$",names(tmcq)):grep("157$",names(tmcq))){
  tmcq[,j] = cbraw(tmcq[,j])
}

#dealing with reverse scores
reverse_scored_items = paste0('',c(28,70,89,93,152,7,17,78,80,82,120,149,121,42,79,143,59,8,19,64,68,105,112,137))
for(j in reverse_scored_items){
  tmcq[,j] = 6 - tmcq[,j]
}
  
#item lists
activation_control_items = paste0('',c(20,26,28,39,46,70,76,89,93,101,103,132,139,152,157))
activity_level_items = paste0('',c(2,15,21,23,37,43,66,102,127))
affiliation_items = paste0('',c(18,33,51,58,106,129,134,145,148,156))
anger_items = paste0('',c(53,61,87,94,99,110,146))
dominance_items = paste0('',c(4,50,60,84,98,122,131,155))
focus_items = paste0('',c(7,17,78,80,82,120,149))
discomfort_items = paste0('',c(5,30,38,69,81,91,125,138,142,154))
fantasy_items = paste0('',c(1,12,48,54,67,71,104,116,151))
fear_items = paste0('',c(11,13,63,75,85,90,117,140,153))
pleasure_high_items = paste0('',c(3,9,41,45,52,65,96,115,119,121,126))
impulse_items = paste0('',c(14,16,22,25,42,72,74,83,108,124,128,130,147))
inhibitory_control_items = paste0('',c(6,40,56,79,88,135,141,143))
pleasure_low_items = paste0('',c(10,32,34,73,86,92,95,113))
sensitive_items = paste0('',c(36,44,57,62,77,109,111,114,123,150))
sadness_items = paste0('',c(24,27,31,35,49,97,100,107,133,144))
shyness_items = paste0('',c(47,55,59,118,136))
soothability_items = paste0('',c(8,19,29,64,68,105,112,137))

#missing data analysis
tmcq = count_missing_items(tmcq,'1','157')
tmcq = comment_missing_data(tmcq,list(activation_control_items,activity_level_items,affiliation_items,
                                      anger_items,dominance_items,focus_items,discomfort_items,fantasy_items,
                                      fear_items,pleasure_high_items,pleasure_low_items,impulse_items,
                                      inhibitory_control_items,sensitive_items,sadness_items,shyness_items,soothability_items),
                            list('activation_control','activity_level','affiliation',
                                 'anger','dominance','focus','discomfort','fantasy',
                                 'fear','pleasure_high','pleasure_low','impulse','inhibitory_control',
                                 'sensitive','sadness','shyness','soothability'))

#obtaining total raw scores
tmcq = summing_items_per_row(tmcq,list(activation_control_items,activity_level_items,affiliation_items,
                                       anger_items,dominance_items,focus_items,discomfort_items,fantasy_items,
                                       fear_items,pleasure_high_items,pleasure_low_items,impulse_items,
                                       inhibitory_control_items,sensitive_items,sadness_items,shyness_items,soothability_items),
                             list('activation_control_total','activity_level_total','affiliation_total',
                                  'anger_total','dominance_total','focus_total','discomfort_total','fantasy_total',
                                  'fear_total','pleasure_high_total','pleasure_low_total','impulse_total','inhibitory_control_total',
                                  'sensitive_total','sadness_total','shyness_total','soothability_total'),T)

#obtaining average scores
for(i in 1:nrow(tmcq)){
  #finding number of NA's and 0's for each scale in each row
  activation_control_na_count = length(which(is.na(tmcq[i,activation_control_items]) | tmcq[i,activation_control_items]==0))
  activity_level_na_count = length(which(is.na(tmcq[i,activity_level_items]) | tmcq[i,activity_level_items]==0))
  affiliation_na_count = length(which(is.na(tmcq[i,affiliation_items]) | tmcq[i,affiliation_items]==0))
  anger_na_count = length(which(is.na(tmcq[i,anger_items]) | tmcq[i,anger_items]==0))
  dominance_na_count = length(which(is.na(tmcq[i,dominance_items]) | tmcq[i,dominance_items]==0))
  focus_na_count = length(which(is.na(tmcq[i,focus_items]) | tmcq[i,focus_items]==0))
  discomfort_na_count = length(which(is.na(tmcq[i,discomfort_items]) | tmcq[i,discomfort_items]==0))
  fantasy_na_count = length(which(is.na(tmcq[i,fantasy_items]) | tmcq[i,fantasy_items]==0))
  fear_na_count = length(which(is.na(tmcq[i,fear_items]) | tmcq[i,fear_items]==0))
  pleasure_high_na_count = length(which(is.na(tmcq[i,pleasure_high_items]) | tmcq[i,pleasure_high_items]==0))
  pleasure_low_na_count = length(which(is.na(tmcq[i,pleasure_low_items]) | tmcq[i,pleasure_low_items]==0))
  impulse_na_count = length(which(is.na(tmcq[i,impulse_items]) | tmcq[i,impulse_items]==0))
  inhibitory_control_na_count = length(which(is.na(tmcq[i,inhibitory_control_items]) | tmcq[i,inhibitory_control_items]==0))
  sensitive_na_count = length(which(is.na(tmcq[i,sensitive_items]) | tmcq[i,sensitive_items]==0))
  sadness_na_count = length(which(is.na(tmcq[i,sadness_items]) | tmcq[i,sadness_items]==0))
  shyness_na_count = length(which(is.na(tmcq[i,shyness_items]) | tmcq[i,shyness_items]==0))
  soothability_na_count = length(which(is.na(tmcq[i,soothability_items]) | tmcq[i,soothability_items]==0))

  #obtaining average scores for each scale
  tmcq$activation_control_avg[i] = ifelse(15-activation_control_na_count != 0,round(tmcq$activation_control_total[i]/(15-activation_control_na_count),2),NA)
  tmcq$activity_level_avg[i] = ifelse(9-activity_level_na_count != 0,round(tmcq$activity_level_total[i]/(9-activity_level_na_count),2),NA)
  tmcq$affiliation_avg[i] = ifelse(10-affiliation_na_count != 0,round(tmcq$affiliation_total[i]/(10-affiliation_na_count),2),NA)
  tmcq$anger_avg[i] = ifelse(7-anger_na_count != 0,round(tmcq$anger_total[i]/(7-anger_na_count),2),NA)
  tmcq$dominance_avg[i] = ifelse(8-dominance_na_count != 0,round(tmcq$dominance_total[i]/(8-dominance_na_count),2),NA)
  tmcq$focus_avg[i] = ifelse(7-focus_na_count != 0,round(tmcq$focus_total[i]/(7-focus_na_count),2),NA)
  tmcq$discomfort_avg[i] = ifelse(10-discomfort_na_count != 0,round(tmcq$discomfort_total[i]/(10-discomfort_na_count),2),NA)
  tmcq$fantasy_avg[i] = ifelse(9-fantasy_na_count != 0,round(tmcq$fantasy_total[i]/(9-fantasy_na_count),2),NA)
  tmcq$fear_avg[i] = ifelse(9-fear_na_count != 0,round(tmcq$fear_total[i]/(9-fear_na_count),2),NA)
  tmcq$pleasure_high_avg[i] = ifelse(11-pleasure_high_na_count != 0,round(tmcq$pleasure_high_total[i]/(11-pleasure_high_na_count),2),NA)
  tmcq$pleasure_low_avg[i] = ifelse(8-pleasure_low_na_count != 0,round(tmcq$pleasure_low_total[i]/(8-pleasure_low_na_count),2),NA)
  tmcq$impulse_avg[i] = ifelse(13-impulse_na_count != 0,round(tmcq$impulse_total[i]/(13-impulse_na_count),2),NA)
  tmcq$inhibitory_control_avg[i] = ifelse(8-inhibitory_control_na_count != 0,round(tmcq$inhibitory_control_total[i]/(8-inhibitory_control_na_count),2),NA)
  tmcq$sensitive_avg[i] = ifelse(10-sensitive_na_count != 0,round(tmcq$sensitive_total[i]/(10-sensitive_na_count),2),NA)
  tmcq$sadness_avg[i] = ifelse(10-sadness_na_count != 0,round(tmcq$sadness_total[i]/(10-sadness_na_count),2),NA)
  tmcq$shyness_avg[i] = ifelse(5-shyness_na_count != 0,round(tmcq$shyness_total[i]/(5-shyness_na_count),2),NA)
  tmcq$soothability_avg[i] = ifelse(8-soothability_na_count != 0,round(tmcq$soothability_total[i]/(8-soothability_na_count),2),NA)
  
  #Big 3 scores
  tmcq$surgency[i] = round(sum(tmcq$activity_level_avg[i],tmcq$pleasure_high_avg[i],tmcq$impulse_avg[i],(6-tmcq$shyness_avg[i]))/4,2)
  tmcq$negative_affectivity[i] = round(sum(tmcq$anger_avg[i],tmcq$discomfort_avg[i],tmcq$fear_avg[i],tmcq$sadness_avg[i],(6-tmcq$soothability_avg[i]))/5,2)
  tmcq$effortful_control[i] = round(sum(tmcq$focus_avg[i],tmcq$inhibitory_control_avg[i],tmcq$pleasure_low_avg[i],tmcq$sensitive_avg[i],tmcq$activation_control_avg[i])/5,2)
}

#putting back prefixes
tmcq = inserting_prefix_into_variables(tmcq,'tmcq_')

#orphaned/duplicate data
tmcq_orphaned_data = orphaned_data_consolidate(tmcq)
tmcq = orphaned_data_remove(tmcq)
tmcq_duplicate_data = duplicate_data_consolidate(tmcq,'tmcq_age')
tmcq = duplicate_data_remove(tmcq,'tmcq_age')

#outliers
tmcq_outliers = tmcq[,c(1:2,grep("_age$",names(tmcq)),grep("_avg$",names(tmcq)),grep("surgency",names(tmcq)),grep("negative_affect",names(tmcq)),grep("effortful_control",names(tmcq)))] 
tmcq_outliers = outlier_list(tmcq_outliers)
tmcq$tmcq_outlier_list = tmcq_outliers$outlier_list
rm(tmcq_outliers)

#archiving data
tmcq_scored = tmcq[,c(1:2,grep("_age$",names(tmcq)):ncol(tmcq))]
write.csv(tmcq_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/tmcq_scored.csv",row.names = F)
write.csv(tmcq[,c(1:2,grep("_resp$",names(tmcq)),grep("_[0-9]+$",names(tmcq)))],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/tmcq_items.csv",row.names = F)

#clean up
rm(reverse_scored_items,activation_control_items,activity_level_items,affiliation_items,
   anger_items,dominance_items,focus_items,discomfort_items,fantasy_items,fear_items,
   pleasure_low_items,pleasure_high_items,impulse_items,inhibitory_control_items,
   sensitive_items,sadness_items,shyness_items,soothability_items,activation_control_na_count,
   activity_level_na_count,affiliation_na_count,anger_na_count,dominance_na_count,
   focus_na_count,discomfort_na_count,fantasy_na_count,fear_na_count,pleasure_high_na_count,
   pleasure_low_na_count,impulse_na_count,inhibitory_control_na_count,sensitive_na_count,
   sadness_na_count,shyness_na_count,soothability_na_count)