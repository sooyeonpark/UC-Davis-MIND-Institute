#pulling out data tables
cbcl_young_t1 = exportRecords(ace_con,forms=c("participant_information","cbcl_ages_1_5"),events="t1_arm_1",labels=F,stringsAsFactors=F)
cbcl_young_t1 = cbcl_young_t1[grepl("Co",cbcl_young_t1$cbcl_ages_1_5_complete)&!is.na(cbcl_young_t1$cbcl_ages_1_5_timestamp),-(2:grep("participant_information_complete",names(cbcl_young_t1)))]
cbcl_young_t1$visit = 1
cbcl_young_t3 = exportRecords(ace_con,forms=c("participant_information","cbcl_ages_1_5"),events="t3_arm_1",labels=F,stringsAsFactors=F)
cbcl_young_t3 = cbcl_young_t3[grepl("Co",cbcl_young_t3$cbcl_ages_1_5_complete)&!is.na(cbcl_young_t3$cbcl_ages_1_5_timestamp),-(2:grep("participant_information_complete",names(cbcl_young_t3)))]
cbcl_young_t3$visit = 3
cbcl_young_rc = rbind(cbcl_young_t1,cbcl_young_t3)
cbcl_old_t3 = exportRecords(ace_con,forms=c("participant_information","cbcl_ages_6_18"),events="t3_arm_1",labels=F,stringsAsFactors=F)
cbcl_old_t3 = cbcl_old_t3[grepl("Co",cbcl_old_t3$cbcl_ages_6_18_complete)&!is.na(cbcl_old_t3$cbcl_ages_6_18_timestamp),-(2:grep("participant_information_complete",names(cbcl_old_t3)))]
if(nrow(cbcl_old_t3)==0){
  rm(cbcl_old_t3)
}
##check once a month whether the data is entered
# cbcl_old_t4 = exportRecords(ace_con,forms=c("participant_information","cbcl_ages_6_18"),events="t4_arm_1",labels=F,stringsAsFactors=F)
# cbcl_old_t4 = cbcl_old_t4[grepl("Co",cbcl_old_t4$cbcl_ages_6_18_complete)&!is.na(cbcl_old_t4$cbcl_ages_6_18_timestamp),-(2:grep("participant_information_complete",names(cbcl_old_t4)))]
# cbcl_old_t4$visit = 4
cbcl_old_t5 = exportRecords(ace_con,forms=c("participant_information","cbcl_ages_6_18"),events="t5_arm_1",labels=F,stringsAsFactors=F)
cbcl_old_t5 = cbcl_old_t5[grepl("Co",cbcl_old_t5$cbcl_ages_6_18_complete)&!is.na(cbcl_old_t5$cbcl_ages_6_18_timestamp),-(2:grep("participant_information_complete",names(cbcl_old_t5)))]
cbcl_old_t5$visit = 5
if(exists('cbcl_old_t3')){
  cbcl_old_rc=rbind(cbcl_old_t3,cbcl_old_t5)
}
cbcl_young = sqlQuery(new_con,"select * from CBCL_AGES_1_5;",stringsAsFactors=F)
cbcl_old = sqlQuery(new_con,"select * from CBCL_AGES_6_18;",stringsAsFactors=F)
#pulling out staar cbcl table
cbcl_staar = sqlQuery(con4,"select * from CBCL_6_18;",stringsAsFactors=F)

#id and visit into characters
cbcl_young = id_visit_changing_into_char(cbcl_young)
cbcl_old = id_visit_changing_into_char(cbcl_old)

#single-entry flag
cbcl_young_entry_flag = entry_flag(cbcl_young,"cbcl_1-5")
names(cbcl_old) = tolower(names(cbcl_old))
cbcl_old_entry_flag = entry_flag(cbcl_old,'cbcl_6-18')
if(!is.null(cbcl_young_entry_flag)){
  cbcl_young = rbind(cbcl_young,cbcl_young_entry_flag[,-ncol(cbcl_young_entry_flag)])
}
if(!is.null(cbcl_old_entry_flag)){
  cbcl_old = rbind(cbcl_old,cbcl_old_entry_flag[,-ncol(cbcl_old_entry_flag)])
}

#removing single-entered rows
cbcl_young = subset(cbcl_young,entry_status==2)
cbcl_old = subset(cbcl_old,entry_status==2)

#combining star table with the rest
cbcl_old = removing_prefix(cbcl_old,"cbcl618_")
names(cbcl_staar) = tolower(names(cbcl_staar))
cbcl_staar = removing_prefix(cbcl_staar,"cb2_")
names(cbcl_staar) = gsub("none$","num",names(cbcl_staar))
cbcl_old_rc = study_id_to_id(cbcl_old_t5,"cb2_")
cbcl_old = identify_same_data(cbcl_old_rc,cbcl_old)
cbcl_old = rbind.fill(cbcl_old,cbcl_old_rc,cbcl_staar)
rm(cbcl_staar)

###cbcl_young###
#removing prefix for convenience
cbcl_young = removing_prefix(cbcl_young,"cbcl15_")
cbcl_young_rc = study_id_to_id(cbcl_young_rc,"cb1_")
cbcl_young = identify_same_data(cbcl_young_rc,cbcl_young)
cbcl_young = rbind.fill(cbcl_young,cbcl_young_rc)

cbcl_young <- cbcl_young[,1:which(names(cbcl_young)=='100c')]

#calculating age
cbcl_young = fxage(cbcl_young,'id','date')

#converting negative scores to NA's (for the sake of summing them)
for(j in which(names(cbcl_young)=='1'):which(names(cbcl_young)=='100c')){ 
  if(class(cbcl_young[,j])=="character"){
    j = j+1
  }
  else{
    cbcl_young[,j] = cbraw(cbcl_young[,j])
  }
}

for(i in 1:nrow(cbcl_young)){
  #for item 100 in other, only count the highest score, not all of them
  cbcl_young$`100`[i] = ifelse(all(is.na(c(cbcl_young$`100a`[i],cbcl_young$`100b`[i],cbcl_young$`100c`[i],cbcl_young$`100`[i]))),0,pmax(cbcl_young$`100`[i],cbcl_young$`100a`[i],cbcl_young$`100b`[i],cbcl_young$`100c`[i],na.rm = TRUE))
}

#lists of items in each domain
reactive_items = paste0("",c(21,46,51,79,82,83,92,97,99))
anxious_depressed_items = paste0("",c(10,33,37,43,47,68,87,90))
somatic_items = paste0("",c(1,7,12,19,24,39,45,52,78,86,93))
withdrawn_items = paste0("",c(2,4,23,62,67,70,71,98))
sleep_items = paste0("",c(22,38,48,64,74,84,94))
attention_items = paste0("",c(5,6,56,59,95))
aggressive_items = paste0("",c(8,15,16,18,20,27,29,35,40,42,44,53,58,66,69,81,85,88,96))
other_items = paste0("",c(3,9,11,13,14,17,25,26,28,30,31,32,34,36,41,49,50,54,55,57,60,61,63,65,72,73,75:77,80,89,91,100))
dsm_affect_items = paste0("",c(13,24,38,43,49,50,71,74,89,90))
dsm_anxiety_items = paste0("",c(10,22,28,32,37,47,48,51,87,99))
#dsm_pdd_items = paste0("",c(3,4,7,21,23,25,63,67,70,76,80,92,98)) changed to asd items that are not currently used
dsm_odd_items = paste0("",c(15,20,44,81,85,88))
dsm_adhd_items = paste0("",c(5,6,8,16,36,59))
internalizing_items = c(reactive_items,anxious_depressed_items,somatic_items,withdrawn_items)
externalizing_items = c(attention_items,aggressive_items)
total_items = c(internalizing_items,externalizing_items,sleep_items,other_items)

#missing data analysis
cbcl_young = count_missing_items(cbcl_young,"1","99")
cbcl_young = comment_missing_data(cbcl_young,list(reactive_items,anxious_depressed_items,somatic_items,withdrawn_items,sleep_items,attention_items,aggressive_items,dsm_affect_items,dsm_anxiety_items,dsm_odd_items,dsm_adhd_items),
                                  list('reactive','anxious/depressed','somatic','withdrawn','sleep problem','attention problem','aggression','dsm_affect','dsm_anxiety','dsm_odd','dsm_adhd'))

#obtaining raw scores
for(i in 1:nrow(cbcl_young)){
  cbcl_young$emotionally_reactive_raw[i] = sum(cbcl_young[i,reactive_items],na.rm=F)
  cbcl_young$anxious_depressed_raw[i] = sum(cbcl_young[i,anxious_depressed_items],na.rm=F)
  cbcl_young$somatic_complaints_raw[i] = sum(cbcl_young[i,somatic_items],na.rm=F)
  cbcl_young$withdrawn_raw[i] = sum(cbcl_young[i,withdrawn_items],na.rm=F)
  cbcl_young$sleep_problem_raw[i] = sum(cbcl_young[i,sleep_items],na.rm=F)
  cbcl_young$attention_problem_raw[i] = sum(cbcl_young[i,attention_items],na.rm=F)
  cbcl_young$aggressive_behavior_raw[i] = sum(cbcl_young[i,aggressive_items],na.rm=F)
  cbcl_young$dsm_affect_raw[i] = sum(cbcl_young[i,dsm_affect_items],na.rm=F)
  cbcl_young$dsm_anxiety_raw[i] = sum(cbcl_young[i,dsm_anxiety_items],na.rm=F)
  #cbcl_young$dsm_asd_raw[i] = sum(cbcl_young[i,dsm_asd_items],na.rm=F)
  cbcl_young$dsm_odd_raw[i] = sum(cbcl_young[i,dsm_odd_items],na.rm=F)
  cbcl_young$dsm_adhd_raw[i] = sum(cbcl_young[i,dsm_adhd_items],na.rm=F)
  cbcl_young$internalizing_raw[i] = sum(cbcl_young[i,internalizing_items],na.rm=F)
  cbcl_young$externalizing_raw[i] = sum(cbcl_young[i,externalizing_items],na.rm=F)
  cbcl_young$total_raw[i] = sum(cbcl_young[i,total_items],na.rm=F)
  if(cbcl_young$missing_items_count[i]>8){
    cbcl_young[i,which(names(cbcl_young)=="emotionally_reactive_raw"):which(names(cbcl_young)=="total_raw")]=NA
  }
}

#modifying norm table
cbcl_tscores=read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/other scripts/scr_cbcl_scales.csv",header = TRUE)
cbcl_tscores_revised = data.frame(cbcl_tscores[,1])
names(cbcl_tscores_revised)="t_score"
cbcl_tscores_revised = modifying_vabs_norm_tables(cbcl_tscores,cbcl_tscores_revised)
rm(cbcl_tscores)

#obtaining t scores
for(i in 1:nrow(cbcl_young)){
  cbcl_young$emotionally_reactive_t[i] = ifelse(!is.na(cbcl_young$emotionally_reactive_raw[i]),
                                                cbcl_tscores_revised[which(cbcl_young$emotionally_reactive_raw[i] >=cbcl_tscores_revised$emotionally_reactive_bottom
                                                                           & cbcl_young$emotionally_reactive_raw[i]<=cbcl_tscores_revised$emotionally_reactive_top),1],NA)
  cbcl_young$anxious_depressed_t[i] = ifelse(!is.na(cbcl_young$anxious_depressed_raw[i]),
                                             cbcl_tscores_revised[which(cbcl_young$anxious_depressed_raw[i] >=cbcl_tscores_revised$anxious_depressed_bottom
                                                                        & cbcl_young$anxious_depressed_raw[i]<=cbcl_tscores_revised$anxious_depressed_top),1],NA)
  cbcl_young$somatic_complaints_t[i] = ifelse(!is.na(cbcl_young$somatic_complaints_raw[i]),
                                              cbcl_tscores_revised[which(cbcl_young$somatic_complaints_raw[i] >=cbcl_tscores_revised$somatic_complaints_bottom
                                                                        & cbcl_young$somatic_complaints_raw[i]<=cbcl_tscores_revised$somatic_complaints_top),1],NA)
  cbcl_young$withdrawn_t[i] = ifelse(!is.na(cbcl_young$withdrawn_raw[i]),
                                     cbcl_tscores_revised[which(cbcl_young$withdrawn_raw[i] >=cbcl_tscores_revised$withdrawn_bottom
                                                                & cbcl_young$withdrawn_raw[i]<=cbcl_tscores_revised$withdrawn_top),1],NA)
  cbcl_young$sleep_problem_t[i] = ifelse(!is.na(cbcl_young$sleep_problem_raw[i]),
                                         cbcl_tscores_revised[which(cbcl_young$sleep_problem_raw[i] >=cbcl_tscores_revised$sleep_problems_bottom
                                                                   & cbcl_young$sleep_problem_raw[i]<=cbcl_tscores_revised$sleep_problems_top),1],NA)
  cbcl_young$attention_problem_t[i] = ifelse(!is.na(cbcl_young$attention_problem_raw[i]),
                                             cbcl_tscores_revised[which(cbcl_young$attention_problem_raw[i] >=cbcl_tscores_revised$attention_problems_bottom
                                                                        & cbcl_young$attention_problem_raw[i]<=cbcl_tscores_revised$attention_problems_top),1],NA)
  cbcl_young$aggressive_t[i] = ifelse(!is.na(cbcl_young$aggressive_behavior_raw[i]),
                                     cbcl_tscores_revised[which(cbcl_young$aggressive_behavior_raw[i] >=cbcl_tscores_revised$aggressive_behavior_bottom
                                                                & cbcl_young$aggressive_behavior_raw[i]<=cbcl_tscores_revised$aggressive_behavior_top),1],NA)
  cbcl_young$dsm_affective_t[i] = ifelse(!is.na(cbcl_young$dsm_affect_raw[i]),
                                         cbcl_tscores_revised[which(cbcl_young$dsm_affect_raw[i] >=cbcl_tscores_revised$dsm_affective_bottom
                                                                   & cbcl_young$dsm_affect_raw[i]<=cbcl_tscores_revised$dsm_affective_top),1],NA)
  cbcl_young$dsm_anxiety_t[i] = ifelse(!is.na(cbcl_young$dsm_anxiety_raw[i]),
                                      cbcl_tscores_revised[which(cbcl_young$dsm_anxiety_raw[i] >=cbcl_tscores_revised$dsm_anxiety_bottom
                                                                 & cbcl_young$dsm_anxiety_raw[i]<=cbcl_tscores_revised$dsm_anxiety_top),1],NA)
  # cbcl_young$dsm_pdd_t[i] = ifelse(!is.na(cbcl_young$dsm_pdd_raw[i]),
  #                                  cbcl_tscores_revised[which(cbcl_young$dsm_pdd_raw[i] >=cbcl_tscores_revised$dsm_pdd_bottom
  #                                                             & cbcl_young$dsm_pdd_raw[i]<=cbcl_tscores_revised$dsm_pdd_top),1],NA)
  cbcl_young$dsm_adhd_t[i] = ifelse(!is.na(cbcl_young$dsm_adhd_raw[i]),
                                    cbcl_tscores_revised[which(cbcl_young$dsm_adhd_raw[i] >=cbcl_tscores_revised$dsm_adhd_bottom
                                                               & cbcl_young$dsm_adhd_raw[i]<=cbcl_tscores_revised$dsm_adhd_top),1],NA)
  cbcl_young$dsm_odd_t[i] = ifelse(!is.na(cbcl_young$dsm_odd_raw[i]),
                                  cbcl_tscores_revised[which(cbcl_young$dsm_odd_raw[i] >=cbcl_tscores_revised$dsm_odd_bottom
                                                             & cbcl_young$dsm_odd_raw[i]<=cbcl_tscores_revised$dsm_odd_top),1],NA)
  cbcl_young$internalizing_t[i] = ifelse(!is.na(cbcl_young$internalizing_raw[i]),
                                         cbcl_tscores_revised[which(cbcl_young$internalizing_raw[i] >=cbcl_tscores_revised$internalizing_bottom
                                                                   & cbcl_young$internalizing_raw[i]<=cbcl_tscores_revised$internalizing_top),1],NA)
  cbcl_young$externalizing_t[i] = ifelse(!is.na(cbcl_young$externalizing_raw[i]),
                                        cbcl_tscores_revised[which(cbcl_young$externalizing_raw[i] >=cbcl_tscores_revised$externalizing_bottom
                                                                   & cbcl_young$externalizing_raw[i]<=cbcl_tscores_revised$externalizing_top),1],NA)
  cbcl_young$total_t[i] = ifelse(!is.na(cbcl_young$total_raw[i]),
                                cbcl_tscores_revised[which(cbcl_young$total_raw[i] >=cbcl_tscores_revised$total_bottom
                                                    & cbcl_young$total_raw[i]<=cbcl_tscores_revised$total_top),1],NA)
}

#orphaned/duplicate data
cbcl_young_orphaned_data = orphaned_data_consolidate(cbcl_young)
cbcl_young = orphaned_data_remove(cbcl_young)
cbcl_young_duplicate_data = duplicate_data_consolidate(cbcl_young,"age")
cbcl_young = duplicate_data_remove(cbcl_young,"age")

#outliers of our interests
cbcl_young = inserting_prefix_into_variables(cbcl_young,"cbcl_")
cbcl_young_outliers = cbcl_young[,c(1:2,grep("_age$",names(cbcl_young)),grep("_t$",names(cbcl_young)))]
cbcl_young_outliers = outlier_list(cbcl_young_outliers)
cbcl_young$cbcl_outlier_list = cbcl_young_outliers$outlier_list
rm(cbcl_young_outliers)

rm(reactive_items,anxious_depressed_items,sleep_items,attention_items,somatic_items,withdrawn_items,
   aggressive_items,other_items,dsm_affect_items,dsm_adhd_items,dsm_odd_items,dsm_anxiety_items,
   internalizing_items,externalizing_items,total_items)

###cbcl_old###

#calculating age
cbcl_old = fxage(cbcl_old,'id','date')

#pulling off gender from participant data
cbcl_old$gender=NULL
cbcl_old = sqldf("select t1.*,t2.gender from cbcl_old t1 left join subj t2 on t1.id=t2.subj_id")
for(i in 1:nrow(cbcl_old)){
  cbcl_old$gender[i] = ifelse(is.na(cbcl_old$gender[i]),NA,ifelse(grepl("F",cbcl_old$gender[i]),"F","M"))
}


#dealing with negative scores
cbcl_old[,"2ex"]=as.character(cbcl_old[,"2ex"])
#cbcl_old[,"2"]=as.numeric(cbcl_old[,"2"])
for(j in which(names(cbcl_old)=='1'):which(names(cbcl_old)=='113c')){
  if(class(cbcl_old[,j]) == "numeric" | class(cbcl_old[,j]) == "integer"){
    cbcl_old[,j] = cbraw(cbcl_old[,j])
  }
  else{
    j = j+1
  }
}

#adjusting value for 113 and 56h
for(i in 1:nrow(cbcl_old)){
  cbcl_old$`113`[i] = ifelse(all(is.na(c(cbcl_old$`113a`[i],cbcl_old$`113b`[i],cbcl_old$`113c`[i]))),0,pmax(cbcl_old$`113a`[i],cbcl_old$`113b`[i],cbcl_old$`113c`[i],na.rm = TRUE))
  cbcl_old$`56h`[i] = ifelse(is.na(cbcl_old$`56h`[i]),0,cbcl_old$`56h`[i])
}

#lists of items in each domain
anxious_depressed_items = paste0("",c(14,29:33,35,45,50,52,71,91,112))
somatic_items = paste0("",c(47,49,51,54,'56a','56b','56c','56d','56e','56f','56g'))
withdrawn_items = paste0("",c(5,42,65,69,75,102:103,111))
social_items = paste0("",c(11:12,25,27,34,36,38,48,62,64,79))
thought_items = paste0('',c(9,18,40,46,58:60,66,70,76,83:85,92,100))
attention_items = paste0("",c(1,4,8,10,13,17,41,61,78,80))
defiant_items = paste0('',c(2,26,28,39,43,63,67,72,73,81,82,90,96,99,101,105,106))
aggressive_items = paste0("",c(3,16,19:23,37,57,68,86:89,94,95,97,104))
other_items = paste0("",c(6,7,15,24,44,53,55,'56h',74,77,93,98,107:110,113))
dsm_depressive_items = paste0("",c(5,14,18,24,35,52,54,76:77,91,100,102:103))
dsm_anxiety_items = paste0("",c(11,29:31,47,50,71,112))
dsm_somatic_items = paste0("56",c("a","b","c","d","e","f","g"))
dsm_adhd_items = paste0("",c(4,8,10,41,78,93,104))
dsm_odd_items = paste0("",c(3,22:23,86,95))
dsm_conduct_items = paste0("",c(15:16,21,26,28,37,39,43,57,67,72,81:82,90,97,101,106))
internalizing_items = c(anxious_depressed_items,somatic_items,withdrawn_items)
externalizing_items = c(defiant_items,aggressive_items)
total_items = c(internalizing_items,externalizing_items,social_items,thought_items,attention_items,other_items)

#missing data analysis
cbcl_old = count_missing_items(cbcl_old,"1","112")
cbcl_old = count_missing_items(cbcl_old,'113','113')
cbcl_old = comment_missing_data(cbcl_old,list(anxious_depressed_items,somatic_items,withdrawn_items,
                                              social_items,thought_items,attention_items,defiant_items,
                                              aggressive_items,dsm_depressive_items,dsm_anxiety_items,
                                              dsm_somatic_items,dsm_odd_items,dsm_adhd_items,dsm_conduct_items),
                                  list('anxious/depressed','somatic','withdrawn','social problem',
                                       'thought problem','attention problem','defiance','aggression',
                                       'depression','anxiety','somatic_dsm','odd','adhd','conduct problem'))

#obtaining raw scores
for(i in 1:nrow(cbcl_old)){
  cbcl_old$anxious_depressed_raw[i] = sum(cbcl_old[i,anxious_depressed_items],na.rm=F)
  cbcl_old$somatic_complaints_raw[i] = sum(cbcl_old[i,somatic_items],na.rm=F)
  cbcl_old$withdrawn_raw[i] = sum(cbcl_old[i,withdrawn_items],na.rm=F)
  cbcl_old$social_problem_raw[i] = sum(cbcl_old[i,social_items],na.rm=F)
  cbcl_old$thought_problem_raw[i] = sum(cbcl_old[i,thought_items],na.rm=F)
  cbcl_old$attention_problem_raw[i] = sum(cbcl_old[i,attention_items],na.rm=F)
  cbcl_old$defiant_raw[i] = sum(cbcl_old[i,defiant_items],na.rm=F)
  cbcl_old$aggressive_behavior_raw[i] = sum(cbcl_old[i,aggressive_items],na.rm=F)
  cbcl_old$dsm_depressive_raw[i] = sum(cbcl_old[i,dsm_depressive_items],na.rm=F)
  cbcl_old$dsm_anxiety_raw[i] = sum(cbcl_old[i,dsm_anxiety_items],na.rm=F)
  cbcl_old$dsm_somatic_raw[i] = sum(cbcl_old[i,dsm_somatic_items],na.rm=F)
  cbcl_old$dsm_adhd_raw[i] = sum(cbcl_old[i,dsm_adhd_items],na.rm=F)
  cbcl_old$dsm_odd_raw[i] = sum(cbcl_old[i,dsm_odd_items],na.rm=F)
  cbcl_old$dsm_conduct_raw[i] = sum(cbcl_old[i,dsm_conduct_items],na.rm=F)
  cbcl_old$internalizing_raw[i] = sum(cbcl_old[i,internalizing_items],na.rm=F)
  cbcl_old$externalizing_raw[i] = sum(cbcl_old[i,externalizing_items],na.rm=F)
  cbcl_old$total_raw[i] = sum(cbcl_old[i,total_items],na.rm=F)
  if(cbcl_old$missing_items_count[i]>8){
    cbcl_old[i,which(names(cbcl_old)=="anxious_depressed_raw"):which(names(cbcl_old)=="total_raw")]=NA
  }
}

#modifying norm table
cbcl_scale = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/scr_cbcl_6_18_scales.csv",stringsAsFactors = F)
cbcl_t_scores = cbcl_scale[,c(1,14)]
cbcl_scale_revised = data.frame(cbcl_scale$t_score)
cbcl_scale = cbcl_scale[,-14]
cbcl_scale_revised = modifying_vabs_norm_tables(cbcl_scale,cbcl_scale_revised)
cbcl_t_scores = cbind(cbcl_t_scores,cbcl_scale_revised[,2:ncol(cbcl_scale_revised)])
rm(cbcl_scale,cbcl_scale_revised)
cbcl_dsm = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/cbcl_6-18yrs_dsm5.csv",stringsAsFactors = F)
cbcl_dsm = cbcl_dsm[,c(1:9)]
cbcl_dsm$gender = ifelse(cbcl_dsm$gender=="Female","F","M")
cbcl_dsm_revised = cbcl_dsm[,c(1:2)]
cbcl_dsm = modifying_vabs_norm_tables(cbcl_dsm,cbcl_dsm_revised)
rm(cbcl_dsm_revised)

#obtaining t scores
for(i in 1:nrow(cbcl_old)){
  cbcl_old$anxious_depressed_t[i] = ifelse(!is.na(cbcl_old$anxious_depressed_raw[i]),
                                             cbcl_t_scores[which(cbcl_old$anxious_depressed_raw[i]>=cbcl_t_scores$anxious_depressed_bottom
                                                                & cbcl_old$anxious_depressed_raw[i]<=cbcl_t_scores$anxious_depressed_top
                                                                & floor(cbcl_old$age[i]/12)>=cbcl_t_scores$age_bottom
                                                                & floor(cbcl_old$age[i]/12)<=cbcl_t_scores$age_top
                                                                & cbcl_old$gender[i] == cbcl_t_scores$gender),1],NA)
  cbcl_old$somatic_complaints_t[i] = ifelse(!is.na(cbcl_old$somatic_complaints_raw[i]),
                                              cbcl_t_scores[which(cbcl_old$somatic_complaints_raw[i] >=cbcl_t_scores$somatic_complaints_bottom
                                                                 & cbcl_old$somatic_complaints_raw[i]<=cbcl_t_scores$somatic_complaints_top
                                                                 & floor(cbcl_old$age[i]/12)>=cbcl_t_scores$age_bottom
                                                                 & floor(cbcl_old$age[i]/12)<=cbcl_t_scores$age_top
                                                                 & cbcl_old$gender[i] == cbcl_t_scores$gender),1],NA)
  cbcl_old$withdrawn_t[i] = ifelse(!is.na(cbcl_old$withdrawn_raw[i]),
                                     cbcl_t_scores[which(cbcl_old$withdrawn_raw[i] >=cbcl_t_scores$withdrawn_bottom
                                                        & cbcl_old$withdrawn_raw[i]<=cbcl_t_scores$withdrawn_top
                                                        & floor(cbcl_old$age[i]/12)>=cbcl_t_scores$age_bottom
                                                        & floor(cbcl_old$age[i]/12)<=cbcl_t_scores$age_top
                                                        & cbcl_old$gender[i] == cbcl_t_scores$gender),1],NA)
  cbcl_old$social_problem_t[i] = ifelse(!is.na(cbcl_old$social_problem_raw[i]),
                                         cbcl_t_scores[which(cbcl_old$social_problem_raw[i] >=cbcl_t_scores$social_problems_bottom
                                                             & cbcl_old$social_problem_raw[i]<=cbcl_t_scores$social_problems_top
                                                             & floor(cbcl_old$age[i]/12)>=cbcl_t_scores$age_bottom
                                                             & floor(cbcl_old$age[i]/12)<=cbcl_t_scores$age_top
                                                             & cbcl_old$gender[i] == cbcl_t_scores$gender),1],NA)
  cbcl_old$thought_problem_t[i] = ifelse(!is.na(cbcl_old$thought_problem_raw[i]),
                                         cbcl_t_scores[which(cbcl_old$thought_problem_raw[i] >=cbcl_t_scores$thought_problems_bottom
                                                            & cbcl_old$thought_problem_raw[i]<=cbcl_t_scores$thought_problems_top
                                                            & floor(cbcl_old$age[i]/12)>=cbcl_t_scores$age_bottom
                                                            & floor(cbcl_old$age[i]/12)<=cbcl_t_scores$age_top
                                                            & cbcl_old$gender[i] == cbcl_t_scores$gender),1],NA)
  cbcl_old$attention_problem_t[i] = ifelse(!is.na(cbcl_old$attention_problem_raw[i]),
                                             cbcl_t_scores[which(cbcl_old$attention_problem_raw[i] >=cbcl_t_scores$attention_problems_bottom
                                                                & cbcl_old$attention_problem_raw[i]<=cbcl_t_scores$attention_problems_top
                                                                & floor(cbcl_old$age[i]/12)>=cbcl_t_scores$age_bottom
                                                                & floor(cbcl_old$age[i]/12)<=cbcl_t_scores$age_top
                                                                & cbcl_old$gender[i] == cbcl_t_scores$gender),1],NA)
  cbcl_old$aggressive_t[i] = ifelse(!is.na(cbcl_old$aggressive_behavior_raw[i]),
                                   cbcl_t_scores[which(cbcl_old$aggressive_behavior_raw[i] >=cbcl_t_scores$aggressive_behavior_bottom
                                                       & cbcl_old$aggressive_behavior_raw[i]<=cbcl_t_scores$aggressive_behavior_top
                                                       & floor(cbcl_old$age[i]/12)>=cbcl_t_scores$age_bottom
                                                       & floor(cbcl_old$age[i]/12)<=cbcl_t_scores$age_top
                                                       & cbcl_old$gender[i] == cbcl_t_scores$gender),1],NA)
  cbcl_old$defiant_t[i] = ifelse(!is.na(cbcl_old$defiant_raw[i]),
                                     cbcl_t_scores[which(cbcl_old$defiant_raw[i] >=cbcl_t_scores$rule_breaking_behavior_bottom
                                                        & cbcl_old$defiant_raw[i]<=cbcl_t_scores$rule_breaking_behavior_top
                                                        & floor(cbcl_old$age[i]/12)>=cbcl_t_scores$age_bottom
                                                        & floor(cbcl_old$age[i]/12)<=cbcl_t_scores$age_top
                                                        & cbcl_old$gender[i] == cbcl_t_scores$gender),1],NA)
  cbcl_old$dsm_depressive_t[i] = ifelse(!is.na(cbcl_old$dsm_depressive_raw[i]),
                                      cbcl_dsm[which(cbcl_old$dsm_depressive_raw[i] >=cbcl_dsm$dep_raw_bottom
                                                     & cbcl_old$dsm_depressive_raw[i]<=cbcl_dsm$dep_raw_top
                                                     & floor(cbcl_old$age[i]/12)>=cbcl_dsm$age_bottom
                                                     & floor(cbcl_old$age[i]/12)<=cbcl_dsm$age_top
                                                     & cbcl_old$gender[i] == cbcl_dsm$gender),1],NA)
  cbcl_old$dsm_anxiety_t[i] = ifelse(!is.na(cbcl_old$dsm_anxiety_raw[i]),
                                      cbcl_dsm[which(cbcl_old$dsm_anxiety_raw[i] >=cbcl_dsm$anx_raw_bottom
                                                     & cbcl_old$dsm_anxiety_raw[i]<=cbcl_dsm$anx_raw_top
                                                     & floor(cbcl_old$age[i]/12)>=cbcl_dsm$age_bottom
                                                     & floor(cbcl_old$age[i]/12)<=cbcl_dsm$age_top
                                                     & cbcl_old$gender[i] == cbcl_dsm$gender),1],NA)
  cbcl_old$dsm_somatic_t[i] = ifelse(!is.na(cbcl_old$dsm_somatic_raw[i]),
                                      cbcl_dsm[which(cbcl_old$dsm_somatic_raw[i] >=cbcl_dsm$som_raw_bottom
                                                     & cbcl_old$dsm_somatic_raw[i]<=cbcl_dsm$som_raw_top
                                                     & floor(cbcl_old$age[i]/12)>=cbcl_dsm$age_bottom
                                                     & floor(cbcl_old$age[i]/12)<=cbcl_dsm$age_top
                                                     & cbcl_old$gender[i] == cbcl_dsm$gender),1],NA)
  cbcl_old$dsm_adhd_t[i] = ifelse(!is.na(cbcl_old$dsm_adhd_raw[i]),
                                      cbcl_dsm[which(cbcl_old$dsm_adhd_raw[i] >=cbcl_dsm$att.hyp_raw_bottom
                                                     & cbcl_old$dsm_adhd_raw[i]<=cbcl_dsm$att.hyp_raw_top
                                                     & floor(cbcl_old$age[i]/12)>=cbcl_dsm$age_bottom
                                                     & floor(cbcl_old$age[i]/12)<=cbcl_dsm$age_top
                                                     & cbcl_old$gender[i] == cbcl_dsm$gender),1],NA)
  cbcl_old$dsm_odd_t[i] = ifelse(!is.na(cbcl_old$dsm_odd_raw[i]),
                                      cbcl_dsm[which(cbcl_old$dsm_odd_raw[i] >=cbcl_dsm$opp.def_raw_bottom
                                                     & cbcl_old$dsm_odd_raw[i]<=cbcl_dsm$opp.def_raw_top
                                                     & floor(cbcl_old$age[i]/12)>=cbcl_dsm$age_bottom
                                                     & floor(cbcl_old$age[i]/12)<=cbcl_dsm$age_top
                                                     & cbcl_old$gender[i] == cbcl_dsm$gender),1],NA)
  cbcl_old$dsm_conduct_t[i] = ifelse(!is.na(cbcl_old$dsm_conduct_raw[i]),
                                      cbcl_dsm[which(cbcl_old$dsm_conduct_raw[i] >=cbcl_dsm$con_raw_bottom
                                                     & cbcl_old$dsm_conduct_raw[i]<=cbcl_dsm$con_raw_top
                                                     & floor(cbcl_old$age[i]/12)>=cbcl_dsm$age_bottom
                                                     & floor(cbcl_old$age[i]/12)<=cbcl_dsm$age_top
                                                     & cbcl_old$gender[i] == cbcl_dsm$gender),1],NA)
  cbcl_old$internalizing_t[i] = ifelse(!is.na(cbcl_old$internalizing_raw[i]),
                                         cbcl_t_scores[which(cbcl_old$internalizing_raw[i] >=cbcl_t_scores$internalizing_bottom
                                                            & cbcl_old$internalizing_raw[i]<=cbcl_t_scores$internalizing_top
                                                            & floor(cbcl_old$age[i]/12)>=cbcl_t_scores$age_bottom
                                                            & floor(cbcl_old$age[i]/12)<=cbcl_t_scores$age_top
                                                            & cbcl_old$gender[i] == cbcl_t_scores$gender),1],NA)
  cbcl_old$externalizing_t[i] = ifelse(!is.na(cbcl_old$externalizing_raw[i]),
                                         cbcl_t_scores[which(cbcl_old$externalizing_raw[i] >=cbcl_t_scores$externalizing_bottom
                                                            & cbcl_old$externalizing_raw[i]<=cbcl_t_scores$externalizing_top
                                                            & floor(cbcl_old$age[i]/12)>=cbcl_t_scores$age_bottom
                                                            & floor(cbcl_old$age[i]/12)<=cbcl_t_scores$age_top
                                                            & cbcl_old$gender[i] == cbcl_t_scores$gender),1],NA)
  cbcl_old$total_t[i] = ifelse(!is.na(cbcl_old$total_raw[i]),
                                 cbcl_t_scores[which(cbcl_old$total_raw[i] >=cbcl_t_scores$total_bottom
                                                    & cbcl_old$total_raw[i]<=cbcl_t_scores$total_top
                                                    & floor(cbcl_old$age[i]/12)>=cbcl_t_scores$age_bottom
                                                    & floor(cbcl_old$age[i]/12)<=cbcl_t_scores$age_top
                                                    & cbcl_old$gender[i] == cbcl_t_scores$gender),1],NA)
}

#orphaned/duplicate data
cbcl_old_orphaned_data = orphaned_data_consolidate(cbcl_old)
cbcl_old = orphaned_data_remove(cbcl_old)
cbcl_old_duplicate_data = duplicate_data_consolidate(cbcl_old,"age")
cbcl_old = duplicate_data_remove(cbcl_old,"age")

#outliers
cbcl_old = inserting_prefix_into_variables(cbcl_old,"cbcl_")
cbcl_old_outliers = cbcl_old[grep("^[0-9]{6}-[0-9]{3}$",cbcl_old$id),c(1:2,grep("age$",names(cbcl_old)),grep("_t$",names(cbcl_old)))]
cbcl_old_outliers = outlier_list(cbcl_old_outliers)
cbcl_staar_outliers = cbcl_old[grep("^[0-9]{4}$",cbcl_old$id),c(1:2,grep("_age$",names(cbcl_old)),grep("_t$",names(cbcl_old)))]
cbcl_staar_outliers = outlier_list(cbcl_staar_outliers)

#finalizing the format of the data
cbcl_young_scored = cbcl_young[,c(1:2,grep("age$",names(cbcl_young)),grep("missing",names(cbcl_young)),grep("_raw$",names(cbcl_young)),grep("_t$",names(cbcl_young)),ncol(cbcl_young))]
cbcl_young_scored$cbcl_version = "Preschool"
cbcl_old_scored = cbcl_old[grep("[0-9]{6}-[0-9]{3}",cbcl_old$id),c(1:2,grep("age$",names(cbcl_old)),grep("missing",names(cbcl_old)),grep("_raw$",names(cbcl_old)),grep("_t$",names(cbcl_old)))]
cbcl_old_scored$cbcl_version = "School Age"
cbcl_old_scored$cbcl_outlier_list = cbcl_old_outliers$outlier_list
cbcl_scored_staar = cbcl_old[grep("^[0-9]{4}$",cbcl_old$id),c(1:2,grep("age$",names(cbcl_old)),grep("missing",names(cbcl_old)),grep("_raw$",names(cbcl_old)),grep("_t$",names(cbcl_old)))]
cbcl_scored_staar$cbcl_outlier_list = cbcl_staar_outliers$outlier_list
cbcl_scored = rbind.fill(cbcl_young_scored,cbcl_old_scored)
cbcl_scored = cbcl_scored[,c(1:12,36:38,13:16,39:41,17:26,42:44,27:30,45:47,31:35)]

#item tables
cbcl_young_items = cbcl_young[,c(1:2,grep("cbcl_[0-9]{1,3}[a-h]{0,1}$",names(cbcl_young)))]
cbcl_young_items = removing_prefix(cbcl_young_items,'cbcl_')
cbcl_young_items = inserting_prefix_into_variables(cbcl_young_items,'cbcl_15_')
cbcl_old_items = cbcl_old[,c(1:2,grep("cbcl_[0-9]{1,3}[a-h]{0,1}$",names(cbcl_old)))]
cbcl_old_items = removing_prefix(cbcl_old_items,'cbcl_')
cbcl_old_items = inserting_prefix_into_variables(cbcl_old_items,'cbcl_618_')

#archiving the data
write.csv(cbcl_scored, "S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/cbcl_scored.csv",row.names = F)
write.csv(cbcl_scored_staar, "S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived_staar/cbcl_scored.csv",row.names = F)
write.csv(cbcl_young_items,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/cbcl_1-5_items.csv",row.names=F)
write.csv(cbcl_old_items[grep("^[0-9]{4}$",cbcl_old$id),],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived_staar/cbcl_items.csv",row.names=F)
write.csv(cbcl_old_items[-grep("^[0-9]{4}$",cbcl_old$id),],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/cbcl_6-18_items.csv",row.names=F)

#cleaning up
rm(anxious_depressed_items,somatic_items,withdrawn_items,social_items,thought_items,attention_items,
   defiant_items,aggressive_items,other_items,internalizing_items,externalizing_items,total_items,
   dsm_conduct_items,dsm_depressive_items,dsm_somatic_items,dsm_anxiety_items,dsm_adhd_items,dsm_odd_items,
   cbcl_t_scores,cbcl_tscores_revised,cbcl_old_outliers,cbcl_staar_outliers,cbcl_young_rc,cbcl_old_rc,
   cbcl_young_t1,cbcl_young_t3,cbcl_old_t5)