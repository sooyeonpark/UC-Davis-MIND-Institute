#pulling the data and initial manipulation
ysr_t5 = exportRecords(ace_con,forms=c("participant_information","youthselfreport"),events="t5_arm_2",labels=F,stringsAsFactors=F)
ysr_t5 = ysr_t5[grepl("Co",ysr_t5$youthselfreport_complete)&!is.na(ysr_t5$youthselfreport_timestamp),-(2:grep("participant_information_complete",names(ysr_t5)))]
ysr_t5$visit = 5
ysr = sqlFetch(new_con,"YouthSelfReport",stringsAsFactors=F)
ysr = id_visit_changing_into_char(ysr)
ysr_entry_flag = entry_flag(ysr,'ysr')
ysr = subset(ysr,entry_status==2)
ysr = removing_prefix(ysr,'ysr_')
ysr_t5 = study_id_to_id(ysr_t5,"ysr_")
ysr_redcap_var = names(ysr)[grep("^[0-9]+$",names(ysr))]
names(ysr)=tolower(names(ysr))
ysr = identify_same_data(ysr_t5,ysr)
ysr = rbind.fill(ysr,ysr_t5)

#calculate age
ysr = fxage(ysr,'id','date')

#item lists
anx_dep_items = paste0('',c(14,29:33,35,45,50,52,71,91,112))
withd_dep_items = paste0('',c(5,42,65,69,75,102:103,111))
somatic_items = c(paste0('',c(47,51,54,'a','b','c','d','e','f','g')))
social_items = paste0('',c(11:12,25,27,34,36,38,48,62,64,79))
thought_items = paste0('',c(9,18,40,46,58,66,70,76,83:85,100))
attention_items = paste0('',c(1,4,8,10,13,17,41,61,78))
defiant_items = paste0('',c(2,26,28,39,43,63,67,72,81:82,90,96,99,101,105))
aggressive_items = paste0('',c(3,16,19:23,37,57,68,86:87,89,94:95,97,104))
affective_dsm_items = paste0('',c(5,14,18,24,35,52,53,76:77,91,100,102:103))
anxiety_dsm_items = paste0('',c(11,29:30,45,50,112))
somatic_dsm_items = c('a','b','c','d','e','f','g')
adhd_dsm_items = paste0('',c(4,8,10,41,78,93,104))
odd_dsm_items = paste0('',c(3,22:23,86,95))
conduct_dsm_items = paste0('',c(16,21,26,28,37,39,43,57,67,72,81:82,90,97,101))

#missing data analysis
ysr = count_missing_items(ysr,'1','112')
ysr = comment_missing_data(ysr,list(anx_dep_items,withd_dep_items,somatic_items,social_items,
                                    thought_items,attention_items,defiant_items,aggressive_items,
                                    affective_dsm_items,anxiety_dsm_items,somatic_dsm_items,
                                    adhd_dsm_items,odd_dsm_items,conduct_dsm_items),
                           list('anxious_depressed','withdrawn_depressed','somatic_complaints',
                                'social_problems','thought_problems','attention_problems',
                                'defiant_behavior','aggressive_behavior','dsm_affective_problems',
                                'dsm_anxiety_problems','dsm_somatic_problems','dsm_adhd',
                                'dsm_odd','dsm_conduct_problems'))

#obtaining subscale scores
ysr = summing_items_per_row(ysr,list(anx_dep_items,withd_dep_items,somatic_items,social_items,
                                     thought_items,attention_items,defiant_items,aggressive_items,
                                     affective_dsm_items,anxiety_dsm_items,somatic_dsm_items,
                                     adhd_dsm_items,odd_dsm_items,conduct_dsm_items),
                            list('anxious_depressed_raw','withdrawn_depressed_raw','somatic_complaints_raw',
                                 'social_problems_raw','thought_problems_raw','attention_problems_raw',
                                 'defiant_behavior_raw','aggressive_behavior_raw','dsm_affective_problems_raw',
                                 'dsm_anxiety_problems_raw','dsm_somatic_problems_raw','dsm_adhd_raw',
                                 'dsm_odd_raw','dsm_conduct_problems_raw'),F)

#obtaining norm scores

#putting back prefix
ysr = inserting_prefix_into_variables(ysr,'ysr_')

#orphaned/duplicate data
ysr_orphaned_data = orphaned_data_consolidate(ysr)
ysr = orphaned_data_remove(ysr)
ysr_duplicate_data = duplicate_data_consolidate(ysr,'ysr_age')
ysr = duplicate_data_remove(ysr,'ysr_age')

#outliers
ysr_outliers = ysr[,c(1:2,grep("_age$",names(ysr)),grep("_t$",names(ysr)))]
ysr_outliers = outlier_list(ysr_outliers)
ysr$ysr_outlier_list = ysr_outliers$outlier_list
rm(ysr_outliers)

#extracting relevant columns and archiving the data
ysr_scored = ysr[,c(1:2,grep("_age$",names(ysr)),grep("missing",names(ysr)),
                    grep("_raw",names(ysr)),grep("_t$",names(ysr)),grep("outlier_list",names(ysr)))]
write.csv(ysr_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/ysr_scored.csv",row.names=F)
write.csv(ysr[,c(1:2,grep("_[0-9]+$",names(ysr)))],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/ysr_items.csv",row.names=F)

#clean up
rm(anx_dep_items,withd_dep_items,somatic_dsm_items,somatic_items,social_items,thought_items,attention_items,ysr_t5,ysr_redcap_var,
   defiant_items,aggressive_items,affective_dsm_items,anxiety_dsm_items,adhd_dsm_items,odd_dsm_items,conduct_dsm_items)
