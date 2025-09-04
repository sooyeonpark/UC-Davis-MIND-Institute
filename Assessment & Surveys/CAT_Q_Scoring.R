catq_t5 = exportRecords(ace_con,forms=c("participant_information","cat_q"),events="t5_arm_2",labels=F,stringsAsFactors=F)
catq_t5 = catq_t5[grepl("Co",catq_t5$cat_q_complete)&!is.na(catq_t5$cat_q_timestamp),-(2:grep("participant_information_complete",names(catq_t5)))]
catq_t5$visit = 5

catq = sqlFetch(new_con,"CAT-Q",stringsAsFactors=F)
catq = id_visit_changing_into_char(catq)
catq_entry_flag = entry_flag(catq,'cat_q')
catq = subset(catq,entry_status==2)
catq = removing_prefix(catq,'catq_')
catq_t5 = study_id_to_id(catq_t5,"catq_")
catq = identify_same_data(catq_t5,catq)
catq = rbind.fill(catq,catq_t5)

catq = fxage(catq,'id','date')

for(j in paste0('',1:25)){
  catq[,j] = ifelse(tolower(catq[,j])=="strongly disagree",1,
                    ifelse(tolower(catq[,j])=="disagree",2,
                           ifelse(tolower(catq[,j])=="somewhat disagree",3,
                                  ifelse(tolower(catq[,j])=="neither agree nor disagree",4,
                                         ifelse(tolower(catq[,j])=="somewhat agree",5,
                                                ifelse(tolower(catq[,j])=="agree",6,
                                                       ifelse(tolower(catq[,j])=="strongly agree",7,NA)))))))
  catq[,j] = as.numeric(catq[,j])
}

comp_items = paste0('',c(1,4:5,8,seq(11,23,by=3)))
mask_items = paste0('',c(2,seq(6,24,by=3)))
assim_items = paste0('',c(3,seq(7,25,by=3)))
total_items = c(comp_items,mask_items,assim_items)

catq = count_missing_items(catq,'1','25')
catq = comment_missing_data(catq,list(comp_items,mask_items,assim_items),list('compensation','masking','assimilation'))

reverse_items = paste0('',c(3,12,19,22,24))
for(j in reverse_items){
 catq[,j] = 8 - catq[,j] 
}

catq = summing_items_per_row(catq,list(comp_items,mask_items,assim_items,total_items),
                             list('compensation_total','masking_total','assimilation_total','raw_total'),F)

catq = inserting_prefix_into_variables(catq,'catq_')

catq_orphaned_data = orphaned_data_consolidate(catq)
catq = orphaned_data_remove(catq)
catq_duplicate_data = duplicate_data_consolidate(catq,'catq_age')
catq = duplicate_data_remove(catq,'catq_age')

catq_outliers = catq[,c(1:2,grep("_age$",names(catq)),grep("_total$",names(catq)))]
catq_outliers = outlier_list(catq_outliers)
catq$catq_outlier_list = catq_outliers$outlier_list

catq_scored = catq[,c(1:2,grep("_age$",names(catq)),grep("missing",names(catq)),grep("_total$",names(catq)),ncol(catq))]
write.csv(catq_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/CAT-Q/catq_scored.csv",row.names=F)

rm(comp_items,mask_items,assim_items,total_items,catq_outliers,reverse_items,catq_t5)
