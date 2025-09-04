#pulling out the tables
pras_t5 = exportRecords(ace_con,forms=c("participant_information","prasasd"),events="t5_arm_1",labels=F,stringsAsFactors=F)
pras_t5 = pras_t5[grepl("Co",pras_t5$prasasd_complete)&!is.na(pras_t5$prasasd_timestamp),-(2:grep("participant_information_complete",names(pras_t5)))]
pras_t5$visit = 5
pras = sqlQuery(con4,"select * from `Parent Rated Anxiety Scale - ASD`",stringsAsFactors=F)

#getting rid of prefixes, changing id and visit into characters, and removing single-entered rows
pras = id_visit_changing_into_char(pras)
pras_entry_flag = entry_flag(pras,'pras')
pras = subset(pras,entry_status==2)
pras = removing_prefix(pras,"pras_")
pras_t5 = study_id_to_id(pras_t5,"pras_")
pras = identify_same_data(pras_t5,pras)
pras = rbind.fill(pras,pras_t5)

#calculating age
pras = fxage(pras,'id','date')

#missing data analysis
pras_items = paste0('',1:25)
pras = count_missing_items(pras,'1','25')
pras = comment_missing_data(pras,list(pras_items),list('total'))

#calculating total scores
for(i in 1:nrow(pras)){
  pras$total[i] = sum(pras[i,pras_items],na.rm=T)
}

#putting prefixes back in & finding outliers
pras = inserting_prefix_into_variables(pras,"pras_")
pras_outliers = pras[,c(1:2,grep("_total",names(pras)))]
pras_outliers = outlier_list(pras_outliers)
pras$pras_outlier_list = pras_outliers$outlier_list
rm(pras_outliers)

#orphaned/duplicate data
pras_orphaned_data = orphaned_data_consolidate(pras)
pras = orphaned_data_remove(pras)
pras_duplicate_data = duplicate_data_consolidate(pras,"pras_age")
pras = duplicate_data_remove(pras,"pras_age")

#archiving the table
pras_scored = pras[,c(1:2,grep("_age$",names(pras)):ncol(pras))]
write.csv(pras_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived_staar/pras_scored.csv",row.names = F)
write.csv(pras[,c(1:2,grep("_rater$",names(pras)):grep("_25$",names(pras)))],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived_staar/pras_items.csv",row.names = F)

#cleaning up
rm(pras_items,pras_t5)
