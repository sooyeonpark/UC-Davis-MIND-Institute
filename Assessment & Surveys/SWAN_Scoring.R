#pulling data and initial manipulation
swan_t5 = exportRecords(ace_con,forms=c("participant_information","swan"),events="t5_arm_1",labels=F,stringsAsFactors=F)
swan_t5 = swan_t5[grepl("Co",swan_t5$swan_complete)&!is.na(swan_t5$swan_timestamp),-(2:grep("participant_information_complete",names(swan_t5)))]
swan_t5$visit = 5
swan = sqlFetch(new_con,"SWAN",stringsAsFactors=F)
swan = id_visit_changing_into_char(swan)
swan_entry_flag = entry_flag(swan,'swan')
swan = subset(swan,entry_status==2)
swan = removing_prefix(swan,'swan_')
swan_t5 = study_id_to_id(swan_t5,"swan_")
swan = identify_same_data(swan_t5,swan)
swan = rbind.fill(swan,swan_t5)

#age calcualtion
swan = fxage(swan,'id','date')

#-9's to NA's
for(j in paste0('',1:30)){
  swan[,j] = ifelse(swan[,j]==-9,NA,swan[,j])
  swan[,j] = -1 * swan[,j]
}

#list of items by scale
inatt_items = paste0('',1:9)
hyper_items = paste0('',10:18)
oppos_items = paste0('',19:27)
sct_items = paste0('',28:30)

#missing data analysis
swan = count_missing_items(swan,'1','30')
swan = comment_missing_data(swan,list(inatt_items,hyper_items,oppos_items,sct_items),
                            list('inattention','hyperactivity','opposition','sct'))

#obtaining raw scores
swan = summing_items_per_row(swan,list(inatt_items,hyper_items,oppos_items,sct_items),
                             list('inattention_total','hyper_total','opposition_total','sct_total'),F)

#obtaining average scores, endorsed items, and dsm adhd
for(i in 1:nrow(swan)){
  swan$inattention_avg[i] = round(sum(swan[i,inatt_items],na.rm=T)/length(which(!is.na(swan[i,inatt_items]))),2)
  swan$hyper_avg[i] = round(sum(swan[i,hyper_items],na.rm=T)/length(which(!is.na(swan[i,hyper_items]))),2)
  swan$opposition_avg[i] = round(sum(swan[i,oppos_items],na.rm=T)/length(which(!is.na(swan[i,oppos_items]))),2)
  swan$sct_avg[i] = round(sum(swan[i,sct_items],na.rm=T)/length(which(!is.na(swan[i,sct_items]))),2)
  swan$inattention_num_endorsed[i] = length(which(swan[i,inatt_items]>=2))
  swan$hyper_num_endorsed[i] = length(which(swan[i,hyper_items]>=2))
  swan$opposition_num_endorsed[i] = length(which(swan[i,oppos_items]>=2))
  swan$sct_num_endorsed[i] = length(which(swan[i,sct_items]>=2))
  swan$dsm_adhd[i] = ifelse(swan$inattention_num_endorsed[i]>=6 & swan$hyper_num_endorsed[i]>=6,'ADHD - Combined',
                            ifelse(swan$hyper_num_endorsed[i]>=6,'ADHD - Hyperactive/Impulsive',
                                   ifelse(swan$inattention_num_endorsed[i]>=6,'ADHD - Inattentive','None')))
}

#inserting prefix
swan = inserting_prefix_into_variables(swan,'swan_')

#orphaned/duplicate data
swan_orphaned_data = orphaned_data_consolidate(swan)
swan = orphaned_data_remove(swan)
swan_duplicate_data = duplicate_data_consolidate(swan,'swan_age')
swan = duplicate_data_remove(swan,'swan_age')

#outliers
swan_outliers = swan[,c(1:2,grep("_age$",names(swan)),grep("_avg$",names(swan)))]
swan_outliers = outlier_list(swan_outliers)
swan$swan_outlier_list = swan_outliers$outlier_list

#archiving data
swan_scored = swan[,c(1:2,grep("_age$",names(swan)),grep("missing",names(swan)),
                      grep("inatt",names(swan)),grep("hyper",names(swan)),grep("oppos",names(swan)),
                      grep("sct",names(swan)),grep("_adhd$",names(swan)),ncol(swan))]
write.csv(swan_scored[,-grep("_total$",names(swan_scored))],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/swan_scored.csv",row.names=F)
write.csv(swan[,c(1:2,grep("_[0-9]+$",names(swan)))],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/swan_items.csv",row.names=F)

#clean up
rm(inatt_items,hyper_items,oppos_items,sct_items,swan_outliers,swan_t5)
