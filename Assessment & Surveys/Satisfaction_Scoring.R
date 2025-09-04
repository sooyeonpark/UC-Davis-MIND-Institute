#importing tables & initial data manipulation
satisfaction = sqlFetch(con4,"Satisfaction_Child",stringsAsFactors=F)
satisfaction_parent = sqlFetch(con4,"SatisfP",stringsAsFactors=F)
satisfaction = id_visit_changing_into_char(satisfaction)
satisfaction_parent = id_visit_changing_into_char(satisfaction_parent)
satisfaction = subset(satisfaction,entry_status==2)
satisfaction_parent = subset(satisfaction_parent,entry_status==2)
satisfaction = removing_prefix(satisfaction,'satisfaction_c_')
satisfaction_parent = removing_prefix(satisfaction_parent,'satisf_p_')

#calculate age
satisfaction = fxage(satisfaction,'id','date')
satisfaction_parent = fxage(satisfaction_parent,'id','date')

#missing data analysis
#satisfaction = count_missing_items(satisfaction,'1','11')
satisfaction_parent = count_missing_items(satisfaction_parent,'1','11')

#obtaining total scores
#satisfaction = summing_items_per_row(satisfaction,list(paste0('',1:11)),list('total'),F)
satisfaction_parent = summing_items_per_row(satisfaction_parent,list(paste0('',1:11)),list('total'),F)

#putting back prefix
#satisfaction = inserting_prefix_into_variables(satisfaction,'satisfaction_')
satisfaction_parent = inserting_prefix_into_variables(satisfaction_parent,'satisfaction_parent_')

#orphaned/duplicate data
#satisfaction_orphaned_data = orphaned_data_consolidate(satisfaction)
#satisfaction = orphaned_data_remove(satisfaction)
#satisfaction_duplicate_data = duplicate_data_consolidate(satisfaction,'satisfaction_age')
#satisfaction = duplicate_data_remove(satisfaction,'satisfaction_age')
satisfaction_parent_orphaned_data = orphaned_data_consolidate(satisfaction_parent)
satisfaction_parent = orphaned_data_remove(satisfaction_parent)
satisfaction_parent_duplicate_data = duplicate_data_consolidate(satisfaction_parent,'satisfaction_parent_age')
satisfaction_parent = duplicate_data_remove(satisfaction_parent,'satisfaction_parent_age')

#outliers & archiving the data
#satisfaction_scored = satisfaction[,c(1:2,grep("_age$",names(satisfaction)),grep("missing",names(satisfaction)),grep("_total$",names(satisfaction)))]
#satisfaction_scored = outlier_list(satisfaction_scored)
#names(satisfaction_scored)[ncol(satisfaction_scored)] = "satisfaction_outlier_list"
satisfaction_parent_scored = satisfaction_parent[,c(1:2,grep("_age$",names(satisfaction_parent)),grep("missing",names(satisfaction_parent)),grep("_total$",names(satisfaction_parent)))]
satisfaction_parent_scored = outlier_list(satisfaction_parent_scored)
names(satisfaction_parent_scored)[ncol(satisfaction_parent_scored)] = "satisfaction_parent_outlier_list"
#write.csv(satisfaction_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived_staar/satisfaction_scored.csv",row.names=F)
write.csv(satisfaction_parent_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived_staar/satisfaction_parent_scored.csv",row.names=F)
write.csv(satisfaction_parent[,c(1:2,grep("_1$",names(satisfaction_parent)):grep("_14$",names(satisfaction_parent)))],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived_staar/satisfaction_parent_items.csv",row.names=F)
