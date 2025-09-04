fqs = sqlQuery(new_con,"select * from Friendship_Quality_Scale;",stringsAsFactors=F)
fqs = id_visit_changing_into_char(fqs)
fqs = removing_prefix(fqs,'fqs_')
fqs = subset(fqs,entry_status==2)

#calculate age
fqs = fxage(fqs,'id','date')

#dealing with reversed scores -> item 9
fqs$`9` = 6 - fqs$`9`

#dealing with negative scores
for(j in which(names(fqs)=="ex"):which(names(fqs)=="46")){
  fqs[,j] = cbraw(fqs[,j])
}

#section item lists
cooperation_items = paste0('',c(1,6,14,19))
conflict_items = paste0('',c(2,7,15,20))
help_items = paste0('',c(3,8,13,16,21))
intimacy_items = paste0('',c(4,9,11,17,22))
closeness_items = paste0('',c(5,10,12,18,23))

#missing data analysis
fqs = count_missing_items(fqs,'1','46')
fqs = comment_missing_data(fqs,list(cooperation_items,conflict_items,help_items,intimacy_items,closeness_items),
                           list('cooperation','conflict','help','intimacy','closeness'))

#summing up scores
fqs = summing_items_per_row(fqs,list(cooperation_items,conflict_items,help_items,intimacy_items,closeness_items),
                            list('cooperation_total','conflict_total','help_total','intimacy_total','closeness_total'),F)

#putting back prefix
fqs = inserting_prefix_into_variables(fqs,"fqs_")

#orphaned/duplicate data
fqs_orphaned_data = orphaned_data_consolidate(fqs)
fqs = orphaned_data_remove(fqs)
length(unique(fqs$id))==nrow(fqs)

#outliers
fqs_outliers = fqs[,c(1:2,54,57:ncol(fqs))]
fqs_outliers = outlier_list(fqs_outliers)
fqs$fqs_outlier_list = fqs_outliers$outlier_list

#archiving the data
fqs_scored = fqs[,c(1:2,grep("_age$",names(fqs)):ncol(fqs))]
write.csv(fqs_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/fqs_scored.csv",row.names=F)
write.csv(fqs[,c(1:2,grep("_[0-9]+$",names(fqs)))],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/fqs_items.csv",row.names=F)

#clean up
rm(cooperation_items,conflict_items,help_items,intimacy_items,closeness_items,fqs_outliers)
