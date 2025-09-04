pas = sqlQuery(con4,"select * from `Pediatric Accomodation Scale`;",stringsAsFactors=F)
pas = id_visit_changing_into_char(pas)
pas = subset(pas,entry_status==2)
pas = removing_prefix(pas,'pas_')

pas = fxage(pas,'id','date')

freq_items = paste0('',c(1:5,7:12))
pi_items = paste0('',c(3:5,11),'si')
ci_items = paste0('',c(1:2,7:10,12),'si')
pas = count_missing_items(pas,'1','5si')
pas = count_missing_items(pas,'7','12si')
pas = comment_missing_data(pas,list(freq_items,pi_items,ci_items),list('frequency','parent_impact','child_impact'))

pas = summing_items_per_row(pas,list(freq_items,pi_items,ci_items),list('frequency_total','parent_impact_total','child_impact_total'),F)
pas$frequency_mean = round(rowMeans(pas[,freq_items]),2)
pas$parent_impact_mean = round(rowMeans(pas[,pi_items]),2)
pas$child_impact_mean = round(rowMeans(pas[,ci_items]),2)

pas = inserting_prefix_into_variables(pas,'pas_')

#orphaned/duplicate data
pas_orphaned_data = orphaned_data_consolidate(pas)
pas = orphaned_data_remove(pas)
pas_duplicate_data = duplicate_data_consolidate(pas,"pas_age")
pas = duplicate_data_remove(pas,"pas_age")

pas_scored = pas[,c(1:2,grep("_age$",names(pas)),grep("missing",names(pas)),grep("_total$",names(pas)),grep("_mean$",names(pas)))]
pas_scored = outlier_list(pas_scored)
names(pas_scored)[ncol(pas_scored)] = "pas_outlier_list"
write.csv(pas_scored,'S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived_staar/pas_scored.csv',na='',row.names=F)
write.csv(pas[,c(1:2,6:45)],'S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived_staar/pas_items.csv',na='',row.names=F)

rm(freq_items,pi_items,ci_items)
