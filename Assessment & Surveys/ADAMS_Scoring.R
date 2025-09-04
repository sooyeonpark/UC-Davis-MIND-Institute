#importing table from ACCESS
adams = sqlQuery(con4,"select * from ADAMS;",stringsAsFactors=F)

adams = id_visit_changing_into_char(adams)
adams = subset(adams,entry_status==2)
adams = removing_prefix(adams,"adams")

#calculating age
adams = fxage(adams,'id','date')

#list of items
mhb_items = paste0('',c(3:4,12,17,22))
dm_itmes = paste0('',c(5,9:10,14,18,23,28))
sa_itmes = paste0('',c(2,6,13,19,21,25,27))
ga_itmes = paste0('',c(1,3,7,11,15,24,26))
cb_itmes = paste0('',c(8,16,20))

#missing data analysis
adams = count_missing_items(adams,'1','20')
adams = comment_missing_data(adams,list(mhb_items,dm_itmes,sa_itmes,ga_itmes,cb_itmes),
                             list('mhb','dm','sa','ga','cb'))

#obtaining raw total
adams = summing_items_per_row(adams,list(mhb_items,dm_itmes,sa_itmes,ga_itmes,cb_itmes),
                             list('mhb_total','dm_total','sa_total','ga_total','cb_total'),F)
#putting back prefix
adams = inserting_prefix_into_variables(adams,'adams_')

#orphaned/duplicate data
adams_orphaned_data = orphaned_data_consolidate(adams)
adams = orphaned_data_remove(adams)
adams_duplicate_data = duplicate_data_consolidate(adams,'adams_age')
adams = duplicate_data_remove(adams,'adams_age')

#outliers
adams_outliers = adams[,c(1:2,grep("_total$",names(adams)))]
adams_outliers = outlier_list(adams_outliers)
adams$adams_outlier_list = adams_outliers$outlier_list

#archiving the data
adams_scored = adams[,c(1:2,grep("_age$",names(adams)):ncol(adams))]
write.csv(adams_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived_staar/adams_scored.csv",row.names=F)

#cleaning up

rm(adams_outliers,mhb_items,dm_itmes,sa_itmes,ga_itmes,cb_itmes)
