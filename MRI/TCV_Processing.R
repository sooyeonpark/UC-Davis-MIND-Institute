tcv = sqlQuery(mri_con,"select * from tblTCV",stringsAsFactors=F)
names(tcv) = tolower(names(tcv))

names(tcv)[1]="id"
names(tcv)[3]="tcv_total"
tcv = id_visit_changing_into_char(tcv)
tcv = tcv[which(tcv$visit != '4p'),]

#outliers
tcv = outlier_list(tcv)
tcv[which(tcv$outlier_list != ''),"outlier_list"] = "tcv_total;"
names(tcv)[ncol(tcv)] = "tcv_outlier_list"
#tcv_outlier_table = sqldf("select id,visit,outlier_list from tcv_processed where outlier_list != ''")

#orphaned/duplicate data
tcv_orphaned_data = orphaned_data_consolidate(tcv)
tcv = orphaned_data_remove(tcv)
if(all(tcv_orphaned_data$id %in% app_cohort_subj$`subj id`==T)){
  rm(tcv_orphaned_data)
}
tcv$age = tcv$visit
tcv_duplicate_data = duplicate_data_consolidate(tcv,"age")
tcv = duplicate_data_remove(tcv,"age")
tcv$age=NULL

#cleaning up

#archiving the table
write.csv(tcv,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/tcv_processed.csv",row.names = F)
