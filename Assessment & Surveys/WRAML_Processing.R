wraml = sqlQuery(new_con,"select * from WRAML_2",stringsAsFactors=F)

#id, visit into characters and removing single-entered rows and prefix
wraml = id_visit_changing_into_char(wraml)
wraml_entry_flag = entry_flag(wraml,'wraml')
wraml = subset(wraml,entry_status==2)

#calculating age
wraml = fxage(wraml,'id','wraml_date')
wraml$wraml_age = NULL
names(wraml)[which(names(wraml)=="age")]="wraml_age"

#converting negative scores to NA
for(j in which(names(wraml)=="wraml_sm_raw"):which(names(wraml)=="wraml_screen_perc")){
  wraml[,j] = cbraw(wraml[,j])
}

#missing data analysis

#orphaned/duplicate data
wraml_orphaned_data = orphaned_data_consolidate(wraml)
wraml = orphaned_data_remove(wraml)
wraml_duplicate_data = duplicate_data_consolidate(wraml,"wraml_age")
wraml = duplicate_data_remove(wraml,"wraml_age")

#archiving the data
wraml_scored = wraml[,c(1:2,grep("wraml_age",names(wraml)),which(names(wraml)=="wraml_sm_raw"):which(names(wraml)=="wraml_screen_perc"))]
write.csv(wraml_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/wraml_scored.csv",row.names = F)
