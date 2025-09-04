#getting the table from db
dsm = sqlQuery(new_con,"select * from DSM_Checklist",stringsAsFactors=F)
dsm_staar = sqlQuery(con4,"select * from DSM_Checklist",stringsAsFactors=F)


#changing id and visit into characters and filtering out single-entered data
dsm = id_visit_changing_into_char(dsm)
dsm_entry_flag = entry_flag(dsm,'dsm')
#combining tables
dsm = rbind.fill(dsm,dsm_staar)
rm(dsm_staar)

#removing single entries
dsm = subset(dsm,entry_status==2)
if(!is.null(dsm_entry_flag)){
  dsm = rbind(dsm,dsm_entry_flag[,-ncol(dsm_entry_flag)])
}
#getting rid of prefix for convenicence
dsm = removing_prefix(dsm,"dsm_")

#calculating age
dsm = fxage(dsm,'id','date')

#calculating scores
#converting responses into numeric values
for(j in which(names(dsm)=="a1"):which(names(dsm)=="d")){
  if(class(dsm[,j])=="character"){
    for(i in 1:nrow(dsm)){
      dsm[i,j]=ifelse(is.na(dsm[i,j]),NA,ifelse(dsm[i,j]=="Yes",1,0))
    }
    dsm[,j]=as.numeric(dsm[,j])
  }
}

#obtaining the total scores
a = paste0("a",1:3)
b = paste0("b",1:4)
for(i in 1:nrow(dsm)){
  dsm$a_total[i] = sum(dsm[i,a],na.rm=T)
  dsm$b_total[i] = sum(dsm[i,b],na.rm=T)
  dsm$cd_total[i] = sum(c(dsm[i,"c"],dsm[i,"d"]),na.rm=T)
  #dsm$abcd_total[i] = dsm$a_total[i]+dsm$b_total[i]+dsm$cd_total[i]
}

#orphaned/duplicate data
dsm = inserting_prefix_into_variables(dsm,"dsm_")
dsm_orphaned_data = orphaned_data_consolidate(dsm)
dsm = orphaned_data_remove(dsm)
dsm_duplicate_data = duplicate_data_consolidate(dsm,"dsm_age")
dsm = duplicate_data_remove(dsm,"dsm_age")

#archiving the data
dsm_scored = dsm[,c(1:2,grep("_age$",names(dsm)),which(names(dsm)==paste0('dsm_',a)),grep("a_total$",names(dsm)),which(names(dsm)==paste0('dsm_',b)),grep("b_total$",names(dsm)),grep("_c$",names(dsm)):grep("_d$",names(dsm)),grep("cd_total$",names(dsm)),grep("valid$",names(dsm)):grep("clincom$",names(dsm)))]
write.csv(dsm_scored[grep("[0-9]{6}-[0-9]{3}",dsm_scored$id),],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/dsm_scored.csv",row.names=F)
write.csv(dsm_scored[grep("^[0-9]{4}$",dsm_scored$id),],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived_staar/dsm_scored.csv",row.names=F)
write.csv(dsm[grep("^[0-9]{4}$",dsm$id),c(1:2,grep("_a[0-5]{1}$",names(dsm)),grep("_b[0-5]{1}$",names(dsm)),grep("_c$",names(dsm)),grep("_d$",names(dsm)))],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived_staar/dsm_items.csv",row.names=F)
write.csv(dsm[-grep("^[0-9]{4}$",dsm$id),c(1:2,grep("_a[0-5]{1}$",names(dsm)),grep("_b[0-5]{1}$",names(dsm)),grep("_c$",names(dsm)),grep("_d$",names(dsm)))],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/dsm_items.csv",row.names=F)

#cleaning up
rm(a,b)
