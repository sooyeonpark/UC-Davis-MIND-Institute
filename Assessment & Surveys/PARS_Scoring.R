pars = sqlFetch(con4,"PARS",stringsAsFactors=F)
pars = id_visit_changing_into_char(pars)
pars = subset(pars,entry_status==2)
pars = removing_prefix(pars,'pars_')

pars = fxage(pars,'id','date')

#dealing with 8 & 9's
for(j in paste0('overall',c(2:3,5:7))){
  pars[,j] = ifelse(pars[,j]>7,NA,pars[,j])
}

pars = summing_items_per_row(pars,list(paste0('overall',c(2:3,5:7))),list('total'),T)

pars = inserting_prefix_into_variables(pars,'pars_')

pars_orphaned_data = orphaned_data_consolidate(pars)
pars = orphaned_data_remove(pars)
pars_duplicate_data = duplicate_data_consolidate(pars,'pars_age')
pars = duplicate_data_remove(pars,'pars_age')

pars_scored = pars[,c(1:2,grep("_age$",names(pars)),grep("_total$",names(pars)))]
pars_scored = outlier_list(pars_scored)
names(pars_scored)[ncol(pars_scored)] = "pars_outlier_list"
write.csv(pars_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived_staar/pars_scored.csv",row.names=F)
write.csv(pars[,c(1:2,grep("_respondent$",names(pars)):grep("_overall7$",names(pars)))],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived_staar/pars_items.csv",row.names=F)
