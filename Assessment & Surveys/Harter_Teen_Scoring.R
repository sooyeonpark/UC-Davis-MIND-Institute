harter_teen_t5 = exportRecords(ace_con,forms=c("participant_information","harter_teen"),events="t5_arm_2",labels=F,stringsAsFactors=F)
harter_teen_t5 = harter_teen_t5[grepl("Co",harter_teen_t5$harter_teen_complete)&!is.na(harter_teen_t5$harter_teen_timestamp),-(2:grep("participant_information_complete",names(harter_teen_t5)))]
harter_teen_t5$visit = 5
harter_teen = sqlFetch(new_con,"Harter_Teen",stringsAsFactors=F)
harter_teen = id_visit_changing_into_char(harter_teen)
harter_teen_entry_flag = entry_flag(harter_teen,'harter-teen')
harter_teen = subset(harter_teen,entry_status==2)
harter_teen = removing_prefix(harter_teen,'harterteen_')

##processing harter_teen_t5
#converting categorical scales to numeric values
for(j in grep("l$",names(harter_teen_t5))){
  harter_teen_t5[,j] = ifelse(grepl("Real",harter_teen_t5[,j]),1,
                              ifelse(grepl("Sort",harter_teen_t5[,j]),2,NA))
  harter_teen_t5[,j] = as.numeric(harter_teen_t5[,j])
}
for(j in grep("r$",names(harter_teen_t5))){
  harter_teen_t5[,j] = ifelse(grepl("Sort",harter_teen_t5[,j]),3,
                              ifelse(grepl("Real",harter_teen_t5[,j]),4,NA))
  harter_teen_t5[,j] = as.numeric(harter_teen_t5[,j])
}

#assigning and consolidating item values
for(j in grep("_[0-9]+$",names(harter_teen_t5),value=T)){
  harter_teen_t5[,j] = ifelse(!is.na(harter_teen_t5[,paste0(j,'l')]),harter_teen_t5[,paste0(j,'l')],
                              ifelse(!is.na(harter_teen_t5[,paste0(j,'r')]),harter_teen_t5[,paste0(j,'r')],NA))
}

harter_teen_t5 = study_id_to_id(harter_teen_t5,"harter_teen_")
harter_teen = identify_same_data(harter_teen_t5,harter_teen)
harter_teen = rbind.fill(harter_teen_t5[,-c(grep("l$",names(harter_teen_t5)),grep("r$",names(harter_teen_t5)))],harter_teen)

harter_teen = fxage(harter_teen,'id','date')

#dealing with negative scores
for(j in paste0('',1:51)){
  harter_teen[,j] = cbraw(harter_teen[,j])
}

#lists of section items
sc_items = paste0('',c(1,seq(12,42,by=10)))
soc_comp_items = paste0('',c(2,13,23,33,44))
ac_items = paste0('',c(seq(4,34,by=10),45))
pa_items = paste0('',c(seq(5,25,by=10),36,46))
bc_items = paste0('',c(8,18,29,39,49)) 
gs_items = paste0('',c(10,seq(21,51,by=10)))
jc_items = paste0('',c(6,16,26,37,47))
rom_items = paste0('',c(7,17,28,38,48))
clo_items = paste0('',c(9,seq(20,50,by=10)))
pvs_items = paste0('',c(3,11,19,27,35,43))
total_items = paste0('',1:51)

#missing data analysis
harter_teen = count_missing_items(harter_teen,'1','51')
harter_teen = comment_missing_data(harter_teen,list(sc_items,soc_comp_items,ac_items,pa_items,
                                                    bc_items,gs_items,jc_items,rom_items,clo_items,pvs_items),
                              list('scholastic_competence','social_competence','athletic_comp',
                                   'physical_appearance','behav_conduct','global_self-worth',
                                   'job_competence','romantic_appeal','close_friendship','pvs'))

#obtaining sums
harter_teen = summing_items_per_row(harter_teen,list(sc_items,soc_comp_items,ac_items,pa_items,
                                                     bc_items,gs_items,jc_items,rom_items,clo_items,
                                                     pvs_items,total_items),
                               list('schol_comp_raw','soc_comp_raw','ath_comp_raw',
                                    'phys_app_raw','behav_cond_raw','globe_sw_raw',
                                    'job_comp_raw','rom_app_raw','close_fri_raw',
                                    'pvs_raw','raw_total'),F)

#obtaining mean scores
for(i in 1:nrow(harter_teen)){
  harter_teen$schol_comp_avg[i] = ifelse(all(is.na(harter_teen[i,sc_items])),NA,round(rowMeans(harter_teen[i,sc_items],na.rm=T),2))
  harter_teen$soc_comp_avg[i] = ifelse(all(is.na(harter_teen[i,soc_comp_items])),NA,round(rowMeans(harter_teen[i,soc_comp_items],na.rm=T),2))
  harter_teen$ath_comp_avg[i] = ifelse(all(is.na(harter_teen[i,ac_items])),NA,round(rowMeans(harter_teen[i,ac_items],na.rm=T),2))
  harter_teen$phys_app_avg[i] = ifelse(all(is.na(harter_teen[i,pa_items])),NA,round(rowMeans(harter_teen[i,pa_items],na.rm=T),2))
  harter_teen$behav_cond_avg[i] = ifelse(all(is.na(harter_teen[i,bc_items])),NA,round(rowMeans(harter_teen[i,bc_items],na.rm=T),2))
  harter_teen$globe_sw_avg[i] = ifelse(all(is.na(harter_teen[i,gs_items])),NA,round(rowMeans(harter_teen[i,gs_items],na.rm=T),2))
  harter_teen$job_comp_avg[i] = ifelse(all(is.na(harter_teen[i,jc_items])),NA,round(rowMeans(harter_teen[i,jc_items],na.rm=T),2))
  harter_teen$rom_app_avg[i] = ifelse(all(is.na(harter_teen[i,rom_items])),NA,round(rowMeans(harter_teen[i,rom_items],na.rm=T),2))
  harter_teen$close_fri_avg[i] = ifelse(all(is.na(harter_teen[i,clo_items])),NA,round(rowMeans(harter_teen[i,clo_items],na.rm=T),2))
  harter_teen$pvs_avg[i] = ifelse(all(is.na(harter_teen[i,pvs_items])),NA,round(rowMeans(harter_teen[i,pvs_items],na.rm=T),2))
  harter_teen$total_avg[i] = ifelse(all(is.na(harter_teen[i,total_items])),NA,round(rowMeans(harter_teen[i,total_items],na.rm=T),2))
}

#putting prefix back
harter_teen = inserting_prefix_into_variables(harter_teen,'harter_teen_')

#orphaned/duplicate data
harter_teen_orphaned_data = orphaned_data_consolidate(harter_teen)
harter_teen = orphaned_data_remove(harter_teen)
harter_teen_duplicate_data = duplicate_data_consolidate(harter_teen,'harter_teen_age')
harter_teen = duplicate_data_remove(harter_teen,'harter_teen_age')

#outliers
harter_teen_outliers = harter_teen[,c(1,grep("visit",names(harter_teen)),grep("age$",names(harter_teen)),grep("avg$",names(harter_teen)))]
harter_teen_outliers = outlier_list(harter_teen_outliers)
harter_teen$harter_teen_outlier_list = harter_teen_outliers$outlier_list
#harter_teen_outlier_table = sqldf("select id,visit,outlier_list from harter_teen where outlier_list != ''")
rm(harter_teen_outliers)

#archiving the data
harter_teen_scored = harter_teen[,c(1,grep("visit",names(harter_teen)),grep("age$",names(harter_teen)),grep("missing",names(harter_teen)),which(names(harter_teen)=="harter_teen_schol_comp_raw"):ncol(harter_teen))]
write.csv(harter_teen_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/harter_teen_scored.csv",row.names=F)
write.csv(harter_teen[,c(1,grep("visit",names(harter_teen)),grep("_[0-9]+$",names(harter_teen)))],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/harter_teen_items.csv",row.names=F)

#cleaning up
rm(sc_items,soc_comp_items,pa_items,gs_items,ac_items,bc_items,pvs_items,
   clo_items,jc_items,rom_items,total_items,harter_teen_t5)
