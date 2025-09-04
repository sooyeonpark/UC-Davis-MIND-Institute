harter = sqlQuery(new_con,"select * from Harter",stringsAsFactors=F)

#initial formatting
harter = id_visit_changing_into_char(harter)
harter_entry_flag = entry_flag(harter,'harter-child')
harter = subset(harter,entry_status==2)
if(!is.null(harter_entry_flag)){
  harter = rbind(harter,harter_entry_flag[,-ncol(harter_entry_flag)])
}
harter = removing_prefix(harter,'hrtr_')

#dealing with negative scores
for(j in which(names(harter)=="a"):which(names(harter)=="42")){
  harter[,j] = cbraw(harter[,j])
}

#calculate age
harter = fxage(harter,'id','date')

#lists of section items
sc_items = paste0('',seq(1,36,by=7))
soc_comp_items = paste0('',seq(2,37,by=7))
ac_items = paste0('',seq(4,39,by=7))
pa_items = paste0('',seq(5,40,by=7))
bc_items = paste0('',seq(6,41,by=7)) 
gs_items = paste0('',seq(7,42,by=7))
pvs_items = paste0('',seq(3,38,by=7))
total_items = paste0('',1:42)

#missing data analysis
harter = count_missing_items(harter,'1','42')
harter = comment_missing_data(harter,list(sc_items,soc_comp_items,ac_items,pa_items,
                                          bc_items,gs_items,pvs_items),
                              list('scholastic_competence','social_competence','athletic_comp',
                                   'physical_appearance','behav_conduct','global_self-worth','pvs'))

#obtaining sums
harter = summing_items_per_row(harter,list(sc_items,soc_comp_items,ac_items,pa_items,
                                           bc_items,gs_items,pvs_items,total_items),
                               list('schol_comp_raw','soc_comp_raw','ath_comp_raw',
                                    'phys_app_raw','behav_cond_raw','globe_sw_raw',
                                    'pvs_raw','raw_total'),F)

#obtaining mean scores
for(i in 1:nrow(harter)){
  harter$schol_comp_avg[i] = ifelse(all(is.na(harter[i,sc_items])),NA,round(rowMeans(harter[i,sc_items],na.rm=T),2))
  harter$soc_comp_avg[i] = ifelse(all(is.na(harter[i,soc_comp_items])),NA,round(rowMeans(harter[i,soc_comp_items],na.rm=T),2))
  harter$ath_comp_avg[i] = ifelse(all(is.na(harter[i,ac_items])),NA,round(rowMeans(harter[i,ac_items],na.rm=T),2))
  harter$phys_app_avg[i] = ifelse(all(is.na(harter[i,pa_items])),NA,round(rowMeans(harter[i,pa_items],na.rm=T),2))
  harter$behav_cond_avg[i] = ifelse(all(is.na(harter[i,bc_items])),NA,round(rowMeans(harter[i,bc_items],na.rm=T),2))
  harter$globe_sw_avg[i] = ifelse(all(is.na(harter[i,gs_items])),NA,round(rowMeans(harter[i,gs_items],na.rm=T),2))
  harter$pvs_avg[i] = ifelse(all(is.na(harter[i,pvs_items])),NA,round(rowMeans(harter[i,pvs_items],na.rm=T),2))
  harter$total_avg[i] = ifelse(all(is.na(harter[i,total_items])),NA,round(rowMeans(harter[i,total_items],na.rm=T),2))
}

#putting prefix back
harter = inserting_prefix_into_variables(harter,'harter_')

#orphaned/duplicate data
harter_orphaned_data = orphaned_data_consolidate(harter)
harter = orphaned_data_remove(harter)
harter_duplicate_data = duplicate_data_consolidate(harter,'harter_age')
harter = duplicate_data_remove(harter,'harter_age')

#outliers
harter_outliers = harter[,c(1:2,grep("age$",names(harter)),grep("avg$",names(harter)))]
harter_outliers = outlier_list(harter_outliers)
harter$harter_outlier_list = harter_outliers$outlier_list
#harter_outlier_table = sqldf("select id,visit,outlier_list from harter where outlier_list != ''")
rm(harter_outliers)

#archiving the data
harter_scored = harter[,c(1:2,grep("age$",names(harter)),grep("missing",names(harter)),which(names(harter)=="harter_schol_comp_raw"):ncol(harter))]
write.csv(harter_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/harter_scored.csv",row.names=F)
write.csv(harter[,c(1:2,6:48)],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/harter_items.csv",row.names=F)

#cleaning up
rm(sc_items,soc_comp_items,pa_items,gs_items,ac_items,bc_items,pvs_items,total_items)
