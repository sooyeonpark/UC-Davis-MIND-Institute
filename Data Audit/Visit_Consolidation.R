#STAAR
star_visits = sqlQuery(channel = con5,
                       "select `SUBJ ID` as id,
                       VisitDate as encounter_date, VisitNumber as visit
                       FROM tblVisits where Status = 'Complete'",stringsAsFactors=F)
star_visits = fxage(star_visits,'id','encounter_date')

#changing visit class to character to make line 105 work
for(i in 1:nrow(star_visits)){
  if(grepl("[a-f]{1}",star_visits$visit[i])){
    star_visits$visit_revised[i] = gsub("[a-f]","",star_visits$visit[i])
  }
  else{
    star_visits$visit_revised[i] = star_visits$visit[i]
  }
}

star_visits = sqldf("select id,visit_revised as visit,encounter_date,age,round(avg(age),2) as mean_age, round(median(age),2) as median_age from star_visits
                    group by id,visit_revised order by id,encounter_date")
write.csv(star_visits[,c(1:2,5:6)],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived_staar/visit.csv",row.names=F)

master_visit2 = rbind(visit_tracking(adi_scored),visit_tracking(adis_processed),
                      visit_tracking(ados_scored),visit_tracking(aqc_scored),visit_tracking(cbcl_scored),
                      visit_tracking(cbq_scored),visit_tracking(ccc_scored),
                      visit_tracking(cdi_wg_scored),visit_tracking(cdi_ws_scored),
                      visit_tracking(celf_processed),visit_tracking(cshq_scored),
                      visit_tracking(das_scored),visit_tracking(masc_scored),
                      visit_tracking(dsm_scored),visit_tracking(edq_scored),
                      visit_tracking(eowpvt),visit_tracking(fps),visit_tracking(fqs),
                      visit_tracking(gi_processed),visit_tracking(gm_processed),
                      visit_tracking(gort_processed),visit_tracking(handedness_scored),
                      visit_tracking(harter_scored),visit_tracking(mori_regions),
                      visit_tracking(mori_total),visit_tracking(mullen_scored),
                      visit_tracking(nih_processed),visit_tracking(pal_processed),
                      visit_tracking(pe_processed),visit_tracking(ppvt_scored),
                      visit_tracking(rbs_scored),visit_tracking(scared_scored),
                      visit_tracking(scan_details_processed),visit_tracking(scq_scored),
                      visit_tracking(seq_scored),visit_tracking(srs_scored),
                      visit_tracking(ssp_scored),visit_tracking(ssp2_scored),
                      visit_tracking(sti_scored),visit_tracking(swan_scored),
                      visit_tracking(tcv),visit_tracking(tmcq),visit_tracking(ysr_scored),
                      visit_tracking(towre_processed),visit_tracking(ts_processed),
                      visit_tracking(vabs_scored),visit_tracking(wraml_scored))
master_visit2 = unique(master_visit2[grep("^[0-9]{6}-[0-9]{3}$",master_visit2$id),])
master_visit2 = sqldf("select * from master_visit2 where visit != '4p' order by id,visit")

#adding subj status column by time points
master_visit2$subj_status = ''
for(i in 1:nrow(master_visit2)){
  if(master_visit2$visit[i] != "5"){
    master_visit2$subj_status[i] = subj[subj$subj_id %in% master_visit2$id[i],paste0('time',master_visit2$visit[i],'_subj_status')]
  }
}

#adding mean & median age from master_visit table
master_visit2 = merge(master_visit2,master_visit,all.x=T)

#adding missing_data_list column to visit table
missing_data_list = data[data$missing_data,2:4]
missing_data_list = ddply(missing_data_list,.(id,visit),summarize,missing_data_list=paste0(measure,collapse="; "))
missing_data_list = merge(missing_data_list,subj[,c(1,2,9)],by.x="id",by.y="subj_id",all.x=T)

##fixing missing data list
#1) getting rid of TD STI's
missing_data_list$missing_data_list = ifelse(missing_data_list$app_diagnosis=="TD" & grepl("STI",missing_data_list$missing_data_list),
                                             gsub("STI","",missing_data_list$missing_data_list),missing_data_list$missing_data_list)
#2) getting rid of ASD SCQ's for T1 and T3 APP & TD APP T3
missing_data_list$missing_data_list = ifelse(missing_data_list$app_diagnosis=="ASD" & missing_data_list$study_cohort == "APP" & grepl("SCQ",missing_data_list$missing_data_list),
                                             gsub("SCQ","",missing_data_list$missing_data_list),missing_data_list$missing_data_list)
missing_data_list$missing_data_list = ifelse(missing_data_list$app_diagnosis=="TD" & missing_data_list$study_cohort == "APP" & grepl("SCQ",missing_data_list$missing_data_list) & missing_data_list$visit=="3",
                                             gsub("SCQ","",missing_data_list$missing_data_list),missing_data_list$missing_data_list)

#merging the list to visit
missing_data_list = missing_data_list[!grepl("^[[:space:]]*[;]*$",missing_data_list$missing_data_list),]
missing_data_list$missing_data_list = gsub("[[:space:]][;]{1}[[:space:]]"," ",missing_data_list$missing_data_list)
missing_data_list$missing_data_list = gsub("^[;]*[[:space:]]","",missing_data_list$missing_data_list)
missing_data_list$missing_data_list = gsub("[;]+[[:space:]]$","",missing_data_list$missing_data_list)
master_visit2 = merge(master_visit2,missing_data_list[,1:3],all.x=T)
names(master_visit2)[ncol(master_visit2)] = "missing_measure_list"
master_visit2$missing_measure_list = ifelse(is.na(master_visit2$missing_measure_list),"",master_visit2$missing_measure_list)
master_visit2$missing_measure_list = ifelse(is.na(master_visit2$mean_age) & master_visit2$visit != "2","All Behavior Measures",
                                            ifelse(is.na(master_visit2$mean_age) & master_visit2$visit == "2", "MRI Failed/Unavailable",master_visit2$missing_measure_list))
for(i in 1:nrow(master_visit2)){
  if(master_visit2$visit[i] != "5"){
    master_visit2$missing_measure_list[i] = ifelse(master_visit2$visit[i] != "2" & master_visit2$missing_measure_list[i] != ""
                                                   & grepl("MRI",subj[subj$subj_id %in% master_visit2$id[i],paste0('time',master_visit2$visit[i],'_subj_status')]),
                                                   paste0(master_visit2$missing_measure_list[i],'; MRI Failed/Unavailable'),
                                                   ifelse(master_visit2$visit[i] != "2" & master_visit2$missing_measure_list[i] == ""
                                                          & grepl("MRI",subj[subj$subj_id %in% master_visit2$id[i],paste0('time',master_visit2$visit[i],'_subj_status')]),
                                                          "MRI Failed/Unavailable",master_visit2$missing_measure_list[i]))
  }
}
master_visit2 = master_visit2[grepl("^[0-9]{1}$",master_visit2$visit),]

#id change modification
master_visit2[master_visit2$id=="111834-100",c("mean_age","median_age","missing_measure_list")] = master_visit2[master_visit2$id=="111834-001",c("mean_age","median_age","missing_measure_list")]
master_visit2 = master_visit2[-which(master_visit2$id=="111834-001"),]

write.csv(master_visit2,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/visit.csv",row.names=F)
