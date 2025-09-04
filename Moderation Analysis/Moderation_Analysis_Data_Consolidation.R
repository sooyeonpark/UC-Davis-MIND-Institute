#preparing each measure table first before putting all together
###Malleable Factors
##Implementation Climate
#filtering out data based on duplicates and type of surveys (no cadre's, t/csodi's)
ics_double_id_index = data.frame(table(ics$subjid)[which(table(ics$subjid)>1)])[,-2]
ics_dup = ics[which(ics$subjid %in% ics_double_id_index),]
ics_dup = subset(ics_dup,!grepl("sodi$",survey) & !grepl("cadre",survey))
ics_unique_id = ics[-which(ics$subjid %in% ics_double_id_index),]
ics_unique_id = choosing_right_data(ics_unique_id,ics_dup,5,103)
ics_unique_id = subset(ics_unique_id,!grepl("sodi$",survey) & !grepl("cadre",survey))

#replacing missing item(s) within each subscale with means of the subscale -> ics setting first, then district
focus_items = paste0('ics_setting_',1:3)
edu_items = paste0('ics_setting_',4:6)
recog_items = paste0('ics_setting_',7:9)
rewards_items = paste0('ics_setting_',10:12)
selection_items = paste0('ics_setting_',13:15)
selection_open_items = paste0('ics_setting_',16:18)
es_items = paste0('ics_setting_',19:21)
ud_items = paste0('ics_setting_',22:25)
for(i in 1:nrow(ics_unique_id)){
  ics_unique_id[i,focus_items[which(is.na(ics_unique_id[i,focus_items]))]] = mean(unlist(ics_unique_id[i,focus_items]),na.rm=T)
  ics_unique_id[i,edu_items[which(is.na(ics_unique_id[i,edu_items]))]] = mean(unlist(ics_unique_id[i,edu_items]),na.rm=T)
  ics_unique_id[i,recog_items[which(is.na(ics_unique_id[i,recog_items]))]] = mean(unlist(ics_unique_id[i,recog_items]),na.rm=T)
  ics_unique_id[i,rewards_items[which(is.na(ics_unique_id[i,rewards_items]))]] = mean(unlist(ics_unique_id[i,rewards_items]),na.rm=T)
  ics_unique_id[i,selection_items[which(is.na(ics_unique_id[i,selection_items]))]] = mean(unlist(ics_unique_id[i,selection_items]),na.rm=T)
  ics_unique_id[i,selection_open_items[which(is.na(ics_unique_id[i,selection_open_items]))]] = mean(unlist(ics_unique_id[i,selection_open_items]),na.rm=T)
  ics_unique_id[i,es_items[which(is.na(ics_unique_id[i,es_items]))]] = mean(unlist(ics_unique_id[i,es_items]),na.rm=T)
  ics_unique_id[i,ud_items[which(is.na(ics_unique_id[i,ud_items]))]] = mean(unlist(ics_unique_id[i,ud_items]),na.rm=T)
}
focus_items = paste0('ics_district_',1:3)
edu_items = paste0('ics_district_',4:6)
recog_items = paste0('ics_district_',7:9)
rewards_items = paste0('ics_district_',10:12)
selection_items = paste0('ics_district_',13:15)
selection_open_items = paste0('ics_district_',16:18)
es_items = paste0('ics_district_',19:21)
ud_items = paste0('ics_district_',22:25)
for(i in 1:nrow(ics_unique_id)){
  ics_unique_id[i,focus_items[which(is.na(ics_unique_id[i,focus_items]))]] = mean(unlist(ics_unique_id[i,focus_items]),na.rm=T)
  ics_unique_id[i,edu_items[which(is.na(ics_unique_id[i,edu_items]))]] = mean(unlist(ics_unique_id[i,edu_items]),na.rm=T)
  ics_unique_id[i,recog_items[which(is.na(ics_unique_id[i,recog_items]))]] = mean(unlist(ics_unique_id[i,recog_items]),na.rm=T)
  ics_unique_id[i,rewards_items[which(is.na(ics_unique_id[i,rewards_items]))]] = mean(unlist(ics_unique_id[i,rewards_items]),na.rm=T)
  ics_unique_id[i,selection_items[which(is.na(ics_unique_id[i,selection_items]))]] = mean(unlist(ics_unique_id[i,selection_items]),na.rm=T)
  ics_unique_id[i,selection_open_items[which(is.na(ics_unique_id[i,selection_open_items]))]] = mean(unlist(ics_unique_id[i,selection_open_items]),na.rm=T)
  ics_unique_id[i,es_items[which(is.na(ics_unique_id[i,es_items]))]] = mean(unlist(ics_unique_id[i,es_items]),na.rm=T)
  ics_unique_id[i,ud_items[which(is.na(ics_unique_id[i,ud_items]))]] = mean(unlist(ics_unique_id[i,ud_items]),na.rm=T)
}

#getting ics subscales with replaced items -> prioritize ics setting scores and then district scores
focus_items = paste0('ics_setting_',1:3)
edu_items = paste0('ics_setting_',4:6)
recog_items = paste0('ics_setting_',7:9)
rewards_items = paste0('ics_setting_',10:12)
selection_items = paste0('ics_setting_',13:15)
selection_open_items = paste0('ics_setting_',16:18)
es_items = paste0('ics_setting_',19:21)
ud_items = paste0('ics_setting_',22:25)
for(i in 1:nrow(ics_unique_id)){
  ics_unique_id$ics_focus_ebp[i] = ifelse(all(!is.na(ics_unique_id[i,focus_items])),sum(unlist(ics_unique_id[i,focus_items])),
                                          ifelse(all(!is.na(ics_unique_id[i,gsub("setting","district",focus_items)])),sum(unlist(ics_unique_id[i,gsub("setting","district",focus_items)])),NA))
  ics_unique_id$ics_edu_support[i] = ifelse(all(!is.na(ics_unique_id[i,edu_items])),sum(unlist(ics_unique_id[i,edu_items])),
                                          ifelse(all(!is.na(ics_unique_id[i,gsub("setting","district",edu_items)])),sum(unlist(ics_unique_id[i,gsub("setting","district",edu_items)])),NA))
  ics_unique_id$ics_recog_ebp[i] = ifelse(all(!is.na(ics_unique_id[i,recog_items])),sum(unlist(ics_unique_id[i,recog_items])),
                                          ifelse(all(!is.na(ics_unique_id[i,gsub("setting","district",recog_items)])),sum(unlist(ics_unique_id[i,gsub("setting","district",recog_items)])),NA))
  ics_unique_id$ics_rewards_ebp[i] = ifelse(all(!is.na(ics_unique_id[i,rewards_items])),sum(unlist(ics_unique_id[i,rewards_items])),
                                          ifelse(all(!is.na(ics_unique_id[i,gsub("setting","district",rewards_items)])),sum(unlist(ics_unique_id[i,gsub("setting","district",rewards_items)])),NA))
  ics_unique_id$ics_selection_ebp[i] = ifelse(all(!is.na(ics_unique_id[i,selection_items])),sum(unlist(ics_unique_id[i,selection_items])),
                                          ifelse(all(!is.na(ics_unique_id[i,gsub("setting","district",selection_items)])),sum(unlist(ics_unique_id[i,gsub("setting","district",selection_items)])),NA))
  ics_unique_id$ics_selection_open[i] = ifelse(all(!is.na(ics_unique_id[i,selection_open_items])),sum(unlist(ics_unique_id[i,selection_open_items])),
                                          ifelse(all(!is.na(ics_unique_id[i,gsub("setting","district",selection_open_items)])),sum(unlist(ics_unique_id[i,gsub("setting","district",selection_open_items)])),NA))
  ics_unique_id$ics_exist_sup_ebp[i] = ifelse(all(!is.na(ics_unique_id[i,es_items])),sum(unlist(ics_unique_id[i,es_items])),
                                          ifelse(all(!is.na(ics_unique_id[i,gsub("setting","district",es_items)])),sum(unlist(ics_unique_id[i,gsub("setting","district",es_items)])),NA))
  ics_unique_id$ics_exist_use_data[i] = ifelse(all(!is.na(ics_unique_id[i,ud_items])),sum(unlist(ics_unique_id[i,ud_items])),
                                          ifelse(all(!is.na(ics_unique_id[i,gsub("setting","district",ud_items)])),sum(unlist(ics_unique_id[i,gsub("setting","district",ud_items)])),NA))
}
rm(focus_items,edu_items,recog_items,rewards_items,selection_items,selection_open_items,es_items,ud_items)

#getting ics total
ics_unique_id = summing_items_per_row(ics_unique_id,list(c(names(ics_unique_id[,104:111]))),list('ics_total'),F)
ics_unique_id = summing_items_per_row(ics_unique_id,list(c(names(ics_unique_id[,104:111]))),list('ics_total_narm'),T)
for(i in 1:nrow(ics_unique_id)){
  ics_unique_id$ics_setting_total[i] = sum(unlist(ics_unique_id[i,grep("ics_setting_[0-9]{1,2}",names(ics_unique_id))]))
  ics_unique_id$ics_district_total[i] = sum(unlist(ics_unique_id[i,grep("ics_district_[0-9]{1,2}",names(ics_unique_id))]))
  ics_unique_id$ics_total_narm[i] = ifelse(all(is.na(ics_unique_id[i,104:111])),NA,ics_unique_id$ics_total_narm[i])
}

##Resource Allocation
#filtering out data based on duplicates and type of surveys (no cadre's, t/csodi's)
resources_double_id_index = data.frame(table(resources$subjid)[which(table(resources$subjid)>1)])[,-2]
resources_dup = resources[which(resources$subjid %in% resources_double_id_index),]
resources_dup = subset(resources_dup,!grepl("sodi$",survey) & !grepl("cadre",survey))
resources_unique_id = resources[-which(resources$subjid %in% resources_double_id_index),]
resources_unique_id = choosing_right_data(resources_unique_id,resources_dup,5,19)
resources_unique_id = subset(resources_unique_id,!grepl("sodi$",survey) & !grepl("cadre",survey))
#replacing missing items with mean scores within subscales
part_items = grep("part_",names(resources))
sp_items = grep("sp_",names(resources))
oc_items = grep("oc_",names(resources))
for(j in 20:22){
  resources_unique_id[,j] = round(resources_unique_id[,j],2)
}
for(i in 1:nrow(resources_unique_id)){
  resources_unique_id[i,part_items[which(is.na(resources_unique_id[i,part_items]))]] = resources_unique_id$ebp_res_partnership_avg[i]
  resources_unique_id[i,sp_items[which(is.na(resources_unique_id[i,sp_items]))]] = resources_unique_id$ebp_res_strategic_planning_avg[i]
  resources_unique_id[i,oc_items[which(is.na(resources_unique_id[i,oc_items]))]] = resources_unique_id$ebp_res_org_capacity_avg[i]
  resources_unique_id$ebp_res_total[i] = sum(unlist(resources_unique_id[i,c(part_items,sp_items,oc_items)]))
}

###Moderators
##Poverty Level
poverty_selpa = read_excel("S:/MIND/RESEARCH/Toddler Treatment Projects/Captain/Data/CAPTAIN data R project/Moderation Analysis/Regional ADA Numbers 9.25.2019.xlsx",
                           sheet="%SD at SELPA Level")
names(poverty_selpa)[c(2,6)] = c("selpa_final","socio_disadvantaged_percentage")
poverty_selpa[,6] = round(poverty_selpa[,6],2)
poverty_district <- read_excel("Moderation Analysis/SELPA District Poverty Info.xlsx", 
                                sheet = "%SD at District Level",col_types = c("blank","text", "blank", "numeric", "blank"))
names(poverty_district) = c("district_final","poverty_rate_district")
poverty_district$district_final = gsub("\u200b","",poverty_district$district_final)
poverty_district$district = gsub(" ","",tolower(poverty_district$district_final))
poverty_district$district = gsub("[[:punct:]]","",poverty_district$district)
poverty_district$district = gsub("\u200b","",poverty_district$district)

##CAPTAIN Participation
#Years of Participation
library(stringr)
#filtering out data based on duplicates and type of surveys (no cadre's, t/csodi's)
p2_participant_info_double_id_index = data.frame(table(p2_participant_info$subjid)[which(table(p2_participant_info$subjid)>1)])[,-2]
p2_participant_info_dup = p2_participant_info[which(p2_participant_info$subjid %in% p2_participant_info_double_id_index),]
p2_participant_info_dup = subset(p2_participant_info_dup,!grepl("sodi$",survey) & !grepl("cadre",survey))
p2_participant_info_unique_id = p2_participant_info[-which(p2_participant_info$subjid %in% p2_participant_info_double_id_index),]
p2_participant_info_unique_id = choosing_right_data(p2_participant_info_unique_id,p2_participant_info_dup,22,48)
p2_participant_info_unique_id = subset(p2_participant_info_unique_id,!grepl("sodi$",survey) & !grepl("cadre",survey))
for(i in 1:nrow(p2_participant_info_unique_id)){
  p2_participant_info_unique_id$years_participation[i] = str_count(p2_participant_info_unique_id$cadre_member_years[i],"-")
  p2_participant_info_unique_id$years_participation[i] = ifelse(p2_participant_info_unique_id$cadre_member_years[i]=='',NA,p2_participant_info_unique_id$years_participation[i])
}
#Performance Score -> Question: can we link cadre id to captain id? cadre subject table ExternalDataReference variable is cadreid in perf score tables
perf_17_18 = read_excel("CAPTAIN performance scores by year.xlsx",sheet="2017-2018")[,-3]
perf_18_19 = read_excel("CAPTAIN performance scores by year.xlsx",sheet="2018-2019")[,-3]
perf_19_20 = read_excel("CAPTAIN performance scores by year.xlsx",sheet="2019-2020")[,-3]
perf_20_21 = read_excel("CAPTAIN performance scores by year.xlsx",sheet="2020-2021")[,-3]
names(perf_17_18) = c("cadre_id","performance_score_17_18")
names(perf_18_19) = c("cadre_id","performance_score_18_19")
names(perf_19_20) = c("cadre_id","performance_score_19_20")
names(perf_20_21) = c("cadre_id","performance_score_20_21")
perf = merge(perf_17_18,perf_18_19,all.x=T)
perf = merge(perf,perf_19_20,all.x=T)
perf = merge(perf,perf_20_21,all.x=T)
for(i in 1:nrow(perf)){
  perf$performance_score_avg[i] = round(mean(unlist(perf[i,2:5]),na.rm=T),2)
}
capid_cadreid = unique(cadre_subj[!is.na(cadre_subj$subjid) & cadre_subj$cadre_id!='',c(4,7)])

###Outcomes
##Training Quality
#filtering out data based on duplicates and type of surveys (no cadre's, t/csodi's)
ebp_use_double_id_index = data.frame(table(ebp_use$subjid)[which(table(ebp_use$subjid)>1)])[,-2]
ebp_use_dup = ebp_use[which(ebp_use$subjid %in% ebp_use_double_id_index),]
ebp_use_dup = subset(ebp_use_dup,!grepl("sodi$",survey) & !grepl("cadre",survey))
ebp_use_unique_id = ebp_use[-which(ebp_use$subjid %in% ebp_use_double_id_index),]
ebp_use_unique_id = choosing_right_data(ebp_use_unique_id,ebp_use_dup,31,45)
ebp_use_unique_id = subset(ebp_use_unique_id,!grepl("sodi$",survey) & !grepl("cadre",survey))
##Behavior
capid_cdeid = read_excel("S:/MIND/RESEARCH/Toddler Treatment Projects/Captain/Data/CAPTAIN data R project/CDE data/master_ADA_IDs.xlsx",
                         sheet="Matched IDs")
names(capid_cdeid)[c(2,4)] = c("district_final","cde_id")
cde_district = merge(capid_cdeid[,c(1:2,4)],aut_district[,-2],all.x=T)
cde_district = merge(cde_district,swd_district[,-2],all.x=T)
##EBP results
#averaging pct correct and fidelity
for(i in 1:nrow(ebp_pct_fid)){
  ebp_pct_fid$pct_correct_avg[i] = round(mean(unlist(ebp_pct_fid[i,grep("_pctcorrect$",names(ebp_pct_fid))]),na.rm=T),2)
  ebp_pct_fid$pct_fidelity_avg[i] = round(mean(unlist(ebp_pct_fid[i,grep("_pctfidelity$",names(ebp_pct_fid))]),na.rm=T),2)
}
#distinguishing which ebp is primary, secondary, tertiary
ebp_allison <- read.csv("S:/MIND/RESEARCH/Toddler Treatment Projects/Captain/Data/CAPTAIN data R project/prelim_ebp_long_7.31.20_responseIDmerge_adddemo (1).csv", stringsAsFactors=FALSE)
ebp_primary = ebp_allison[ebp_allison$intervention_no==1,c(1,5,12:13)]
names(ebp_primary) = c("subjid","intervention","primary_pct_cor","primary_pct_fid")
ebp_secondary = ebp_allison[ebp_allison$intervention_no==2,c(1,6,12:13)]
names(ebp_secondary) = c("subjid","intervention","secondary_pct_cor","secondary_pct_fid")
ebp_tertiary = ebp_allison[ebp_allison$intervention_no==3,c(1,7,12:13)]
names(ebp_tertiary) = c("subjid","intervention","tertiary_pct_cor","tertiary_pct_fid")
for(j in 3:4){
  ebp_primary[,j] = round(ebp_primary[,j],2)
  ebp_secondary[,j] = round(ebp_secondary[,j],2)
  ebp_tertiary[,j] = round(ebp_tertiary[,j],2)
}
#ebp_pct_fid = merge()

#putting data together
ma = people_survey_map[,c(1:2,4:13)] #getting complete participant list from the master list
ma = subset(ma,!grepl("sodi$",survey) & !grepl("cadre",survey))
ma = unique(ma[,c(1,3:12)])
ma = merge(ma,ics_unique_id[,c(1,104:106)],all.x=T) #merging ics scores
ma = merge(ma,resources_unique_id[,c(1,20:22)],by="subjid",all.x=T) #merging resources allocation
ma = merge(ma,poverty_selpa[,c(2,6)],by="selpa_final",all.x=T) #merging poverty level data
ma = merge(ma,capid_cadreid,by="subjid",all.x=T) #merging cadre id var to import performance scores
ma = merge(ma,perf[,c(1,6)],by="cadre_id",all.x=T) #merging performance scores
ma = merge(ma,p2_participant_info_unique_id[,c(1,21,50)],by="subjid",all.x=T) #merging years of participation and asd exp
ma = merge(ma,ebp_use_unique_id[,c(1,43:45)],by="subjid",all.x=T) #merging ebp use
ma = merge(ma,ebp_pct_fid[,c(1,57:58)],by="subjid",all.x=T)

##ASD Experience
ma$exp_asd = ifelse(grepl("^Little to no",ma$exp_asd),0,
                    ifelse(grepl("^Some",ma$exp_asd),1,
                           ifelse(grepl("^Moderate",ma$exp_asd),2,
                                  ifelse(grepl("^Extensive",ma$exp_asd),3,NA))))
ma$exp_asd = as.numeric(ma$exp_asd)
#ma = merge(ma,cde_district[,-1],all.x=T) #merging cde data
ma$sort = as.numeric(gsub("CAP","",ma$subjid))
ma$sort = ifelse(is.na(ma$sort),as.numeric(gsub("CAPG","",ma$subjid)),ma$sort)
ma = ma[order(ma$sort),1:(ncol(ma)-1)]
write_xlsx(ma,"S:/MIND/RESEARCH/Toddler Treatment Projects/Captain/Data/CAPTAIN data R project/Moderation Analysis/moderation_analysis.xlsx")
which(table(ma$subjid)>1) #all the duplicate rows with less info deleted
#cap164, cap197, cap355 -> having two cadre id's but no perf scores so randomly deleted
#all the others deleted except cap101,110,201,2226,244,621,777,78 when they didn't have selpa/district info.

#creating correlation table
#malleable factors
write.table(round(cor(ma[,c(12:19,36:38)],use="complete.obs"),2),"S:/MIND/RESEARCH/Toddler Treatment Projects/Captain/Data/CAPTAIN data R project/Moderation Analysis/cor_tables.csv",row.name=T,sep=",")
write.table(round(cor(ma[,c(37,41:47)],use="complete.obs"),2),"S:/MIND/RESEARCH/Toddler Treatment Projects/Captain/Data/CAPTAIN data R project/Moderation Analysis/cor_tables.csv",row.name=T,append=T,sep=",")
write.table(round(cor(ma[,48:52],use="complete.obs"),2),"S:/MIND/RESEARCH/Toddler Treatment Projects/Captain/Data/CAPTAIN data R project/Moderation Analysis/cor_tables.csv",row.name=T,append=T,sep=",")

#clean up
rm(ebp_use_double_id_index,ics_double_id_index,p2_participant_info_double_id_index,
   resources_double_id_index,perf_17_18,perf_18_19,perf_19_20,perf_20_21,part_items,
   sp_items,oc_items)

#number of overlapping data
length(which(!is.na(ma$ics_setting_total)))
length(which(!is.na(ma$ebp_res_total)))
length(which(!is.na(ma$ebp_res_org_capacity_avg)))
length(which(!is.na(ma$socio_disadvantaged_percentage)))
length(which(!is.na(ma$performance_score_avg)))
length(which(!is.na(ma$exp_asd)))
length(which(!is.na(ma$years_participation)))
length(which(!is.na(ma$ebp_use_primary_quality_outcome)))
length(which(!is.na(ma$primary_pct_cor)))
length(which(!is.na(ma$primary_pct_fid)))
length(which(!is.na(ma$ics_setting_total) & !is.na(ma$ebp_res_total)))
length(which(!is.na(ma$ics_setting_total) & !is.na(ma$socio_disadvantaged_percentage)))
length(which(!is.na(ma$ics_setting_total) & !is.na(ma$years_participation)))
length(which(!is.na(ma$ics_setting_total) & !is.na(ma$performance_score_avg)))
length(which(!is.na(ma$ics_setting_total) & !is.na(ma$exp_asd)))
length(which(!is.na(ma$ics_setting_total) & !is.na(ma$ebp_use_primary_quality_outcome)))
length(which(!is.na(ma$ics_setting_total) & !is.na(ma$primary_pct_cor)))
length(which(!is.na(ma$ics_setting_total) & !is.na(ma$primary_pct_fid)))
length(which(!is.na(ma$ebp_res_total) & !is.na(ma$socio_disadvantaged_percentage)))
length(which(!is.na(ma$ebp_res_total) & !is.na(ma$performance_score_avg)))
length(which(!is.na(ma$ebp_res_total) & !is.na(ma$exp_asd)))
length(which(!is.na(ma$ebp_res_total) & !is.na(ma$years_participation)))
length(which(!is.na(ma$ebp_res_total) & !is.na(ma$ebp_use_primary_quality_outcome)))
length(which(!is.na(ma$ebp_res_total) & !is.na(ma$primary_pct_cor)))
length(which(!is.na(ma$ebp_res_total) & !is.na(ma$primary_pct_fid)))
length(which(!is.na(ma$socio_disadvantaged_percentage) & !is.na(ma$performance_score_avg)))
length(which(!is.na(ma$socio_disadvantaged_percentage) & !is.na(ma$exp_asd)))
length(which(!is.na(ma$socio_disadvantaged_percentage) & !is.na(ma$years_participation)))
length(which(!is.na(ma$socio_disadvantaged_percentage) & !is.na(ma$ebp_use_primary_quality_outcome)))
length(which(!is.na(ma$socio_disadvantaged_percentage) & !is.na(ma$primary_pct_cor)))
length(which(!is.na(ma$socio_disadvantaged_percentage) & !is.na(ma$primary_pct_fid)))
length(which(!is.na(ma$performance_score_avg) & !is.na(ma$exp_asd)))
length(which(!is.na(ma$performance_score_avg) & !is.na(ma$years_participation)))
length(which(!is.na(ma$performance_score_avg) & !is.na(ma$ebp_use_primary_quality_outcome)))
length(which(!is.na(ma$performance_score_avg) & !is.na(ma$primary_pct_cor)))
length(which(!is.na(ma$performance_score_avg) & !is.na(ma$primary_pct_fid)))
length(which(!is.na(ma$ics_setting_total) & !is.na(ma$exp_asd) & !is.na(ma$ebp_use_primary_quality_outcome)))
length(which(!is.na(ma$ics_setting_total) & !is.na(ma$exp_asd) & !is.na(ma$primary_pct_fid)))
length(which(!is.na(ma$ics_setting_total) & !is.na(ma$performance_score_avg) & !is.na(ma$primary_pct_fid)))
length(which(!is.na(ma$ics_setting_total) & !is.na(ma$performance_score_avg) & !is.na(ma$ebp_use_primary_quality_outcome)))
length(which(!is.na(ma$ics_setting_total) & !is.na(ma$socio_disadvantaged_percentage) & !is.na(ma$ebp_use_primary_quality_outcome)))
length(which(!is.na(ma$ics_setting_total) & !is.na(ma$socio_disadvantaged_percentage) & !is.na(ma$primary_pct_cor)))

#looking through distributions
hist(ma$ics_setting_focus_ebp)
hist(ma$ics_setting_edu_support)
hist(ma$ics_setting_recog_ebp)
hist(ma$ics_setting_rewards_ebp)
hist(ma$ics_setting_selection_ebp)
hist(ma$ics_setting_selection_open)
hist(ma$ics_setting_exist_sup_ebp)
hist(ma$ics_setting_exist_use_data)
hist(ma$ebp_res_partnership_avg)
hist(ma$ebp_res_strategic_planning_avg)
hist(ma$ebp_res_org_capacity_avg)
hist(ma$socio_disadvantaged_percentage)
hist(ma$performance_score_17_18)
hist(ma$performance_score_18_19)
hist(ma$performance_score_19_20)
hist(ma$performance_score_20_21)
hist(ma$performance_score_avg)
hist(ma$exp_asd)
hist(ma$years_participation)
hist(ma$ebp_use_primary_quality_outcome)
hist(ma$ebp_use_secondary_quality_outcocme)
hist(ma$ebp_use_tertiary_quality_outcome)
hist(ma$pct_correct_avg)
hist(ma$pct_fidelity_avg)

#checking how many non-empty rows -> perf score and ebp pct correct and fid seem to be the bottleneck
#ics = 2242
#ebp_res = 786
#poverty level = 959
#perf score = 258
#asd exp = 2636
#ebp use = 1482
#pct corr and fid = 244

###############################################################################
#looking for correlation#
##Malleable factors
#within ics (imp.cli)
cor.test(ma$ics_school_focus_ebp,ma$ics_school_edu_support) #.713
cor.test(ma$ics_school_focus_ebp,ma$ics_school_recog_ebp) #.64
cor.test(ma$ics_school_focus_ebp,ma$ics_school_rewards_ebp) #.4
cor.test(ma$ics_school_focus_ebp,ma$ics_school_selection_ebp) #.63
cor.test(ma$ics_school_focus_ebp,ma$ics_school_selection_open) #.48
cor.test(ma$ics_school_focus_ebp,ma$ics_school_exist_sup_ebp) #.62
cor.test(ma$ics_school_focus_ebp,ma$ics_school_exist_use_data) #.66
cor.test(ma$ics_school_edu_support,ma$ics_school_recog_ebp) #.62
cor.test(ma$ics_school_edu_support,ma$ics_school_rewards_ebp) #.59
cor.test(ma$ics_school_edu_support,ma$ics_school_selection_ebp) #.62
cor.test(ma$ics_school_edu_support,ma$ics_school_selection_open) #.42
cor.test(ma$ics_school_edu_support,ma$ics_school_exist_sup_ebp) #.77
cor.test(ma$ics_school_edu_support,ma$ics_school_exist_use_data) #.68
cor.test(ma$ics_school_recog_ebp,ma$ics_school_rewards_ebp) #.56
cor.test(ma$ics_school_recog_ebp,ma$ics_school_selection_ebp) #.68
cor.test(ma$ics_school_recog_ebp,ma$ics_school_selection_open) #.48
cor.test(ma$ics_school_recog_ebp,ma$ics_school_exist_sup_ebp) #.57
cor.test(ma$ics_school_recog_ebp,ma$ics_school_exist_use_data) #.57
cor.test(ma$ics_school_rewards_ebp,ma$ics_school_selection_ebp) #.54
cor.test(ma$ics_school_rewards_ebp,ma$ics_school_selection_open) #.29
cor.test(ma$ics_school_rewards_ebp,ma$ics_school_exist_sup_ebp) #.62
cor.test(ma$ics_school_rewards_ebp,ma$ics_school_exist_use_data) #.57
cor.test(ma$ics_school_selection_ebp,ma$ics_school_selection_open) #.62
cor.test(ma$ics_school_selection_ebp,ma$ics_school_exist_sup_ebp) #.7
cor.test(ma$ics_school_selection_ebp,ma$ics_school_exist_use_data) #.69
cor.test(ma$ics_school_selection_open,ma$ics_school_exist_sup_ebp) #.47
cor.test(ma$ics_school_selection_open,ma$ics_school_exist_use_data) #.48
cor.test(ma$ics_school_exist_sup_ebp,ma$ics_school_exist_use_data) #.81
#within resource allocation
cor.test(ma$ebp_res_org_capacity_avg,ma$ebp_res_partnership_avg) #.8
cor.test(ma$ebp_res_org_capacity_avg,ma$ebp_res_strategic_planning_avg) #.83
cor.test(ma$ebp_res_strategic_planning_avg,ma$ebp_res_partnership_avg) #.84
##Moderators
#btw poverty pct and cap participation yrs
cor.test(ma$socio_disadvantaged_percentage,ma$years_participation) #.09
#brw poverty pct and perf scores
cor.test(ma$socio_disadvantaged_percentage,ma$performance_score_avg) #-.08
cor.test(ma$socio_disadvantaged_percentage,ma$performance_score_17_18) #-.12
cor.test(ma$socio_disadvantaged_percentage,ma$performance_score_18_19) #~0
cor.test(ma$socio_disadvantaged_percentage,ma$performance_score_19_20) #~0
cor.test(ma$socio_disadvantaged_percentage,ma$performance_score_20_21) #~0
#btw cap participation yrs and perf score avg
cor.test(ma$years_participation,ma$performance_score_avg) #~0
cor.test(ma$years_participation,ma$performance_score_17_18) #~0
cor.test(ma$years_participation,ma$performance_score_18_19) #~0
cor.test(ma$years_participation,ma$performance_score_19_20) #~0
cor.test(ma$years_participation,ma$performance_score_20_21) #-.18
##Outcome
#within ebp use (training quality)
cor.test(ma$ebp_use_primary_quality_outcome,ma$ebp_use_secondary_quality_outcocme) #.73
cor.test(ma$ebp_use_primary_quality_outcome,ma$ebp_use_tertiary_quality_outcome) #.68
cor.test(ma$ebp_use_tertiary_quality_outcome,ma$ebp_use_secondary_quality_outcocme) #.7
#within ebp pct & fid
cor.test(ma$pct_correct_avg,ma$pct_fidelity_avg) #.11
#btw ebp use and ebp pct & fid
cor.test(ma$ebp_use_primary_quality_outcome,ma$pct_correct_avg) #~0
cor.test(ma$ebp_use_primary_quality_outcome,ma$pct_fidelity_avg) #.21
cor.test(ma$ebp_use_secondary_quality_outcocme,ma$pct_correct_avg) #.1
cor.test(ma$ebp_use_secondary_quality_outcocme,ma$pct_fidelity_avg) #.22
cor.test(ma$ebp_use_tertiary_quality_outcome,ma$pct_correct_avg) #.13
cor.test(ma$ebp_use_tertiary_quality_outcome,ma$pct_fidelity_avg) #.27

#question to ask: what should we do for multiple data within the same id throughout different types of surveys? Which one to use?
#answer: use the one with higher completion %
#correlation within the factors
#check the distribution for each var