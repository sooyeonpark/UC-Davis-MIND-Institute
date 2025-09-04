#pulling data from different db's
cdi_wg_t1 = exportRecords(ace_con,forms=c("participant_information","cdiwg"),events="t1_arm_1",labels=F,stringsAsFactors=F)
cdi_wg_t1 = cdi_wg_t1[grepl("Co",cdi_wg_t1$cdiwg_complete)&!is.na(cdi_wg_t1$cdiwg_timestamp),-(2:grep("participant_information_complete",names(cdi_wg_t1)))]
cdi_wg_t1$visit = 1
cdi_wg = sqlQuery(new_con,"select * from CDI_Words_and_Gestures;",stringsAsFactors=F)

#changing id and visit into character vars and getting rid of single-entered rows and prefix
cdi_wg = id_visit_changing_into_char(cdi_wg)
cdi_wg_entry_flag = entry_flag(cdi_wg,'cdi_wg')
cdi_wg = subset(cdi_wg,entry_status==2)
cdi_wg = rbind(cdi_wg,cdi_wg_entry_flag[,-ncol(cdi_wg_entry_flag)])
cdi_wg = removing_prefix(cdi_wg,"cdiwg_")
cdi_wg_t1 = study_id_to_id(cdi_wg_t1,"cdiwg_")

#processing redcap cdiwg table before combining the tables
for(i in 1:nrow(cdi_wg_t1)){
  cdi_wg_t1$iib[i] = length(which(cdi_wg_t1[i,grep("iib___[0-9]{1,2}$",names(cdi_wg_t1))]=="Checked"))
  cdi_wg_t1$iic[i] = length(which(cdi_wg_t1[i,grep("iic___[0-9]{1,2}$",names(cdi_wg_t1))]=="Checked"))
  cdi_wg_t1$iid[i] = length(which(cdi_wg_t1[i,grep("iid___[0-9]{1,2}$",names(cdi_wg_t1))]=="Checked"))
  cdi_wg_t1$iie[i] = length(which(cdi_wg_t1[i,grep("iie___[0-9]{1,2}$",names(cdi_wg_t1))]=="Checked"))
}

#obtaining section Ic1&IC2 scores
cdi_wg$Ic1 = cdi_wg_section_1cn2a_score(cdi_wg$Ic1)
cdi_wg$Ic2 = cdi_wg_section_1cn2a_score(cdi_wg$Ic2)

#obtaining section Id scores
section_1da_items = grep("Id[0-9]{1,2}a",names(cdi_wg))
section_1db_items = grep("Id[0-9]{1,2}b",names(cdi_wg))
for(j in c(section_1da_items,section_1db_items)){
  for(i in 1:nrow(cdi_wg)){
    cdi_wg[i,j] = cdi_wg_section_1d_score(cdi_wg[i,j])
  }
  cdi_wg[,j] = as.numeric(cdi_wg[,j])
}

#getting sums of processed section Id scores
for(i in 1:nrow(cdi_wg)){
  cdi_wg$words_understood[i] = sum(cdi_wg[i,section_1da_items],na.rm=F)
  cdi_wg$words_produced[i] = sum(cdi_wg[i,section_1db_items],na.rm=F)
}

#combining data tables & calculating age
names(cdi_wg) = tolower(names(cdi_wg))
cdi_wg = identify_same_data(cdi_wg_t1,cdi_wg)
cdi_wg = rbind.fill(cdi_wg,cdi_wg_t1[,-grep("___",names(cdi_wg_t1))])
cdi_wg = fxage(cdi_wg,'id','date')

#scoring section IIa
section_2a_items = grep("iia[0-9]{1,2}",names(cdi_wg))
for(j in section_2a_items){
  for(i in 1:nrow(cdi_wg)){
    cdi_wg[i,j] = cdi_wg_section_1cn2a_score(cdi_wg[i,j])
  }
  cdi_wg[,j] = as.numeric(cdi_wg[,j])
}

#summing up the processed iia items along with other totals
late_gesture_items = grep("ii[c-e]",names(cdi_wg))
for(i in 1:nrow(cdi_wg)){
  cdi_wg$`2a`[i] = sum(cdi_wg[i,section_2a_items],na.rm=F)
  cdi_wg$early_gestures[i] = sum(c(cdi_wg[i,"2a"],cdi_wg[i,"iib"]),na.rm=F)
  cdi_wg$late_gestures[i] = sum(cdi_wg[i,late_gesture_items],na.rm=F)
  cdi_wg$total_gestures[i] = sum(c(cdi_wg$early_gestures[i],cdi_wg$late_gestures[i]),na.rm=F) 
}

#adding percentile scores -> age exceeded the norm table
# cdi_wg_norm = read_xlsx("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/cdi_wg_norm_table.xlsx")
# for(i in 1:nrow(cdi_wg)){
#   index = which(cdi_wg_norm$var=="pu" & cdi_wg_norm$raw<=cdi_wg$Ib[i]
#                 & cdi_wg_norm$age==floor(cdi_wg$age[i]) & cdi_wg_norm$gender==cdi_wg$dem_child_sex[i])
#   index = index[length(index)] #can be multiple percentile values with the same raw score(s)
#   cdi_wg$total_phrases_understood_percentile[i] = ifelse(length(index)==0,NA,cdi_wg_norm$percentile[index])
#   index = which(cdi_wg_norm$var=="twu" & cdi_wg_norm$raw<=cdi_wg$total_words_understood[i]
#                 & cdi_wg_norm$age==floor(cdi_wg$age[i]) & cdi_wg_norm$gender==cdi_wg$dem_child_sex[i])
#   index = index[length(index)] #can be multiple percentile values with the same raw score(s)
#   cdi_wg$total_words_understood_percentile[i] = ifelse(length(index)==0,NA,cdi_wg_norm$percentile[index])
#   index = which(cdi_wg_norm$var=="twp" & cdi_wg_norm$raw<=cdi_wg$total_words_produced[i]
#                 & cdi_wg_norm$age==floor(cdi_wg$age[i]) & cdi_wg_norm$gender==cdi_wg$dem_child_sex[i])
#   index = index[length(index)] #can be multiple percentile values with the same raw score(s)
#   cdi_wg$total_wods_produced_percentile[i] = ifelse(length(index)==0,NA,cdi_wg_norm$percentile[index])
#   index = which(cdi_wg_norm$var=="eg" & cdi_wg_norm$raw<=cdi_wg$early_gestures[i]
#                 & cdi_wg_norm$age==floor(cdi_wg$age[i]) & cdi_wg_norm$gender==cdi_wg$dem_child_sex[i])
#   index = index[length(index)] #can be multiple percentile values with the same raw score(s)
#   cdi_wg$early_gestures_percentile[i] = ifelse(length(index)==0,NA,cdi_wg_norm$percentile[index])
#   index = which(cdi_wg_norm$var=="lg" & cdi_wg_norm$raw<=cdi_wg$late_gestures[i]
#                 & cdi_wg_norm$age==floor(cdi_wg$age[i]) & cdi_wg_norm$gender==cdi_wg$dem_child_sex[i])
#   index = index[length(index)] #can be multiple percentile values with the same raw score(s)
#   cdi_wg$late_gestures_percentile[i] = ifelse(length(index)==0,NA,cdi_wg_norm$percentile[index])
#   index = which(cdi_wg_norm$var=="tg" & cdi_wg_norm$raw<=cdi_wg$total_gestures[i]
#                 & cdi_wg_norm$age==floor(cdi_wg$age[i]) & cdi_wg_norm$gender==cdi_wg$dem_child_sex[i])
#   index = index[length(index)] #can be multiple percentile values with the same raw score(s)
#   cdi_wg$total_gestures_percentile[i] = ifelse(length(index)==0,NA,cdi_wg_norm$percentile[index])
# }

#putting back preix
cdi_wg = inserting_prefix_into_variables(cdi_wg,"cdi_wg_")
names(cdi_wg)[grep("id[a-z,0-9]+",names(cdi_wg))]=paste0("cdi_wg_",names(cdi_wg)[grep("id[a-z,0-9]+",names(cdi_wg))])

#orphaned/duplicate data
cdi_wg_orphaned_data = orphaned_data_consolidate(cdi_wg)
cdi_wg = orphaned_data_remove(cdi_wg)
cdi_wg_duplicate_data = duplicate_data_consolidate(cdi_wg,"cdi_wg_age")
cdi_wg = duplicate_data_remove(cdi_wg,"cdi_wg_age")

#outliers
cdi_wg_outliers = cdi_wg[,c(1:2,grep("_age$",names(cdi_wg)),grep("_words_",names(cdi_wg)),
                            grep("_2a",names(cdi_wg)),grep("_ii[b-e]$",names(cdi_wg)),
                            grep("_gestures$",names(cdi_wg)))]
cdi_wg_outliers = outlier_list(cdi_wg_outliers)
cdi_wg$cdi_wg_outlier_list = cdi_wg_outliers$outlier_list
rm(cdi_wg_outliers)

#archiving the data
cdi_wg_scored = cdi_wg[,c(1:2,grep("_age$",names(cdi_wg)),grep("_ia1",names(cdi_wg)):grep("ic2",names(cdi_wg)),
                          grep("_words_",names(cdi_wg)),grep("_2a",names(cdi_wg)),grep("_ii[b-e]$",names(cdi_wg)),
                          grep("gestures$",names(cdi_wg)),grep("outlier_list$",names(cdi_wg)))]
names(cdi_wg_scored)[grep("_ib$",names(cdi_wg_scored))] = "cdi_wg_phrases_understood"
names(cdi_wg_scored) = gsub("_ii","_2",names(cdi_wg_scored))
names(cdi_wg_scored) = gsub("_i","_1",names(cdi_wg_scored))
write.csv(cdi_wg_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/cdi_wg_scored.csv",row.names=F)
write.csv(cdi_wg[,c(1:2,grep("_i[a-d]",names(cdi_wg)),grep("_ii[a-e]",names(cdi_wg)))],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/cdi_wg_items.csv",row.names=F)

#cleaning up
rm(section_1da_items,section_1db_items,section_2a_items,late_gesture_items,cdi_wg_t1)
