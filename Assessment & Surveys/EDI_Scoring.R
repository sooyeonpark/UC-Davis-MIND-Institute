edi_t5 = exportRecords(ace_con,forms=c("participant_information","emotiondysregulationinventory"),events="t5_arm_1",labels=F,stringsAsFactors=F)
edi_t5 = edi_t5[grepl("Co",edi_t5$emotiondysregulationinventory_complete)&!is.na(edi_t5$emotiondysregulationinventory_timestamp),-(2:grep("participant_information_complete",names(edi_t5)))]
edi_t5$visit = 5
edi = sqlFetch(new_con,"EmotionDysregulationInventory",stringsAsFactors=F)
edi = id_visit_changing_into_char(edi)
edi_entry_flag = entry_flag(edi,'edi')
edi = removing_prefix(edi,"edi_")
edi_t5 = study_id_to_id(edi_t5,"edi_")
edi = identify_same_data(edi_t5,edi)
edi = rbind.fill(edi,edi_t5)

#calculating age
edi = fxage(edi,'id','date')

#changing the categories into numbers
for(j in paste0('',1:30)){
  edi[,j] = ifelse(edi[,j]=="Moderate",2,
                   ifelse(edi[,j]=="Mild",1,
                          ifelse(edi[,j]=="Not at all",0,
                                 ifelse(edi[,j]=="Severe",3,
                                        ifelse(edi[,j]=="Very Severe",4,NA)))))
  edi[,j] = as.numeric(edi[,j])
}

#list of items
dysphoria_items = paste0('',c(15,20,23,26,29:30))
reactivity_items = paste0('',c(1:14,16:19,21:22,24:25,27:28))

#missing data analysis
edi = count_missing_items(edi,'1','30')
edi = comment_missing_data(edi,list(dysphoria_items,reactivity_items),
                           list('dysphoria','reactivity'))

#obtaining raw total
edi = summing_items_per_row(edi,list(dysphoria_items,reactivity_items),
                            list('dysphoria_raw','reactivity_raw'),F)

#obtaining norm scores
edi_dys_norm = read_excel("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/EDI/EDI_NormConversionTables.xlsx",sheet="DysphoriaNorms")[-(1:2),c(1,4,8)]
names(edi_dys_norm) = c("raw","clinical_sample_t","general_t")
edi_rea_norm = read_excel("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/EDI/EDI_NormConversionTables.xlsx",sheet="ReactivityNorms")[-(1:2),c(1,4,8)]
names(edi_rea_norm) = c("raw","clinical_sample_t","general_t")
for(j in 2:3){
  edi_dys_norm[,j] = as.numeric(unlist(edi_dys_norm[,j]))
  edi_rea_norm[,j] = as.numeric(unlist(edi_rea_norm[,j]))
  edi_dys_norm[,j] = round(edi_dys_norm[,j],1)
  edi_rea_norm[,j] = round(edi_rea_norm[,j],1)
}
for(i in 1:nrow(edi)){
  edi$dysphoria_clinical_sample_t[i] = unlist(edi_dys_norm[which(edi_dys_norm$raw==edi$dysphoria_raw[i]),"clinical_sample_t"])
  edi$dysphoria_general_sample_t[i] = unlist(edi_dys_norm[which(edi_dys_norm$raw==edi$dysphoria_raw[i]),"general_t"])
  edi$reactivity_clinical_sample_t[i] = unlist(edi_rea_norm[which(edi_rea_norm$raw==edi$reactivity_raw[i]),"clinical_sample_t"])
  edi$reactivity_general_sample_t[i] = unlist(edi_rea_norm[which(edi_rea_norm$raw==edi$reactivity_raw[i]),"general_t"])
}

#putting back prefix
edi = inserting_prefix_into_variables(edi,'edi_')

#orpahned/duplicate data
edi_orphaned_data = orphaned_data_consolidate(edi)
edi = orphaned_data_remove(edi)
edi_duplicate_data = duplicate_data_consolidate(edi,'edi_age')
edi = duplicate_data_remove(edi,'edi_age')

#outliers
edi_outliers = edi[,c(1:2,grep("_age$",names(edi)),grep("_t$",names(edi)))]
edi_outliers = outlier_list(edi_outliers)
edi$edi_outlier_list = edi_outliers$outlier_list

#archiving the data
edi_scored = edi[,c(1:2,grep("age$",names(edi)):ncol(edi))]
write.csv(edi_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/edi_scored.csv",row.names=F)
write.csv(edi[,c(1:2,grep("_[0-9]+$",names(edi)))],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/edi_items.csv",row.names=F)

#clean up
rm(dysphoria_items,reactivity_items,edi_outliers,edi_t5)
