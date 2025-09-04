#combining two tables from two different db's
hpt = sqlQuery(new_con,"select * from Hand_Preference_Task;",stringsAsFactors=F)

#filter out data without being double-entered
hpt = id_visit_changing_into_char(hpt)
hpt_entry_flag = entry_flag(hpt,"hpt")
hpt = subset(hpt,entry_status==2)
if(!is.null(hpt_entry_flag)){
  hpt = rbind(hpt,hpt_entry_flag[,-ncol(hpt_entry_flag)])
}

#calculating age
hpt = removing_prefix(hpt,'HPT_')
hpt = fxage(hpt,'id','date')

#correcting negative scores
hpt = hpt[,c(1:4,8:11,5:7,12:ncol(hpt))]
for(i in 1:nrow(hpt)){
  for(j in which(names(hpt)=="1_BTR"):which(names(hpt)=="Pennies_note")){
    hpt[i,j]=cbraw(hpt[i,j])
  }
}

#calclulating total scores for each hand & missing sections/trials
for(i in 1:nrow(hpt)){
  hpt$right_total[i] = sum(c(hpt$`1_BTR`[i],hpt$`5_PR`[i],hpt$`7_MR`[i],hpt$`9_CR`[i],hpt$`11_PenniesR`[i]),na.rm=T)
  hpt$left_total[i] = sum(c(hpt$`2_BTL`[i],hpt$`6_PL`[i],hpt$`8_ML`[i],hpt$`10_CL`[i],hpt$`12_PenniesL`[i]),na.rm=T)
  for(j in grep("_[A-Z]{1,2}[a-z]*R$",names(hpt))){
    hpt[i,j] = ifelse(is.na(hpt[i,j]) & !is.na(hpt[i,(j+1)]),0,hpt[i,j])
    hpt[i,(j+1)] = ifelse(is.na(hpt[i,(j+1)]) & !is.na(hpt[i,j]),0,hpt[i,(j+1)])
  }
  missing_section = which(is.na(hpt[i,grep("_[A-Z]{1,2}[a-z]*R$",names(hpt))])) #don't need to count both left and right
  missing_trial_5 = length(which(missing_section>3))
  hpt$missing_trials[i] = 5*missing_trial_5 + 3*(length(missing_section)-missing_trial_5)
  rm(missing_section,missing_trial_5)
}

#Ratio Score of Right Handedness
hpt$right_hand_ratio = round(hpt$right_total/(hpt$right_total+hpt$left_total),2)

#handedness variable
hpt$handedness = ifelse(hpt$right_hand_ratio>0.5,"Right",ifelse(hpt$right_hand_ratio<0.5,"Left","Both"))

#putting back prefix
hpt = inserting_prefix_into_variables(hpt,"hpt_")

#orpnaned/duplicate data
hpt_orphaned_data = orphaned_data_consolidate(hpt)
hpt = orphaned_data_remove(hpt)
hpt_duplicate_data = duplicate_data_consolidate(hpt,"hpt_age")
hpt = duplicate_data_remove(hpt,"hpt_age")

#extracting necessary columns
hpt_scored = hpt[,c(1:2,grep("_age$",names(hpt)):ncol(hpt))]

#running edinburgh script
source("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/Edinburgh/EHI_Scoring.R")

#archiving the data
write.csv(hpt_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/hpt_scored.csv",row.names = F)
write.csv(hpt[,c(1:2,grep("_1_",names(hpt)):grep("_PJ$",names(hpt)))],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/hpt_items.csv",row.names = F)
write.csv(edin[grep("^[0-9]{6}-[0-9]{3}$",edin$id),c(1:2,grep("_age$",names(edin)):ncol(edin))],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/edinburgh_scored.csv",row.names=F)
write.csv(handedness_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/handedness_scored.csv",row.names = F)
