#pulling out the data table and norm table
ssp2_t3 = exportRecords(ace_con,forms=c("participant_information","ssp2"),events="t3_arm_1",labels=F,stringsAsFactors=F)
ssp2_t3 = ssp2_t3[grepl("Co",ssp2_t3$ssp2_complete)&!is.na(ssp2_t3$ssp2_timestamp),-(2:grep("participant_information_complete",names(ssp2_t3)))]
ssp2_t3$visit = 3
ssp2 = sqlQuery(new_con, "SELECT * FROM `Short_Sensory_Profile_2`;",stringsAsFactors=F)
ssp2_norm = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/scr_short sensory profile 2.csv",stringsAsFactors = F)

#id and visit into characters, removing prefixes and single-entered rows
ssp2 = id_visit_changing_into_char(ssp2)
ssp2_entry_flag = entry_flag(ssp2,'ssp2')
ssp2 = subset(ssp2,entry_status==2)
if(!is.null(ssp2_entry_flag)){
  ssp2 = rbind(ssp2,ssp2_entry_flag[,-ncol(ssp2_entry_flag)])
}
ssp2 = removing_prefix(ssp2,"ssp2_")
ssp2_t3 = study_id_to_id(ssp2_t3,"ssp2_")
ssp2 = identify_same_data(ssp2_t3,ssp2)
ssp2 = rbind.fill(ssp2,ssp2_t3)

#calculating age
ssp2 = fxage(ssp2,'id','date')

#obtaining raw scores
#lists of section items
sk_items = paste0('q',c(6:8,11,14,31:32))
av_items = paste0('q',c(16:18,20,22:24,26))
sn_items = paste0('q',c(1:5,21,25,28:29,33))
rg_items = paste0('q',c(9:10,12:13,15,27,30,34))
sensory_items = paste0('q',c(1:14))
behavior_items = paste0('q',c(15:34))

for(j in which(names(ssp2)=="q1"):which(names(ssp2)=="q34")){
  ssp2[,j] = cbraw(ssp2[,j])
}

#missing data analysis
ssp2 = count_missing_items(ssp2,'q1','q34')
ssp2 = comment_missing_data(ssp2,list(sk_items,av_items,sn_items,rg_items,sensory_items,behavior_items),
                            list('Seeking','Avoiding','Sensitivity','Registration','Sensory','Behavioral'))

#summing the items up to get section total raw scores
ssp2 = summing_items_per_row(ssp2,list(sk_items,av_items,sn_items,rg_items,sensory_items,behavior_items),
                             list('sk_raw','av_raw','sn_raw','rg_raw','sensory_raw','behavior_raw'),F)

#obtaining normed summary
for(i in 1:nrow(ssp2)){
  ssp2$sk_summary_scores[i] = ifelse(is.na(ssp2$sk_raw[i]),NA,ssp2_norm[which(ssp2$sk_raw[i]==ssp2_norm$raw),"sk"])
  ssp2$av_summary_scores[i] = ifelse(is.na(ssp2$av_raw[i]),NA,ssp2_norm[which(ssp2$av_raw[i]==ssp2_norm$raw),"av"])
  ssp2$sn_summary_scores[i] = ifelse(is.na(ssp2$sn_raw[i]),NA,ssp2_norm[which(ssp2$sn_raw[i]==ssp2_norm$raw),"sn"])
  ssp2$rg_summary_scores[i] = ifelse(is.na(ssp2$rg_raw[i]),NA,ssp2_norm[which(ssp2$rg_raw[i]==ssp2_norm$raw),"rg"])
  ssp2$sensory_summary_scores[i] = ifelse(is.na(ssp2$sensory_raw[i]),NA,ssp2_norm[which(ssp2$sensory_raw[i]==ssp2_norm$raw),"sensory"])
  ssp2$behavior_summary_scores[i] = ifelse(is.na(ssp2$behavior_raw[i]),NA,ssp2_norm[which(ssp2$behavior_raw[i]==ssp2_norm$raw),"behavioral"])
}

#putting back the prefix
ssp2 = inserting_prefix_into_variables(ssp2,"ssp2_")

#orphaned/duplicate data
ssp2_orphaned_data = orphaned_data_consolidate(ssp2)
ssp2 = orphaned_data_remove(ssp2)
ssp2_duplicate_data = duplicate_data_consolidate(ssp2,"ssp2_age")
ssp2 = duplicate_data_remove(ssp2,"ssp2_age")

#outliers?
ssp2_outliers = ssp2[,c(1:2,which(names(ssp2)=="ssp2_age"),grep("sk_raw",names(ssp2)):ncol(ssp2))]
ssp2_outliers = outlier_list(ssp2_outliers)
ssp2$ssp2_outlier_list = ssp2_outliers$outlier_list
#ssp2_outlier_table = sqldf("select id, visit, outlier_list from ssp2 where outlier_list != ''")
rm(ssp2_outliers)

#archiving the data
ssp2_scored = ssp2[,c(1:2,grep("_age$",names(ssp2)),grep("missing",names(ssp2)),grep("sk_raw",names(ssp2)):ncol(ssp2))]
write.csv(ssp2_scored, "S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/ssp2_scored.csv",row.names = F)
write.csv(ssp2[,c(1:2,grep("_q[0-9]+$",names(ssp2)))], "S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/ssp2_items.csv",row.names = F)

#cleaning up
rm(sk_items,av_items,sn_items,rg_items,sensory_items,behavior_items,ssp2_t3)
