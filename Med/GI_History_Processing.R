gi = sqlQuery(med_con,"select * from `GI History`",stringsAsFactors=F)

gi = id_visit_changing_into_char(gi)
gi_entry_flag = entry_flag(gi,'gi_history')
gi = subset(gi,entry_status==2)
#gi = rbind(gi,gi_entry_flag[,-ncol(gi_entry_flag)])

gi = fxage(gi,'id','gi_hist_date')
names(gi)[ncol(gi)] = "gi_hist_age"

gi_orphaned_data = orphaned_data_consolidate(gi)
gi = orphaned_data_remove(gi)
gi_duplicate_data = duplicate_data_consolidate(gi,"gi_hist_age")
gi = duplicate_data_remove(gi,"gi_hist_age")

gi_processed = gi[,c(1:2,ncol(gi),
                     which(names(gi)=="gi_hist_previous_abdominal"):which(names(gi)=="gi_hist_gastrointestinal_diagnosis_list"))]
write.csv(gi_processed,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/gi_processed.csv",row.names = F)
