pe = sqlQuery(med_con,"select * from `Physical Exam`",stringsAsFactors=F)

pe = id_visit_changing_into_char(pe)
pe_entry_flag = entry_flag(pe,'physical_exam')
pe = subset(pe,entry_status==2)
#pe = rbind(pe,pe_entry_flag[,-ncol(pe_entry_flag)])
pe = removing_prefix(pe,"phys_exam_")

pe = fxage(pe,'id','date')

pe = inserting_prefix_into_variables(pe,"pe_")

pe_orphaned_data = orphaned_data_consolidate(pe)
pe = orphaned_data_remove(pe)
pe_duplicate_data = duplicate_data_consolidate(pe,"pe_age")
pe = duplicate_data_remove(pe,"pe_age")

pe_processed = pe[,c(1:2,ncol(pe),which(names(pe)=="pe_occiput"):which(names(pe)=="pe_neurological_other"))]
write.csv(pe_processed,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/pe_processed.csv",row.names=F)
