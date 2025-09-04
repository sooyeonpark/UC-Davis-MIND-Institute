pal = sqlQuery(new_con,"select * from PAL_II",stringsAsFactors=F)

pal = id_visit_changing_into_char(pal)
pal_entry_flag = entry_flag(pal,'pal')
pal = subset(pal,entry_status==2)
if(!is.null(pal_entry_flag)){
  pal = rbind(pal,pal_entry_flag[,-ncol(pal_entry_flag)])
}
pal = removing_prefix(pal,'^pal_')

for(j in which(names(pal)=="awal_raw"):ncol(pal)){
  pal[,j] = cbraw(pal[,j])
}

pal = fxage(pal,'id','date')

pal = comment_missing_data(pal,list(grep("_raw$",names(pal))),list('','','','','','','','',''))
pal[which(pal$missing_items_comment != ''),"missing_items_comment"] = "Raw/Scaled score(s) missing in the data entry;"

pal = inserting_prefix_into_variables(pal,'pal_')

pal_orphaned_data = orphaned_data_consolidate(pal)
pal = orphaned_data_remove(pal)
pal_duplicate_data = duplicate_data_consolidate(pal,"pal_age")
pal = duplicate_data_remove(pal,"pal_age")

pal_processed = pal[,c(1:2,grep("pal_age",names(pal)),grep("missing",names(pal)),grep("awal_raw",names(pal)):which(names(pal)=="pal_hwgtt_comp"))]
write.csv(pal_processed,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/pal_processed.csv",row.names = F)
