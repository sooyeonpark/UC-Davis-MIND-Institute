dxcf = sqlQuery(new_con,"select * from Diagnostic_Consensus;",stringsAsFactors=F)

#initial data manipulation
dxcf = id_visit_changing_into_char(dxcf)
dxcf_entry_flag = entry_flag(dxcf,'dxcf')
dxcf = subset(dxcf, entry_status==2 & visit != '4P')
if(!is.null(cshq_entry_flag)){
  dxcf = rbind(dxcf,dxcf_entry_flag[,-ncol(dxcf_entry_flag)])
}
dxcf = removing_prefix(dxcf,"dxcf_")

#calculate age -> no need!
#dxcf = fxage(dxcf,'id','date')

#filling in clinimp variable using confidence variable
table(dxcf$clinimp)
dxcf$clinimp = ifelse(dxcf$clinimp=="" & dxcf$confidence != "",ifelse(grepl(" not ASD",dxcf$confidence),"Not ASD",dxcf$clinimp),dxcf$clinimp)
table(dxcf$clinimp) #98 blank rows became "Not ASD"

#putting back prefixes
dxcf = inserting_prefix_into_variables(dxcf,'dxcf_')

#orphaned/duplicate data
dxcf_orphaned_data = orphaned_data_consolidate(dxcf)
dxcf = orphaned_data_remove(dxcf)
which(table(dxcf$id,dxcf$visit)>1)
table(dxcf$dxcf_clinimp)

#merging med hist mental health cols

#archiving the data
dxcf_processed = dxcf[,c(1:2,grep("clinimp$",names(dxcf)):grep("comments$",names(dxcf)))]
names(dxcf_processed)[ncol(dxcf_processed)] = "dxcf_clincom"
write.csv(dxcf_processed,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/dxcf_processed.csv",row.names=F)
