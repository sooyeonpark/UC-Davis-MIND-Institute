nih_dccs = sqlQuery(new_con,"select * from NIHToolbox_DCCS",stringsAsFactors=F)
nih_flanker = sqlQuery(new_con,"select * from NIHToolbox_Flanker",stringsAsFactors=F)
nih_fc = sqlQuery(new_con,"select * from NIHToolbox_FluidCognition",stringsAsFactors=F)
nih_lswm = sqlQuery(new_con,"select * from NIHToolbox_LSWM",stringsAsFactors=F)
nih_pc = sqlQuery(new_con,"select * from NIHToolbox_PatternComp",stringsAsFactors=F)
nih_ps = sqlQuery(new_con,"select * from NIHToolbox_PictureSeq",stringsAsFactors=F)

#lowering the characters for col names
names(nih_dccs)=tolower(names(nih_dccs))
names(nih_flanker)=tolower(names(nih_flanker))
names(nih_fc)=tolower(names(nih_fc))
names(nih_lswm)=tolower(names(nih_lswm))
names(nih_pc)=tolower(names(nih_pc))
names(nih_ps)=tolower(names(nih_ps))

#merging all tables
nih = merge(nih_dccs,nih_flanker,all=T)
nih = merge(nih,nih_fc,all=T)
nih = merge(nih,nih_lswm,all=T)
nih = merge(nih,nih_pc,all=T)
nih = merge(nih,nih_ps,all=T)
nih = nih[!is.na(nih$testdate),]

#initial formatting
nih = id_visit_changing_into_char(nih)
nih = removing_prefix(nih,"nih_")

#calculating age based on visitdate
#modifying the date column first (not in a standard date format)
nih = nih[grep("/",nih$visitdate),]
nih$visitdate=gsub(" ","",nih$visitdate)
date = strsplit(nih$visitdate,"/")
for(i in 1:length(date)){
  nih$visitdate[i] = paste0(date[[i]][3],"-",date[[i]][1],"-",date[[i]][2])
}
nih = fxage(nih,'id','visitdate')

#orphaned/duplicate data
nih = inserting_prefix_into_variables(nih,"nih_")
nih_orphaned_data = orphaned_data_consolidate(nih)
nih = orphaned_data_remove(nih)
nih_duplicate_data = duplicate_data_consolidate(nih,"nih_age")
nih = duplicate_data_remove(nih,"nih_age")

#outliers
nih_outliers = nih[,c(1:2,ncol(nih),grep("ageadj_scale$",names(nih)))]
nih_outliers = outlier_list(nih_outliers)
nih$nih_outlier_list = nih_outliers$outlier_list
#nih_outlier_table = sqldf("select id,visit,outlier_list from nih where outlier_list != ''")
rm(nih_outliers)

#archiving the data
nih_processed = nih[,c(1:2,grep("age$",names(nih)),grep("_computed$",names(nih)),grep("_ageadj_scale$",names(nih)),grep("_np$",names(nih)),ncol(nih))]
write.csv(nih_processed,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/nih_toolbox_processed.csv",row.names = F)

#clean up
rm(nih_dccs,nih_fc,nih_flanker,nih_lswm,nih_pc,nih_ps,date)
