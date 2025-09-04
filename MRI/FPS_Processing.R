fps = sqlQuery(mri_con,"select * from tblFPS;",stringsAsFactors=F)

names(fps)[1] = "id"
#fps$visit = 4
fps = id_visit_changing_into_char(fps)
fps = removing_prefix(fps,"fps_")
names(fps)[grep("datacomment",names(fps))]="data_comment"

#calcualte age
fps = fxage(fps,'id','date')

#inserting prefixes back
fps = inserting_prefix_into_variables(fps,"fps_")

#orphaned data
fps_orphaned_data = orphaned_data_consolidate(fps)
fps = orphaned_data_remove(fps)
fps_duplicate_data = duplicate_data_consolidate(fps,"fps_age")
fps = duplicate_data_remove(fps,"fps_age")

#archiving
fps_processed = fps[,c(1:2,grep("age$",names(fps)),grep("habituation1slope",names(fps)):grep("Field36$",names(fps)))]
for(j in c(4:12,15:21,24:30)){
  fps_processed[,j] = round(fps_processed[,j],2)
}
write.csv(fps_processed,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/fps_processed.csv",row.names=F)
