#importing latest mori files
mori_regions = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/MRI/Columns_MoriRegions_LATEST.csv",sep="\t",header=T,stringsAsFactors = F)
mori_total = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/MRI/Columns_MoriRegionsTotal_LATEST.csv",sep="\t",header=T,stringsAsFactors = F)

mori_regions = removing_prefix(mori_regions,"Type1.")
mori_total = removing_prefix(mori_total,"Type2.")
names(mori_regions)[which(names(mori_regions)=="STS")]="id"
names(mori_total)[which(names(mori_total)=="STS")]="id"
names(mori_regions)[which(names(mori_regions)=="Timepoint")]="visit"
names(mori_total)[which(names(mori_total)=="Timepoint")]="visit"
names(mori_regions) = tolower(names(mori_regions))
names(mori_total) = tolower(names(mori_total))
names(mori_regions) = gsub("\\.","_",names(mori_regions))
names(mori_total) = gsub("\\.","_",names(mori_total))

#changing id and visit into character variables
mori_regions = id_visit_changing_into_char(mori_regions)
mori_total = id_visit_changing_into_char(mori_total)

#getting rid of pilot data
mori_regions = mori_regions[which(mori_regions$visit != '4p'),]
mori_total = mori_total[which(mori_total$visit != '4p'),]

#finding outliers
mori_regions = outlier_list(mori_regions)
mori_total = outlier_list(mori_total)

#adding prefix
mori_regions = inserting_prefix_into_variables(mori_regions,"mori_")
mori_total = inserting_prefix_into_variables(mori_total,"mori_total_")

#orphaned/duplicate data
mori_regions_orphaned_data = orphaned_data_consolidate(mori_regions)
mori_regions = orphaned_data_remove(mori_regions)
mori_total_orphaned_data = orphaned_data_consolidate(mori_total)
mori_total = orphaned_data_remove(mori_total)
if(all(mori_total$id %in% app_cohort_subj$`subj id`)==T){
  rm(mori_regions_orphaned_data,mori_total_orphaned_data)
}
mori_regions$age = mori_regions$visit
mori_total$age = mori_total$visit
mori_regions_duplicate_data = duplicate_data_consolidate(mori_regions,"age")
mori_total_duplicate_data = duplicate_data_consolidate(mori_total,"age")
#mori_regions = duplicate_data_remove(mori_regions,"age")
mori_regions$age = NULL
mori_total$age = NULL

#archiving the data
write.csv(mori_regions,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/mori_regions.csv",row.names = F)
write.csv(mori_total,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/mori_total.csv",row.names = F)
