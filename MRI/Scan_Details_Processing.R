scan_details = sqlQuery(mri_con,"select * from ScanDetails",stringsAsFactors=F)

#changing column names and classes
names(scan_details) = tolower(names(scan_details))
names(scan_details)[1] = "id"
scan_details = id_visit_changing_into_char(scan_details)

#calculating age -> traditional method!
#importing dob from subject folder
scan_details = subset(scan_details,tolower(status)=="success" | tolower(status)=="failed")
scan_details[which(tolower(scan_details$status)=="failed"),"scan date"]=NA
scan_details = merge(scan_details,subj[,c(1,3)],by.x="id",by.y="subj_id",all.x=T)
scan_details$age = elapsed_months(scan_details$`scan date`,scan_details$dob)
scan_details[scan_details$status=="FAILED","status"] = "Failed"

#orphaned/duplicate data
scan_details_orphaned_data = orphaned_data_consolidate(scan_details)
scan_details = orphaned_data_remove(scan_details)
scan_details_orphaned_data = scan_details_orphaned_data[-which(scan_details_orphaned_data$id %in% app_cohort_subj$`subj id`==T),]
scan_details_success = subset(scan_details,tolower(status)=="success")
scan_details_failure = subset(scan_details,tolower(status)!="success")
scan_details_duplicate_data = duplicate_data_consolidate(scan_details_success,"age")
scan_details_success = duplicate_data_remove(scan_details_success,"age")
scan_details = rbind(scan_details_success,scan_details_failure)

#extracting necessary columns, cutting down rows, and putting prefix
scan_details_processed = scan_details[,c(1:2,grep("^age$",names(scan_details)),4:grep("^scanning notes$",names(scan_details)))]
names(scan_details_processed) = gsub("^scan ","",names(scan_details_processed))
names(scan_details_processed)[1:16] = gsub(" ","_",names(scan_details_processed)[1:16])
names(scan_details_processed)[17] = gsub(" ","",names(scan_details_processed)[17])
names(scan_details_processed)[18:43] = gsub(" ","_",names(scan_details_processed)[18:43])
scan_details_processed = inserting_prefix_into_variables(scan_details_processed,"scan_")

#merging "master jedi" sheet
mri_master = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/MRI/mastersheet.csv",sep=",",header=T,stringsAsFactors = F)
names(mri_master) = gsub("\\.","_",tolower(names(mri_master)))
names(mri_master)[1:2] = c("id","visit")
scan_details_processed = merge(scan_details_processed,mri_master[,c(1:3,6:39,41:42,44:45)],all.x=T)
for(j in 1:ncol(scan_details_processed)){
  if(class(scan_details_processed[,j])=="logical"){
    scan_details_processed[,j] = as.character(scan_details_processed[,j])
  }
}

#scan_id column
scan_details_processed$scan_id = ifelse(is.na(scan_details_processed$scan_id) & scan_details_processed$scan_status != "Failed" & scan_details_processed$visit != "1",
                                        paste0(scan_details_processed$id,'_',as.numeric(scan_details_processed$visit)-1),
                                        ifelse(is.na(scan_details_processed$scan_id) & scan_details_processed$scan_status != "Failed" & scan_details_processed$visit == "1",
                                               scan_details_processed$id,scan_details_processed$scan_id))

#archiving the data
names(scan_details_processed) = gsub("__","_",names(scan_details_processed))
names(scan_details_processed) = gsub("^comment","scan_comment",names(scan_details_processed))
names(scan_details_processed)[grep("^flags_",names(scan_details_processed))] = "scan_flags"
write.csv(scan_details_processed,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/scan_details_processed.csv",row.names = F)

#cleaning up
rm(scan_details_success,scan_details_failure,mri_master)
