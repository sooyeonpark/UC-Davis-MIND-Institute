#data = data.frame() before running this function
data_entry_check = function(form,armnum,dt){
  library(redcapAPI)
  entry1 = exportRecords(rc_p2_con,forms=c("inclusionexclusionscreener",form),
                         events=paste0("data_entry_arm_",armnum),labels=F,stringsAsFactors=F)
  entry1 = entry1[,-(2:grep("inclusionexclusionscreener_complete",names(entry1)))]
  entry1[,length(entry1)]=NULL
  entry1 = inserting_prefix_into_variables(entry1,"first_")
  entry2 = exportRecords(rc_p2_con,forms=c("inclusionexclusionscreener",form),
                         events=paste0("data_entry_validat_arm_",armnum),labels=F,stringsAsFactors=F)
  entry2 = entry2[,-(2:grep("inclusionexclusionscreener_complete",names(entry2)))]
  entry2[,length(entry2)]=NULL
  entry2 = inserting_prefix_into_variables(entry2,"second_")
  
  #merging two entry tables with variables of interest
  entry = merge(entry1,entry2,all.x=T)
  #comparing first entry and second
  for(j in 3:length(entry1)){
    discrepant = entry[which(tolower(entry[,j]) != tolower(entry[,(j+length(entry1)-1)])),c(1,j,(j+length(entry1)-1))]
    if(nrow(discrepant)>0){
      discrepant$var = gsub("first_","",names(entry)[j])
      names(discrepant) = c("id","entry1","entry2","var")
      dt = rbind(dt,discrepant)
    }
  }
  rm(entry1,entry2)
  return(dt)
}

#ex. data_entry_discrepancy = data_entry_check("asdpeds_rating_form",11,data_entry_discrepancy)
#Questions to ask: do we need to compare the scores between other measures in any other arms? tap equivalent to asdpeds?
# discrepancy = data.frame()
# discrepancy = data_entry_check('cbe',2,discrepancy)
# discrepancy = data_entry_check('infanttoddler_esdm_curriculum',2,discrepancy)
# discrepancy = data_entry_check('aosi',2,discrepancy)
# discrepancy$visit = '1a'
# data_entry_discrepancy = discrepancy
# discrepancy = data.frame()
# discrepancy = data_entry_check('cbe',3,discrepancy)
# discrepancy = data_entry_check('infanttoddler_esdm_curriculum',3,discrepancy)
# discrepancy = data_entry_check('aosi',3,discrepancy)
# discrepancy$visit = '1b'
# data_entry_discrepancy = rbind(data_entry_discrepancy,discrepancy)
# discrepancy = data.frame()
# discrepancy = data_entry_check('cbe',4,discrepancy)
# discrepancy = data_entry_check('infanttoddler_esdm_curriculum',4,discrepancy)
# discrepancy = data_entry_check('aosi',4,discrepancy)
# discrepancy$visit = '2'
# data_entry_discrepancy = rbind(data_entry_discrepancy,discrepancy)
# discrepancy = data.frame()
# discrepancy = data_entry_check('cbe',5,discrepancy)
# discrepancy = data_entry_check('infanttoddler_esdm_curriculum',5,discrepancy)
# discrepancy = data_entry_check('aosi',5,discrepancy)
# discrepancy$visit = '3'
# data_entry_discrepancy = rbind(data_entry_discrepancy,discrepancy)
# discrepancy = data.frame()
# discrepancy = data_entry_check('cbe',6,discrepancy)
# discrepancy = data_entry_check('infanttoddler_esdm_curriculum',6,discrepancy)
# discrepancy = data_entry_check('aosi',6,discrepancy)
# discrepancy$visit = '4'
# data_entry_discrepancy = rbind(data_entry_discrepancy,discrepancy)
# data_entry_discrepancy = data_entry_discrepancy[order(data_entry_discrepancy$id),c("id","visit","var","entry1","entry2")]
# data_entry_discrepancy = data_entry_discrepancy[!grepl("reliability$",data_entry_discrepancy$var),]
# data_entry_discrepancy = data_entry_discrepancy[!is.na(data_entry_discrepancy$entry1)&!is.na(data_entry_discrepancy$entry2),]
# rm(discrepancy)
# write.csv(data_entry_discrepancy,"data_entry_discrepancy.csv",row.names=F)
