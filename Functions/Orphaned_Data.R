orphaned_data_consolidate = function(dt){
  #list of orphaned subject for a specific test measure with the data
  #dt = where you want to identify orphaned data from
  names(dt)[1] = tolower(names(dt)[1])
  #subj = merging_brain_staar_study_subjects()
  orphaned_data_index = which(!(dt$id %in% subj$subj_id))
  if(length(orphaned_data_index) > 0){
    dt2 = dt[orphaned_data_index,]
    return(dt2)
  }
}

# orphaned_data_consolidate_app = function(dt){
#   #list of orphaned subject for a specific test measure with the data
#   #dt = where you want to identify orphaned data from
#   names(dt)[1] = tolower(names(dt)[1])
#   app_cohort_subj = sqlQuery(channel = new_con2,
#                              query = "SELECT t1.`SUBJ ID` AS subj_id, t1.`SUBJ GRP ID` AS study_cohort,
#                              t1.`SUBJ STATUS` as subj_status,t1.`SUBJ DOB` AS dob, t1.`SUBJ GENDER` AS gender,
#                              t1.`SUBJ APP DIAG` AS app_diagnosis,t1.`APP_DIAG_Comment`as app_diagnosis_comment
#                              FROM tblSubject AS t1 where t1.`SUBJ TYPE`='Primary';",stringsAsFactors=F)
#   orphaned_data_index = which(!(dt$id %in% app_cohort_subj$subj_id))
#   if(length(orphaned_data_index) > 0){
#     dt2 = dt[orphaned_data_index,]
#     return(dt2)
#   }
# }

orphaned_data_remove = function(dt){
  orphaned = orphaned_data_consolidate(dt)
  print(which(dt$id %in% orphaned$id))
  if(length(which(dt$id %in% orphaned$id))>0){
    dt = dt[-which(dt$id %in% orphaned$id & dt$visit %in% orphaned$visit),]
  }
  return(dt)
}

combining_multiple_data = function(dt,dt_list,measure_name,measure_name_list){
  #dt = original data table
  #dt2 = new data table planning to be merged into dt (can be a list) -> measure_name2 can be a list as well
  if(ncol(dt) != 2){
    dt$measure = measure_name
  }
  for(l in 1:length(dt_list)){
    dt2 = dt_list[[l]]
    dt2$measure = measure_name_list[[l]]
    dt = rbind(dt[,c("id","visit","measure")],dt2[,c("id","visit","measure")])
    dt = unique(dt)
  }
  library(plyr)
  dt = ddply(dt,.(id,visit),summarize,measure_list = paste0(measure,collapse=", "))
  return(dt)
}
#ex. orphaned_data_list = combining_two_data(orphaned_data_list,cshq_orphaned_data,"measure_list","cshq")