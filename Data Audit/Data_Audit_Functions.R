#write a function that pulls out each measure in each visit and compare each other
#& subj$app_diagnosis=="ASD"
tracking_access_comparison=function(dt,visit_num,tracking_type,measure_name){
  #extracting relevant rows from tracking sheet and db to start comparing
  tracking = assess_survey_primary[which(tolower(assess_survey_primary$Type)==tracking_type),]
  access = dt[which(dt$visit==visit_num),]
  single_entry_flag_table = single_entry_flag_table[which(single_entry_flag_table$visit==visit_num),]
  single_entry_flag_table = single_entry_flag_table[grep(tolower(measure_name),single_entry_flag_table$flag_list),]
  
  #building up the list of rows missing in db
  access_missing = data.frame(tracking[which(tracking[,1] %in% subj[subj$study_cohort != 'STAAR',1] == T & tracking[,1] %in% access[,1] == F),1])
  if(nrow(access_missing)>0){
    access_missing$missing_from = 'ACCESS'
    access_missing$measure = measure_name
    access_missing$id = as.character(access_missing[,1])
    access_missing$visit = visit_num
    access_missing = merge(access_missing,tracking[,c(1:2,4,6)],by.x="id",by.y="STS ID",all.x=T)
    access_missing$suspected_reason = ''
    access_missing$possible_solution = ''
    #putting possible reasons for missing data in access db
    for(i in 1:nrow(access_missing)){
      status = assess_survey_primary[which(assess_survey_primary[,1]==access_missing$id[i]&tolower(assess_survey_primary$Type)==tracking_type),"Status"]
      comment = assess_survey_primary[which(assess_survey_primary[,1]==access_missing$id[i]&tolower(assess_survey_primary$Type)==tracking_type),"Comment"]
      #missing = assess_survey_primary[which(assess_survey_primary[,1]==access_missing$id[i] & tolower(assess_survey_primary$Type)==tracking_type),"Missing Data"]
      visit_status = subj[which(subj[,1]==access_missing$id[i]),paste0('time',visit_num,'_subj_status')]
      #tracked missing data for complete participants
      if((status == "Missing Data") & grepl("Completed",visit_status)){
        access_missing$suspected_reason[i] = "Tracked missing data"
        access_missing$possible_solution[i] = "NA"
      }
      #visit in progress
      else if(!grepl("Completed",visit_status)){
        if(grepl("Ineligible",visit_status)){
          access_missing$suspected_reason[i] = "Missing data: Ineligible Participant"
          access_missing$possible_solution[i] = "NA"
        }
        else{
          access_missing$suspected_reason[i] = "Missing data: Visit Incomplete"
          access_missing$possible_solution[i] = ifelse(visit_status %in% c("Consented-Active","Agreed","","Scheduled") | grepl("rescheduled",tolower(visit_status)),"Wait til the data gets collected/entered","NA")
        }
      }
      #tracking table doesn't say "validated"
      else if(!is.na(status) & grepl("Completed",visit_status)){
        if(status != "Validated"){
          if(status == "Problem with data"){
            access_missing$suspected_reason[i] = "Problem with Data"
            access_missing$possible_solution[i] = "NA"
          }
          else if(grepl("not valid",tolower(comment)) | grepl("invalid",tolower(status))){
            access_missing$suspected_reason[i] = "Invalid data"
            access_missing$possible_solution[i] = "NA"
          }
          else if(grepl("not administered",tolower(status)) | grepl("incomplete",tolower(status)) | grepl("too",tolower(comment))){
            access_missing$suspected_reason[i] = "Didn't/Couldn't collect data"
            access_missing$possible_solution[i] = "NA"
          }
          else{
            access_missing$suspected_reason[i] = "Data not validated"
            access_missing$possible_solution[i] = "Inquire Brianna to check"
          }
        }
      }
      #duplicate data check?
      if(access_missing$id[i] %in% duplicate_data_list$id
         & visit_num %in% duplicate_data_list[which(duplicate_data_list$id == access_missing$id[i]),"visit"]){
        if(tolower(measure_name) %in% duplicate_data_list[which(duplicate_data_list$id == access_missing$id[i]),"measure_list"]){
          access_missing$suspected_reason[i] = "Duplicate data"
          access_missing$possible_solution[i] = "Figure out the correct time point/age for the data"
        }
      }
      #single entry check
      if(access_missing$id[i] %in% single_entry_flag_table$id){
        access_missing$suspected_reason[i] = "Single-entered data"
        access_missing$possible_solution[i] = "Double enter the data"
      }
    }
    rm(status,visit_status)
  }
  #building up the list of rows missing in tracking tables
  tracking_missing = data.frame(access[which(access[,1] %in% tracking[,1] == F),1])
  if(nrow(tracking_missing)>0){
    tracking_missing$missing_from = 'Tracking Table'
    tracking_missing$measure = measure_name
    tracking_missing$id = as.character(tracking_missing[,1])
    tracking_missing$visit = visit_num
    tracking_missing$suspected_reason = ''
    tracking_missing$possible_solution = ''
    for(i in 1:nrow(tracking_missing)){
      if(any(c("Consented-Ineligible","Ineligible") %in% subj[which(subj[,1]==tracking_missing[i,"id"]),"app_diagnosis"])==T){
        tracking_missing$suspected_reason[i] = "Ineligible participant"
        tracking_missing$possible_solution[i] = ""
      }
      else{
        tracking_missing$suspected_reason[i] = "Not entered in the tracking table"
        tracking_missing$possible_solution[i] = "Enter corresponding info to the tracking"
      }
    }
  }
  #combining two tables
  access_columns = c("missing_from","measure","id","visit","Comp Date","Status","Missing Data","suspected_reason","possible_solution")
  if(nrow(access_missing)>0 & nrow(tracking_missing)>0){
    library(plyr)
    access_tracking_missing = rbind.fill(access_missing[,access_columns],tracking_missing[,2:ncol(tracking_missing)])
    names(access_tracking_missing)[5:7] = c("task_date","task_status","missing_data")
    access_tracking_missing[which(access_tracking_missing$missing_from=="Tracking Table"),"task_status"]="Validated"
    access_tracking_missing[which(access_tracking_missing$missing_from=="Tracking Table"),"missing_data"]=0
    return(access_tracking_missing)
  }
  else if(nrow(access_missing)>0 & nrow(tracking_missing) == 0){
    access_missing = access_missing[,access_columns]
    names(access_missing)[5:7] = c("task_date","task_status","missing_data")
    return(access_missing)
  }
  else if(nrow(tracking_missing)>0 & nrow(access_missing) == 0){
    tracking_missing$task_status="Validated"
    tracking_missing$missing_data=0
    return(tracking_missing[,-1])
  }
  rm(access_columns)
}

#getting all the id and visit pairs within a table
visit_tracking = function(dt){
  id_visit = dt[,c("id","visit")]
  id_visit = unique(id_visit)
  return(id_visit)
}

#auditing age outliers
age_outlier_list_audit = function(dt,measure){
  #dt = list of data frames to pull out from
  #measure = list of measures corresponding to "dt"
  result = data.frame()
  for(l in 1:length(dt)){
    age_outliers_list = dt[[l]][grepl("_age;",dt[[l]][,grep("outlier_list$",names(dt[[l]]))]),c(1:2,grep("_age$",names(dt[[l]]))[1])]
    if(nrow(age_outliers_list) != 0){
      names(age_outliers_list) = c("id","visit","age")
      age_outliers_list$measure = measure[[l]]
      result = rbind(result,age_outliers_list)
    }
  }
  return(result)
}
# age_outliers = age_outlier_list(list(adi_scored,adis_processed,ados_scored,cbcl_scored,
#                                      cbq_scored,ccc_scored,cdi_wg_scored,cdi_ws_scored,
#                                      celf_processed,cshq_scored,das_scored,edq_scored,
#                                      eowpvt,fqs_scored,gi_processed,gort_scored,harter_scored,
#                                      masc_scored,masc_p_scored,mullen_scored,nih_processed,
#                                      pal_processed,ppvt_scored,rbs_scored,scared_scored,
#                                      scared_p_scored,scq_scored,seq_scored,srs_scored,
#                                      ssp_scored,ssp2_scored,sti_scored,tmcq_scored,towre_scored,
#                                      vabs_scored,wraml_scored),
#                                 list('adi','adis','ados','cbcl','cbq','ccc','cdi_wg','cdi_ws',
#                                      'celf','cshq','das','edq','eowpvt','fqs','gi_history',
#                                      'gort','harter','masc','masc_parent','mullen','nih_toolbox',
#                                      'pal','ppvt','rbs','scared','scared_parent','scq','seq','srs',
#                                      'ssp','ssp2','sti','tmcq','towre','vabs','wraml'))
# age_outliers2 = age_outliers
# age_outliers = ddply(age_outliers[,c(1:2,4)],.(id,visit),summarize,measure_list=paste0(measure,collapse="; "))
# age_outliers2 = sqldf("select id,visit,avg(age) as avg_age from age_outliers2 group by id,visit")
# age_outliers2$avg_age = round(age_outliers2$avg_age,2)
# age_outliers = merge(age_outliers,age_outliers2,all.x=T)
# age_outliers = age_outliers[,c(1:2,4,3)]
# rm(age_outliers2)  

#auditing unscorable data
unscorable_data_audit = function(dt,measure){
  #dt = list of data frames to pull out from
  #measure = list of measures corresponding to "dt"
  result = data.frame()
  for(l in 1:length(dt)){
    unscorable_data_id_visit = dt[[l]][dt[[l]][,grep("missing_items_comment$",names(dt[[l]]))] != '' & !is.na(dt[[l]][,grep("missing_items_comment$",names(dt[[l]]))]),c(1:2,grep("missing_items_comment$",names(dt[[l]])))]
    if(nrow(unscorable_data_id_visit) != 0){
      names(unscorable_data_id_visit) = c("id","visit","missing_list")
      unscorable_data_id_visit$measure = measure[[l]]
      result = rbind(result,unscorable_data_id_visit)
    }
  }
  return(result)
}
# unscorable_data_list = unscorable_data_audit(list(adi_scored,adis_processed,ados_scored,cbcl_scored,
#                                      cbq_scored,ccc_scored,cdi_wg_scored,cdi_ws_scored,
#                                      celf_processed,cshq_scored,das_scored,edq_scored,
#                                      eowpvt,fqs_scored,gi_processed,gort_scored,harter_scored,
#                                      masc_scored,masc_p_scored,mullen_scored,nih_processed,
#                                      pal_processed,ppvt_scored,rbs_scored,scared_scored,
#                                      scared_p_scored,scq_scored,seq_scored,srs_scored,
#                                      ssp_scored,ssp2_scored,sti_scored,tmcq_scored,towre_scored,
#                                      vabs_scored,wraml_scored),
#                                 list('adi','adis','ados','cbcl','cbq','ccc','cdi_wg','cdi_ws',
#                                      'celf','cshq','das','edq','eowpvt','fqs','gi_history',
#                                      'gort','harter','masc','masc_parent','mullen','nih_toolbox',
#                                      'pal','ppvt','rbs','scared','scared_parent','scq','seq','srs',
#                                      'ssp','ssp2','sti','tmcq','towre','vabs','wraml'))

#checking if an id is existing in multiple tables
id_check = function(id,table_list,table_name){
  #id can be vector list
  #table_list should be in a form of list
  tables_with_id = c()
  for(l in 1:length(table_list)){
    id_in_table = id %in% table_list[[l]]$id
    if(any(id_in_table == TRUE)){
      tables_with_id = c(tables_with_id,table_name[l])
    }
  }
  return(tables_with_id)
}

date_comparison = function(dt,access_dt,visit_num,measure){
  #dt = existing data table that you want to append the result from
  #access_dt should be a list and measure can be either a vector or a list
  #and those two need to match up the lengths
  measure = toupper(measure)
  result = dt
  for(l in 1:length(measure)){
    tracking_type = ifelse(visit_num==1,measure[l],paste0(measure[l],' (',visit_num,')'))
    print(tracking_type)
    access = access_dt[[l]]
    access_date_var_name = names(access)[grep("date$",names(access))]
    access = access[access$visit==visit_num,c("id","visit",access_date_var_name)]
    names(access)[ncol(access)]="db_date"
    if(length(which(assess_survey$Type==toupper(tracking_type)))>2 & nrow(access)>0){
      init_row = nrow(result)
      tracking_data = assess_survey[assess_survey$Type==tracking_type,1:4]
      comparison = merge(access,tracking_data,by.x="id",by.y="STS ID",all.x=T)
      for(i in 1:nrow(comparison)){
        if(!any(is.na(c(comparison[i,"Comp Date"],comparison[i,"db_date"])))
           & as.POSIXct(comparison[i,"Comp Date"]) != as.POSIXct(comparison[i,"db_date"])){
          result = rbind.fill(result,comparison[i,])
        }
      }
      result$measure[init_row:nrow(result)] = measure[l]
    }
  }
  return(result[,-grep("Type",names(result))])
}

#example code
result = data.frame()
result = date_comparison(result,list(adi,adis,ados_g1,ados_g2,ados_g3,ados2_1,ados2_2,
                                     ados2_3,cbcl_young,cbcl_old,cbq,ccc,cdi_wg,cdi_ws,celf,
                                     cshq,das_ey,das_sa,demograph,dsm,edin,edq,eowpvt3,
                                     eowpvt4,fps,fqs,gort,harter,hpt,masc,mullen,nih,pal,ppvt3,
                                     ppvt4,rbs,scared,scq,seq,sti,srs,ssp,ssp2,towre,vabs,wraml),
                         1,as.character(expression(adi,adis,ados,ados,ados,ados,ados,ados,
                                                   cbcl,cbcl,cbq,ccc,`cdi-wg`,`cdi-ws`,celf,sleep,
                                                   das,das,demographic,dsm5,edinburgh,edq,eowpvt,eowpvt,
                                                   fps,fqs,gort,harter,hpt,masc_c,msel,toolbox,pal,ppvt,ppvt,
                                                   rbs,scared_c,scq,seq,services & treat,srs,ssp1,ssp2,towre,vabs,wraml)))
result = date_comparison(result,list(adi,adis,ados_g1,ados_g2,ados_g3,ados2_1,ados2_2,
                                     ados2_3,cbcl_young,cbcl_old,cbq,ccc,cdi_wg,cdi_ws,celf,
                                     cshq,das_ey,das_sa,demograph,dsm,edin,edq,eowpvt3,
                                     eowpvt4,fps,fqs,gort,harter,hpt,masc,mullen,nih,pal,ppvt3,
                                     ppvt4,rbs,scared,scq,seq,sti,srs,ssp,ssp2,towre,vabs,wraml),
                         3,as.character(expression(adi,adis,ados,ados,ados,ados,ados,ados,
                                                   cbcl,cbcl,cbq,ccc,`cdi-wg`,`cdi-ws`,celf,sleep,
                                                   das,das,demographic,dsm5,edinburgh,edq,eowpvt,eowpvt,
                                                   fps,fqs,gort,harter,hpt,masc_c,msel,toolbox,pal,ppvt,ppvt,
                                                   rbs,scared_c,scq,seq,services & treat,srs,ssp1,ssp2,towre,vabs,wraml)))
result = date_comparison(result,list(adi,adis,ados_g1,ados_g2,ados_g3,ados2_1,ados2_2,
                                     ados2_3,cbcl_young,cbcl_old,cbq,ccc,cdi_wg,cdi_ws,celf,
                                     cshq,das_ey,das_sa,demograph,dsm,edin,edq,eowpvt3,
                                     eowpvt4,fps,fqs,gort,harter,hpt,masc,mullen,nih,pal,ppvt3,
                                     ppvt4,rbs,scared,scq,seq,sti,srs,ssp,ssp2,towre,vabs,wraml),
                         4,as.character(expression(adi,adis,ados,ados,ados,ados,ados,ados,
                                                   cbcl,cbcl,cbq,ccc,`cdi-wg`,`cdi-ws`,celf,sleep,
                                                   das,das,demographic,dsm5,edinburgh,edq,eowpvt,eowpvt,
                                                   fps,fqs,gort,harter,hpt,masc_c,msel,toolbox,pal,ppvt,ppvt,
                                                   rbs,scared_c,scq,seq,services & treat,srs,ssp1,ssp2,towre,vabs,wraml)))

adding_rows_to_tracking = function(tracking_dt,measure_name,visit_num,type,date_source){
  dt = subset(data_tracking,tolower(measure)==measure_name & visit==visit_num)
  dt$Type = type
  dt = merge(dt,date_source[,c(1:2,grep("date$",names(date_source)))],all.x=T)
  names(dt)[ncol(dt)]="Comp Date"
  dt$Comment = ""
  if(all(tracking_dt %in% behav_assess)){
    dt$Administrator = ""
    col_order = c("id","Type","Comp Date","task_status","Administrator","Comment","missing_data")
  }
  else{
    dt$Respondent = ""
    col_order = c("id","Type","task_status","Comp Date","Respondent","Comment","missing_data")
  }
  dt = dt[,c(col_order)]
  dt$missing_data = FALSE
  names(dt) = names(tracking_dt)
  added_tracking_dt = rbind(tracking_dt,dt)
  return(added_tracking_dt)
}
