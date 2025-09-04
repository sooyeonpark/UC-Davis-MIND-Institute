##NON-STAAR STUDIES##
#pulling out data tables
demograph = sqlQuery(new_con,"select * from Demographic;",stringsAsFactors=F)

#id and visit into characters
demograph = id_visit_changing_into_char(demograph)

#removing single-entered rows
demograph_entry_flag = entry_flag(demograph,"demograph")
demograph = subset(demograph,entry_status==2)
if(!is.null(demograph_entry_flag)){
  demograph = rbind(demograph,demograph_entry_flag[,-ncol(demograph_entry_flag)])
}

#removing prefix for convenience
demograph = removing_prefix(demograph,"demo_")

#subsetting data based on each Timepoint
demograph_t1 = subset(demograph,visit==1)
demograph_t3 = subset(demograph,visit==3)
demograph_t4 = subset(demograph,visit==4)

#updating information while keeping the oldest ones
demograph_t1_t3 = filling_info(demograph_t1,demograph_t3)
demograph_final = filling_info(demograph_t1_t3,demograph_t4)

demographics = demograph_final[,c(1:2,which(names(demograph_final)=="ch_raceth"):ncol(demograph_final))]
for(i in 1:nrow(demographics)){
  for(j in 3:ncol(demographics)){
    demographics[i,j] = gsub("\\d{1,2}\\. ","",demographics[i,j])
  }
}

#getting rid of -8's
for(j in 3:ncol(demographics)){
  for(i in 1:nrow(demographics)){
    if(!is.na(demographics[i,j])){
      if(demographics[i,j]=="-8"){
        demographics[i,j]=NA
      }
    }
  }
}

#parents' edu to categories
demographics[,"mth_edu"] = ifelse(demographics[,"mth_edu"]=="1","None",
                                   ifelse(demographics[,"mth_edu"]=="2","Upto 4th grade",
                                          ifelse(demographics[,"mth_edu"]=="3","5-6th grade",
                                                 ifelse(demographics[,"mth_edu"]=="4","7-8th grade",
                                                        ifelse(demographics[,"mth_edu"]=="5","9th grade",
                                                               ifelse(demographics[,"mth_edu"]=="6","10th grade",
                                                                      ifelse(demographics[,"mth_edu"]=="7","11th grade",
                                                                             ifelse(demographics[,"mth_edu"]=="8","12th grade, no diploma",
                                                                                    ifelse(demographics[,"mth_edu"]=="9","High school graduate/GED or equivalent",
                                                                                           ifelse(demographics[,"mth_edu"]=="10","Some college credit (less than 1 year)",
                                                                                                  ifelse(demographics[,"mth_edu"]=="11","Technical college/Vocational school",
                                                                                                         ifelse(demographics[,"mth_edu"]=="12","Associate's degree",
                                                                                                                ifelse(demographics[,"mth_edu"]=="13","Bachelor's degree",
                                                                                                                       ifelse(demographics[,"mth_edu"]=="14","Master's degree",
                                                                                                                              ifelse(demographics[,"mth_edu"]=="15","Professional degree",
                                                                                                                                     ifelse(demographics[,"mth_edu"]=="16","Doctorate degree",demographics[,"mth_edu"]))))))))))))))))
demographics[,"fat_edu"] = ifelse(demographics[,"fat_edu"]=="1","None",
                                  ifelse(demographics[,"fat_edu"]=="2","Upto 4th grade",
                                         ifelse(demographics[,"fat_edu"]=="3","5-6th grade",
                                                ifelse(demographics[,"fat_edu"]=="4","7-8th grade",
                                                       ifelse(demographics[,"fat_edu"]=="5","9th grade",
                                                              ifelse(demographics[,"fat_edu"]=="6","10th grade",
                                                                     ifelse(demographics[,"fat_edu"]=="7","11th grade",
                                                                            ifelse(demographics[,"fat_edu"]=="8","12th grade, no diploma",
                                                                                   ifelse(demographics[,"fat_edu"]=="9","High school graduate/GED or equivalent",
                                                                                          ifelse(demographics[,"fat_edu"]=="10","Some college credit (less than 1 year)",
                                                                                                 ifelse(demographics[,"fat_edu"]=="11","Technical college/Vocational school",
                                                                                                        ifelse(demographics[,"fat_edu"]=="12","Associate's degree",
                                                                                                               ifelse(demographics[,"fat_edu"]=="13","Bachelor's degree",
                                                                                                                      ifelse(demographics[,"fat_edu"]=="14","Master's degree",
                                                                                                                             ifelse(demographics[,"fat_edu"]=="15","Professional degree",
                                                                                                                                    ifelse(demographics[,"fat_edu"]=="16","Doctorate degree",demographics[,"fat_edu"]))))))))))))))))

#checking orphaned data
demograph_orphaned_data = orphaned_data_consolidate(demographics)
demographics = orphaned_data_remove(demographics)
if(all(demograph_orphaned_data$id %in% app_cohort_subj$`subj id`==T)){
  rm(demograph_orphaned_data)
}

#REDCap data processing
demograph_t1 = exportRecords(ace_con,forms=c("participant_information","demographic"),events="t1_arm_1",labels=F,stringsAsFactors=F)
demograph_t1 = demograph_t1[grepl("Co",demograph_t1$demographic_complete)&!is.na(demograph_t1$demographic_timestamp),-(2:grep("participant_information_complete",names(demograph_t1)))]
demograph_t1$visit = 1
demograph_t3 = exportRecords(ace_con,forms=c("participant_information","demographic"),events="t3_arm_1",labels=F,stringsAsFactors=F)
demograph_t3 = demograph_t3[grepl("Co",demograph_t3$demographic_complete)&!is.na(demograph_t3$demographic_timestamp),-(2:grep("participant_information_complete",names(demograph_t3)))]
demograph_t3$visit = 3
demograph_t5 = exportRecords(ace_con,forms=c("participant_information","demographic"),events="t5_arm_1",labels=F,stringsAsFactors=F)
demograph_t5 = demograph_t5[grepl("Co",demograph_t5$demographic_complete)&!is.na(demograph_t5$demographic_timestamp),-(2:grep("participant_information_complete",names(demograph_t5)))]
demograph_t5$visit = 5
demograph_rc = rbind(demograph_t1,demograph_t3,demograph_t5)
demograph_rc = study_id_to_id(demograph_rc,"demo_")

raceth = c("Hispanic/Latino","Non-Hispanic/Latino","African-American","Asian","Pacific Islander or Native Hawaiian","White or Caucasian","American Indian","Other")
demograph_rc$ch_raceth = ''
demograph_rc = string_aggregation(demograph_rc,"^ch_raceth__",raceth,"ch_raceth")
demograph_rc$mth_raceth = ''
demograph_rc = string_aggregation(demograph_rc,"^mth_raceth__",raceth,"mth_raceth")
edu = c("None","Upto 4th grade","5-6th grade","7-8th grade","9th grade","10th grade","11th grade","12th grade, no diploma","High school graduate/GED or equivalent",
        "Some college (less than 1 year)","Some college (more than 1 year)","Technical college/Vocational school","Associate's degree","Bachelor's degree",
        "Master's degree","Professional degree","Doctorate degree","Other")
demograph_rc$mth_edu = ''
demograph_rc = string_aggregation(demograph_rc,"^mth_edu__",edu,"mth_edu")
demograph_rc$fat_raceth = ''
demograph_rc = string_aggregation(demograph_rc,"^fat_raceth__",raceth,"fat_raceth")
demograph_rc$fat_edu = ''
demograph_rc = string_aggregation(demograph_rc,"^fat_edu__",edu,"fat_edu")
demograph_rc$pcg_raceth = ''
demograph_rc = string_aggregation(demograph_rc,"^pcg_raceth__",raceth,"pcg_raceth")
demograph_rc$pcg_edu = ''
demograph_rc = string_aggregation(demograph_rc,"^pcg_edu__",edu,"pcg_edu")
demographics = rbind.fill(demographics,demograph_rc[,-grep("__",names(demograph_rc))])

#updating information while keeping the oldest ones
demograph_t1 = subset(demographics,visit==1)
demograph_t3 = subset(demographics,visit==3)
demograph_t4 = subset(demographics,visit==4)
demograph_t5 = subset(demographics,visit==5)
demograph_t1_t3 = filling_info(demograph_t1,demograph_t3)
demograph_t1_t4 = rbind(demograph_t1_t3,demograph_t4)
demographics = filling_info(demograph_t1_t4,demograph_t5)
if(!exists('demograph_t1_t4')){
  demographics = filling_info(demograph_t1_t3,demograph_t5)
}

#calculating parent's age at the time of child's birth
demographics = demographics[,1:grep("pcg_occ$",names(demographics))]
demographics = merge(demographics,parents[grep("FAT$",parents$parent_id),c("subj_id","dob")],by.x="id",by.y="subj_id",all.x=T)
demographics = merge(demographics,parents[grep("MTH$",parents$parent_id),c("subj_id","dob")],by.x="id",by.y="subj_id",all.x=T)
demographics = merge(demographics,subj[,c("subj_id","dob")],by.x="id",by.y="subj_id",all.x=T)
demographics = unique(demographics)
names(demographics)[grep("dob",names(demographics))] = c("fat_dob","mth_dob","ch_dob")
demographics$mth_age_when_childbirth = elapsed_months(demographics$ch_dob,demographics$mth_dob)
demographics$fat_age_when_childbirth = elapsed_months(demographics$ch_dob,demographics$fat_dob)

#inserting prefixes
demographics = inserting_prefix_into_variables(demographics,"demo_")

#archiving the table after checking the duplicate
if(length(which(table(demographics$id,demographics$visit)>1))==0){
  write.csv(demographics[,-c(2,grep("_dob$",names(demographics)))],"S:/MIND/RESEARCH/APP Behavior Data/ACE Data R processing/archived/demographics.csv",row.names = F)
}

##STAAR##
#querying the data
demograph_staar = sqlQuery(con4,"select * from Demographics",stringsAsFactors=F)

#id and visit into characters
demograph_staar = id_visit_changing_into_char(demograph_staar)

#removing single-entered rows
demograph_staar = subset(demograph_staar,entry_status==2)

#removing prefix for convenience
demograph_staar = removing_prefix(demograph_staar,"demo_")

#same variable name within the data
names(demograph_staar)[grep("^id$",names(demograph_staar))[2]] = "mental_impairment"

#calculate age
demograph_staar = fxage(demograph_staar,'id','date')

#getting rid of -9's
for(j in 6:ncol(demograph_staar)){
  for(i in 1:nrow(demograph_staar)){
    if(!is.na(demograph_staar[i,j])){
      if(demograph_staar[i,j]=="-9"){
        demograph_staar[i,j]=NA
      }
    }
  }
}

#getting rid of "#." in edu variables
for(i in 1:nrow(demograph_staar)){
  for(j in grep("_edu$",names(demograph_staar))){
    demograph_staar[i,j] = gsub("\\d{1,2}\\. ","",demograph_staar[i,j])
  }
}

#calculating parent's age at the time of child's birth
# demographics = merge(demographics,parents[grep("FAT$",parents$parent_id),c("subj_id","dob")],by.x="id",by.y="subj_id",all.x=T)
# demographics = merge(demographics,parents[grep("MTH$",parents$parent_id),c("subj_id","dob")],by.x="id",by.y="subj_id",all.x=T)
# demographics = merge(demographics,subj[,c("subj_id","dob")],by.x="id",by.y="subj_id",all.x=T)
# names(demographics)[grep("dob",names(demographics))] = c("fat_dob","mth_dob","ch_dob")
# demographics$mth_age_when_childbirth = elapsed_months(demographics$ch_dob,demographics$mth_dob)
# demographics$fat_age_when_childbirth = elapsed_months(demographics$ch_dob,demographics$fat_dob)

#inserting prefixes
demograph_staar = inserting_prefix_into_variables(demograph_staar,"demo_")

#checking orphaned/duplicate data
demograph_orphaned_data_staar = orphaned_data_consolidate(demograph_staar)
demograph_staar = orphaned_data_remove(demograph_staar)
demograph_duplicate_data = duplicate_data_consolidate(demograph_staar,'demo_age')
demograph_staar = duplicate_data_remove(demograph_staar,'demo_age')

#archiving
demograph_staar_processed = demograph_staar[,c(1:2,grep("^demo_age$",names(demograph_staar)),grep("^demo_grade",names(demograph_staar)):(ncol(demograph_staar)-1))]
demograph_staar_processed = demograph_staar_processed[,-grep("_job$",names(demograph_staar_processed))]
write.csv(demograph_staar_processed,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived_staar/demographics_processed.csv",row.names=F)

#cleaning up
rm(demograph_t1,demograph_t3,demograph_t4,demograph_t1_t3,demograph_t1_t4,demograph_t5,demograph_final,raceth,edu)
