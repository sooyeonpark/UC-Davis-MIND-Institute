# sti_t5 = exportRecords(ace_con,forms=c("participant_information","sti_t4"),events="t5_arm_1",labels=F,stringsAsFactors=F)
# sti_t5 = sti_t5[grepl("Co",sti_t5$sti_1_complete)&!is.na(sti_t5$sti_1_timestamp),-(2:grep("participant_information_complete",names(sti_t5)))]
# sti_t5$visit = 5
sti = sqlQuery(new_con, "select * from STI;",stringsAsFactors=F)

#id and visit into characters, removing single-entered rows and prefix
sti = id_visit_changing_into_char(sti)
sti_entry_flag = entry_flag(sti,'sti')
sti = subset(sti,entry_status==2)
if(!is.null(sti_entry_flag)){
  sti = rbind(sti,sti_entry_flag[,-ncol(sti_entry_flag)])
}
sti = removing_prefix(sti,"sti_")

#calculating age
sti = fxage(sti,'id','date')

#change end date where it is NA's
for(i in 1:nrow(sti)){
  for(j in grep("end_",names(sti))){
    if(is.na(sti[i,j]) & !is.na(sti[i,(j-1)])){
      sti[i,j] = sti[i,"date"]
    }
  }
}

therapy_slots = length(grep("type", names(sti)))

for(i in 1:therapy_slots) {
  #i<-1
  TherapyID<-as.character(i)
  
  TherapyData<-data.frame(sti$id)
  TherapyData$visit<-sti$visit
  
  # Pull this therapy's columns from STI Scored
  TherapyData$adults<-sti[,which(names(sti)==paste0("adults_",TherapyID))]
  TherapyData$grp<-sti[,which(names(sti)==paste0("grp_",TherapyID))]
  TherapyData$chldrn<-sti[,which(names(sti)==paste0("chldrn_",TherapyID))]
  TherapyData$brks<-sti[,which(names(sti)==paste0("brks_",TherapyID))]
  TherapyData$hrs<-sti[,which(names(sti)==paste0("hrs_",TherapyID))]
  TherapyData$end<-sti[,which(names(sti)==paste0("end_",TherapyID))]
  TherapyData$begin<-sti[,which(names(sti)==paste0("begin_",TherapyID))]
  
  # TherapyData$begin_str<-as.character(TherapyData$begin)
  # TherapyData$end_str<-as.character(TherapyData$end)
  
  #   Local Computations  
  #        Correct missing adult / kid counts for individual therapy
  TherapyData$adults_corrected<-ifelse(is.na(TherapyData$adults) & TherapyData$grp=="Individual", 1, TherapyData$adults)
  TherapyData$chldrn_corrected<-ifelse(is.na(TherapyData$chldrn) & TherapyData$grp=="Individual", 1, TherapyData$chldrn)
  
  #         Correct NA weeks on break, assume 0
  TherapyData$WeeksOnBreak=ifelse(is.na(TherapyData$brks),0,TherapyData$brks)
  
  #         Conver chr to Dates
  # TherapyData$end_assumed_asDate<-as.Date(TherapyData$end_assumed_asChar,"%Y-%m-%d")
  # TherapyData$begin_asDate<-as.Date(TherapyData$begin_str, "%Y-%m-%d")  
  
  #         Local Computations 
  TherapyData$DateDifference_asWeeks<-as.numeric(difftime(TherapyData$end,TherapyData$begin, units = "weeks"))
  TherapyData$WeeksNotOnBreak=TherapyData$DateDifference_asWeeks-TherapyData$WeeksOnBreak  
  TherapyData$TotalHours=TherapyData$WeeksNotOnBreak * TherapyData$hrs
  TherapyData$AdultToKidsRatio<-TherapyData$adults_corrected / TherapyData$chldrn_corrected
  TherapyData$class_ratio=ifelse(TherapyData$AdultToKidsRatio>1,1,TherapyData$AdultToKidsRatio)
  TherapyData$intensity<-TherapyData$TotalHours * TherapyData$class_ratio
  
  #        Store results back into in sti
  
  sti[paste0("DateDifference_asWeeks_",TherapyID)]<-TherapyData$DateDifference_asWeeks
  sti[paste0("WeeksNotOnBreak_",TherapyID)]<-TherapyData$WeeksNotOnBreak
  sti[paste0("TotalHours_",TherapyID)]<-TherapyData$TotalHours
  sti[paste0("class_ratio_",TherapyID)]<-TherapyData$class_ratio
  #sti[paste0("AdultToKidsRatio_MaxesAtOne_",TherapyID)]<-TherapyData$AdultToKidsRatio_MaxesAtOne
  sti[paste0("intensity_",TherapyID)]<-TherapyData$intensity
  rm(TherapyData)
}

for(i in 1:nrow(sti)){
  sti$hours_total[i] = ifelse((all(is.na(sti[i,grep("^begin_[0-9]{1,2}$",names(sti))]))&all(is.na(sti[i,grep("^end_[0-9]{1,2}$",names(sti))])))
                              |all(is.na(sti[i,grep("^hrs_[0-9]{1,2}",names(sti))])),0,round(sum(unlist(sti[i,grep("^TotalHours_",names(sti))]),na.rm = T),2))
  sti$intensity_total[i] = ifelse(sti$hours_total[i]==0,0,round(sum(unlist(sti[i,grep("^intensity_",names(sti))]),na.rm = T),2))
}

#adding prefixes
sti = inserting_prefix_into_variables(sti,"sti_")

#orphaned/duplicate data
sti_orphaned_data = orphaned_data_consolidate(sti)
sti = orphaned_data_remove(sti)
sti_duplicate_data = duplicate_data_consolidate(sti,"sti_age")
sti = duplicate_data_remove(sti,"sti_age")

#extracting necessary columns and finding outliers
sti_scored = sti[,c(1:2,which(names(sti)=="sti_age"),
                    which(names(sti)=="sti_hours_total"):ncol(sti))]
sti_scored = outlier_list(sti_scored)
names(sti_scored)[ncol(sti_scored)]="sti_outlier_list"
# sti$outlier_list = sti_scored$outlier_list
# sti_scored$outlier_list = NULL
# sti_outlier_table = sqldf("select id,visit,outlier_list from sti where outlier_list != ''")

#archiving the table
write.csv(sti_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/sti_scored.csv",row.names=F)

#cleaning up
rm(therapy_slots,TherapyID)
