ssp_t1 = exportRecords(ace_con,forms=c("participant_information","ssp_1"),events="t1_arm_1",labels=F,stringsAsFactors=F)
ssp_t1 = ssp_t1[grepl("Co",ssp_t1$ssp_1_complete)&!is.na(ssp_t1$ssp_1_timestamp),-(2:grep("participant_information_complete",names(ssp_t1)))]
ssp_t1$visit = 1
ssp_t3 = exportRecords(ace_con,forms=c("participant_information","ssp_1"),events="t3_arm_1",labels=F,stringsAsFactors=F)
ssp_t3 = ssp_t3[grepl("Co",ssp_t3$ssp_1_complete)&!is.na(ssp_t3$ssp_1_timestamp),-(2:grep("participant_information_complete",names(ssp_t3)))]
ssp_t3$visit = 3
ssp_t5 = exportRecords(ace_con,forms=c("participant_information","ssp_1"),events="t5_arm_1",labels=F,stringsAsFactors=F)
ssp_t5 = ssp_t5[grepl("Co",ssp_t5$ssp_1_complete)&!is.na(ssp_t5$ssp_1_timestamp),-(2:grep("participant_information_complete",names(ssp_t5)))]
ssp_t5$visit = 5
ssp_rc = rbind(ssp_t1,ssp_t3,ssp_t5)
ssp = sqlQuery(new_con,"select * from Short_Sensory_Profile;",stringsAsFactors=F)

#changing id and visit variables into characters
ssp = id_visit_changing_into_char(ssp)

#removing rows without double-entered
ssp_entry_flag = entry_flag(ssp,'ssp')
ssp = subset(ssp,entry_status==2)
if(!is.null(ssp_entry_flag)){
  ssp = rbind(ssp,ssp_entry_flag[,-ncol(ssp_entry_flag)])
}

#removing preexisting prefix for convenience
ssp = removing_prefix(ssp,"ssp1_")
ssp_rc = study_id_to_id(ssp_rc,"ssp1_")
names(ssp_rc)[grep("^q",names(ssp_rc))]=names(ssp)[c(7:15,17,19:ncol(ssp))]
ssp = identify_same_data(ssp_rc,ssp)
ssp = rbind.fill(ssp,ssp_rc)

#calculating age
ssp = fxage(ssp,'id',"date")

#converting categorical responses into numeric scale
for(j in which(names(ssp)=="discomfortingrooming"):which(names(ssp)=="coverseyes")){
  for(i in 1:nrow(ssp)){
    ssp[i,j] = ifelse(is.na(ssp[i,j]),NA,ifelse(grepl("^Always$",ssp[i,j]),1,
                                                ifelse(grepl("^Frequently$",ssp[i,j]),2,
                                                       ifelse(grepl("^Occasionally$",ssp[i,j]),3,
                                                              ifelse(grepl("^Seldom$",ssp[i,j]),4,
                                                                     ifelse(grepl("^Never$",ssp[i,j]),5,ssp[i,j]))))))
  }
  if(names(ssp)[j] != "tastes_list" & names(ssp)[j] != "textures_list"){
    ssp[,j] = as.numeric(ssp[,j])
  }
}

#obtaining subscale scores
ts_items = which(names(ssp)=="discomfortingrooming"):which(names(ssp)=="rubspsot")
tss_items = c(which(names(ssp)=="avoidstastes"):grep("certaintastes",names(ssp)),grep("foodtextures",names(ssp)),grep("pickyeater",names(ssp)))
ms_items = which(names(ssp)=="anxiousfeetoffground"):which(names(ssp)=="upsidedown")
uss_items = which(names(ssp)=="strangenoises"):which(names(ssp)=="twistedclothing")
af_items = which(names(ssp)=="distractedbynoise"):which(names(ssp)=="difficultypayingattention")
lew_items = which(names(ssp)=="weakmuscles"):which(names(ssp)=="poorendurance")
vas_items = which(names(ssp)=="negativeresponsetonoise"):which(names(ssp)=="coverseyes")
total = c(ts_items,tss_items,ms_items,uss_items,af_items,lew_items,vas_items)

#missing data analysis
ssp = count_missing_items(ssp,'discomfortingrooming','coverseyes')
ssp = comment_missing_data(ssp,list(ts_items,tss_items,ms_items,uss_items,af_items,lew_items,vas_items),
                           list('tactile_sensitivity','tss','movement_sensitivity',
                                'underresponsive/seeking_sensation','auditory_filtering','low_energy/weak',
                                'visual/auditory_sensitivity'))
  
#summing items
ssp = summing_items_per_row(ssp,list(ts_items,tss_items,ms_items,uss_items,
                                     af_items,lew_items,vas_items,total),
                            list('ts','tss','ms','uss','af','lew','vas','total'),F)
ssp[which(ssp$missing_items_count==38),grep("^ts$",names(ssp)):grep("^total$",names(ssp))]=NA

#putting the scores into different category
for(i in 1:nrow(ssp)){
  ssp$ts_summary[i] = ifelse(ssp[i,"ts"]>=30,"Typical Performance",
                             ifelse(ssp[i,"ts"]<30 & ssp[i,"ts"]>=27,"Probable Difference",
                                    ifelse(ssp[i,"ts"]<27 & ssp[i,"ts"]>=7,"Definite Difference",NA)))
  ssp$tss_summary[i] = ifelse(ssp[i,"tss"]>=15,"Typical Performance",
                             ifelse(ssp[i,"tss"]<15 & ssp[i,"tss"]>=12,"Probable Difference",
                                    ifelse(ssp[i,"tss"]<12 & ssp[i,"tss"]>=4,"Definite Difference",NA)))
  ssp$ms_summary[i] = ifelse(ssp[i,"ms"]>=13,"Typical Performance",
                             ifelse(ssp[i,"ms"]<13 & ssp[i,"ms"]>=11,"Probable Difference",
                                    ifelse(ssp[i,"ms"]<11 & ssp[i,"ms"]>=3,"Definite Difference",NA)))
  ssp$uss_summary[i] = ifelse(ssp[i,"uss"]>=27,"Typical Performance",
                             ifelse(ssp[i,"uss"]<27 & ssp[i,"uss"]>=24,"Probable Difference",
                                    ifelse(ssp[i,"uss"]<24 & ssp[i,"uss"]>=7,"Definite Difference",NA)))
  ssp$af_summary[i] = ifelse(ssp[i,"af"]>=23,"Typical Performance",
                             ifelse(ssp[i,"af"]<23 & ssp[i,"af"]>=20,"Probable Difference",
                                    ifelse(ssp[i,"af"]<20 & ssp[i,"af"]>=6,"Definite Difference",NA)))
  ssp$lew_summary[i] = ifelse(ssp[i,"lew"]>=26,"Typical Performance",
                             ifelse(ssp[i,"lew"]<26 & ssp[i,"lew"]>=24,"Probable Difference",
                                    ifelse(ssp[i,"lew"]<24 & ssp[i,"lew"]>=6,"Definite Difference",NA)))
  ssp$vas_summary[i] = ifelse(ssp[i,"vas"]>=19,"Typical Performance",
                             ifelse(ssp[i,"vas"]<19 & ssp[i,"vas"]>=16,"Probable Difference",
                                    ifelse(ssp[i,"vas"]<16 & ssp[i,"vas"]>=5,"Definite Difference",NA)))
  ssp$total_summary[i] = ifelse(ssp[i,"total"]>=155,"Typical Performance",
                             ifelse(ssp[i,"total"]<155 & ssp[i,"total"]>=142,"Probable Difference",
                                    ifelse(ssp[i,"total"]<142 & ssp[i,"total"]>=38,"Definite Difference",NA)))
}

#inserting prefixes
ssp = inserting_prefix_into_variables(ssp,"ssp_")

#orphaned/duplicate data
ssp_orphaned_data = orphaned_data_consolidate(ssp)
ssp = orphaned_data_remove(ssp)
ssp_duplicate_data = duplicate_data_consolidate(ssp,"ssp_age")
ssp = duplicate_data_remove(ssp,"ssp_age")

#outliers
ssp_outliers = ssp[,c(1:2,grep("_age$",names(ssp)),which(names(ssp)=="ssp_ts"):which(names(ssp)=="ssp_total"))]
ssp_outliers = outlier_list(ssp_outliers)
ssp$ssp_outlier_list = ssp_outliers$outlier_list
rm(ssp_outliers)
#ssp_outlier_table = sqldf("select id,visit,outlier_list from ssp where outlier_list !=''")

#extracting necessary columns
ssp_scored = ssp[,c(1:2,which(names(ssp)=="ssp_age"):ncol(ssp))]
#-which(ssp$ssp_missing_items_comment=="All data are missing")

#archiving the data
write.csv(ssp_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/ssp_scored.csv",row.names=F)
write.csv(ssp[,c(1:2,7:46)],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/ssp_items.csv",row.names=F)

#cleaning up
rm(ts_items,tss_items,ms_items,uss_items,af_items,lew_items,vas_items,total,
   ssp_t1,ssp_t3,ssp_t5,ssp_rc)
