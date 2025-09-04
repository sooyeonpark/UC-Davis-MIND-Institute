#pulling the data table
cshq_t1 = exportRecords(ace_con,forms=c("participant_information","cshq"),events="t1_arm_1",labels=F,stringsAsFactors=F)
cshq_t1 = cshq_t1[grepl("Co",cshq_t1$cshq_complete)&!is.na(cshq_t1$cshq_timestamp),-(2:grep("participant_information_complete",names(cshq_t1)))]
cshq_t1$visit = 1
cshq_t3 = exportRecords(ace_con,forms=c("participant_information","cshq"),events="t3_arm_1",labels=F,stringsAsFactors=F)
cshq_t3 = cshq_t3[grepl("Co",cshq_t3$cshq_complete)&!is.na(cshq_t3$cshq_timestamp),-(2:grep("participant_information_complete",names(cshq_t3)))]
cshq_t3$visit = 3
cshq_rc = rbind(cshq_t1,cshq_t3)
cshq = sqlQuery(new_con,"select * from CSHQ;",stringsAsFactors=F)

#taking only double-entered data
cshq = id_visit_changing_into_char(cshq)
cshq_entry_flag = entry_flag(cshq,'cshq')
cshq = subset(cshq,entry_status==2)
if(!is.null(cshq_entry_flag)){
  cshq = rbind(cshq,cshq_entry_flag[,-ncol(cshq_entry_flag)])
}

#removing prefixes for now
cshq = removing_prefix(cshq,"SLEEP_")
cshq_rc = study_id_to_id(cshq_rc,"cshq_")
cshq = identify_same_data(cshq_rc,cshq)
cshq = rbind.fill(cshq,cshq_rc)

#calculating age
cshq = fxage(cshq,'id','date')

#set SNORTS scores to sometimes
cshq$SNORTS = "Sometimes"

# For age<5,  manually set Wets Bed (item 20) to Sometimes
for(i in 1:nrow(cshq)){
  cshq$`20`[i] = ifelse(cshq$`20`[i] == "Rarely" | cshq$`20`[i] == "Missing", ifelse(cshq$age[i] < 60,"Sometimes",cshq$`20`[i]), cshq$`20`[i])
  cshq$`52`[i] = ifelse(cshq$`52`[i] == "" | is.na(cshq$`52`[i]),"Rarely",cshq$`52`[i])
  cshq$`53`[i] = ifelse(cshq$`53`[i] == "" | is.na(cshq$`53`[i]),"Rarely",cshq$`53`[i])
}

#subdomain item lists
bedtime_resist = paste0('',c(1,3,4,7,10,12))
sleep_onset = paste0('',2)
sleep_duration = paste0('',c(16,18:19))
sleep_anxiety = paste0('',c(7,11:12,29))
night_waking = paste0('',c(24,34:35))
parasomnia = paste0('',c(20:23,26,31:32))
breathing = paste0('',c(27:28,'SNORTS'))
daytime_sleepiness = paste0('',c(40,42:45,48,52:53))
total = c(bedtime_resist,sleep_anxiety,sleep_duration,sleep_onset,parasomnia,breathing,daytime_sleepiness)
bh = paste0('',c(1:2,16,19,22,35,48))
reverse_items = paste0('',c(1:3,18:19,40))

#assigning numeric scores to categorical values
for(j in c(unique(total),night_waking)){
  for(i in 1:nrow(cshq)){
    cshq[i,j] = ifelse(cshq[i,j]=="Usually"|cshq[i,j]=="Falls Asleep"|cshq[i,j]==3,3,
                       ifelse(cshq[i,j]=="Sometimes"|cshq[i,j]=="Very Sleepy"|cshq[i,j]==2,2,
                              ifelse(cshq[i,j]=="Rarely"|cshq[i,j]=="Missing"|cshq[i,j]==1,1,NA)))
  }
  cshq[,j] = as.numeric(cshq[,j])
}

#dealing with reverse items
for(j in reverse_items){
  cshq[,j] = 4 - cshq[,j]
}

#dealing with item 52 and 53
cshq$`52` = cshq$`52`-1
cshq$`53` = cshq$`53`-1

#missing data analysis
cshq = count_missing_items(cshq,'1','36')
cshq = count_missing_items(cshq,'40','SNORTS')
cshq = comment_missing_data(cshq,list(bedtime_resist,sleep_onset,sleep_duration,sleep_anxiety,night_waking,
                                      parasomnia,breathing,daytime_sleepiness,bh),
                            list('bedtime_resist','sleep_onset','sleep_duration','sleep_anxiety','night_waking',
                                 'parasomnia','breathing','daytime_sleepiness','burt_hatch'))

#summing up scores
cshq = summing_items_per_row(cshq,list(bedtime_resist,sleep_onset,sleep_duration,sleep_anxiety,night_waking,
                                       parasomnia,breathing,daytime_sleepiness,bh,unique(total)),
                             list('bedtime_resist','sleep_onset','sleep_duration','sleep_anxiety','night_waking',
                                  'parasomnia','breathing','daytime_sleepiness','burt_hatch_sum','total'),F)

#inserting prefix to variables
cshq = inserting_prefix_into_variables(cshq,"cshq_")

#orphaned/duplicate data
cshq_orphaned_data = orphaned_data_consolidate(cshq)
cshq = orphaned_data_remove(cshq)
cshq_duplicate_data = duplicate_data_consolidate(cshq,"cshq_age")
cshq = duplicate_data_remove(cshq,"cshq_age")

#outliers
cshq_outliers = cshq[,c(1:2,grep("_age$",names(cshq)),76:ncol(cshq))]
cshq_outliers = outlier_list(cshq_outliers)
cshq$cshq_outlier_list = cshq_outliers$outlier_list
#cshq_outlier_table = sqldf("select id,visit,outlier_list from cshq where outlier_list != ''")

#extracting necessary columns & obtaining outliers
cshq_scored = cshq[,c(1:2,grep("_age$",names(cshq)):ncol(cshq))]

#archiving data
write.csv(cshq_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/cshq_scored.csv",row.names = F)
write.csv(cshq[,c(1:2,5,7:72)],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/cshq_items.csv",row.names = F)

#clean up
rm(bedtime_resist,sleep_anxiety,sleep_duration,sleep_onset,night_waking,parasomnia,cshq_outliers,
   breathing,daytime_sleepiness,total,reverse_items,bh,cshq_rc,cshq_t1,cshq_t3)
