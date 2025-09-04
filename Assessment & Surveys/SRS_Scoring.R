#pulling the data
srs_t1 = exportRecords(ace_con,forms=c("participant_information","srs_preschool"),events="t1_arm_1",labels=F,stringsAsFactors=F)
srs_t1 = srs_t1[grepl("Co",srs_t1$srs_preschool_complete)&!is.na(srs_t1$srs_preschool_timestamp),-(2:grep("participant_information_complete",names(srs_t1)))]
srs_t1$visit = 1
names(srs_t1) = gsub("_pre","",names(srs_t1))
names(srs_t1) = gsub("school","",names(srs_t1))
srs_t3 = exportRecords(ace_con,forms=c("participant_information","srs_school_age"),events="t3_arm_1",labels=F,stringsAsFactors=F)
srs_t3 = srs_t3[grepl("Co",srs_t3$srs_school_age_complete)&!is.na(srs_t3$srs_school_age_timestamp),-(2:grep("participant_information_complete",names(srs_t3)))]
srs_t3$visit = 3
names(srs_t3) = gsub("_school","",names(srs_t3))
names(srs_t3) = gsub("_age","",names(srs_t3))
srs_t5 = exportRecords(ace_con,forms=c("participant_information","srs_school_age"),events="t5_arm_1",labels=F,stringsAsFactors=F)
srs_t5 = srs_t5[grepl("Co",srs_t5$srs_school_age_complete)&!is.na(srs_t5$srs_school_age_timestamp),-(2:grep("participant_information_complete",names(srs_t5)))]
srs_t5$visit = 5
names(srs_t5) = gsub("_school","",names(srs_t5))
names(srs_t5) = gsub("_age","",names(srs_t5))
srs_adult_t5 = exportRecords(ace_con,forms=c("participant_information","srs_adult"),events="t5_arm_1",labels=F,stringsAsFactors=F)
srs_adult_t5 = srs_adult_t5[grepl("Co",srs_adult_t5$srs_adult_complete)&!is.na(srs_adult_t5$srs_adult_timestamp),-(2:grep("participant_information_complete",names(srs_adult_t5)))]
srs_adult_t5$visit = 5
srs_rc = rbind.fill(srs_t1,srs_t3,srs_t5)
srs = sqlQuery(new_con,"select * from `Social Responsiveness Scale-Child`;",stringsAsFactors=F)
srs_adult = sqlQuery(new_con,"select * from `Social Reciprocity Scale-Adult`;",stringsAsFactors=F)
srs_adt = sqlQuery(new_con,"select * from SRSAdultBH",stringsAsFactors=F)
srs_staar = sqlQuery(con4,"select * from `SRS-2`;",stringsAsFactors=F)

#combining tables
srs_staar = removing_prefix(srs_staar,"srs2_")
srs = removing_prefix(srs,"src_")
srs = rbind.fill(srs,srs_staar)
rm(srs_staar)

#id and visit into characters, removing single-entered rows and prefix
srs = id_visit_changing_into_char(srs)
srs_entry_flag = entry_flag(srs,'srs_child')
srs = subset(srs,entry_status ==2)
if(!is.null(srs_entry_flag)){
  srs = rbind(srs,srs_entry_flag[,-ncol(srs_entry_flag)])
}
srs_rc = study_id_to_id(srs_rc,"srs_")
srs = identify_same_data(srs_rc,srs)
srs = rbind.fill(srs,srs_rc)

#combining two adult tables
srs_adult = id_visit_changing_into_char(srs_adult)
srs_adult = removing_prefix(srs_adult,"sra_")
srs_adt = removing_prefix(srs_adt,"srsadultbh_")
names(srs_adt)[7] = 'date'
names(srs_adt)[5] = 'resp'
srs_adt$birth_date = NULL
srs_adult = rbind.fill(srs_adt,srs_adult)
rm(srs_adt)

srs_adult_entry_flag = entry_flag(srs_adult,'srs_adult')
srs_adult = subset(srs_adult,entry_status==2)
srs_adult$id = gsub("fat","FAT",srs_adult$id)
srs_adult_t5 = study_id_to_id(srs_adult_t5,"srs_")
srs_adult = identify_same_data(srs_adult_t5,srs_adult)
srs_adult = rbind.fill(srs_adult,srs_adult_t5)

#pulling out columns to begin scoring
srs = srs[,c(1:2,15,which(names(srs)=="1"):which(names(srs)=="65"))]

#getting norm tables
srs_pre_dsm = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/SRS T Scores_Parent Rating_2 to 4_dsm 5 subscales.csv",stringsAsFactors = F)
srs_pre_treat = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/SRS T Scores_Parent Rating_2 to 4_treatment scale.csv",stringsAsFactors = F)
srs_pre_total = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/SRS T Scores_Parent Rating_2 to 4_total score.csv",stringsAsFactors = F)

srs_sch_dsm = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/SRS T Scores_Parent Rating_4 to 18_dsm 5 subscales.csv",stringsAsFactors = F)
srs_sch_treat = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/SRS T Scores_Parent Rating_4 to 18_treatment scale.csv",stringsAsFactors = F)
srs_sch_total = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/SRS T Scores_Parent Rating_4 to 18_total score.csv",stringsAsFactors = F)

srs_adult_dsm = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/SRS T Scores_Adult_dsm 5 subscales.csv",stringsAsFactors = F)
srs_adult_treat = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/SRS T Scores_Adult_treatment scale.csv",stringsAsFactors = F)
srs_adult_total = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/SRS T Scores_Adult_total score.csv",stringsAsFactors = F)

#fix one missing row in dsm table
srs_sch_dsm = rbind(srs_sch_dsm,c("Male",83,77,NA,77,NA))
srs_sch_dsm = unique(srs_sch_dsm)

#considering gender for school-aged kids
srs_sch_dsm$gender = ifelse(srs_sch_dsm$gender=='male','Male','Female')
srs_sch_treat$gender = ifelse(srs_sch_treat$gender=='male','Male','Female')
srs_sch_total$gender = ifelse(srs_sch_total$gender=='male','Male','Female')

#obtaining gender for subjects in srs table
srs = sqldf("select t1.*, gender from srs as t1 left join subj on t1.id = subj.subj_id")

#calculating age
srs = fxage(srs,'id','date')

#determine which version of child SRS was given based on age of 54 months as cutoff
srs$version <- ifelse(srs$age >= 54, "School-Age","Preschool")

#orphaned data
srs_orphaned_data = orphaned_data_consolidate(srs)
srs = orphaned_data_remove(srs)

#reduce scale from 1-4 to 0-3
for(i in which(names(srs)=='1'):which(names(srs)=='65')){
  srs[,i] <- ifelse(srs[,i]==4,3,ifelse(srs[,i]==3,2,ifelse(srs[,i]==2,1,ifelse(srs[,i]==1,0,NA))))
}

#dealing with reverse items
srs_reverse_items = paste0('',c(3,7,11:12,15,17,21:22,26,32,38,40,43,45,48,52,55))
srs[,srs_reverse_items] = 3 - srs[,srs_reverse_items]

#counting original total missing items
srs = count_missing_items(srs,'1','65')

##Scoring Preschool
srs_pre = srs[which(srs$version=="Preschool"),]

#dealing with missing items to 1 or 2
child_pre_missing_items_1 = paste0('',c(3,5,7,9,11:12,15,17,19,21:22,25:26,28,31,38,40,43,45,48,56))
child_pre_missing_items_2 = paste0('',c(52,55))
for(j in c(child_pre_missing_items_1,child_pre_missing_items_2)){
  for(i in 1:nrow(srs_pre)){
    srs_pre[i,child_pre_missing_items_1] = ifelse(is.na(srs_pre[i,child_pre_missing_items_1]),
                                              1,srs_pre[i,child_pre_missing_items_1])
    srs_pre[i,child_pre_missing_items_2] = ifelse(is.na(srs_pre[i,child_pre_missing_items_2]),
                                              2,srs_pre[i,child_pre_missing_items_2])
  }
}

#list of items
awareness_items = paste0('',c(2,7,25,32,45,52,54,56))
cognition_items = paste0('',c(5,10,15,17,30,40,42,44,48,58,59,62))
communication_items = paste0('',c(12,13,16,18,19,21,22,26,33,35,36,37,38,41,46,47,51,53,55,57,60,61))
motivation_items = paste0('',c(1,3,6,9,11,23,27,34,43,64,65))
rrb_items = paste0('',c(4,8,14,20,24,28,29,31,39,49,50,63))
sci_items = c(awareness_items,cognition_items,communication_items,motivation_items)
total_items = c(sci_items,rrb_items)

#commenting missing data
srs_pre = comment_missing_data(srs_pre,list(awareness_items,cognition_items,communication_items,
                                            motivation_items,rrb_items,sci_items),
                               list('awareness','cognition','communication','motivation',
                                    'restricted_inerests/repetitive_behavior','social_communication&interaction'))

srs_pre = summing_items_per_row(srs_pre,list(awareness_items,cognition_items,communication_items,
                                             motivation_items,rrb_items,sci_items,total_items),
                                list('awr_raw','cog_raw','com_raw','mot_raw','rrb_raw','sci_raw','total_raw'),F)

#calculating t scores
for(i in 1:nrow(srs_pre)){
  srs_pre$awr_t[i] = ifelse(is.na(srs_pre$awr_raw[i]),NA,srs_pre_treat[which(srs_pre$awr_raw[i]==srs_pre_treat$raw.score),"srs_awr2"])
  srs_pre$cog_t[i] = ifelse(is.na(srs_pre$cog_raw[i]),NA,srs_pre_treat[which(srs_pre$cog_raw[i]==srs_pre_treat$raw.score),"srs_cog2"])
  srs_pre$com_t[i] = ifelse(is.na(srs_pre$com_raw[i]),NA,srs_pre_treat[which(srs_pre$com_raw[i]==srs_pre_treat$raw.score),"srs_com2"])
  srs_pre$mot_t[i] = ifelse(is.na(srs_pre$mot_raw[i]),NA,srs_pre_treat[which(srs_pre$mot_raw[i]==srs_pre_treat$raw.score),"srs_mot2"])
  srs_pre$rrb_t[i] = ifelse(is.na(srs_pre$rrb_raw[i]),NA,srs_pre_treat[which(srs_pre$rrb_raw[i]==srs_pre_treat$raw.score),"srs_rrb2"])
  srs_pre$sci_t[i] = ifelse(is.na(srs_pre$sci_raw[i]),NA,srs_pre_dsm[which(srs_pre$sci_raw[i]==srs_pre_dsm$raw.score),"srs_sci2"])
  srs_pre$total_t[i] = ifelse(is.na(srs_pre$total_raw[i]),NA,srs_pre_total[which(srs_pre$total_raw[i]==srs_pre_total$raw.score),"total.score2"])
}

#obtaining outliers
srs_pre = inserting_prefix_into_variables(srs_pre,"srs_")
srs_pre_outliers = srs_pre[,c(1:2,grep("_age$",names(srs_pre)),grep("_t$",names(srs_pre)))]
srs_pre_outliers = outlier_list(srs_pre_outliers)
srs_pre$srs_outlier_list = srs_pre_outliers$outlier_list
srs_pre_scored = srs_pre[,c(1:2,grep("_age$",names(srs_pre)),grep("missing",names(srs_pre)),grep("awr_raw",names(srs_pre)):ncol(srs_pre),grep("version$",names(srs_pre)))]

##Scoring School-aged
srs_sch = srs[which(srs$version=="School-Age"),]

#dealing with missing items to 1
child_sch_missing_items = paste0('',c(3,7,11:12,25:26,38,45,52,55))
for(j in child_sch_missing_items){
  for(i in 1:nrow(srs_sch)){
    srs_sch[i,j] = ifelse(is.na(srs_sch[i,j]),1,srs_sch[i,j])
  }
}

#commenting missing data
srs_sch = comment_missing_data(srs_sch,list(awareness_items,cognition_items,communication_items,
                                            motivation_items,rrb_items,sci_items),
                               list('awareness','cognition','communication','motivation',
                                    'restricted_inerests/repetitive_behavior','social_communication&interaction'))

#calcualting total raw scores
srs_sch = summing_items_per_row(srs_sch,list(awareness_items,cognition_items,communication_items,motivation_items,
                                             rrb_items,sci_items,total_items),
                                list('awr_raw','cog_raw','com_raw','mot_raw','rrb_raw','sci_raw','total_raw'),F)

#calculating t scores
srs_sch_dsm = unique(srs_sch_dsm)
srs_sch_dsm = rbind(srs_sch_dsm,c("Male",83,77,NA,77,NA))
for(i in 1:nrow(srs_sch)){
  srs_sch$awr_t[i] = ifelse(is.na(srs_sch$awr_raw[i]),NA,srs_sch_treat[which(srs_sch$awr_raw[i]==srs_sch_treat$raw.score
                                         & srs_sch$gender[i]==srs_sch_treat$gender),"srs_awr2"])
  srs_sch$cog_t[i] = ifelse(is.na(srs_sch$cog_raw[i]),NA,srs_sch_treat[which(srs_sch$cog_raw[i]==srs_sch_treat$raw.score
                                         & srs_sch$gender[i]==srs_sch_treat$gender),"srs_cog2"])
  srs_sch$com_t[i] = ifelse(is.na(srs_sch$com_raw[i]),NA,srs_sch_treat[which(srs_sch$com_raw[i]==srs_sch_treat$raw.score
                                         & srs_sch$gender[i]==srs_sch_treat$gender),"srs_com2"])
  srs_sch$mot_t[i] = ifelse(is.na(srs_sch$mot_raw[i]),NA,srs_sch_treat[which(srs_sch$mot_raw[i]==srs_sch_treat$raw.score
                                         & srs_sch$gender[i]==srs_sch_treat$gender),"srs_mot2"])
  srs_sch$rrb_t[i] = ifelse(is.na(srs_sch$rrb_raw[i]),NA,srs_sch_treat[which(srs_sch$rrb_raw[i]==srs_sch_treat$raw.score
                                         & srs_sch$gender[i]==srs_sch_treat$gender),"srs_rrb2"])
  srs_sch$sci_t[i] = ifelse(is.na(srs_sch$sci_raw[i]),NA,srs_sch_dsm[which(srs_sch$sci_raw[i]==srs_sch_dsm$raw.score
                                       & srs_sch$gender[i]==srs_sch_dsm$gender),"srs_sci2"])
  srs_sch$total_t[i] = ifelse(is.na(srs_sch$total_raw[i]),NA,srs_sch_total[which(srs_sch$total_raw[i]==srs_sch_total$raw.score
                                           & srs_sch$gender[i]==srs_sch_total$gender),"total.score2"])
}

#adding back prefixes and extracting rows by the study
srs_sch = inserting_prefix_into_variables(srs_sch,"srs_")

#obtaining outliers
srs_sch_outliers = srs_sch[grep("[0-9]{6}-[0-9]{3}",srs_sch$id),c(1:2,grep("_age$",names(srs_sch)),grep("_t$",names(srs_sch)))]
srs_outliers_staar = srs_sch[grep("^[0-9]{4}$",srs_sch$id),c(1:2,grep("_age$",names(srs_sch)),grep("_t$",names(srs_sch)))]
srs_sch_outliers = outlier_list(srs_sch_outliers)
srs_outliers_staar = outlier_list(srs_outliers_staar)
srs_sch_outliers = rbind(srs_sch_outliers,srs_outliers_staar)
srs_sch = sqldf("select t1.*,t2.outlier_list as srs_outlier_list from srs_sch t1
                left join srs_sch_outliers t2 on t1.id=t2.id and t1.visit=t2.visit")
# srs_outlier_table = sqldf("select id,visit,outlier_list from srs_pre_outliers where outlier_list != ''
#                           union select id,visit,outlier_list from srs_sch_outliers where outlier_list != ''")
# srs_outlier_table_staar = sqldf("select id,visit,outlier_list from srs_outliers_staar where outlier_list != ''")
rm(srs_sch_outliers,srs_outliers_staar,srs_pre_outliers)

#orphaned/duplicate data
srs_sch_scored = srs_sch[grep("[0-9]{6}-[0-9]{3}",srs_sch$id),c(1:2,grep("_age$",names(srs_sch)),grep("missing",names(srs_pre)),grep("awr_raw",names(srs_sch)):ncol(srs_sch),grep("version$",names(srs_sch)))]
srs_scored_staar = srs_sch[grep("^[0-9]{4}$",srs_sch$id),c(1:2,grep("_age$",names(srs_sch)),grep("missing",names(srs_pre)),grep("awr_raw",names(srs_sch)):ncol(srs_sch),grep("version$",names(srs_sch)))]
srs_scored = rbind(srs_pre_scored,srs_sch_scored)
srs_duplicate_data = duplicate_data_consolidate(srs_scored,"srs_age")
srs_scored = duplicate_data_remove(srs_scored,"srs_age")

##Scoring Adults

#importing child id
srs_adult = sqldf("select t1.*,t2.subj_id from srs_adult t1 left join parents t2 on t1.id=t2.parent_id")

#converting individual items into scores
for(i in which(names(srs_adult)=='1'):which(names(srs_adult)=='65')){
  srs_adult[,i] <- ifelse(srs_adult[,i]==4,3,ifelse(srs_adult[,i]==3,2,ifelse(srs_adult[,i]==2,1,ifelse(srs_adult[,i]==1,0,NA))))
}

#dealing with reverse items
srs_adult[,srs_reverse_items] = 3 - srs_adult[,srs_reverse_items]

#dealing with missing items to 1
adult_missing_items = paste0('',c(1,3,6:7,11:12,15,19,21,25:26,31,38,40,43,45,52,55))
for(j in adult_missing_items){
  for(i in 1:nrow(srs_adult)){
    srs_adult[i,j] = ifelse(is.na(srs_adult[i,j]),1,srs_adult[i,j])
  }
}

#missing data analysis
srs_adult = count_missing_items(srs_adult,'1','65')
srs_adult = comment_missing_data(srs_adult,list(awareness_items,cognition_items,communication_items,
                                              motivation_items,rrb_items,sci_items),
                                 list('awareness','cognition','communication','motivation',
                                      'restricted_inerests/repetitive_behavior','social_communication&interaction'))


#calcualting total raw scores
srs_adult = summing_items_per_row(srs_adult,list(awareness_items,cognition_items,communication_items,motivation_items,
                                             rrb_items,sci_items,total_items),
                                list('awr_raw','cog_raw','com_raw','mot_raw','rrb_raw','sci_raw','total_raw'),F)
 
#calculating t-scores
for(i in 1:nrow(srs_adult)){
  srs_adult$awr_t[i] = ifelse(is.na(srs_adult$awr_raw[i]),NA,srs_adult_treat[which(srs_adult$awr_raw[i]==srs_adult_treat$raw.score),"srs_awr2"])
  srs_adult$cog_t[i] = ifelse(is.na(srs_adult$awr_raw[i]),NA,srs_adult_treat[which(srs_adult$cog_raw[i]==srs_adult_treat$raw.score),"srs_cog2"])
  srs_adult$com_t[i] = ifelse(is.na(srs_adult$awr_raw[i]),NA,srs_adult_treat[which(srs_adult$com_raw[i]==srs_adult_treat$raw.score),"srs_com2"])
  srs_adult$mot_t[i] = ifelse(is.na(srs_adult$awr_raw[i]),NA,srs_adult_treat[which(srs_adult$mot_raw[i]==srs_adult_treat$raw.score),"srs_mot2"])
  srs_adult$rrb_t[i] = ifelse(is.na(srs_adult$awr_raw[i]),NA,srs_adult_treat[which(srs_adult$rrb_raw[i]==srs_adult_treat$raw.score),"srs_rrb2"])
  srs_adult$sci_t[i] = ifelse(is.na(srs_adult$awr_raw[i]),NA,srs_adult_dsm[which(srs_adult$sci_raw[i]==srs_adult_dsm$raw.score),"srs_sci2"])
  srs_adult$total_t[i] = ifelse(is.na(srs_adult$awr_raw[i]),NA,srs_adult_total[which(srs_adult$total_raw[i]==srs_adult_total$raw.score),"total.score2"])
}

#splitting mother and father data
srs_adult = unique(srs_adult)
#srs_adult$srs_version = "Adult"
srs_mom = subset(srs_adult,grepl("-MTH$",srs_adult$id))
srs_dad = subset(srs_adult,grepl("-FAT$",srs_adult$id))
srs_mom = inserting_prefix_into_variables(srs_mom,'srs_mth_')
srs_dad = inserting_prefix_into_variables(srs_dad,'srs_fat_')

#obtaining outliers
srs_mom_outliers = srs_mom[,c(1:2,grep("subj_id$",names(srs_mom)),grep("_t$",names(srs_mom)))]
srs_dad_outliers = srs_dad[,c(1:2,grep("subj_id$",names(srs_dad)),grep("_t$",names(srs_dad)))]
srs_mom_outliers = outlier_list(srs_mom_outliers)
srs_mom$srs_mth_outlier_list = srs_mom_outliers$outlier_list
srs_dad_outliers = outlier_list(srs_dad_outliers)
srs_dad$srs_fat_outlier_list = srs_dad_outliers$outlier_list
# srs_adult_outlier_table = sqldf("select srs_mth_subj_id as id,visit,outlier_list from srs_mom_outliers where outlier_list != ''
#                                 union select srs_fat_subj_id as id,visit,outlier_list from srs_dad_outliers where outlier_list != ''")
rm(srs_mom_outliers,srs_dad_outliers)

#archiving the data and 
srs_mom_scored = srs_mom[,c(grep("subj_id$",names(srs_mom)),2,grep("items_count$",names(srs_mom)):ncol(srs_mom))]
srs_dad_scored = srs_dad[,c(grep("subj_id$",names(srs_dad)),2,grep("items_count$",names(srs_dad)):ncol(srs_dad))]
names(srs_mom_scored)[1] = "id"
names(srs_dad_scored)[1] = "id"

#orphaned/duplicate data
srs_mom_orphaned_data = orphaned_data_consolidate(srs_mom_scored)
srs_mom_scored = orphaned_data_remove(srs_mom_scored)
srs_dad_orphaned_data = orphaned_data_consolidate(srs_dad_scored)
srs_dad_scored = orphaned_data_remove(srs_dad_scored)
if(length(which(table(srs_mom_scored$id,srs_mom_scored$visit)>1))==0){
  write.csv(srs_mom_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/srs_mom_scored.csv",row.names = F)
}
if(length(which(table(srs_dad_scored$id,srs_dad_scored$visit)>1))==0){
  write.csv(srs_dad_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/srs_dad_scored.csv",row.names = F)
}

#archiving the data
write.csv(srs_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/srs_scored.csv",row.names = F)
write.csv(srs_scored_staar,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived_staar/srs_scored.csv",row.names = F)
write.csv(srs[-grep("^[0-9]{4}$",srs$id),c(1:2,grep("^1$",names(srs)):grep("^65$",names(srs)))],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/srs_items.csv",row.names = F)
write.csv(srs[grep("^[0-9]{4}$",srs$id),c(1:2,grep("^1$",names(srs)):grep("^65$",names(srs)))],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived_staar/srs_items.csv",row.names = F)

#cleaning up
rm(srs_reverse_items,child_pre_missing_items_1,child_pre_missing_items_2,child_sch_missing_items,srs_pre_dsm,
   srs_pre_total,srs_pre_treat,srs_sch,srs_sch_total,srs_sch_treat,srs_sch_dsm,srs_pre_scored,srs_sch_scored,
   srs_pre,awareness_items,cognition_items,communication_items,motivation_items,rrb_items,sci_items,total_items,
   srs_adult,srs_adult_dsm,srs_adult_total,srs_adult_treat,srs_mom,srs_dad,adult_missing_items,srs_t1,srs_t3,
   srs_t5,srs_rc,srs_adult_t5)
