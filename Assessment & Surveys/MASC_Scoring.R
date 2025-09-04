#pulling data tables
masc_t5 = exportRecords(ace_con,forms=c("participant_information","masc_child"),events="t5_arm_2",labels=F,stringsAsFactors=F)
masc_t5 = masc_t5[grepl("Co",masc_t5$masc_child_complete)&!is.na(masc_t5$masc_child_timestamp),-(2:grep("participant_information_complete",names(masc_t5)))]
masc_t5$visit = 5
masc = sqlQuery(new_con,"select * from MASC_2_Child;",stringsAsFactors=F)
masc_p = sqlQuery(new_con,"select * from MASC_2_Parent;",stringsAsFactors=F)
masc_p2 = sqlQuery(con4,"select * from MASC_2_Parent;",stringsAsFactors=F)
masc_p_t5 = exportRecords(ace_con,forms=c("participant_information","masc_parent"),events="t5_arm_1",labels=F,stringsAsFactors=F)
masc_p_t5 = masc_p_t5[grepl("Co",masc_p_t5$masc_parent_complete)&!is.na(masc_p_t5$masc_parent_timestamp),-(2:grep("participant_information_complete",names(masc_p_t5)))]
masc_p_t5$visit = 5

#changing categories to numeric scales for redcap data before merging the data
for(j in grep("[0-9]+$",names(masc_t5))){
  masc_t5[,j] = ifelse(tolower(masc_t5[,j])=="never",0,
                       ifelse(tolower(masc_t5[,j])=="rarely",1,
                              ifelse(tolower(masc_t5[,j])=="sometimes",2,
                                     ifelse(tolower(masc_t5[,j])=="often",3,NA))))
  #masc_t5[,j] = as.numeric(masc_t5[,j])
}
for(j in grep("[0-9]+$",names(masc_p_t5))){
  masc_p_t5[,j] = ifelse(tolower(masc_p_t5[,j])=="never",0,
                       ifelse(tolower(masc_p_t5[,j])=="rarely",1,
                              ifelse(tolower(masc_p_t5[,j])=="sometimes",2,
                                     ifelse(tolower(masc_p_t5[,j])=="often",3,NA))))
  masc_p_t5[,j] = as.numeric(masc_p_t5[,j])
}

#id, visit into characters, removing single-entered rows and prefixes
masc = id_visit_changing_into_char(masc)
masc_p = id_visit_changing_into_char(masc_p)
masc_child_entry_flag = entry_flag(masc,'masc_child')
masc_parent_entry_flag = entry_flag(masc_p,'masc_adult')
masc = subset(masc,entry_status==2)
masc_p = subset(masc_p,entry_status==2)
if(!is.null(masc_child_entry_flag)){
  masc = rbind(masc,masc_child_entry_flag[,-ncol(masc_child_entry_flag)])
}
if(!is.null(masc_parent_entry_flag)){
  masc_p = rbind(masc_p,masc_parent_entry_flag[,-ncol(masc_parent_entry_flag)])
}
masc = removing_prefix(masc,"mascc_")
masc_t5 = study_id_to_id(masc_t5,"mascc_")
masc = identify_same_data(masc_t5,masc)
masc = rbind.fill(masc,masc_t5)
masc_p = removing_prefix(masc_p,"mascp_")
masc_p2 = removing_prefix(masc_p2,"mascp_")
masc_p_t5 = study_id_to_id(masc_p_t5,"mascp_")
masc_p = identify_same_data(masc_p_t5,masc_p)
masc_p = rbind.fill(masc_p,masc_p2,masc_p_t5)

##Child Scoring
#importing norm table
masc_t = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/MASC_t.csv",stringsAsFactors = F)

#calculating age
masc = fxage(masc,'id','date')

#calculating raw total for sections
#list of items
sap_items = paste0('',c(26,30,33,4,7,9,17,19,23))
gad_items = paste0('',c(seq(27,31,by=2),1,6,39:40,13,17,22))
hr_items = paste0('',c(29,3,10,16,22))
pf_items = paste0('',c(32,36,38,14))
sa_items = c(hr_items,pf_items)
oc_items = paste0('',41:50)
panic_items = paste0('',c(31,37,6,12,18,20,24))
tr_items = paste0('',c(27,1,34,8,15))
ps_items = c(panic_items,tr_items)
ha_items = paste0('',c(28,2,35,5,11,13,21,25))
total_items = c(sap_items,sa_items,oc_items,ps_items,ha_items,paste0('',39:40))

#making sure there is no item with negative scores
for(j in which(names(masc)=='1'):which(names(masc)=='50')){
  masc[,j] = cbraw(masc[,j])
}

#missing data analysis
masc = count_missing_items(masc,'1','50')
masc = comment_missing_data(masc,list(sap_items,gad_items,hr_items,pf_items,
                                      sa_items,oc_items,panic_items,tr_items,
                                      ps_items,ha_items),
                            list('separation_anxiety/phobias','generalized_anxiety',
                                 'humiliation/rejection','performance_fears','social_anxiety',
                                 'obsessions/compulsions','panic','tense/restless',
                                 'physical_symptoms','harm_avoidance'))

#summing the scores
masc = summing_items_per_row(masc,list(sap_items,gad_items,hr_items,pf_items,
                                       sa_items,oc_items,panic_items,tr_items,
                                       ps_items,ha_items,total_items),
                             list('sap_raw','gad_raw','hr_raw','pf_raw','sa_raw',
                                  'oc_raw','panic_raw','tr_raw','ps_raw','ha_raw','total_raw'),F)

#obtaining t scores
#pulling out gender first
masc = sqldf("select t1.*,gender from masc t1 left join subj on t1.id=subj.subj_id")

#assigning t-scores from norm table
for(i in 1:nrow(masc)){
  masc$sap_t[i] = ifelse(is.na(masc$sap_raw[i]),NA,masc_t[which(masc$gender[i]==masc_t$gender
                               & masc$sap_raw[i]==masc_t$raw),"sap"])
  masc$gad_t[i] = ifelse(is.na(masc$gad_raw[i]),NA,masc_t[which(masc$gender[i]==masc_t$gender
                               & masc$gad_raw[i]==masc_t$raw),"gad"])
  masc$hr_t[i] = ifelse(is.na(masc$hr_raw[i]),NA,masc_t[which(masc$gender[i]==masc_t$gender
                               & masc$hr_raw[i]==masc_t$raw),"hr"])
  masc$pf_t[i] = ifelse(is.na(masc$pf_raw[i]),NA,masc_t[which(masc$gender[i]==masc_t$gender
                               & masc$pf_raw[i]==masc_t$raw),"pf"])
  masc$sa_t[i] = ifelse(is.na(masc$sa_raw[i]),NA,masc_t[which(masc$gender[i]==masc_t$gender
                               & masc$sa_raw[i]==masc_t$raw),"sa"])
  masc$oc_t[i] = ifelse(is.na(masc$oc_raw[i]),NA,masc_t[which(masc$gender[i]==masc_t$gender
                               & masc$oc_raw[i]==masc_t$raw),"oc"])
  masc$panic_t[i] = ifelse(is.na(masc$panic_raw[i]),NA,masc_t[which(masc$gender[i]==masc_t$gender
                               & masc$panic_raw[i]==masc_t$raw),"panic"])
  masc$tr_t[i] = ifelse(is.na(masc$tr_raw[i]),NA,masc_t[which(masc$gender[i]==masc_t$gender
                               & masc$tr_raw[i]==masc_t$raw),"tr"])
  masc$ps_t[i] = ifelse(is.na(masc$ps_raw[i]),NA,masc_t[which(masc$gender[i]==masc_t$gender
                              & masc$ps_raw[i]==masc_t$raw),"ps"])
  masc$ha_t[i] = ifelse(is.na(masc$ha_raw[i]),NA,masc_t[which(masc$gender[i]==masc_t$gender
                               & masc$ha_raw[i]==masc_t$raw),"ha"])
  masc$total_t[i] = ifelse(is.na(masc$total_raw[i]),NA,ifelse(masc$gender[i]=="Male" & masc$total_raw[i]>120,90,
                           masc_t[which(masc$gender[i]==masc_t$gender & masc$total_raw[i]==masc_t$raw),"total"]))
}

#inconsistency score
masc$i1 = abs(masc$`2` - masc$`11`)
masc$i2 = abs(masc$`3` - masc$`10`)
masc$i3 = abs(masc$`4` - masc$`9`)
masc$i4 = abs(masc$`5` - masc$`13`)
masc$i5 = abs(masc$`8` - masc$`15`)
masc$i6 = abs(masc$`16` - masc$`22`)
masc$i7 = abs(masc$`43` - masc$`44`)
masc$i8 = abs(masc$`45` - masc$`46`)
for(i in 1:nrow(masc)){
  masc$inconsistency_index[i] = sum(masc[i,paste0('i',1:8)],na.rm=F)
}

#putting back prefixes
masc = inserting_prefix_into_variables(masc,"masc_")

#orphaned/duplicate data
masc_orphaned_data = orphaned_data_consolidate(masc)
masc = orphaned_data_remove(masc)
masc_duplicate_data = duplicate_data_consolidate(masc,"masc_age")
masc = duplicate_data_remove(masc,"masc_age")

#finding outliers
masc_outliers = masc[,c(1:2,grep("_age$",names(masc)),grep("_t$",names(masc)),ncol(masc))]
masc_outliers = outlier_list(masc_outliers)
masc$masc_outlier_list = masc_outliers$outlier_list
#masc_outlier_table = sqldf("select id,visit,outlier_list from masc where outlier_list != ''")
rm(masc_outliers)

##Adult Scoring
#calculate age
masc_p = fxage(masc_p,'id','date')

#making sure there is no item with negative scores
for(j in which(names(masc_p)=='1'):which(names(masc_p)=='50')){
  masc_p[,j] = cbraw(masc_p[,j])
}

#missing data analysis
masc_p = count_missing_items(masc_p,'1','50')
masc_p = comment_missing_data(masc_p,list(sap_items,gad_items,hr_items,pf_items,
                                      sa_items,oc_items,panic_items,tr_items,
                                      ps_items,ha_items),
                            list('separation_anxiety/phobias','generalized_anxiety',
                                 'humiliation/rejection','performance_fears','social_anxiety',
                                 'obsessions/compulsions','panic','tense/restless',
                                 'physical_symptoms','harm_avoidance'))

#summing the scores
masc_p = summing_items_per_row(masc_p,list(sap_items,gad_items,hr_items,pf_items,
                                       sa_items,oc_items,panic_items,tr_items,
                                       ps_items,ha_items,total_items),
                             list('sap_raw','gad_raw','hr_raw','pf_raw','sa_raw',
                                  'oc_raw','panic_raw','tr_raw','ps_raw','ha_raw','total_raw'),F)

#obtaining t scores
#pulling out gender first
masc_p = sqldf("select t1.*,gender from masc_p t1 left join subj on t1.id=subj.subj_id")

#assigning t-scores from norm table
for(i in 1:nrow(masc_p)){
  masc_p$sap_t[i] = ifelse(is.na(masc_p$sap_raw[i]),NA,masc_t[which(masc_p$gender[i]==masc_t$gender
                                                                    & masc_p$sap_raw[i]==masc_t$raw),"sap"])
  masc_p$gad_t[i] = ifelse(is.na(masc_p$gad_raw[i]),NA,masc_t[which(masc_p$gender[i]==masc_t$gender
                                                                    & masc_p$gad_raw[i]==masc_t$raw),"gad"])
  masc_p$hr_t[i] = ifelse(is.na(masc_p$hr_raw[i]),NA,masc_t[which(masc_p$gender[i]==masc_t$gender
                                                                  & masc_p$hr_raw[i]==masc_t$raw),"hr"])
  masc_p$pf_t[i] = ifelse(is.na(masc_p$pf_raw[i]),NA,masc_t[which(masc_p$gender[i]==masc_t$gender
                                                                  & masc_p$pf_raw[i]==masc_t$raw),"pf"])
  masc_p$sa_t[i] = ifelse(is.na(masc_p$sa_raw[i]),NA,masc_t[which(masc_p$gender[i]==masc_t$gender
                                                                  & masc_p$sa_raw[i]==masc_t$raw),"sa"])
  masc_p$oc_t[i] = ifelse(is.na(masc_p$oc_raw[i]),NA,masc_t[which(masc_p$gender[i]==masc_t$gender
                                                                  & masc_p$oc_raw[i]==masc_t$raw),"oc"])
  masc_p$panic_t[i] = ifelse(is.na(masc_p$panic_raw[i]),NA,masc_t[which(masc_p$gender[i]==masc_t$gender
                                                                  & masc_p$panic_raw[i]==masc_t$raw),"panic"])
  masc_p$tr_t[i] = ifelse(is.na(masc_p$tr_raw[i]),NA,masc_t[which(masc_p$gender[i]==masc_t$gender
                                                              & masc_p$tr_raw[i]==masc_t$raw),"tr"])
  masc_p$ps_t[i] = ifelse(is.na(masc_p$ps_raw[i]),NA,masc_t[which(masc_p$gender[i]==masc_t$gender
                                                              & masc_p$ps_raw[i]==masc_t$raw),"ps"])
  masc_p$ha_t[i] = ifelse(is.na(masc_p$ha_raw[i]),NA,masc_t[which(masc_p$gender[i]==masc_t$gender
                                                              & masc_p$ha_raw[i]==masc_t$raw),"ha"])
  masc_p$total_t[i] = ifelse(is.na(masc_p$total_raw[i]),NA,ifelse(masc_p$gender[i]=="Male" & masc_p$total_raw[i]>120,90,
                                                              masc_t[which(masc_p$gender[i]==masc_t$gender & masc_p$total_raw[i]==masc_t$raw),"total"]))
}

#inconsistency score
masc_p$i1 = abs(masc_p$`2` - masc_p$`11`)
masc_p$i2 = abs(masc_p$`3` - masc_p$`10`)
masc_p$i3 = abs(masc_p$`4` - masc_p$`9`)
masc_p$i4 = abs(masc_p$`5` - masc_p$`13`)
masc_p$i5 = abs(masc_p$`8` - masc_p$`15`)
masc_p$i6 = abs(masc_p$`16` - masc_p$`22`)
masc_p$i7 = abs(masc_p$`43` - masc_p$`44`)
masc_p$i8 = abs(masc_p$`45` - masc_p$`46`)
for(i in 1:nrow(masc_p)){
  masc_p$inconsistency_index[i] = sum(masc_p[i,paste0('i',1:8)],na.rm=F)
}

#putting back prefixes
masc_p = inserting_prefix_into_variables(masc_p,"masc_parent_")

#orphaned/duplicate data
masc_p_orphaned_data = orphaned_data_consolidate(masc_p)
masc_p = orphaned_data_remove(masc_p)
which(table(masc_p$id,masc_p$visit)>1)

#finding outliers
masc_p_outliers = masc_p[,c(1:2,grep("_age$",names(masc_p)),grep("_t$",names(masc_p)),ncol(masc_p))]
masc_p_outliers = outlier_list(masc_p_outliers)
masc_p$masc_parent_outlier_list = masc_p_outliers$outlier_list
#masc_p_outlier_table = sqldf("select id,visit,outlier_list from masc_p where outlier_list != ''")
rm(masc_p_outliers)

#merging the data
masc_scored = masc[,c(1:2,grep("age",names(masc)),grep("missing",names(masc)),grep("_raw$",names(masc)),grep("_t$",names(masc)),grep("index$",names(masc)):ncol(masc))]
masc_p_scored = masc_p[,c(1:2,grep("resp$",names(masc_p)),grep("age",names(masc_p)),grep("missing",names(masc_p)),grep("_raw$",names(masc_p)),grep("_t$",names(masc_p)),grep("index$",names(masc_p)):ncol(masc_p))]

#archiving the data
write.csv(masc_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/masc_scored.csv",row.names = F)
write.csv(masc[-grep("^[0-9]{4}$",masc$id),c(1:2,grep("_resp$",names(masc_p)):grep("_50$",names(masc_p)))],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/masc_items.csv",row.names = F)
write.csv(masc_p_scored[-grep("^[0-9]{4}$",masc_p_scored$id),],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/masc_parent_scored.csv",row.names = F)
write.csv(masc_p_scored[grep("^[0-9]{4}$",masc_p_scored$id),-4],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived_staar/masc_parent_scored.csv",row.names = F)
write.csv(masc_p[grep("^[0-9]{4}$",masc_p$id),c(1:2,grep("_resp$",names(masc_p)):grep("_50$",names(masc_p)))],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived_staar/masc_parent_items.csv",row.names = F)
write.csv(masc_p[-grep("^[0-9]{4}$",masc_p$id),c(1:2,grep("_resp$",names(masc_p)):grep("_50$",names(masc_p)))],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/masc_parent_items.csv",row.names = F)

#cleaning up
rm(sap_items,gad_items,hr_items,pf_items,sa_items,oc_items,panic_items,tr_items,ps_items,ha_items,total_items,
   masc_t,masc_p2,masc_t5,masc_p_t5)
