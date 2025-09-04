ccc_t5 = exportRecords(ace_con,forms=c("participant_information","ccc2"),events="t5_arm_1",labels=F,stringsAsFactors=F)
ccc_t5 = ccc_t5[grepl("Co",ccc_t5$ccc2_complete)&!is.na(ccc_t5$ccc2_timestamp),-(2:grep("participant_information_complete",names(ccc_t5)))]
ccc_t5$visit = 5
ccc = sqlQuery(new_con,"select * from CCC_2",stringsAsFactors=F)

ccc = id_visit_changing_into_char(ccc)
ccc_entry_flag = entry_flag(ccc,'ccc')
ccc = subset(ccc,entry_status==2)
if(!is.null(ccc_entry_flag)){
  ccc = rbind(ccc,ccc_entry_flag[,-ncol(ccc_entry_flag)])
}
ccc = removing_prefix(ccc,'ccc_')
ccc_t5 = study_id_to_id(ccc_t5,"ccc_")
ccc = identify_same_data(ccc_t5,ccc)
ccc = rbind.fill(ccc,ccc_t5)

#importing norm table
ccc_scale=read.csv('S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/CCC2_NormTables_AgeAll.csv',header = TRUE)
ccc_scale[202,8] = NA

#calculating age
ccc = fxage(ccc,'id','date')

#dealing with negative scores
for(j in which(names(ccc)=="1"):which(names(ccc)=="70")){
  ccc[,j] = cbraw(ccc[,j])
}

#list of subsection items
speech_item = paste0('',c(2,24,29,38,44,51,58))
syntax_item = paste0('',c(1,17,27,36,43,55,69))
semantics_item = paste0('',c(4,6,12,32,46,64,66))
coherence_item = paste0('',c(10,25,40,48,50,53,68))
initiation_item = paste0('',c(5,21,35,37,45,59,70))
sl_item = paste0('',c(11,18,23,30,42,61,62))
context_item = paste0('',c(15,19,28,34,41,54,60))
nvcom_item = paste0('',c(8,14,20,31,39,56,65))
sr_item = paste0('',c(3,7,13,16,33,57,67))
interests_item = paste0('',c(9,22,26,47,49,52,63))

#dealing with reversed scoring items
for(j in which(names(ccc)=='51'):which(names(ccc)=='70')){
  ccc[,j] = 3 - ccc[,j]
}

#missing data analysis
ccc = count_missing_items(ccc,'1','70')
ccc = comment_missing_data(ccc,list(speech_item,syntax_item,semantics_item,coherence_item,initiation_item,
                                    sl_item,context_item,nvcom_item,sr_item,interests_item),
                           list('speech','syntax','semantics','coherence','initiation','scripted_language',
                                'context','nonverbal_communication','social_relations','interests'))

#summing up
ccc = summing_items_per_row(ccc,list(speech_item,syntax_item,semantics_item,coherence_item,initiation_item,
                                     sl_item,context_item,nvcom_item,sr_item,interests_item),
                            list('speech_raw','syntax_raw','semantics_raw','coherence_raw','initiation_raw',
                                 'scr_lan_raw','context_raw','nvcom_raw','soc_rel_raw','interests_raw'),F)

#scaled scores
ccc=sqldf("SELECT t1.*, t2.t_score as speech_scaled
             from ccc as t1
          LEFT JOIN ccc_scale as t2
          on t1.speech_raw = t2.speech
          AND
          cast(t1.age/12 as int) = t2.Age
          ")

ccc=sqldf("SELECT t1.*, t2.t_score as syntax_scaled
          from ccc as t1
          LEFT JOIN ccc_scale as t2
          on t1.syntax_raw = t2.syn
          AND
          cast(t1.age/12 as int) = t2.Age
          ")

ccc=sqldf("SELECT t1.*, t2.t_score as semantics_scaled
          from ccc as t1
          LEFT JOIN ccc_scale as t2
          on t1.semantics_raw = t2.sem
          AND
          cast(t1.age/12 as int) = t2.Age
          ")

ccc=sqldf("SELECT t1.*, t2.t_score as coherence_scaled
          from ccc as t1
          LEFT JOIN ccc_scale as t2
          on t1.coherence_raw = t2.coher
          AND
          cast(t1.age/12 as int) = t2.Age
          ")

ccc=sqldf("SELECT t1.*, t2.t_score as initiation_scaled
          from ccc as t1
          LEFT JOIN ccc_scale as t2
          on t1.initiation_raw = t2.init
          AND
          cast(t1.age/12 as int) = t2.Age
          ")

ccc=sqldf("SELECT t1.*, t2.t_score as scr_lan_scaled
          from ccc as t1
          LEFT JOIN ccc_scale as t2
          on t1.scr_lan_raw = t2.scrpt_lang
          AND
          cast(t1.age/12 as int) = t2.Age
          ")

ccc=sqldf("SELECT t1.*, t2.t_score as context_scaled
          from ccc as t1
          LEFT JOIN ccc_scale as t2
          on t1.context_raw = t2.context
          AND
          cast(t1.age/12 as int) = t2.Age
          ")

ccc=sqldf("SELECT t1.*, t2.t_score as nvcom_scaled
          from ccc as t1
          LEFT JOIN ccc_scale as t2
          on t1.nvcom_raw = t2.nv_comm
          AND
          cast(t1.age/12 as int) = t2.Age
          ")

ccc=sqldf("SELECT t1.*, t2.t_score as soc_rel_scaled
          from ccc as t1
          LEFT JOIN ccc_scale as t2
          on t1.soc_rel_raw = t2.soc_rel
          AND
          cast(t1.age/12 as int) = t2.Age
          ")
ccc=sqldf("SELECT t1.*, t2.t_score as interests_scaled
          from ccc as t1
          LEFT JOIN ccc_scale as t2
          on t1.interests_raw = t2.inter
          AND
          cast(t1.age/12 as int) = t2.Age
          ")

ccc = summing_items_per_row(ccc,list(grep("_scaled$",names(ccc))),list('gcc_raw'),T)
ccc_gcc_scale=read.csv('S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/CCC2_NormTables_GCC.csv',header = TRUE)
ccc=sqldf("SELECT t1.*, t2.t_score as gcc_scaled
          from ccc as t1
          LEFT JOIN ccc_gcc_scale as t2
          on t1.gcc_raw = t2.gcc_raw
          ")

ccc$sidi = (ccc$initiation_scaled+ccc$nvcom_scaled+ccc$soc_rel_scaled+ccc$interests_scaled) - (ccc$speech_scaled+ccc$syntax_scaled+ccc$semantics_scaled+ccc$coherence_scaled)

#orphaned/duplicate data
ccc = inserting_prefix_into_variables(ccc,"ccc_")
ccc_orphaned_data = orphaned_data_consolidate(ccc)
ccc = orphaned_data_remove(ccc)
ccc_duplicate_data = duplicate_data_consolidate(ccc,"ccc_age")
ccc = duplicate_data_remove(ccc,"ccc_age")

#outliers
ccc_outliers = ccc[,c(1:2,grep("age$",names(ccc)),grep("_scaled$",names(ccc)),ncol(ccc))]
ccc_outliers = outlier_list(ccc_outliers)
ccc$ccc_outlier_list = ccc_outliers$outlier_list
#ccc_outlier_table = sqldf("select id,visit,outlier_list from ccc_outliers where outlier_list != ''")
rm(ccc_outliers)

#archiving the data
ccc_scored = ccc[,c(1:2,grep("age$",names(ccc)),grep("missing",names(ccc)),grep("info_",names(ccc)),grep("_raw$",names(ccc)),grep("_scaled$",names(ccc)),grep("sidi",names(ccc)),ncol(ccc))]
write.csv(ccc_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/ccc_scored.csv",row.names = F)
write.csv(ccc[,c(1:2,grep("_[0-9]+$",names(ccc)))],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/ccc_items.csv",row.names = F)

rm(ccc_scale,ccc_gcc_scale,speech_item,syntax_item,semantics_item,coherence_item,context_item,
   sr_item,sl_item,nvcom_item,interests_item,initiation_item,ccc_t5)
