#querying the table
edq_t1 = exportRecords(ace_con,forms=c("participant_information","edq"),events="t1_arm_1",labels=F,stringsAsFactors=F)
edq_t1 = edq_t1[grepl("Co",edq_t1$edq_complete)&!is.na(edq_t1$edq_timestamp),-(2:grep("participant_information_complete",names(edq_t1)))]
edq_t1$visit = 1
edq = sqlQuery(new_con,"select * from EDQ;",stringsAsFactors=F)

#changing the variable classes, and taking out incomplete entries
edq = id_visit_changing_into_char(edq)
edq_entry_flag = entry_flag(edq,'edq')
edq = subset(edq,entry_status==2)
edq = removing_prefix(edq,"EDQ_")
# edq_t1 = study_id_to_id(edq_t1,"edq_")
# edq = identify_same_data(edq_t1,edq)
# edq = rbind.fill(edq_t1,edq)

#calculating age
edq = fxage(edq,'id','date')

#cleaning individual items entries and counting missing items
for(j in which(names(edq)=='1'):which(names(edq)=='45')){
  for(i in 1:nrow(edq)){
    edq[i,j] = ifelse(edq[i,j]>=0 & edq[i,j]<=3,edq[i,j],NA)
  }
  edq[,j] = as.numeric(edq[,j])
}

#domain items
social_items = paste0('',c(seq(1,13,by=3),19:20,seq(22,37,by=3),39,42))
comm_items = paste0('',c(seq(3,15,by=3),16,18,seq(24,36,by=3),41,43:44))
rb_items = paste0('',c(seq(2,17,by=3),21,seq(23,38,by=3),40,45))
`3a_items` = grep("3A[1-9]{1}$",names(edq),value=T)
`3b_items` = grep("3B[1-9]{1}$",names(edq),value=T)
`3c_items` = grep("3C[1-9]{1}$",names(edq),value=T)
`3d_items` = grep("3D[1-9]{1}$",names(edq),value=T)
rg_items = c(`3a_items`,`3b_items`)

#change part 3 categories into numeric scores
for(j in c(`3a_items`,`3b_items`,`3c_items`,`3d_items`)){
  edq[,j] = itemscore_yesno(edq[,j])
}

#missing data analysis
edq = count_missing_items(edq,'1','45')
edq = count_missing_items(edq,'3A1','3D3')
edq = comment_missing_data(edq,list(social_items,comm_items,rb_items,`3a_items`,
                                    `3b_items`,`3c_items`,`3d_items`,rg_items),
                           list('social','communication','repetitive_behavior',
                                'communication_loss','social_loss','adaptive_functioning_loss',
                                'motor_loss','regression'))

#part 1 obtaining scores
edq = summing_items_per_row(edq,list(social_items,comm_items,rb_items),
                            list('social','communication','rbs'),F)

#part 3 scoring
edq = summing_items_per_row(edq,list(`3a_items`,`3b_items`,`3c_items`,`3d_items`,rg_items),
                            list('communication_loss','social_loss',
                                 'adaptive_functioning_loss','motor_loss','regression_score'),F)

#putting prefixes back
edq = inserting_prefix_into_variables(edq,"edq_")

#orphaned/duplicate data
edq_orphaned_data = orphaned_data_consolidate(edq)
edq = orphaned_data_remove(edq)
edq_duplicate_data = duplicate_data_consolidate(edq,"edq_age")
edq = duplicate_data_remove(edq,"edq_age")

#outliers
edq_outliers = edq[,c(1:2,grep("_age$",names(edq)),which(names(edq)=="edq_social"):which(names(edq)=="edq_rbs"))]
edq_outliers = outlier_list(edq_outliers)
edq$edq_outlier_list = edq_outliers$outlier_list
#edq_outlier_table = sqldf("select id,visit,outlier_list from edq_outliers where outlier_list != ''")
rm(edq_outliers)

#archiving the data
for(j in c(54:58,60,94:95,100:101)){
  for(i in 1:nrow(edq)){
    edq[i,j] = cbraw(edq[i,j])
  }
}
edq_scored = edq[,c(1:2,grep("_age$",names(edq)):which(names(edq)=="edq_rbs"),
                    grep("RO$",names(edq)):grep("SW$",names(edq)),grep("SP$",names(edq)),which(names(edq)=="edq_communication_loss"):(ncol(edq)-1),
                    grep("4_1$",names(edq)):grep("4_2$",names(edq)),grep("4_6",names(edq)):grep("4_7$",names(edq)),ncol(edq))]
write.csv(edq_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/edq_scored.csv",row.names = F)
write.csv(edq[,c(1:2,6,9:105)],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/edq_items.csv",row.names = F)

#cleaning up
rm(social_items,comm_items,rb_items,`3a_items`,`3b_items`,`3c_items`,`3d_items`,rg_items)
