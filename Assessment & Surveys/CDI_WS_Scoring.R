#pulling data from different db's
cdi_ws_t1 = exportRecords(ace_con,forms=c("participant_information","cdiws"),events="t1_arm_1",labels=F,stringsAsFactors=F)
cdi_ws_t1 = cdi_ws_t1[grepl("Co",cdi_ws_t1$cdiws_complete)&!is.na(cdi_ws_t1$cdiws_timestamp),-(2:grep("participant_information_complete",names(cdi_ws_t1)))]
cdi_ws_t1$visit = 1
cdi_ws_t3 = exportRecords(ace_con,forms=c("participant_information","cdiws"),events="t3_arm_1",labels=F,stringsAsFactors=F)
cdi_ws_t3 = cdi_ws_t3[grepl("Co",cdi_ws_t3$cdiws_complete)&!is.na(cdi_ws_t3$cdiws_timestamp),-(2:grep("participant_information_complete",names(cdi_ws_t3)))]
cdi_ws_t3$visit = 3
cdi_ws_rc = rbind(cdi_ws_t1,cdi_ws_t3)
cdi_ws = sqlQuery(new_con,"select * from CDI_Words_and_Sentences;",stringsAsFactors=F)

#changing id and visit into character vars and getting rid of single-entered rows and prefix
cdi_ws = id_visit_changing_into_char(cdi_ws)
cdi_ws_entry_flag = entry_flag(cdi_ws,"cdi_ws")
cdi_ws = subset(cdi_ws,entry_status==2)
if(!is.null(cdi_ws_entry_flag)){
  cdi_ws = rbind(cdi_ws,cdi_ws_entry_flag[,-ncol(cdi_ws_entry_flag)])
}
cdi_ws = removing_prefix(cdi_ws,"cdiws_")
cdi_ws_rc = study_id_to_id(cdi_ws_rc,"cdiws_")

#processing redcap cdiwg table before combining the tables
for(i in 1:nrow(cdi_ws_rc)){
  cdi_ws_rc$ia1[i] = length(which(cdi_ws_rc[i,grep("ia1___[0-9]{1,2}$",names(cdi_ws_rc))]=="Checked"))
  cdi_ws_rc$ia2[i] = length(which(cdi_ws_rc[i,grep("ia2___[0-9]{1,2}$",names(cdi_ws_rc))]=="Checked"))
  cdi_ws_rc$ia3[i] = length(which(cdi_ws_rc[i,grep("ia3___[0-9]{1,2}$",names(cdi_ws_rc))]=="Checked"))
  cdi_ws_rc$ia4[i] = length(which(cdi_ws_rc[i,grep("ia4___[0-9]{1,2}$",names(cdi_ws_rc))]=="Checked"))
  cdi_ws_rc$ia5[i] = length(which(cdi_ws_rc[i,grep("ia5___[0-9]{1,2}$",names(cdi_ws_rc))]=="Checked"))
  cdi_ws_rc$ia6[i] = length(which(cdi_ws_rc[i,grep("ia6___[0-9]{1,2}$",names(cdi_ws_rc))]=="Checked"))
  cdi_ws_rc$ia7[i] = length(which(cdi_ws_rc[i,grep("ia7___[0-9]{1,2}$",names(cdi_ws_rc))]=="Checked"))
  cdi_ws_rc$ia8[i] = length(which(cdi_ws_rc[i,grep("ia8___[0-9]{1,2}$",names(cdi_ws_rc))]=="Checked"))
  cdi_ws_rc$ia9[i] = length(which(cdi_ws_rc[i,grep("ia9___[0-9]{1,2}$",names(cdi_ws_rc))]=="Checked"))
  cdi_ws_rc$ia10[i] = length(which(cdi_ws_rc[i,grep("ia10___[0-9]{1,2}$",names(cdi_ws_rc))]=="Checked"))
  cdi_ws_rc$ia11[i] = length(which(cdi_ws_rc[i,grep("ia11___[0-9]{1,2}$",names(cdi_ws_rc))]=="Checked"))
  cdi_ws_rc$ia12[i] = length(which(cdi_ws_rc[i,grep("ia12___[0-9]{1,2}$",names(cdi_ws_rc))]=="Checked"))
  cdi_ws_rc$ia13[i] = length(which(cdi_ws_rc[i,grep("ia13___[0-9]{1,2}$",names(cdi_ws_rc))]=="Checked"))
  cdi_ws_rc$ia14[i] = length(which(cdi_ws_rc[i,grep("ia14___[0-9]{1,2}$",names(cdi_ws_rc))]=="Checked"))
  cdi_ws_rc$ia15[i] = length(which(cdi_ws_rc[i,grep("ia15___[0-9]{1,2}$",names(cdi_ws_rc))]=="Checked"))
  cdi_ws_rc$ia16[i] = length(which(cdi_ws_rc[i,grep("ia16___[0-9]{1,2}$",names(cdi_ws_rc))]=="Checked"))
  cdi_ws_rc$ia17[i] = length(which(cdi_ws_rc[i,grep("ia17___[0-9]{1,2}$",names(cdi_ws_rc))]=="Checked"))
  cdi_ws_rc$ia18[i] = length(which(cdi_ws_rc[i,grep("ia18___[0-9]{1,2}$",names(cdi_ws_rc))]=="Checked"))
  cdi_ws_rc$ia19[i] = length(which(cdi_ws_rc[i,grep("ia19___[0-9]{1,2}$",names(cdi_ws_rc))]=="Checked"))
  cdi_ws_rc$ia20[i] = length(which(cdi_ws_rc[i,grep("ia20___[0-9]{1,2}$",names(cdi_ws_rc))]=="Checked"))
  cdi_ws_rc$ia21[i] = length(which(cdi_ws_rc[i,grep("ia21___[0-9]{1,2}$",names(cdi_ws_rc))]=="Checked"))
  cdi_ws_rc$ia22[i] = length(which(cdi_ws_rc[i,grep("ia22___[0-9]{1,2}$",names(cdi_ws_rc))]=="Checked"))
  cdi_ws_rc$iib1[i] = length(which(cdi_ws_rc[i,grep("iib1___[0-9]{1,2}$",names(cdi_ws_rc))]=="Checked"))
  cdi_ws_rc$iib2[i] = length(which(cdi_ws_rc[i,grep("iib2___[0-9]{1,2}$",names(cdi_ws_rc))]=="Checked"))
  cdi_ws_rc$iic1[i] = length(which(cdi_ws_rc[i,grep("iic1___[0-9]{1,2}$",names(cdi_ws_rc))]=="Checked"))
  cdi_ws_rc$iic2[i] = length(which(cdi_ws_rc[i,grep("iic2___[0-9]{1,2}$",names(cdi_ws_rc))]=="Checked"))
}

cdi_ws = identify_same_data(cdi_ws_rc,cdi_ws)
names(cdi_ws) = tolower(names(cdi_ws))
cdi_ws = rbind.fill(cdi_ws,cdi_ws_rc[,-grep("___",names(cdi_ws_rc))])

#calculating age
cdi_ws = fxage(cdi_ws,'id','date')

#scoring ia
a1_items = grep("^i{1}a[0-9]{1,2}",names(cdi_ws))
for(i in 1:nrow(cdi_ws)){
  for(j in a1_items){
    cdi_ws[i,j] = cbraw(cdi_ws[i,j])
  }
  cdi_ws$words_produced[i] = sum(cdi_ws[i,a1_items],na.rm=F)
}

#scoring ib and iia -> same scoring method as ws section 1c&2a
b1_items = grep("^i{1}b[0-9]{1,2}",names(cdi_ws))
a2_items = grep("^i{2}a[0-9]{1,2}",names(cdi_ws))
#converting the items
for(j in c(b1_items,a2_items)){
  for(i in 1:nrow(cdi_ws)){
    cdi_ws[i,j] = cdi_wg_section_1cn2a_score(cdi_ws[i,j])
  }
  cdi_ws[,j] = as.numeric(cdi_ws[,j])
}
#summing
cdi_ws = summing_items_per_row(cdi_ws,list(b1_items),list("word_usage"),F)
cdi_ws = summing_items_per_row(cdi_ws,list(a2_items),list("word_endings_1"),F)

#scoring iib and iic -> simply add them by section
b2_items = grep("^i{2}b[0-9]{1,2}",names(cdi_ws))
c2_items = grep("^i{2}c[0-9]{1,2}",names(cdi_ws))
cdi_ws = summing_items_per_row(cdi_ws,list(b2_items),list("word_forms"),F)
cdi_ws = summing_items_per_row(cdi_ws,list(c2_items),list("word_endings_2"),F)

#scoring section d
for(i in 1:nrow(cdi_ws)){
  cdi_ws$words_combining[i] = cdi_wg_section_1cn2a_score(cdi_ws$iid0[i])
  cdi_ws$M3L[i] = ifelse(is.na(cdi_ws$words_combining[i]),NA,ifelse(cdi_ws$words_combining[i] != 1,1,round(cdi_ws$iid1a[i]/cdi_ws$iid1a2,2)))
}

#scoring section e
e2_items = grep("^i{2}e[0-9]{1,2}",names(cdi_ws))
for(j in e2_items){
  for(i in 1:nrow(cdi_ws)){
    cdi_ws[i,j] = cdi_ws_section_e_score(cdi_ws[i,j])
  }
  cdi_ws[,j] = as.numeric(cdi_ws[,j])
}
for(i in 1:nrow(cdi_ws)){
  cdi_ws$complexity[i] = ifelse(cdi_ws$words_combining[i] != 1,0,sum(cdi_ws[i,e2_items],na.rm=F))
}

#percentile scores -> all the participants' age exceed max norm table age
# cdi_ws_perc = read_xlsx("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/cdi_ws_norm_table.xlsx")
# cdi_ws = merge(cdi_ws,subj[,c(1,4)],by.x="id",by.y="subj_id",all.x=T) #merging gender var
# cdi_ws$gender = gsub("[a-z]{3,5}$","",cdi_ws$gender)
# for(i in 1:nrow(cdi_ws)){
#   index = which(cdi_ws_perc$var=="twp" & cdi_ws_perc$raw<=cdi_ws$words_produced[i]
#                 & cdi_ws_perc$age==floor(cdi_ws$age[i]) & cdi_ws_perc$gender==cdi_ws$gender[i])
#   index = index[length(index)] #can be multiple percentile values with the same raw score(s)
#   cdi_ws$words_produced_percentile[i] = ifelse(length(index)==0,NA,cdi_ws_perc$percentile[index])
# }

#putting back prefix
cdi_ws = inserting_prefix_into_variables(cdi_ws,"cdi_ws_")

#orphaned/duplicate data
cdi_ws_orphaned_data = orphaned_data_consolidate(cdi_ws)
cdi_ws = orphaned_data_remove(cdi_ws)
cdi_ws_duplicate_data = duplicate_data_consolidate(cdi_ws,"cdi_ws_age")
cdi_ws = duplicate_data_remove(cdi_ws,"cdi_ws_age")

#outliers
cdi_ws_outliers = cdi_ws[,c(1:2,which(names(cdi_ws)=="cdi_ws_age"),which(names(cdi_ws)=="cdi_ws_words_produced"),
                            which(names(cdi_ws)=="cdi_ws_word_forms"),ncol(cdi_ws))]
cdi_ws_outliers = outlier_list(cdi_ws_outliers)
cdi_ws$cdi_ws_outlier_list = cdi_ws_outliers$outlier_list
#cdi_ws_outlier_table = sqldf("select id,visit,outlier_list from cdi_ws where outlier_list != ''")
rm(cdi_ws_outliers)

#archiving the data
cdi_ws_scored = cdi_ws[,c(1:2,which(names(cdi_ws)=="cdi_ws_age"):ncol(cdi_ws))]
write.csv(cdi_ws_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/cdi_ws_scored.csv",row.names=F)
write.csv(cdi_ws[,c(1:2,grep("_ia1$",names(cdi_ws)):grep("_iie37",names(cdi_ws)))],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/cdi_ws_items.csv",row.names=F)

#clean up
rm(a1_items,b1_items,a2_items,b2_items,c2_items,e2_items,cdi_ws_t1,cdi_ws_t3,cdi_ws_rc)
