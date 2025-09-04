das_sa = sqlQuery(new_con, "select * from DAS_SCHOOL_AGE;",stringsAsFactors=F)

#cleaning the data
das_sa = id_visit_changing_into_char(das_sa)
das_sa_entry_flag = entry_flag(das_sa,'das-SA')
das_sa = subset(das_sa, entry_status == 2 & visit != '4p')
if(!is.null(das_sa_entry_flag)){
  das_sa = rbind(das_sa,das_sa_entry_flag[,-ncol(das_sa_entry_flag)])
}
names(das_sa) = gsub("dasii_sar_","",names(das_sa))

#calculating age
das_sa = fxage(das_sa,'id','date')

#missing data analysis
das_raw_score_vars = paste0(c('rdes','wdef','pcon','mat','vsim','sqr'),'_raw')
das_sa = comment_missing_data(das_sa,list(das_raw_score_vars),list('raw score'))
das_sa[which(das_sa$missing_items_comment != ''),"missing_items_comment"] = "Raw score(s) missing in the data entry;"

#getting ability score
for(i in 1:nrow(das_sa)){
  das_sa$rd_as[i] = ifelse(!is.na(das_sa$rdes_raw[i]),das_as_sa[which(
    das_as_sa$subtest == 'recalldesign' & das_sa$rdes_raw[i] == das_as_sa[,6] & das_sa$rdes_isa[i] == das_as_sa[,"itemset_text"])
    ,7],NA)
  das_sa$wd_as[i] = ifelse(!is.na(das_sa$wdef_raw[i]),das_as_sa[which(
    das_as_sa$subtest == 'WordDef' & das_sa$wdef_raw[i] == das_as_sa[,6] & das_sa$wdef_isa[i] == das_as_sa[,4])
    ,7],NA)
  das_sa$pc_as[i] = ifelse(!is.na(das_sa$pcon_raw[i]),das_as_ey[which(
    das_as_ey$subtest == 'pattconstd' & das_sa$pcon_raw[i] == das_as_ey[,6] & das_sa$pcon_isa[i] == das_as_ey[,4])
    ,7],NA)
  das_sa$pc_alt_as[i] = ifelse(!is.na(das_sa$pcon_alt_raw[i]),das_as_ey[which(
    das_as_ey$subtest == 'pattconalt' & das_sa$pcon_alt_raw[i] == das_as_ey[,6] & das_sa$pcon_alt_isa[i] == das_as_ey[,4])
    ,7],NA)
  das_sa$mat_as[i] = ifelse(!is.na(das_sa$mat_raw[i]),das_as_ey[which(
    das_as_ey$subtest == 'Matrices' & das_sa$mat_raw[i] == das_as_ey[,6] & das_sa$mat_isa[i] == das_as_ey[,4])
    ,7],NA)
  das_sa$vs_as[i] = ifelse(!is.na(das_sa$vsim_raw[i]),das_as_sa[which(
    das_as_sa$subtest == 'VerbSim' & das_sa$vsim_raw[i] == das_as_sa[,6] & das_sa$vsim_isa[i] == das_as_sa[,4])
    ,7],NA)
  das_sa$seqr_as[i] = ifelse(!is.na(das_sa$sqr_raw[i]),das_as_sa[which(
    das_as_sa$subtest == 'SeqQuant' & das_sa$sqr_raw[i] == das_as_sa[,6] & das_sa$sqr_isa[i] == das_as_sa[,4])
    ,7],NA)
}

##obtaining age equivalents using for loop
#changing some values in the norm table first tho
das_ae_sa$age_eq[which(das_ae_sa$age_eq==-998)] = NA
das_ae_sa$age_eq[which(das_ae_sa$age_eq==216)] = NA

#matching the age equivalent values with ability scores
for(i in 1:nrow(das_sa)){
  das_sa$rd_ae[i] = ifelse(!is.na(das_sa$rd_as[i]),das_ae_sa[which(das_ae_sa$subtest == 'RecallDesign_Ability' & das_sa$rd_as[i]>=das_ae_sa[,4]
                                                             & das_sa$rd_as[i]<=das_ae_sa[,5]),7],NA)
  das_sa$wd_ae[i] = ifelse(!is.na(das_sa$wd_as[i]),das_ae_sa[which(das_ae_sa$subtest == 'WordDef_Ability' & das_sa$wd_as[i]>=das_ae_sa[,4]
                                                             & das_sa$wd_as[i]<=das_ae_sa[,5]),7],NA)
  das_sa$pc_ae[i] = ifelse(!is.na(das_sa$pc_as[i]),das_ae_sa[which(das_ae_sa$subtest == 'PattConStd_Ability' & das_sa$pc_as[i]>=das_ae_sa[,4]
                                                             & das_sa$pc_as[i]<=das_ae_sa[,5]),7],NA)
  das_sa$pc_alt_ae[i] = ifelse(!is.na(das_sa$pc_alt_as[i]),das_ae_sa[which(das_ae_sa$subtest == 'PattConAlt_Ability' & das_sa$pc_alt_as[i]>=das_ae_sa[,4]
                                                                     & das_sa$pc_alt_as[i]<=das_ae_sa[,5]),7],NA)
  das_sa$mat_ae[i] = ifelse(!is.na(das_sa$mat_as[i]),das_ae_sa[which(das_ae_sa$subtest == 'Matrices_Ability' & das_sa$mat_as[i]>=das_ae_sa[,4]
                                                               & das_sa$mat_as[i]<=das_ae_sa[,5]),7],NA)
  das_sa$vs_ae[i] = ifelse(!is.na(das_sa$vs_as[i]),das_ae_sa[which(das_ae_sa$subtest == 'VerbSim_Ability' & das_sa$vs_as[i]>=das_ae_sa[,4]
                                                             & das_sa$vs_as[i]<=das_ae_sa[,5]),7],NA)
  das_sa$seqr_ae[i] = ifelse(!is.na(das_sa$seqr_as[i]),das_ae_sa[which(das_ae_sa$subtest == 'SeqQuant_Ability' & das_sa$seqr_as[i]>=das_ae_sa[,4]
                                                             & das_sa$seqr_as[i]<=das_ae_sa[,5]),7],NA)
}

##obtaining T-scores for subtests
#merging minage, maxage in das_age table with das_subt_sa table using das2_agetable_pk
das_subt_sa = sqldf("select t1.*,t2.minage,t2.maxage from das_subt_sa as t1 left join
                    das_age as t2 on t1.das2_agetable_pk=t2.das2_agetable_pk")

#obtaining t-scores from das_subt_sa using subtest, age range, and ability score range
for(i in 1:nrow(das_sa)){
  das_sa$rd_t[i] = ifelse(!is.na(das_sa$rd_as[i]),das_subt_sa[which(das_subt_sa$subtest == 'RecallDesign_Ability' & das_sa$rd_as[i] >= das_subt_sa[,6]
                                                              & das_sa$rd_as[i] <= das_subt_sa[,7] & das_sa$age[i] >= das_subt_sa[,8]
                                                              & floor(das_sa$age[i]) <= das_subt_sa[,9]),5],NA)
  das_sa$wd_t[i]= ifelse(!is.na(das_sa$wd_as[i]),das_subt_sa[which(das_subt_sa$subtest == 'WordDef_Ability' & das_sa$wd_as[i] >= das_subt_sa[,6]
                                                             & das_sa$wd_as[i] <= das_subt_sa[,7] & das_sa$age[i] >= das_subt_sa[,8]
                                                             & floor(das_sa$age[i]) <= das_subt_sa[,9]),5],NA)
  das_sa$pc_t[i]= ifelse(!is.na(das_sa$pc_as[i]),das_subt_sa[which(das_subt_sa$subtest == 'PattConStd_Ability' & das_sa$pc_as[i] >= das_subt_sa[,6]
                                                             & das_sa$pc_as[i] <= das_subt_sa[,7] & das_sa$age[i] >= das_subt_sa[,8]
                                                             & floor(das_sa$age[i]) <= das_subt_sa[,9]),5],NA)
  das_sa$pc_alt_t[i]= ifelse(!is.na(das_sa$pc_alt_as[i]),das_subt_sa[which(das_subt_sa$subtest == 'PattConAlt_Ability' & das_sa$pc_alt_as[i] >= das_subt_sa[,6]
                                                             & das_sa$pc_alt_as[i] <= das_subt_sa[,7] & das_sa$age[i] >= das_subt_sa[,8]
                                                             & floor(das_sa$age[i]) <= das_subt_sa[,9]),5],NA)
  das_sa$mat_t[i] = ifelse(!is.na(das_sa$mat_as[i]),das_subt_sa[which(das_subt_sa$subtest == 'Matrices_Ability' & das_sa$mat_as[i] >= das_subt_sa[,6]
                                                            & das_sa$mat_as[i] <= das_subt_sa[,7] & das_sa$age[i] >= das_subt_sa[,8]
                                                            & floor(das_sa$age[i]) <= das_subt_sa[,9]),5],NA)
  das_sa$vs_t[i]= ifelse(!is.na(das_sa$vs_as[i]),das_subt_sa[which(das_subt_sa$subtest == 'VerbSim_Ability' & das_sa$vs_as[i] >= das_subt_sa[,6]
                                                             & das_sa$vs_as[i] <= das_subt_sa[,7] & das_sa$age[i] >= das_subt_sa[,8]
                                                             & floor(das_sa$age[i]) <= das_subt_sa[,9]),5],NA)
  das_sa$seqr_t[i]= ifelse(!is.na(das_sa$seqr_as[i]),das_subt_sa[which(das_subt_sa$subtest == 'SeqQuant_Ability' & das_sa$seqr_as[i] >= das_subt_sa[,6]
                                                             & das_sa$seqr_as[i] <= das_subt_sa[,7] & das_sa$age[i] >= das_subt_sa[,8]
                                                             & floor(das_sa$age[i]) <= das_subt_sa[,9]),5],NA)
}

#calculating sum of t-scores
for(i in 1:nrow(das_sa)){
  das_sa$vb_t[i] = as.numeric(das_sa$wd_t[i]) + as.numeric(das_sa$vs_t[i])
  das_sa$nvr_t[i] = as.numeric(das_sa$mat_t[i]) + as.numeric(das_sa$seqr_t[i])
  das_sa$spatial_t[i] = as.numeric(das_sa$rd_t[i]) + as.numeric(das_sa$pc_t[i])
  if(is.na(das_sa$pc_t[i])){
    das_sa$spatial_t[i] = as.numeric(das_sa$rd_t[i]) + as.numeric(das_sa$pc_alt_t[i])
  }
  das_sa$gca_t[i] = das_sa$vb_t[i] + das_sa$nvr_t[i] + das_sa$spatial_t[i]
  das_sa$snc_t[i] = das_sa$nvr_t[i] + das_sa$spatial_t[i]
}

#subsetting standard score norm table first 
das_ss_sa = subset(das_ss,das2_level == 'SA')

#extracting standard scores
for(i in 1:nrow(das_sa)){
  das_sa$vb_ss[i] = ifelse(!is.na(das_sa$vb_t[i]),das_ss_sa[which(das_sa$vb_t[i] == das_ss_sa[,4]
                                                                     & das_ss_sa$cluster == 'Verbal'),5],NA)
  das_sa$nvr_ss[i] = ifelse(!is.na(das_sa$nvr_t[i]),das_ss_sa[which(das_sa$nvr_t[i] == das_ss_sa[,4]
                                                                       & das_ss_sa$cluster == 'NonverbalReasoning'),5],NA)
  das_sa$spatial_ss[i] = ifelse(!is.na(das_sa$spatial_t[i]),das_ss_sa[which(das_sa$spatial_t[i] == das_ss_sa[,4]
                                                                               & das_ss_sa$cluster == 'Spatial'),5],NA)
  das_sa$gca_ss[i] = ifelse(!is.na(das_sa$gca_t[i]),das_ss_sa[which(das_sa$gca_t[i] == das_ss_sa[,4]
                                                                       & das_ss_sa$cluster == 'GCA'),5],NA)
  das_sa$snc_ss[i] = ifelse(!is.na(das_sa$snc_t[i]),das_ss_sa[which(das_sa$snc_t[i] == das_ss_sa[,4]
                                                                       & das_ss_sa$cluster == 'SNC'),5],NA)
}

##outliers
#extracting columns of interest
das_sa = inserting_prefix_into_variables(das_sa,"das_")
das_sa_outliers = das_sa[,c(1:2,grep("_age$",names(das_sa)),
                            grep("_t$",names(das_sa)),grep("_ss$",names(das_sa)))]
das_sa_outliers = outlier_list(das_sa_outliers)
das_sa$das_outlier_list = das_sa_outliers$outlier_list
#das_sa_outlier_table = sqldf("select id, visit, outlier_list from das_sa where outlier_list != ''")
rm(das_sa_outliers)

#archiving the data
names(das_sa)[which(names(das_sa)=="das_comments")] = "das_clincom"
das_sa_scored= das_sa[,c(1:2,grep("_age$",names(das_sa)),grep("valid",names(das_sa)),
                         grep("clincom",names(das_sa)),grep("missing",names(das_sa)),
                         grep("_raw$",names(das_sa)),which(names(das_sa)=="das_rd_as"):which(names(das_sa)=="das_pc_alt_as"),
                         grep("mat_as$",names(das_sa)),which(names(das_sa)=="das_vs_as"):ncol(das_sa))]
das_sa_scored$das_version = "School Age"
