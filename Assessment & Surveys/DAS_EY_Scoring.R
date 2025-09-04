#pulling out table
das_ey = sqlQuery(new_con, "select * from DAS_Early_Years;",stringsAsFactors=F)

#id, visit into characters, removing incomplete data entries, etc
das_ey = id_visit_changing_into_char(das_ey)
das_ey_entry_flag = entry_flag(das_ey,'das-EY')
das_ey = subset(das_ey, entry_status == 2)
if(!is.null(das_ey_entry_flag)){
  das_ey = rbind(das_ey,das_ey_entry_flag[,-ncol(das_ey_entry_flag)])
}
names(das_ey) = gsub("dasii_eyr_","",names(das_ey))
names(das_ey) = gsub("sar_","",names(das_ey))

#norm tables
con <- odbcConnectAccess2007("S:/MIND/RESEARCH/APP Behavior Data/Local Data Entry/APP Data entry Aug 8 2011.mdb")
das_as = sqlQuery(con, "select * from scr_norms_DAS2_ability_scores;",stringsAsFactors=F)
das_as_ey = subset(das_as, vers == "EY")
das_as_sa = subset(das_as, vers == "SA")
das_ae = sqlQuery(con, "select * from scr_norms_DAS2_AgeEquiv;",stringsAsFactors=F)
das_ae_ey = subset(das_ae, vers == "EY")
das_ae_sa = subset(das_ae, vers == "SA")
das_age = sqlQuery(con, "select * from scr_norms_DAS2_AgeTable;",stringsAsFactors=F)
das_ss = sqlQuery(con, "select * from scr_norms_das2_StdScores;",stringsAsFactors=F)
das_ss_ey = subset(das_ss, das2_level == "EYL")
das_subt = sqlQuery(con, "select * from scr_norms_DAS2_Subtest_Tscores;",stringsAsFactors=F)
das_subt_ey = subset(das_subt, vers == "EY")
das_subt_sa = subset(das_subt, vers == "SA")
odbcClose(con) #clean up by closing connection

#calculating age
das_ey = fxage(das_ey,'id','date')

#missing data analysis
das_raw_score_vars = paste0(c('vcom','psim','nvoc','pcon','mat','copy'),'_raw')
das_ey = comment_missing_data(das_ey,list(das_raw_score_vars),list('raw score'))
das_ey[which(das_ey$missing_items_comment != ''),"missing_items_comment"] = "Raw score(s) missing in the data entry;"

#getting ability score
das_ey[which(das_ey$pcon_alt_raw == -9),"pcon_alt_raw"] = NA
das_ey[which(das_ey$mat_raw == -9),"mat_raw"] = NA
for(i in 1:nrow(das_ey)){
  das_ey$vc_as[i] = ifelse(!is.na(das_ey$vcom_raw[i]),das_as_ey[which(
    das_as_ey$subtest == 'VerbComp' & das_ey$vcom_raw[i] == das_as_ey[,6] & das_ey$vcom_isa[i] == das_as_ey[,4])
    ,7],NA)
  das_ey$ps_as[i] = ifelse(!is.na(das_ey$psim_raw[i]),das_as_ey[which(
    das_as_ey$subtest == 'PictSim' & das_ey$psim_raw[i] == das_as_ey[,6] & das_ey$psim_isa[i] == das_as_ey[,4])
    ,7],NA)
  das_ey$nv_as[i] = ifelse(!is.na(das_ey$nvoc_raw[i]),das_as_ey[which(
    das_as_ey$subtest == 'NameVoc' & das_ey$nvoc_raw[i] == das_as_ey[,6] & das_ey$nvoc_isa[i] == das_as_ey[,4])
    ,7],NA)
  das_ey$pc_as[i] = ifelse(!is.na(das_ey$pcon_raw[i]),das_as_ey[which(
    das_as_ey$subtest == 'pattconstd' & das_ey$pcon_raw[i] == das_as_ey[,6] & das_ey$pcon_isa[i] == das_as_ey[,4])
    ,7],NA)
  das_ey$pc_alt_as[i] = ifelse(!is.na(das_ey$pcon_alt_raw[i]),das_as_ey[which(
    das_as_ey$subtest == 'pattconalt' & das_ey$pcon_alt_raw[i] == das_as_ey[,6] & das_ey$pcon_alt_isa[i] == das_as_ey[,4])
    ,7],NA)
  das_ey$mat_as[i] = ifelse(!is.na(das_ey$mat_raw[i]),das_as_ey[which(
    das_as_ey$subtest == 'Matrices' & das_ey$mat_raw[i] == das_as_ey[,6] & das_ey$mat_isa[i] == das_as_ey[,4])
    ,7],NA)
  das_ey$cp_as[i] = ifelse(!is.na(das_ey$copy_raw[i]),das_as_ey[which(
    das_as_ey$subtest == 'Copying' & das_ey$copy_raw[i] == das_as_ey[,6] & das_ey$copy_isa[i] == das_as_ey[,4])
    ,7],NA)
}

##obtaining age equivalents using for loop
#changing some values in the norm table first tho
das_ae_ey$age_eq[which(das_ae_ey$age_eq==28)] = NA
das_ae_ey$age_eq[which(das_ae_ey$age_eq==-997)] = NA

#matching the age equivalent values with ability scores
for(i in 1:nrow(das_ey)){
  das_ey$vc_ae[i] = ifelse(!is.na(das_ey$vc_as[i]),das_ae_ey[which(das_ae_ey$subtest == 'VerbComp_Ability' & das_ey$vc_as[i]>=das_ae_ey[,4]
                                 & das_ey$vc_as[i]<=das_ae_ey[,5]),7],NA)
  das_ey$ps_ae[i] = ifelse(!is.na(das_ey$ps_as[i]),das_ae_ey[which(das_ae_ey$subtest == 'PictSim_Ability' & das_ey$ps_as[i]>=das_ae_ey[,4]
                                 & das_ey$ps_as[i]<=das_ae_ey[,5]),7],NA)
  das_ey$nv_ae[i] = ifelse(!is.na(das_ey$nv_as[i]),das_ae_ey[which(das_ae_ey$subtest == 'NameVoc_Ability' & das_ey$nv_as[i]>=das_ae_ey[,4]
                                 & das_ey$nv_as[i]<=das_ae_ey[,5]),7],NA)
  das_ey$pc_ae[i] = ifelse(!is.na(das_ey$pc_as[i]),das_ae_ey[which(das_ae_ey$subtest == 'PattConStd_Ability' & das_ey$pc_as[i]>=das_ae_ey[,4]
                                 & das_ey$pc_as[i]<=das_ae_ey[,5]),7],NA)
  das_ey$pc_alt_ae[i] = ifelse(!is.na(das_ey$pc_alt_as[i]),das_ae_ey[which(das_ae_ey$subtest == 'PattConAlt_Ability' & das_ey$pc_alt_as[i]>=das_ae_ey[,4]
                                 & das_ey$pc_alt_as[i]<=das_ae_ey[,5]),7],NA)
  das_ey$mat_ae[i] = ifelse(!is.na(das_ey$mat_as[i]),das_ae_ey[which(das_ae_ey$subtest == 'Matrices_Ability' & das_ey$mat_as[i]>=das_ae_ey[,4]
                                 & das_ey$mat_as[i]<=das_ae_ey[,5]),7],NA)
  das_ey$cp_ae[i] = ifelse(!is.na(das_ey$cp_as[i]),das_ae_ey[which(das_ae_ey$subtest == 'Copying_Ability' & das_ey$cp_as[i]>=das_ae_ey[,4]
                                 & das_ey$cp_as[i]<=das_ae_ey[,5]),7],NA)
}

##obtaining T-scores for subtests
#merging minage, maxage in das_age table with das_subt_ey table using das2_agetable_pk
das_subt_ey = sqldf("select t1.*,t2.minage,t2.maxage from das_subt_ey as t1 left join
                    das_age as t2 on t1.das2_agetable_pk=t2.das2_agetable_pk")

#obtaining t-scores from das_subt_ey using subtest, age range, and ability score range
for(i in 1:nrow(das_ey)){
  if(floor(das_ey$age[i]) < 108){
    das_ey$vc_t[i] = ifelse(!is.na(das_ey$vc_as[i]),das_subt_ey[which(das_subt_ey$subtest == 'VerbComp_Ability' & das_ey$vc_as[i] >= das_subt_ey[,6]
                                  & das_ey$vc_as[i] <= das_subt_ey[,7] & das_ey$age[i] >= das_subt_ey[,8]
                                  & floor(das_ey$age[i]) <= das_subt_ey[,9]),5],NA)
    das_ey$ps_t[i]= ifelse(!is.na(das_ey$ps_as[i]),das_subt_ey[which(das_subt_ey$subtest == 'PictSim_Ability' & das_ey$ps_as[i] >= das_subt_ey[,6]
                                  & das_ey$ps_as[i] <= das_subt_ey[,7] & das_ey$age[i] >= das_subt_ey[,8]
                                  & floor(das_ey$age[i]) <= das_subt_ey[,9]),5],NA)
    das_ey$nv_t[i]= ifelse(!is.na(das_ey$nv_as[i]),das_subt_ey[which(das_subt_ey$subtest == 'NameVoc_Ability' & das_ey$nv_as[i] >= das_subt_ey[,6]
                                  & das_ey$nv_as[i] <= das_subt_ey[,7] & das_ey$age[i] >= das_subt_ey[,8]
                                  & floor(das_ey$age[i]) <= das_subt_ey[,9]),5],NA)
    das_ey$pc_t[i]= ifelse(!is.na(das_ey$pc_as[i]),das_subt_ey[which(das_subt_ey$subtest == 'PattConStd_Ability' & das_ey$pc_as[i] >= das_subt_ey[,6]
                                  & das_ey$pc_as[i] <= das_subt_ey[,7] & das_ey$age[i] >= das_subt_ey[,8]
                                  & floor(das_ey$age[i]) <= das_subt_ey[,9]),5],NA)
    das_ey$pc_alt_t[i]= ifelse(!is.na(das_ey$pc_alt_as[i]),das_subt_ey[which(das_subt_ey$subtest == 'PattConAlt_Ability' & das_ey$pc_alt_as[i] >= das_subt_ey[,6]
                                  & das_ey$pc_alt_as[i] <= das_subt_ey[,7] & das_ey$age[i] >= das_subt_ey[,8]
                                  & floor(das_ey$age[i]) <= das_subt_ey[,9]),5],NA)
    das_ey$mat_t[i] = ifelse(!is.na(das_ey$mat_as[i]),das_subt_ey[which(das_subt_ey$subtest == 'Matrices_Ability' & das_ey$mat_as[i] >= das_subt_ey[,6]
                                  & das_ey$mat_as[i] <= das_subt_ey[,7] & das_ey$age[i] >= das_subt_ey[,8]
                                  & floor(das_ey$age[i]) <= das_subt_ey[,9]),5],NA)
    das_ey$cp_t[i]= ifelse(!is.na(das_ey$cp_as[i]),das_subt_ey[which(das_subt_ey$subtest == 'Copying_Ability' & das_ey$cp_as[i] >= das_subt_ey[,6]
                                  & das_ey$cp_as[i] <= das_subt_ey[,7] & das_ey$age[i] >= das_subt_ey[,8]
                                  & floor(das_ey$age[i]) <= das_subt_ey[,9]),5],NA)
  }
  else{
    das_ey$vc_t[i] = NA
    das_ey$ps_t[i] = NA
    das_ey$nv_t[i] = NA
    das_ey$pc_t[i] = NA
    das_ey$pc_alt_t[i] = NA
    das_ey$mat_t[i] = NA
    das_ey$cp_t[i] = NA
  }
}

#calculating sum of t-scores
for(i in 1:nrow(das_ey)){
  das_ey$vb_t[i] = as.numeric(das_ey$vc_t[i]) + as.numeric(das_ey$nv_t[i])
  das_ey$nvr_t[i] = as.numeric(das_ey$ps_t[i]) + as.numeric(das_ey$mat_t[i])
  das_ey$spatial_t[i] = as.numeric(das_ey$pc_t[i]) + as.numeric(das_ey$cp_t[i])
  das_ey$gca_t[i] = das_ey$vb_t[i] + das_ey$nvr_t[i] + das_ey$spatial_t[i]
  das_ey$snc_t[i] = das_ey$nvr_t[i] + das_ey$spatial_t[i]
}

#obtaining standard scores using sum of t scores using sqldf
#subsetting standard score norm table first 
das_ss_upper = subset(das_ss,das2_level == 'EYU')

#extracting standard scores
for(i in 1:nrow(das_ey)){
  das_ey$vb_ss[i] = ifelse(!is.na(das_ey$vb_t[i]),das_ss_upper[which(das_ey$vb_t[i] == das_ss_upper[,4]
                                                            & das_ss_upper$cluster == 'Verbal'),5],NA)
  das_ey$nvr_ss[i] = ifelse(!is.na(das_ey$nvr_t[i]),das_ss_upper[which(das_ey$nvr_t[i] == das_ss_upper[,4]
                                                            & das_ss_upper$cluster == 'NonverbalReasoning'),5],NA)
  das_ey$spatial_ss[i] = ifelse(!is.na(das_ey$spatial_t[i]),das_ss_upper[which(das_ey$spatial_t[i] == das_ss_upper[,4]
                                                            & das_ss_upper$cluster == 'Spatial'),5],NA)
  das_ey$gca_ss[i] = ifelse(!is.na(das_ey$gca_t[i]),das_ss_upper[which(das_ey$gca_t[i] == das_ss_upper[,4]
                                                            & das_ss_upper$cluster == 'GCA'),5],NA)
  das_ey$snc_ss[i] = ifelse(!is.na(das_ey$snc_t[i]),das_ss_upper[which(das_ey$snc_t[i] == das_ss_upper[,4]
                                                            & das_ss_upper$cluster == 'SNC'),5],NA)
  
}

##outliers
#extracting columns of interest
das_ey = inserting_prefix_into_variables(das_ey,"das_")
das_ey_outliers = das_ey[,c(1:2,30,grep("_t$",names(das_ey),grep("_ss$",names(das_ey))))]
das_ey_outliers = outlier_list(das_ey_outliers)
das_ey$das_outlier_list = das_ey_outliers$outlier_list
#das_ey_outlier_table = sqldf("select id, visit, outlier_list from das_ey_outliers where outlier_list != ''")
rm(das_ey_outliers)

#archiving the data
das_ey_scored = das_ey[,c(1:2,grep("_age$",names(das_ey)),grep("valid",names(das_ey)),grep("clincom",names(das_ey)),
                          grep("missing",names(das_ey)),grep("_raw$",names(das_ey)),grep("vc_as$",names(das_ey)):grep("cp_as$",names(das_ey)),
                          grep("mat_as$",names(das_ey)),grep("pc_as$",names(das_ey)):grep("outlier_list$",names(das_ey)))]
das_ey_scored$das_version = "Early Years"
