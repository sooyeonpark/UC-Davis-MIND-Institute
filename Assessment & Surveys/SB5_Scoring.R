sb5 = sqlFetch(new_con,"Stanford-Binet 5",stringsAsFactors=F)
sb5 = id_visit_changing_into_char(sb5)
sb5_entry_flag = entry_flag(sb5,'sb5')
sb5 = subset(sb5,entry_status==2)
sb5 = removing_prefix(sb5,'sb5_')

#calculating age
sb5 = fxage(sb5,'id','date')

#missing data analysis
sb5 = comment_missing_data(sb5,list(grep("_raw$",names(sb5))),list('','','','',''))
sb5[which(sb5$missing_items_comment != ''),"missing_items_comment"] = "Raw score(s) missing in the data entry;"
for(i in 1:nrow(sb5)){
  sb5$missing_items_comment[i] = ifelse(sb5$version[i]=="Routing" & !is.na(sb5$nv_fr_raw[i]) & !is.na(sb5$v_kn_raw[i]),"",sb5$missing_items_comment[i])
}

#norm tables
sb5_ae = read_excel("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/sb5_ae.xlsx")
sb5_prorated = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/sb5_Prorated.csv", stringsAsFactors=FALSE)
sb5_scaled = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/sb5_ScaledScore.csv", stringsAsFactors=FALSE)
sb5_std = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/sb5_StdScore.csv", stringsAsFactors=FALSE)

#obtaining ae for each raw scores
sb5 = summing_items_per_row(sb5,list(grep("_fr_raw$",names(sb5)),grep("_kn_raw$",names(sb5)),
                                     grep("_qr_raw$",names(sb5)),grep("_vs_raw$",names(sb5)),
                                     grep("_wm_raw$",names(sb5)),grep("^nv_[a-z]{2}_raw$",names(sb5)),
                                     grep("^v_[a-z]{2}_raw$",names(sb5)),grep("_raw$",names(sb5))),
                            list('fr_raw_total','kn_raw_total','qr_raw_total','vs_raw_total',
                                 'wm_raw_total','nonverbal_total','verbal_total','full_scale_total'),F)
for(i in 1:nrow(sb5)){
  sb5$fr_ae[i] = sb5_ae[which(sb5_ae$scale=="Fluid Reasoning"
                        & sb5_ae$raw_total==sb5$fr_raw_total[i]),"ae"]
  sb5$kn_ae[i] = sb5_ae[which(sb5_ae$scale=="Knowledge"
                        & sb5_ae$raw_total==sb5$kn_raw_total[i]),"ae"]
  sb5$qr_ae[i] = sb5_ae[which(sb5_ae$scale=="Quantitative Reasoning"
                        & sb5_ae$raw_total==sb5$qr_raw_total[i]),"ae"]
  sb5$vs_ae[i] = sb5_ae[which(sb5_ae$scale=="Visual Spatial Processing"
                        & sb5_ae$raw_total==sb5$vs_raw_total[i]),"ae"]
  sb5$wm_ae[i] = sb5_ae[which(sb5_ae$scale=="Working Memory"
                        & sb5_ae$raw_total==sb5$wm_raw_total[i]),"ae"]
  sb5$nv_ae[i] = sb5_ae[which(sb5_ae$scale=="Nonverbal"
                        & sb5_ae$raw_total==sb5$nonverbal_total[i]),"ae"]
  sb5$v_ae[i] = sb5_ae[which(sb5_ae$scale=="Verbal"
                       & sb5_ae$raw_total==sb5$verbal_total[i]),"ae"]
  sb5$fs_ae[i] = sb5_ae[which(sb5_ae$scale=="Full Scale"
                        & sb5_ae$raw_total==sb5$full_scale_total[i]),"ae"]
  
}

#putting in scaled scores
sb5$nv_fr_scaled = NA
sb5$nv_kn_scaled=NA
sb5$nv_qr_scaled=NA
sb5$nv_vs_scaled=NA
sb5$nv_wm_scaled=NA
sb5$v_fr_scaled=NA
sb5$v_kn_scaled=NA
sb5$v_qr_scaled=NA
sb5$v_vs_scaled=NA
sb5$v_wm_scaled=NA
for(i in 1:nrow(sb5)){
  #clarifying which age to use from norm table
  age_list = sb5_scaled$age[which(sb5_scaled$age<=sb5$age[i] & sb5_scaled$domain=="Nonverbal")] #Nonverbal nor Verbal don't matter
  correct_age = age_list[length(age_list)]
  #routing only version -> nv_fr & v_kn
  if(sb5$version[i]=="Routing"){
    nv_fr_index = which(sb5_scaled$age==correct_age & sb5_scaled$domain=="Nonverbal" & sb5_scaled$fr<=sb5$nv_fr_raw[i])
    nv_fr_index = nv_fr_index[length(nv_fr_index)]
    sb5$nv_fr_scaled[i] = ifelse(length(nv_fr_index)==0,NA,sb5_scaled[nv_fr_index,"scaled_score"])
    v_kn_index = which(sb5_scaled$age==correct_age & sb5_scaled$domain=="Verbal" & sb5_scaled$kn<=sb5$v_kn_raw[i])
    v_kn_index = v_kn_index[length(v_kn_index)]
    sb5$v_kn_scaled[i] = ifelse(length(v_kn_index)==0,NA,sb5_scaled[v_kn_index,"scaled_score"])
  }
  #full battery version
  else if(sb5$version[i]=="Full Battery"){
    nv_fr_index = which(sb5_scaled$age==correct_age & sb5_scaled$domain=="Nonverbal" & sb5_scaled$fr<=sb5$nv_fr_raw[i])
    nv_fr_index = nv_fr_index[length(nv_fr_index)]
    sb5$nv_fr_scaled[i] = ifelse(length(nv_fr_index)==0,NA,sb5_scaled[nv_fr_index,"scaled_score"])
    nv_kn_index = which(sb5_scaled$age==correct_age & sb5_scaled$domain=="Nonverbal" & sb5_scaled$kn<=sb5$nv_kn_raw[i])
    nv_kn_index = nv_kn_index[length(nv_kn_index)]
    sb5$nv_kn_scaled[i] = ifelse(length(nv_kn_index)==0,NA,sb5_scaled[nv_kn_index,"scaled_score"])
    nv_qr_index = which(sb5_scaled$age==correct_age & sb5_scaled$domain=="Nonverbal" & sb5_scaled$qr<=sb5$nv_qr_raw[i])
    nv_qr_index = nv_qr_index[length(nv_qr_index)]
    sb5$nv_qr_scaled[i] = ifelse(length(nv_qr_index)==0,NA,sb5_scaled[nv_qr_index,"scaled_score"])
    nv_vs_index = which(sb5_scaled$age==correct_age & sb5_scaled$domain=="Nonverbal" & sb5_scaled$vs<=sb5$nv_vs_raw[i])
    nv_vs_index = nv_vs_index[length(nv_vs_index)]
    sb5$nv_vs_scaled[i] = ifelse(length(nv_vs_index)==0,NA,sb5_scaled[nv_vs_index,"scaled_score"])
    nv_wm_index = which(sb5_scaled$age==correct_age & sb5_scaled$domain=="Nonverbal" & sb5_scaled$wm<=sb5$nv_wm_raw[i])
    nv_wm_index = nv_wm_index[length(nv_wm_index)]
    sb5$nv_wm_scaled[i] = ifelse(length(nv_wm_index)==0,NA,sb5_scaled[nv_wm_index,"scaled_score"])
    v_fr_index = which(sb5_scaled$age==correct_age & sb5_scaled$domain=="Verbal" & sb5_scaled$fr<=sb5$v_fr_raw[i])
    v_fr_index = v_fr_index[length(v_fr_index)]
    sb5$v_fr_scaled[i] = ifelse(length(v_fr_index)==0,NA,sb5_scaled[v_fr_index,"scaled_score"])
    v_kn_index = which(sb5_scaled$age==correct_age & sb5_scaled$domain=="Verbal" & sb5_scaled$kn<=sb5$v_kn_raw[i])
    v_kn_index = v_kn_index[length(v_kn_index)]
    sb5$v_kn_scaled[i] = ifelse(length(v_kn_index)==0,NA,sb5_scaled[v_kn_index,"scaled_score"])
    v_qr_index = which(sb5_scaled$age==correct_age & sb5_scaled$domain=="Verbal" & sb5_scaled$qr<=sb5$v_qr_raw[i])
    v_qr_index = v_qr_index[length(v_qr_index)]
    sb5$v_qr_scaled[i] = ifelse(length(v_qr_index)==0,NA,sb5_scaled[v_qr_index,"scaled_score"])
    v_vs_index = which(sb5_scaled$age==correct_age & sb5_scaled$domain=="Verbal" & sb5_scaled$vs<=sb5$v_vs_raw[i])
    v_vs_index = v_vs_index[length(v_vs_index)]
    sb5$v_vs_scaled[i] = ifelse(length(v_vs_index)==0,NA,sb5_scaled[v_vs_index,"scaled_score"])
    v_wm_index = which(sb5_scaled$age==correct_age & sb5_scaled$domain=="Verbal" & sb5_scaled$wm<=sb5$v_wm_raw[i])
    v_wm_index = v_wm_index[length(v_wm_index)]
    sb5$v_wm_scaled[i] = ifelse(length(v_wm_index)==0,NA,sb5_scaled[v_wm_index,"scaled_score"])
  }
}

#summing up scaled scores
sb5 = summing_items_per_row(sb5,list(grep("fr_scaled$",names(sb5)),grep("kn_scaled$",names(sb5)),
                                     grep("qr_scaled$",names(sb5)),grep("vs_scaled$",names(sb5)),
                                     grep("wm_scaled$",names(sb5)),c("nv_fr_scaled","v_kn_scaled"),
                                     grep("^nv_[a-z]{2}_scaled$",names(sb5)),grep("^v_[a-z]{2}_scaled$",names(sb5))),
                            list('fr_scaled_sum','kn_scaled_sum','qr_scaled_sum','vs_scaled_sum',
                                 'wm_scaled_sum','abiq_scaled_sum','nviq_scaled_sum','viq_scaled_sum'),F)

#adjusting and commenting for prorated scores & obtaining std,percentile,ci for scaled sums
for(i in 1:nrow(sb5)){
  #adjusting prorated scores
  nv_raw_na_count = length(which(is.na(sb5[i,grep("nv_[a-z]{2}_raw$",names(sb5))])))
  v_raw_na_count = length(which(is.na(sb5[i,grep("v_[a-z]{2}_raw$",names(sb5))])))
  if(sb5$version[i] == "full battery" & (nv_raw_na_count>0| v_raw_na_count>0)){
    sb5$nviq_scaled_sum[i] = ifelse(nv_raw_na_count==2,sb5_prorated[sb5_prorated$scaled_sum_3==sb5$nviq_scaled_sum[i],"prorated_score"],
                                    ifelse(nv_raw_na_count==1,sb5_prorated[sb5_prorated$scaled_sum_4==sb5$nviq_scaled_sum[i],"prorated_score"],NA))
    sb5$viq_scaled_sum[i] = ifelse(v_raw_na_count==2,sb5_prorated[sb5_prorated$scaled_sum_3==sb5$viq_scaled_sum[i],"prorated_score"],
                                   ifelse(v_raw_na_count==1,sb5_prorated[sb5_prorated$scaled_sum_4==sb5$viq_scaled_sum[i],"prorated_score"],NA))
    #if the score is prorated, let's mention that in valid var
    sb5$valid[i] = paste0(sb5$valid[i],"; scores prorated;")
  }
  #obaining fsiq scaled sum
  sb5$fsiq_scaled_sum[i] = sb5$nviq_scaled_sum[i] + sb5$viq_scaled_sum[i]
  #obtaining std,percentile,ci
  fr_index = which(sb5_std$scale=="Fluid Reasoning" & sb5_std$scaled_sum==sb5$fr_scaled_sum[i])
  sb5$fr_ss[i] = ifelse(length(fr_index)==0,NA,sb5_std[fr_index,"ss"])
  sb5$fr_percentile[i] = ifelse(length(fr_index)==0,'',sb5_std[fr_index,"percentile"])
  sb5$fr_ci_95[i] = ifelse(length(fr_index)==0,'',sb5_std[fr_index,"ci_95"])
  kn_index = which(sb5_std$scale=="Knowledge" & sb5_std$scaled_sum==sb5$kn_scaled_sum[i])
  sb5$kn_ss[i] = ifelse(length(kn_index)==0,NA,sb5_std[kn_index,"ss"])
  sb5$kn_percentile[i] = ifelse(length(kn_index)==0,'',sb5_std[kn_index,"percentile"])
  sb5$kn_ci_95[i] = ifelse(length(kn_index)==0,'',sb5_std[kn_index,"ci_95"])
  qr_index = which(sb5_std$scale=="Quantitative Reasoning" & sb5_std$scaled_sum==sb5$qr_scaled_sum[i])
  sb5$qr_ss[i] = ifelse(length(qr_index)==0,NA,sb5_std[qr_index,"ss"])
  sb5$qr_percentile[i] = ifelse(length(qr_index)==0,'',sb5_std[qr_index,"percentile"])
  sb5$qr_ci_95[i] = ifelse(length(qr_index)==0,'',sb5_std[qr_index,"ci_95"])
  vs_index = which(sb5_std$scale=="Visual Spatial Processing" & sb5_std$scaled_sum==sb5$vs_scaled_sum[i])
  sb5$vs_ss[i] = ifelse(length(vs_index)==0,NA,sb5_std[vs_index,"ss"])
  sb5$vs_percentile[i] = ifelse(length(vs_index)==0,'',sb5_std[vs_index,"percentile"])
  sb5$vs_ci_95[i] = ifelse(length(vs_index)==0,'',sb5_std[vs_index,"ci_95"])
  wm_index = which(sb5_std$scale=="Working Memory" & sb5_std$scaled_sum==sb5$wm_scaled_sum[i])
  sb5$wm_ss[i] = ifelse(length(wm_index)==0,NA,sb5_std[wm_index,"ss"])
  sb5$wm_percentile[i] = ifelse(length(wm_index)==0,'',sb5_std[wm_index,"percentile"])
  sb5$wm_ci_95[i] = ifelse(length(wm_index)==0,'',sb5_std[wm_index,"ci_95"])
  abiq_index = which(sb5_std$scale=="Abbreviated Battery" & sb5_std$scaled_sum==sb5$abiq_scaled_sum[i])
  sb5$abiq[i] = ifelse(length(abiq_index)==0,NA,sb5_std[abiq_index,"ss"])
  sb5$abiq_percentile[i] = ifelse(length(abiq_index)==0,'',sb5_std[abiq_index,"percentile"])
  sb5$abiq_ci_95[i] = ifelse(length(abiq_index)==0,'',sb5_std[abiq_index,"ci_95"])
  nviq_index = which(sb5_std$scale=="Nonverbal" & sb5_std$scaled_sum==sb5$nviq_scaled_sum[i])
  sb5$nviq[i] = ifelse(length(nviq_index)==0,NA,sb5_std[nviq_index,"ss"])
  sb5$nviq_percentile[i] = ifelse(length(nviq_index)==0,'',sb5_std[nviq_index,"percentile"])
  sb5$nviq_ci_95[i] = ifelse(length(nviq_index)==0,'',sb5_std[nviq_index,"ci_95"])
  viq_index = which(sb5_std$scale=="Verbal" & sb5_std$scaled_sum==sb5$viq_scaled_sum[i])
  sb5$viq[i] = ifelse(length(viq_index)==0,NA,sb5_std[viq_index,"ss"])
  sb5$viq_percentile[i] = ifelse(length(viq_index)==0,'',sb5_std[viq_index,"percentile"])
  sb5$viq_ci_95[i] = ifelse(length(viq_index)==0,'',sb5_std[viq_index,"ci_95"])
  fsiq_index = which(sb5_std$scale=="Full Scale" & sb5_std$scaled_sum==sb5$fsiq_scaled_sum[i])
  sb5$fsiq[i] = ifelse(length(fsiq_index)==0,NA,sb5_std[fsiq_index,"ss"])
  sb5$fsiq_percentile[i] = ifelse(length(fsiq_index)==0,'',sb5_std[fsiq_index,"percentile"])
  sb5$fsiq_ci_95[i] = ifelse(length(fsiq_index)==0,'',sb5_std[fsiq_index,"ci_95"])
}
sb5$valid = gsub("^; ","",sb5$valid)

#putting back prefix
sb5 = inserting_prefix_into_variables(sb5,'sb5_')

#orphaned/duplicate data
sb5_orphaned_data = orphaned_data_consolidate(sb5)
sb5 = orphaned_data_remove(sb5)
sb5_duplicate_data = duplicate_data_consolidate(sb5,'sb5_age')
sb5 = duplicate_data_remove(sb5,'sb5_age')

#outliers -> for full-scale:scaled_sum,_ss vars, for routing:abiq_scaled_sum
sb5_outliers = sb5[,c(1:2,grep("_age$",names(sb5)),grep("_scaled_sum$",names(sb5)),
                      grep("_ss$",names(sb5)),grep("iq$",names(sb5)))]
sb5_outliers = outlier_list(sb5_outliers)
sb5$sb5_outlier_list = sb5_outliers$outlier_list
rm(sb5_outliers)

#archiving the data
sb5_scored = sb5[,c(1:2,grep("_age$",names(sb5)):grep("_comment$",names(sb5)),
                    grep("_raw$",names(sb5)),grep("fr_raw_total$",names(sb5)):ncol(sb5))]
for(j in grep("_ae$",names(sb5_scored))){
  sb5_scored[,j] = as.character(sb5_scored[,j])
}
write.csv(sb5_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/sb5_scored.csv",row.names=F)

#clean up
rm(abiq_index,viq_index,nviq_index,fsiq_index,sb5_ae,sb5_prorated,sb5_scaled,sb5_std,nv_fr_index,nv_kn_index,
   nv_qr_index,nv_vs_index,nv_wm_index,v_fr_index,v_kn_index,v_qr_index,v_vs_index,
   v_wm_index,age_list,correct_age,fr_index,kn_index,qr_index,vs_index,wm_index,
   nv_raw_na_count,v_raw_na_count)
