#importing norm tables
wasi_t = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/wasi_t_scores.csv",stringsAsFactors = F)
names(wasi_t)[1] = "age_months"
wasi_fsiq_composite = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/wasi_composite_scores_fsiq4.csv",stringsAsFactors = F)
wasi_vci_composite = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/wasi_composite_scores_vci.csv",stringsAsFactors = F)

#modifying t score norm table
wasi_t = wasi_t[,c("t_score","age_months","bd","vc","mr","si")]
wasi_t_revised = data.frame(wasi_t[,1])
names(wasi_t_revised) = 't_score'
wasi_t_revised = modifying_vabs_norm_tables(wasi_t,wasi_t_revised)

#querying the data
wasi = sqlQuery(con4,"select * from WASI;",stringsAsFactors=F)

#initial manipulations
wasi = id_visit_changing_into_char(wasi)
wasi_entry_flag = entry_flag(wasi,'wasi')
wasi = subset(wasi,entry_status==2)
wasi = removing_prefix(wasi,'wasi_')

#calculate age
wasi = fxage(wasi,'id','date')

#missing data analysis
wasi = comment_missing_data(wasi,list(names(wasi)[5:8]),list('','','',''))
wasi[wasi$missing_items_comment != '',"missing_items_comment"] = "Raw score(s) missing in the data entry"

#obtain t scores
for(i in 1:nrow(wasi)){
  wasi$sim_t[i] = ifelse(is.na(wasi$sim[i]),NA,wasi_t_revised[which(floor(wasi$age[i])>=wasi_t_revised$age_months_bottom
                                                                    & floor(wasi$age[i])<=wasi_t_revised$age_months_top
                                                                    & wasi$sim[i]>=wasi_t_revised$si_bottom
                                                                    & wasi$sim[i]<=wasi_t_revised$si_top),"t_score"])
  wasi$matrix_t[i] = ifelse(is.na(wasi$matrix[i]),NA,wasi_t_revised[which(floor(wasi$age[i])>=wasi_t_revised$age_months_bottom
                                                                    & floor(wasi$age[i])<=wasi_t_revised$age_months_top
                                                                    & wasi$matrix[i]>=wasi_t_revised$mr_bottom
                                                                    & wasi$matrix[i]<=wasi_t_revised$mr_top),"t_score"])
  wasi$vocab_t[i] = ifelse(is.na(wasi$vocab[i]),NA,wasi_t_revised[which(floor(wasi$age[i])>=wasi_t_revised$age_months_bottom
                                                                    & floor(wasi$age[i])<=wasi_t_revised$age_months_top
                                                                    & wasi$vocab[i]>=wasi_t_revised$vc_bottom
                                                                    & wasi$vocab[i]<=wasi_t_revised$vc_top),"t_score"])
  wasi$block_t[i] = ifelse(is.na(wasi$block[i]),NA,wasi_t_revised[which(floor(wasi$age[i])>=wasi_t_revised$age_months_bottom
                                                                    & floor(wasi$age[i])<=wasi_t_revised$age_months_top
                                                                    & wasi$block[i]>=wasi_t_revised$bd_bottom
                                                                    & wasi$block[i]<=wasi_t_revised$bd_top),"t_score"])
}

#summing t scores
wasi = summing_items_per_row(wasi,list(c('vocab_t','sim_t'),c('block_t','matrix_t'),grep("_t$",names(wasi),value=T),c('vocab_t','matrix_t')),list('vci_t','pri_t','fsiq4_t','fsiq2_t'),F)

#obtaing composite scores
for(i in 1:nrow(wasi)){
  wasi$vci_composite[i] = ifelse(is.na(wasi$vci_t[i]),NA,wasi_vci_composite[wasi$vci_t[i]==wasi_vci_composite$t_score_sum,"vci"])
  wasi$fsiq4_composite[i] = ifelse(is.na(wasi$fsiq4_t[i]),NA,wasi_fsiq_composite[wasi$fsiq4_t[i]==wasi_fsiq_composite$t_score_sum,"fsiq4"])
}

#putting back prefix
wasi = inserting_prefix_into_variables(wasi,'wasi_')

#orphaned/duplicate data
wasi_orphaned_data = orphaned_data_consolidate(wasi)
wasi = orphaned_data_remove(wasi)
wasi_duplicate_data = duplicate_data_consolidate(wasi,'wasi_age')
wasi = duplicate_data_remove(wasi,'wasi_age')

#outliers
wasi_outliers = wasi[,c(1:2,grep("_age$",names(wasi)),grep("_t$",names(wasi)),grep("_composite$",names(wasi)))]
wasi_outliers = outlier_list(wasi_outliers)
wasi$wasi_outlier_list = wasi_outliers$outlier_list

#archiving the data
wasi_scored = wasi[,c(1:2,grep("_age$",names(wasi)),grep("missing",names(wasi)),
                      grep("block",names(wasi)),grep("vocab",names(wasi)),grep("matrix",names(wasi)),
                      grep("sim",names(wasi)),grep("vci_t$",names(wasi)):grep("fsiq2_t$",names(wasi)),
                      grep("_composite$",names(wasi)),ncol(wasi))]
write.csv(wasi_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived_staar/wasi_scored.csv",row.names = F)

#clean up
rm(wasi_fsiq_composite,wasi_t,wasi_vci_composite,wasi_t_revised,wasi_outliers)
