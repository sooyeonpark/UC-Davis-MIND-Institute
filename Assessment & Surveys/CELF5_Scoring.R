celf5 = sqlFetch(new_con,"CELF_5",stringsAsFactors=F)
celf5 = id_visit_changing_into_char(celf5)
celf5_entry_flag = entry_flag(celf5,'celf')
celf5 = subset(celf5,entry_status==2)
celf5 = removing_prefix(celf5,'celf5_')

#calculating age
celf5 = fxage(celf5,'id','date')

#missing data analysis
celf5 = comment_missing_data(celf5,list(grep("_raw$",names(celf5))),list('','','','',''))
celf5[which(celf5$missing_items_comment != ''),"missing_items_comment"] = "Raw score(s) missing in the data entry;"

#modifying norm tables
celf_scaled = read_xlsx("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/celf5_scaledscore.xlsx")
celf_cls = read_xlsx("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/celf5_cls.xlsx")
celf_ae = read_xlsx("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/celf5_ae.xlsx")
names(celf_scaled) = tolower(names(celf_scaled))
names(celf_cls) = tolower(names(celf_cls))
names(celf_ae) = tolower(names(celf_ae))
write.csv(celf_scaled[,1:6],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/celf_scale.csv",row.names=F)
celf_scale = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/celf_scale.csv",stringsAsFactors=F)
celf_scale_revised = celf_scale[,1:2]
celf_scale_revised = modifying_vabs_norm_tables(celf_scale,celf_scale_revised)
celf_scale_revised = cbind(celf_scale_revised,celf_scaled[,7:15])
write.csv(celf_ae,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/celf_ae.csv",row.names=F)
celf_ae = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/celf_ae.csv",stringsAsFactors = F)
celf_ae_revised = data.frame(celf_ae[,1])
names(celf_ae_revised) = "ae"
celf_ae_revised = modifying_vabs_norm_tables(celf_ae,celf_ae_revised)
rm(celf_scale,celf_scaled,celf_ae)

#obtaining ae,scaled scores,percentile,and ci
for(i in 1:nrow(celf5)){
  #ae
  celf5$fs_age_eq[i] = celf_ae_revised[which(celf_ae_revised$fs_bottom<=celf5$fs_raw[i] & celf_ae_revised$fs_top>=celf5$fs_raw[i]),"test age"]
  celf5$rs_age_eq[i] = celf_ae_revised[which(celf_ae_revised$rs_bottom<=celf5$rs_raw[i] & celf_ae_revised$rs_top>=celf5$rs_raw[i]),"test age"]
  celf5$sr_age_eq[i] = celf_ae_revised[which(celf_ae_revised$sr_bottom<=celf5$sr_raw[i] & celf_ae_revised$sr_top>=celf5$sr_raw[i]),"test age"]
  #scaled scores; clarifying which age to use from scale norm table first
  age_list = celf_scale_revised$age[which(celf_scale_revised$age<=celf5$age[i])] #Nonverbal nor Verbal don't matter
  correct_age = age_list[length(age_list)]
  celf5$fs_scaled[i] = celf_scale_revised[which(celf_scale_revised$age==correct_age
                                   & celf5$fs_raw[i]>=celf_scale_revised$fs_bottom
                                   & celf5$fs_raw[i]<=celf_scale_revised$fs_top),"scaled.score"]
  celf5$fs_perc[i] = celf_scale_revised[which(celf_scale_revised$age==correct_age
                                       & celf5$fs_raw[i]>=celf_scale_revised$fs_bottom
                                       & celf5$fs_raw[i]<=celf_scale_revised$fs_top),"percentile"]
  celf5$fs_ci_95[i] = celf_scale_revised[which(celf_scale_revised$age==correct_age
                                  & celf5$fs_raw[i]>=celf_scale_revised$fs_bottom
                                  & celf5$fs_raw[i]<=celf_scale_revised$fs_top),"95% ci fs"]
  celf5$rs_scaled[i] = celf_scale_revised[which(celf_scale_revised$age==correct_age
                                   & celf5$rs_raw[i]>=celf_scale_revised$rs_bottom
                                   & celf5$rs_raw[i]<=celf_scale_revised$rs_top),"scaled.score"]
  celf5$rs_perc[i] = celf_scale_revised[which(celf_scale_revised$age==correct_age
                                       & celf5$rs_raw[i]>=celf_scale_revised$rs_bottom
                                       & celf5$rs_raw[i]<=celf_scale_revised$rs_top),"percentile"]
  celf5$rs_ci_95[i] = celf_scale_revised[which(celf_scale_revised$age==correct_age
                                  & celf5$rs_raw[i]>=celf_scale_revised$rs_bottom
                                  & celf5$rs_raw[i]<=celf_scale_revised$rs_top),"95% ci rs"]
  celf5$usp_scaled[i] = celf_scale_revised[which(celf_scale_revised$age==correct_age
                                   & celf5$usp_raw[i]>=celf_scale_revised$usp_bottom
                                   & celf5$usp_raw[i]<=celf_scale_revised$usp_top),"scaled.score"]
  celf5$usp_perc[i] = celf_scale_revised[which(celf_scale_revised$age==correct_age
                                       & celf5$usp_raw[i]>=celf_scale_revised$usp_bottom
                                       & celf5$usp_raw[i]<=celf_scale_revised$usp_top),"percentile"]
  celf5$usp_ci_95[i] = celf_scale_revised[which(celf_scale_revised$age==correct_age
                                  & celf5$usp_raw[i]>=celf_scale_revised$usp_bottom
                                  & celf5$usp_raw[i]<=celf_scale_revised$usp_top),"95% ci usp"]
  celf5$sr_scaled[i] = celf_scale_revised[which(celf_scale_revised$age==correct_age
                                   & celf5$sr_raw[i]>=celf_scale_revised$sr_bottom
                                   & celf5$sr_raw[i]<=celf_scale_revised$sr_top),"scaled.score"]
  celf5$sr_perc[i] = celf_scale_revised[which(celf_scale_revised$age==correct_age
                                       & celf5$sr_raw[i]>=celf_scale_revised$sr_bottom
                                       & celf5$sr_raw[i]<=celf_scale_revised$sr_top),"percentile"]
  celf5$sr_ci_95[i] = celf_scale_revised[which(celf_scale_revised$age==correct_age
                                  & celf5$sr_raw[i]>=celf_scale_revised$sr_bottom
                                  & celf5$sr_raw[i]<=celf_scale_revised$sr_top),"95% ci sr"]
}

#summing scaled scores
celf5 = summing_items_per_row(celf5,list(grep("_scaled$",names(celf5))),list('scaled_sum'),F)

#obtaining cls std scores
for(i in 1:nrow(celf5)){
  age_list = celf_scale_revised$age[which(celf_scale_revised$age<=celf5$age[i])]
  correct_age = age_list[length(age_list)]
  celf5$cls_std[i] = celf_cls[which(celf_cls$age==correct_age
                                & celf_cls$`cls sum of scaled`==celf5$scaled_sum[i]),"core language standard score"]
  celf5$cls_perc[i] = celf_cls[which(celf_cls$age==correct_age
                                & celf_cls$`cls sum of scaled`==celf5$scaled_sum[i]),"percentile"]
  celf5$cls_ci_95[i] = celf_cls[which(celf_cls$age==correct_age
                                & celf_cls$`cls sum of scaled`==celf5$scaled_sum[i]),"95% confidence interval"]
}

#putting back prefix
celf5 = inserting_prefix_into_variables(celf5,'celf_')

#orphaned/duplicate data
celf5_orphaned_data = orphaned_data_consolidate(celf5)
celf5 = orphaned_data_remove(celf5)
celf5_duplicate_data = duplicate_data_consolidate(celf5,'celf_age')
celf5 = duplicate_data_remove(celf5,'celf_age')

#outliers
celf5_outliers = celf5[,c(1:2,grep("_scaled$",names(celf5)),grep("_scaled$",names(celf5)),grep("scaled_sum",names(celf5)),grep("_std$",names(celf5)))]
celf5_outliers = outlier_list(celf5_outliers)
celf5$celf_outlier_list = celf5_outliers$outlier_list

#extracting necessary variables and archiving
celf_scored = celf5[,c(1:2,grep("_age$",names(celf5)),grep("_comment$",names(celf5)),
                       grep("_raw$",names(celf5)),grep("fs_scaled$",names(celf5)):ncol(celf5),
                       grep("_valid",names(celf5)),grep("_clincom$",names(celf5)))]
celf_scored$celf_cls_std = as.numeric(celf_scored$celf_cls_std)
celf_scored$celf_cls_perc = as.numeric(celf_scored$celf_cls_perc)
celf_scored$celf_cls_ci_95 = as.character(celf_scored$celf_cls_ci_95)
write.csv(celf_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/celf5_scored.csv",row.names=F)
#combining CELF 4 and CELF 5 and rearranging the cols -> scales are differnt, so maybe not a good idea
# celf_final = rbind.fill(celf_processed,celf_scored)
# celf_final = celf_final[,c()]
# write.csv(celf_final,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archive/celf_combined.csv",row.names=F)

#clean up
rm(celf_ae_revised,celf_cls,celf_scale_revised,celf5_outliers)
