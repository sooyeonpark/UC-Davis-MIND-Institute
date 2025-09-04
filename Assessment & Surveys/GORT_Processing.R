#importing tables
gort = sqlQuery(new_con,"select * from GORT_5",stringsAsFactors=F)
gort_ae = read_excel("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/gort_ae.xlsx")
gort_ge = read_excel("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/gort_grade.xlsx")
gort_perc = read_excel("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/gort_percentile.xlsx")
gort_scale = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/gort_scaled_use.csv",stringsAsFactors = F)
gort_ori = read_excel("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/gort_ori.xlsx")

#norm tables manipulation
gort_scale_revised = data.frame(gort_scale[,1])
names(gort_scale_revised) = "scaled_score"
gort_scale_revised = modifying_vabs_norm_tables(gort_scale,gort_scale_revised)
gort_ae$ae = ifelse(grepl("^[0-9]",gort_ae$ae),as.numeric(gort_ae$ae)*12,gort_ae$ae)

gort = id_visit_changing_into_char(gort)
gort_entry_flag = entry_flag(gort,'gort')
gort = subset(gort,entry_status==2)
if(!is.null(gort_entry_flag)){
  gort = rbind(gort,gort_entry_flag[,-ncol(gort_entry_flag)])
}
gort = removing_prefix(gort,'gort_')

#age calculation
gort = fxage(gort,'id','date')

#obtaining norm scores
# for(i in 1:nrow(gort)){
#   gort$rate_scaled[i] = gort_scale_revised[gort_scale_revised$age_bottom <= floor(gort$age[i])
#                                            & gort_scale_revised$age_top >= floor(gort$age[i])
#                                            & gort_scale_revised$rate_raw_bottom <= gort$rate_raw[i]
#                                            & gort_scale_revised$rate_raw_top >= gort$rate_raw[i],"scaled_score"]
#   gort$rate_perc[i] = gort_perc[gort_perc$scaled_score==gort$rate_scaled[i],"percentile"]
#   gort$rate_ae[i] = gort_ae[gort_ae$rate==gort$rate_raw[i],"ae"]
#   gort$rate_grade[i] = gort_ge[gort_ge$rate==gort$rate_raw[i],"ge"]
#   gort$accur_scaled[i] = gort_scale_revised[gort_scale_revised$age_bottom <= floor(gort$age[i])
#                                            & gort_scale_revised$age_top >= floor(gort$age[i])
#                                            & gort_scale_revised$accuracy_raw_bottom <= gort$accur_raw[i]
#                                            & gort_scale_revised$accuracy_raw_top >= gort$accur_raw[i],"scaled_score"]
#   gort$accur_perc[i] = gort_perc[gort_perc$scaled_score==gort$accur_scaled[i],"percentile"]
#   gort$accur_ae[i] = gort_ae[gort_ae$accuracy==gort$accur_raw[i],"ae"]
#   gort$accur_grade[i] = gort_ge[gort_ge$accuracy==gort$accur_raw[i],"ge"]
#   gort$fluen_scaled[i] = gort_scale_revised[gort_scale_revised$age_bottom <= floor(gort$age[i])
#                                            & gort_scale_revised$age_top >= floor(gort$age[i])
#                                            & gort_scale_revised$fluency_raw_bottom <= gort$fluen_raw[i]
#                                            & gort_scale_revised$fluency_raw_top >= gort$fluen_raw[i],"scaled_score"]
#   gort$fluen_perc[i] = gort_perc[gort_perc$scaled_score==gort$fluen_scaled[i],"percentile"]
#   gort$fluen_ae[i] = gort_ae[gort_ae$fluency==gort$fluen_raw[i],"ae"]
#   gort$fluen_grade[i] = gort_ge[gort_ge$fluency==gort$fluen_raw[i],"ge"]
#   gort$compre_scaled[i] = gort_scale_revised[gort_scale_revised$age_bottom <= floor(gort$age[i])
#                                            & gort_scale_revised$age_top >= floor(gort$age[i])
#                                            & gort_scale_revised$comp_raw_bottom <= gort$compre_raw[i]
#                                            & gort_scale_revised$comp_raw_top >= gort$compre_raw[i],"scaled_score"]
#   gort$compre_perc[i] = gort_perc[gort_perc$scaled_score==gort$compre_scaled[i],"percentile"]
#   gort$compre_ae[i] = gort_ae[gort_ae$comp==gort$compre_raw[i],"ae"]
#   gort$compre_grade[i] = gort_ge[gort_ge$comp==gort$compre_raw[i],"ge"]
#   gort$sum_scaled[i] = sum(gort[i,grep("_scaled$",names(gort))])
#   gort$ori[i] = gort_ori[gort$sum_scaled[i]==gort_ori$sum,"ori"]
#   gort$ori_perc[i] = gort_ori[gort$sum_scaled[i]==gort_ori$sum,"percentile"]
# }

#putting back prefix
gort = inserting_prefix_into_variables(gort,'gort_')

#orphaned/duplicate data
gort_orphaned_data = orphaned_data_consolidate(gort)
gort = orphaned_data_remove(gort)
gort_duplicate_data = duplicate_data_consolidate(gort,"gort_age")
gort = duplicate_data_remove(gort,"gort_age")

#outliers
gort_outliers = gort[,c(1:2,7,grep("_raw$",names(gort)),grep("_scaled$",names(gort)))]
gort_outliers = outlier_list(gort_outliers)
gort$outlier_list = gort_outliers$outlier_list
#gort_outlier_table= sqldf("select id,visit,outlier_list from gort where outlier_list != ''")
rm(gort_outliers)

#archiving the data
gort_processed = gort[,c(1:2,which(names(gort)=="gort_age"),grep("rate_raw",names(gort)):ncol(gort))]
write.csv(gort_processed,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/gort_processed.csv",row.names = F)
write.csv(gort[,c(1:2,grep("_[0-9]+_",names(gort)))],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/gort_items.csv",row.names = F)

#clean up
rm(gort_scale,gort_ae,gort_ge,gort_perc,gort_ori,gort_scale_revised)
