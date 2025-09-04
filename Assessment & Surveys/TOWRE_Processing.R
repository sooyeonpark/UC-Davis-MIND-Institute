#importing data and norm tables
towre = sqlQuery(new_con,"select * from TOWRE_2",stringsAsFactors=F)
towre_ae = read_excel("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/towre_ae.xlsx")
towre_ge = read_excel("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/towre_ge.xlsx")
towre_perc = read_excel("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/towre_percentile.xlsx")
towre_scale = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/towre_scaled_use.csv",stringsAsFactors = F)
towre_twrei = read_excel("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/towre_twrei.xlsx")

#norm table modification
towre_scale_revised = towre_scale[,1:4]
names(towre_scale_revised)[1] = "swe_raw"
towre_scale_revised = modifying_vabs_norm_tables(towre_scale,towre_scale_revised)
for(j in grep("age_",names(towre_scale_revised))){
  towre_scale_revised[,j] = 12*towre_scale_revised[,j]
}
#***need to use ceiling(swe_age) and ceiling(pde_age) to extract correct values!***
towre_ae$swe_ae = ifelse(grepl("^[0-9]",towre_ae$swe_ae),as.numeric(towre_ae$swe_ae)*12,towre_ae$swe_ae)
towre_ae$pde_ae = ifelse(grepl("^[0-9]",towre_ae$pde_ae),as.numeric(towre_ae$pde_ae)*12,towre_ae$pde_ae)

#initial data manipulation
towre = id_visit_changing_into_char(towre)
towre_entry_flag = entry_flag(towre,'towre')
towre = subset(towre,entry_status==2)
towre = removing_prefix(towre,'towre_')

#dealing with negative scores
for(j in which(names(towre)=="swe_raw"):ncol(towre)){
  towre[,j] = cbraw(towre[,j])
}

#calculating age
towre = fxage(towre,'id','date')

#missing data analysis
towre = comment_missing_data(towre,list(grep("_raw$",names(towre))),list('raw','raw'))
towre[which(towre$missing_items_comment != ''),"missing_items_comment"] = "Raw score(s) missing in the data entry;"

#obtaining norm scores
# for(i in 1:nrow(towre)){
#   towre$swe_scaled[i] = towre_scale_revised[floor(towre$age[i]) >= ceiling(towre_scale_revised$swe_age_bottom)
#                                            & floor(towre$age[i]) <= ceiling(towre_scale_revised$swe_age_top)
#                                            & towre_scale_revised$swe_raw == towre$swe_raw[i],"swe_scaled"]
#   towre$swe_ae[i] = towre_ae[towre_ae$swe==towre$swe_raw[i],"swe_ae"]
#   towre$swe_grade[i] = towre_ge[towre_ge$swe==towre$swe_raw[i],"swe_ge"]
#   towre$pde_scaled[i] = towre_scale_revised[floor(towre$age[i]) <= ceiling(towre_scale_revised$pde_age_bottom)
#                                             & floor(towre$age[i]) <= ceiling(towre_scale_revised$pde_age_top)
#                                             & towre_scale_revised$pde_raw >= towre$pde_raw[i],"pde_scaled"]
#   towre$pde_ae[i] = towre_ae[towre_ae$pde==towre$pde_raw[i],"pde_ae"]
#   towre$pde_grade[i] = towre_ge[towre_ge$pde==towre$pde_raw[i],"pde_ge"]
#   towre$sum_scaled[i] = sum(towre[i,grep("_scaled$",names(towre))])
#   towre$twrei[i] = towre_twrei[towre_twrei$sum==towre$sum_scaled[i],"twrei"]
#   towre$twrei_perc[i] = towre_perc[towre_twrei$sum==towre$sum_scaled[i],"percentile"]
# }

#putting back prefix
towre = inserting_prefix_into_variables(towre,'towre_')

#orphaned/duplicate data
towre_orphaned_data = orphaned_data_consolidate(towre)
towre = orphaned_data_remove(towre)
towre_duplicate_data = duplicate_data_consolidate(towre,"towre_age")
towre = duplicate_data_remove(towre,"towre_age")

#outliers
towre_outliers = towre[,c(1:2,grep("towre_age",names(towre)),grep("_scaled$",names(towre)))]
towre_outliers = outlier_list(towre_outliers)
towre$towre_outlier_list = towre_outliers$outlier_list

#extacting relevant columns & archiving data
towre_processed = towre[,c(1:2,grep("towre_age",names(towre)),grep("missing",names(towre)),grep("swe_raw",names(towre)):(ncol(towre)-2),ncol(towre))]
write.csv(towre_processed,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/towre_processed.csv",row.names = F)

#clean up
rm(towre_ae,towre_ge,towre_twrei,towre_scale,towre_scale_revised,towre_perc)
