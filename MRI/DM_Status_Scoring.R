#mri_time1 = sqlQuery(mri_con,"select * from tbl_Time_1",stringsAsFactors=F) #to obtain id list and scan date
#you need subject table as well -> to obtain gender,app_diagnosis

#combining columns from multiple tables
dm_status = scan_details[scan_details$visit=="1",1:3] #first grabbing list of id's and scan dates for T1
names(dm_status)[3] = "scan_date"
dm_status = merge(dm_status,growth_measurement[which(growth_measurement$visit=='1'),c("id","vitals_date","vitals_ht_in")],all.x=T)
dm_status = merge(dm_status,subj[,c("subj_id","gender","app_diagnosis")],by.x="id",by.y="subj_id",all.x=T)
dm_status = merge(dm_status,tcv[which(tcv$visit=='1'),c("id","tcv_total")],all.x=T)

#calculate scan_age, vital_age, and interval between them
dm_status = fxage(dm_status,'id','scan_date')
names(dm_status)[ncol(dm_status)] = 'scan_age'
dm_status = fxage(dm_status,'id','vitals_date')
names(dm_status)[ncol(dm_status)] = 'vitals_age'

#NA-ing heights when the interval btw scan_age and vitals_age > 6
dm_status$interval = round(abs(dm_status$scan_age - dm_status$vitals_age),2)
dm_status$vitals_ht_in = ifelse(dm_status$interval>6,NA,dm_status$vitals_ht_in)

#obtaining tcv_c perheight
dm_status$`tcv_c/height` = dm_status$tcv_total/as.numeric(dm_status$vitals_ht_in)

#obtaining z-scores for tcv_c/height
#finding mean and median from norm samples
dm_norm_boys = read_excel("MRI/DM-NormSample_BOYS.xlsx",col_names = "id")
dm_norm_girls = read_excel("MRI/DM-NormSample_GIRLS.xlsx",col_names = "id")
dm_norm_boys = merge(dm_norm_boys,dm_status[,c("id","tcv_c/height")],all.x=T)
dm_norm_girls = merge(dm_norm_girls,dm_status[,c("id","tcv_c/height")],all.x=T)

#finding mean and sd for each gender
dm_norm_girls[41,2] = 1046.572/38.0
dm_norm_girls[18,2] = 945.52/38.5

dm_boys_mean = mean(dm_norm_boys$`tcv_c/height`)
dm_boys_sd = sd(dm_norm_boys$`tcv_c/height`)
dm_girls_mean = mean(dm_norm_girls$`tcv_c/height`)
dm_girls_sd = sd(dm_norm_girls$`tcv_c/height`)

for(i in 1:nrow(dm_status)){
  dm_status$z_score[i] = ifelse(dm_status$gender[i]=="Male",
                           round((dm_status$`tcv_c/height`[i]-dm_boys_mean)/dm_boys_sd,3),
                           round((dm_status$`tcv_c/height`[i]-dm_girls_mean)/dm_girls_sd,3))
}
dm_status$z_score = as.numeric(dm_status$z_score)

#assinging DM status
for(i in 1:nrow(dm_status)){
  dm_status$dm[i] = ifelse(dm_status$z_score[i]>=1.5,"DM",ifelse(dm_status$z_score[i]<=-1.5,"neg1.5","N"))
  dm_status$dm[i] = ifelse(is.na(dm_status$vitals_ht_in[i]) & !is.na(dm_status$tcv_total[i]),"NoHeight",
                           ifelse(!is.na(dm_status$vitals_ht_in[i]) & is.na(dm_status$tcv_total[i]),"NoTCV",
                                  ifelse(is.na(dm_status$vitals_ht_in[i]) & is.na(dm_status$tcv_total[i]),"NoHeight&TCV",dm_status$dm[i])))
  dm_status$dm[i] = ifelse(is.na(dm_status$gender[i]),paste0(dm_status$dm[i],"&Gender"),dm_status$dm[i])
  dm_status$status[i] = ifelse(!is.na(dm_status$app_diagnosis[i]) & dm_status$app_diagnosis[i] !="Ineligible",
                                   paste0(dm_status$app_diagnosis[i],"-",dm_status$dm[i]),paste0("UnknownDx-",dm_status$dm[i]))
}

#orphaned data
dm_status_orphaned_data = orphaned_data_consolidate(dm_status)
dm_status = orphaned_data_remove(dm_status)
dm_status_orphaned_data = dm_status_orphaned_data[-which(dm_status_orphaned_data$id %in% app_cohort_subj$`subj id`==T),]
if(is.null(dm_status_orphaned_data)){
  rm(dm_status_orphaned_data)
}
dm_status$visit = 1
which(table(dm_status$id,dm_status$visit)>1)

#putting back prefix
dm_status = inserting_prefix_into_variables(dm_status,'dm_')

#extracting only necessary columns
dm_status_processed = dm_status[,c(1,grep("scan_age",names(dm_status)),grep("interval",names(dm_status)),grep("z_score",names(dm_status)),grep("dm_status",names(dm_status)))]

#archiving the table
write.csv(dm_status_processed,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/dm_status.csv",row.names = F)

rm(dm_boys_mean,dm_boys_sd,dm_norm_girls,dm_girls_mean,dm_girls_sd,dm_norm_boys)
