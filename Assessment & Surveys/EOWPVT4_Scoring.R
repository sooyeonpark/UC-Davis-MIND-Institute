eowpvt4 = sqlQuery(new_con, "select * from EOWPVT4;",stringsAsFactors=F)

# exclude incomplete data entry & changing id and visit into characters
eowpvt4 = id_visit_changing_into_char(eowpvt4)
eowpvt4_entry_flag = entry_flag(eowpvt4,'eowpvt4')
eowpvt4 = subset(eowpvt4, entry_status == 2)
if(!is.null(eowpvt4_entry_flag)){
  eowpvt4 = rbind(eowpvt4,eowpvt4_entry_flag[,-ncol(eowpvt4_entry_flag)])
}
eowpvt4 = removing_prefix(eowpvt4,"eowpvt4_")

#calculating age
eowpvt4 = fxage(eowpvt4,'id','date')

#changing negative raw scores to NA's
eowpvt4$original_raw = eowpvt4$raw
eowpvt4$raw = cbraw(eowpvt4$raw)

#missing data analysis
eowpvt4[,"missing_items_comment"] = ifelse(eowpvt4$original_raw == -1,'Raw score missing: got all practice items wrong',
                                       ifelse(eowpvt4$original_raw == -2,'Raw score missing: attended to task, but could not produce words',
                                              ifelse(eowpvt4$original_raw == -8,'Raw score missing: did not attend to task',
                                                     ifelse(eowpvt4$original_raw == -9,'Raw score missing: did not attempt testing',''))))

## raw to std score
#importing norm tables
options(stringsAsFactors = F)
EOWPVT4_norm <- read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/EOWPVT4_norm.csv")
EOWPVT4_AgeEqu_norm <- read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/EOWPVT4_AgeEqu_norm.csv")

#obtaining standard scores
#finding matching row and column from norm table
for(i in 1:nrow(eowpvt4)){
  if(!is.na(eowpvt4$raw[i])){
    row = which(eowpvt4$raw[i]==EOWPVT4_norm$raw_score)
    age_in_month = as.data.frame(t(c(NA,24:83)))
    if(!is.na(eowpvt4$age[i])){
      col = which(floor(eowpvt4$age[i])==age_in_month)
      eowpvt4$ss[i] = EOWPVT4_norm[row,col]
    }
    if(eowpvt4$ss[i] == "<55"){
      eowpvt4$ss[i] = NA
    }
  }
  else{
    eowpvt4$ss[i] = NA
  }
}
eowpvt4$ss = as.numeric(eowpvt4$ss)
rm(age_in_month,row,col)

#AgeEqu -> get it using sqldf
eowpvt4 = sqldf("select t1.*, t2.AgeEqu_Year as ae_yr, t2.AgeEqu_Month as ae_mon from eowpvt4 as t1
               left join EOWPVT4_AgeEqu_norm as t2 on t1.raw = t2.Raw_Score")
#converting Age Equivalent into months
for(i in 1:nrow(eowpvt4)){
  if(!is.na(eowpvt4$raw[i])){
    if(eowpvt4$ae_yr[i] != "<1"){
      eowpvt4$ae[i] = as.numeric(eowpvt4$ae_yr[i])*12+as.numeric(eowpvt4$ae_mon[i])
    }
  }
  else{
    eowpvt4$ae[i] = NA
  }
}

#putting prefix
eowpvt4 = inserting_prefix_into_variables(eowpvt4,"eowpvt_")

#orphaned/duplicate data
eowpvt4_orphaned_data = orphaned_data_consolidate(eowpvt4)
eowpvt4 = orphaned_data_remove(eowpvt4)
eowpvt4_duplicate_data = duplicate_data_consolidate(eowpvt4,"eowpvt_age")
eowpvt4 = duplicate_data_remove(eowpvt4,"eowpvt_age")

#finding outliers
eowpvt4_scored = eowpvt4[,c(1,2,grep("_age$",names(eowpvt4)),grep("missing",names(eowpvt4)),
                            grep("eowpvt_raw",names(eowpvt4)),grep("_ss$",names(eowpvt4)),grep("_ae$",names(eowpvt4)))]
eowpvt4_scored = outlier_list(eowpvt4_scored)
# eowpvt4$outlier_list = eowpvt4_scored$outlier_list
# eowpvt4_scored$outlier_list = NULL
# eowpvt4_outlier_table = sqldf("select id, visit, outlier_list from eowpvt4 where (outlier_list != '' and outlier_list != 'eowpvt_age; ')")

#combining tables
eowpvt3_scored$eowpvt_version = "3rd edition"
eowpvt4_scored$eowpvt_version = "4th edition"
eowpvt = rbind(eowpvt3_scored,eowpvt4_scored)
names(eowpvt)[grep("outlier_list",names(eowpvt))]="eowpvt_outlier_list"

#duplicate for eowpvt?
eowpvt_duplicate_data = duplicate_data_consolidate(eowpvt,"eowpvt_age")
eowpvt = duplicate_data_remove(eowpvt,"eowpvt_age")

write.csv(eowpvt,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/eowpvt_scored.csv",row.names = F)
rm(EOWPVT4_norm,EOWPVT4_AgeEqu_norm)
