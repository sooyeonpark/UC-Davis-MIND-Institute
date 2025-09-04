#pulling data table
ppvt4 = sqlQuery(new_con,"select * from PPVT4;",stringsAsFactors=F)

#extracting data & cleaning
ppvt4 = id_visit_changing_into_char(ppvt4)
ppvt4_entry_flag = entry_flag(ppvt4,'ppvt4')
ppvt4 = subset(ppvt4, entry_status == 2)
if(!is.null(ppvt4_entry_flag)){
  ppvt4 = rbind(ppvt4,ppvt4_entry_flag[,-ncol(ppvt4_entry_flag)])
}
ppvt4 = removing_prefix(ppvt4,"ppvt4_")

#dealing with negative raw scores to NA's
ppvt4$original_raw = ppvt4$raw
ppvt4$raw_score = cbraw(ppvt4$raw)

#calculating age
ppvt4 = fxage(ppvt4,'id','date')

#missing data analysis
ppvt4[,"missing_items_comment"] = ifelse(ppvt4$original_raw == -1,'Raw score missing: got all practice items wrong',
                                           ifelse(ppvt4$original_raw == -2,'Raw score missing: attended to task, but could not point',
                                                  ifelse(ppvt4$original_raw == -8,'Raw score missing: did not attend to task',
                                                         ifelse(ppvt4$original_raw == -9,'Raw score missing: did not attempt testing',''))))

## raw to std score
#importing norm tables
ppvt4_std <- read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/scr_PPVT4_Std.csv",stringsAsFactors = F)
ppvt4_ae <- read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/scr_PPVT4_AgeEqu.csv",stringsAsFactors = F)

#AgeEqu -> get it using sqldf
ppvt4 = sqldf("select t1.*, t2.AgeEqu_Year as ae_yr, t2.AgeEqu_Month as ae_mon from ppvt4 as t1
               left join ppvt4_ae as t2 on t1.raw_score = t2.Raw_Score")

#converting Age Equivalent into months
for(i in 1:nrow(ppvt4)){
  if(is.na(ppvt4$raw_score[i]) | ppvt4$ae_yr[i]=="<2"){
    ppvt4$ae[i] = NA
  }
  else{
    ppvt4$ae[i] = as.numeric(ppvt4$ae_yr[i])*12+as.numeric(ppvt4$ae_mon[i])
  }
}

#filling in the blanks in std norm table
for(j in seq(3,ncol(ppvt4_std), by=2)){
  ppvt4_std[which(ppvt4_std[,j-1]==0),j-1] = NA
  loc = which(is.na(ppvt4_std[,j]) & !is.na(ppvt4_std[,j-1]))
  for(i in 1:length(loc)){
    ppvt4_std[loc[i],j] = ppvt4_std[loc[i],j-1]
  }
}

#changing the name of the std table to match with subject age
#numbers represent the average of the age range
names(ppvt4_std)[2:ncol(ppvt4_std)] = rep(seq(30.5,82.5,by=2),each=2)

#obtaining ss for each rows
#getting rid of scores of zeros first
ppvt4[which(ppvt4$raw_score==0),"raw_score"]=NA

for (i in 1:nrow(ppvt4)){
  if(is.na(ppvt4$raw_score[i]) | ppvt4$age[i]<30 | is.na(ppvt4$age[i])){
    ppvt4$ss[i] = NA
  }
  else{
    #modifying the age to match with the columns in the norm table
    top = floor(ppvt4$age[i])-0.5
    bottom = floor(ppvt4$age[i])+0.5
    #matching with the corresponding columns in the norm table
    age_range = grep(top, names(ppvt4_std))
    age_range2 = grep(bottom, names(ppvt4_std))
    #obtaining standard score from the norm table
    if(length(age_range)>0){
      std_raw = ppvt4_std[,c(1,age_range)]
      ppvt4$ss[i] = std_raw[which(std_raw[,2] <= ppvt4$raw_score[i] & std_raw[,3] >= ppvt4$raw_score[i]),1]
    }
    else if(length(age_range2)>0){
      std_raw = ppvt4_std[,c(1,age_range2)]
      ppvt4$ss[i] = std_raw[which(std_raw[,2] <= ppvt4$raw_score[i] & std_raw[,3] >= ppvt4$raw_score[i]),1]
    }
  }
}

#modifying table
ppvt4 = inserting_prefix_into_variables(ppvt4,"ppvt_")

#orphaned/duplicate data
ppvt4_orphaned_data = orphaned_data_consolidate(ppvt4)
ppvt4 = orphaned_data_remove(ppvt4)
ppvt4_duplicate_data = duplicate_data_consolidate(ppvt4,"ppvt_age")
ppvt4 = duplicate_data_remove(ppvt4,"ppvt_age")

#getting outliers for age and standard scores
ppvt4_scored = ppvt4[,c(1:2,grep("^ppvt_age$",names(ppvt4)),grep("missing",names(ppvt4)),
                        grep("raw_score",names(ppvt4)),grep("ae$",names(ppvt4)):ncol(ppvt4),
                        grep("_valid$",names(ppvt4)):grep("_clincom$",names(ppvt4)))]
ppvt4_outliers = ppvt4[,c(1:2,grep("^ppvt_age$",names(ppvt4)),34:35)]
ppvt4_outliers = outlier_list(ppvt4_outliers)
ppvt4_scored$ppvt_outlier_list= ppvt4_outliers$outlier_list
rm(ppvt4_outliers)
# ppvt_outlier_table = sqldf("select id,visit,outlier_list from ppvt3 where outlier_list != ''
#                            union select id,visit,outlier_list from ppvt4 where outlier_list != ''")

#matching column names with ppvt3 table
names(ppvt3_scored)[which(names(ppvt3_scored)=="ppvt_age_eq")] = "ppvt_ae"
ppvt4_scored$ppvt_version = "4th edition"

ppvt_scored = rbind.fill(ppvt3_scored,ppvt4_scored)
ppvt_orphaned_data = orphaned_data_consolidate(ppvt_scored)
ppvt_scored = orphaned_data_remove(ppvt_scored)
ppvt_duplicate_data = duplicate_data_consolidate(ppvt_scored,"ppvt_age")
ppvt_scored = duplicate_data_remove(ppvt_scored,"ppvt_age")

#archiving the data
write.csv(ppvt_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/ppvt_scored.csv",row.names = F)

rm(top,bottom,age_range,age_range2,loc,ppvt4_std,ppvt4_ae)
