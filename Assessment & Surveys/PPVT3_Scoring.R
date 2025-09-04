#pulling out tables
ppvt3 = sqlQuery(new_con,"select * from PPVT111",stringsAsFactors=F)
con <- odbcConnectAccess2007("S:/MIND/RESEARCH/APP Behavior Data/Local Data Entry/APP Data entry Aug 8 2011.mdb")
ppvt3_ae = sqlQuery(con,"select * from scr_PPVTIII_age_equivalent",stringsAsFactors=F)
ppvt3_ss = sqlQuery(con,"select * from scr_PPVTIII_standard_scores",stringsAsFactors=F)
odbcClose(con)

#extracting data & cleaning
ppvt3 = id_visit_changing_into_char(ppvt3)
ppvt3_entry_flag = entry_flag(ppvt3,'ppvt3')
ppvt3 = subset(ppvt3, entry_status == 2)
ppvt3$entry_date = NULL
if(!is.null(ppvt3_entry_flag)){
  ppvt3 = rbind(ppvt3,ppvt3_entry_flag[,-ncol(ppvt3_entry_flag)])
}

#dealing with negative raw scores
ppvt3$original_raw = ppvt3$score_raw
ppvt3$raw_score = cbraw(ppvt3$score_raw)

#calculating age
ppvt3 = fxage(ppvt3,'id','interview_date')

#missing data analysis
ppvt3[,"missing_items_comment"] = ifelse(ppvt3$original_raw == -1,'Raw score missing: got all practice items wrong',
                                           ifelse(ppvt3$original_raw == -2,'Raw score missing: attended to task, but could not point',
                                                  ifelse(ppvt3$original_raw == -8,'Raw score missing: did not attend to task',
                                                         ifelse(ppvt3$original_raw == -9,'Raw score missing: did not attempt testing',''))))

## raw to std score
#AgeEqu -> get it using sqldf
ppvt3 = sqldf("select t1.*, t2.age_eq from ppvt3 as t1
             left join ppvt3_ae as t2 on t1.raw_score = t2.`ppvtiiiA raw score`")

#std score
#modifying norm table first
for(i in 1:nrow(ppvt3_ss)){
  ppvt3_ss$raw_score_bottom[i] =  strsplit(as.character(ppvt3_ss$iiiA_raw_score[i]),"-")[[1]][1]
  ppvt3_ss$raw_score_top[i] =  strsplit(as.character(ppvt3_ss$iiiA_raw_score[i]),"-")[[1]][2]
  if(is.na(ppvt3_ss$raw_score_top[i])){
    ppvt3_ss$raw_score_top[i] = ppvt3_ss$raw_score_bottom[i]
  }
}
ppvt3_ss$raw_score_bottom = as.numeric(ppvt3_ss$raw_score_bottom)
ppvt3_ss$raw_score_top = as.numeric(ppvt3_ss$raw_score_top)

#obtaining std score using for loop
for(i in 1:nrow(ppvt3)){
  if(!is.na(ppvt3[i,"raw_score"])){
    if(ppvt3[i,"age"]<74 & ppvt3[i,"age"]>=30){
      ppvt3[i,"ss"] = ppvt3_ss[which(ppvt3[i,"raw_score"]>=ppvt3_ss[,"raw_score_bottom"] & ppvt3[i,"raw_score"]<=ppvt3_ss[,"raw_score_top"] &
                            floor(ppvt3$age[i]) == ppvt3_ss[,1]),3]
    }
  }
}

#checking orphaned/duplicate data
ppvt3 = inserting_prefix_into_variables(ppvt3,"ppvt_")
ppvt3_orphaned_data = orphaned_data_consolidate(ppvt3)
ppvt3 = orphaned_data_remove(ppvt3)
ppvt3_duplicate_data=duplicate_data_consolidate(ppvt3,"ppvt_age")
ppvt3 = duplicate_data_remove(ppvt3,"ppvt_age")

#modifying column names and obtaining outliers
ppvt3_scored = ppvt3[,c(1:2,grep("^ppvt_age$",names(ppvt3)),grep("missing",names(ppvt3)),
                        grep("^ppvt_raw",names(ppvt3)),which(names(ppvt3)=="ppvt_age_eq"):grep("_ss$",names(ppvt3)))]
ppvt3_scored$ppvt_version = "3rd edition"
ppvt3_outliers = ppvt3_scored[,c(1:3,which(names(ppvt3_scored)=="ppvt_age_eq"):grep("_ss$",names(ppvt3_scored)))]
ppvt3_outliers = outlier_list(ppvt3_outliers)
ppvt3_scored$ppvt_outlier_list = ppvt3_outliers$outlier_list
rm(ppvt3_outliers)
#write.csv(ppvt,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/ppvt_scored.csv",row.names = F)

rm(ppvt3_ae,ppvt3_ss)
