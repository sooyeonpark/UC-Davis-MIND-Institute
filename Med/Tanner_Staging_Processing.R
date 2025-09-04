ts = sqlQuery(med_con,"select * from `Tanner Staging`",stringsAsFactors=F)

ts = id_visit_changing_into_char(ts)
ts_entry_flag = entry_flag(ts,'tanner_staging')
ts = subset(ts,entry_status==2)
ts = removing_prefix(ts,"tanner_staging_")

ts = fxage(ts,'id','date')

ts$develop = gsub("^[B,G]","",ts$develop)
ts$pubic = gsub("^Ph","",ts$pubic)
ts$develop = as.numeric(ts$develop)
ts$pubic = as.numeric(ts$pubic)
ts$average = (ts$develop+ts$pubic)/2
for(i in 1:nrow(ts)){
  ts$highest[i] = max(c(ts$develop[i],ts$pubic[i]))
}

ts = inserting_prefix_into_variables(ts,"ts_")

ts_orphaned_data = orphaned_data_consolidate(ts)
ts = orphaned_data_remove(ts)
ts_duplicate_data = duplicate_data_consolidate(ts,"ts_age")
ts = duplicate_data_remove(ts,"ts_age")

ts_processed = ts[,c(1:2,grep("_age$",names(ts)),
                     grep("_develop$",names(ts)):grep("_pubic$",names(ts)),
                     grep("_average$",names(ts)):grep("_highest$",names(ts)))]
write.csv(ts_processed,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/ts_processed.csv",row.names=F)
