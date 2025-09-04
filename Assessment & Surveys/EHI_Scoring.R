#pulling out tables
edin = sqlQuery(new_con,"select * from Modified_edinburgh",stringsAsFactors=F)
edin_entry_flag = entry_flag(edin,'edinburgh')
edin_staar = sqlQuery(con4,"select * from Edinburgh",stringsAsFactors=F)

#consolidating tables
library(plyr)
edin = rbind.fill(edin,edin_staar)
rm(edin_staar)

#id and visit into characters, as well as removing single-entered rows and prefix
edin = id_visit_changing_into_char(edin)
edin = subset(edin,entry_status==2)
edin = removing_prefix(edin,"modedin_")

#calculating age
edin = fxage(edin,'id','date')

#putting back prefix
edin = inserting_prefix_into_variables(edin,"edin_")

#orphaned/duplicate data
edin_orphaned_data = orphaned_data_consolidate(edin)
edin = orphaned_data_remove(edin)
edin_duplicate_data = duplicate_data_consolidate(edin,"edin_age")
edin = duplicate_data_remove(edin,"edin_age")

#determining handedness
for(i in 1:nrow(edin)){
  edin$edin_left_total[i] = length(which(edin[i,grep("_sp$",names(edin)):grep("_sc$",names(edin))]=="Left")) + 0.5*length(which(edin[i,grep("_sp$",names(edin)):grep("_sc$",names(edin))]=="Both"))
  edin$edin_right_total[i] = length(which(edin[i,grep("_sp$",names(edin)):grep("_sc$",names(edin))]=="Right")) + + 0.5*length(which(edin[i,grep("_sp$",names(edin)):grep("_sc$",names(edin))]=="Both"))
  edin$edin_right_hand_ratio[i] = round(edin$edin_right_total[i]/(edin$edin_left_total[i]+edin$edin_right_total[i]),2)
  edin$edin_handedness[i] = ifelse(edin$edin_left_total[i]>edin$edin_right_total[i],"Left",
                                   ifelse(edin$edin_left_total[i]==edin$edin_right_total[i],"Both","Right"))
}

#archiving edinburgh data for staar
# write.csv(edin[grep("^[0-9]{4}$",edin$id),c(1:2,grep("_age$",names(edin)),grep("left_total$",names(edin)):ncol(edin))],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived_staar/edinburgh_scored.csv",row.names=F)
# write.csv(edin[grep("^[0-9]{4}$",edin$id),c(1:2,grep("_respon$",names(edin)):grep("_sc$",names(edin)))],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived_staar/edinburgh_items.csv",row.names=F)
write.csv(edin[grep("^[0-9]{6}-[0-9]{3}$",edin$id),c(1:2,grep("_respon$",names(edin)):grep("_sc$",names(edin)))],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/edinburgh_items.csv",row.names=F)

#merge edin_handedness to hpt by id
handedness_scored = sqldf("select t1.*,t2.edin_age,t2.edin_left_total,t2.edin_right_total,
                          t2.edin_right_hand_ratio,t2.edin_handedness from hpt_scored t1
                          left join edin t2 on t1.id = t2.id")
