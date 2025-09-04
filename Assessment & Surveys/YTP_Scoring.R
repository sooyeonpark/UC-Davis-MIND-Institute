ytp = sqlFetch(con4,"YTP",stringsAsFactors=F)
ytp = id_visit_changing_into_char(ytp)
ytp = subset(ytp,entry_status==2)
ytp = removing_prefix(ytp,'YTP_')

ytp = fxage(ytp,'id','date')

ytp$rate_mean = round(rowMeans(ytp[,paste0('p',1:3,'_rate')]),2)

ytp = inserting_prefix_into_variables(ytp,'ytp_')

#orphaned/duplicate data
ytp_orphaned_data = orphaned_data_consolidate(ytp)
ytp = orphaned_data_remove(ytp)
ytp_duplicate_data = duplicate_data_consolidate(ytp,"ytp_age")
ytp = duplicate_data_remove(ytp,"ytp_age")

ytp_scored = ytp[,c(1:2,grep("_age$",names(ytp)),grep("_p1$",names(ytp)):grep("_p3_notes$",names(ytp)),grep("_mean$",names(ytp)))]
ytp_scored = ytp_scored[,-grep("_notes$",names(ytp_scored))]
ytp_scored = outlier_list(ytp_scored)
names(ytp_scored)[ncol(ytp_scored)] = "ytp_outlier_list"
write.csv(ytp_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived_staar/ytp_scored.csv",row.names=F)
