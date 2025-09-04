#querying the data from access
cgi = sqlQuery(con4,"select * from CGI;",stringsAsFactors=F)

#id and visit into characters, remove single entries
cgi = id_visit_changing_into_char(cgi)
cgi = subset(cgi,entry_status==2)
cgi = removing_prefix(cgi,'cgi_')

#calculating age
cgi = fxage(cgi,'id','date')

#orphaned/duplicate data
cgi = inserting_prefix_into_variables(cgi,'cgi_')
cgi_orphaned_data = orphaned_data_consolidate(cgi)
cgi = orphaned_data_remove(cgi)
cgi_duplicate_data = duplicate_data_consolidate(cgi,"cgi_age")
cgi = duplicate_data_remove(cgi,"cgi_age")

#archiving
cgi_processed = cgi[,c(1:2,grep("_age$",names(cgi)),grep("type$",names(cgi)),grep("score$",names(cgi)))]
write.csv(cgi_processed,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived_staar/cgi_processed.csv",row.names = F)
