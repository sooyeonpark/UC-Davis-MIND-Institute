sss = sqlFetch(con4,"SSS",stringsAsFactors=F)
sss = id_visit_changing_into_char(sss)
sss = subset(sss,entry_status==2)
sss = removing_prefix(sss,'sss_')

sss = fxage(sss,'id','date')

#any scoring?

sss = inserting_prefix_into_variables(sss,'sss_')
