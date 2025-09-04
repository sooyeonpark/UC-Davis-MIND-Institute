#pulling out the table
growth_measurement = sqlQuery(med_con,"select * from `Growth Measurements`",stringsAsFactors=F)

#changing id and visit into characters and removing single-entered rows
growth_measurement = id_visit_changing_into_char(growth_measurement)
growth_measurement_entry_flag = entry_flag(growth_measurement,'growth measurement')
growth_measurement = subset(growth_measurement,entry_status==2 & visit != '4p')
growth_measurement = removing_prefix(growth_measurement,"vitals_")

#calculating age
growth_measurement = fxage(growth_measurement,'id','date')

#adding bmi variable
for(i in 1:nrow(growth_measurement)){
  growth_measurement$bmi[i] = round((growth_measurement$wt_lb[i]*.453)/(growth_measurement$ht_in[i]*.0254)^2,2)
}

#adding prefixes
growth_measurement = inserting_prefix_into_variables(growth_measurement,"vitals_")

#orphaned/duplicate data
growth_measurement_orphaned_data = orphaned_data_consolidate(growth_measurement)
growth_measurement = orphaned_data_remove(growth_measurement)
growth_measurement_duplicate_data = duplicate_data_consolidate(growth_measurement,"vitals_age")
growth_measurement = duplicate_data_remove(growth_measurement,"vitals_age")

#archiving the data
gm_processed = growth_measurement[,c(1:2,grep("_age$",names(growth_measurement)),
                                     grep("hc_cm",names(growth_measurement)):grep("ht_in$",names(growth_measurement)),
                                     grep("bmi$",names(growth_measurement)))]
gm_processed = gm_processed[-grep("x",gm_processed$visit),]
write.csv(gm_processed,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/gm_processed.csv",row.names=F)
