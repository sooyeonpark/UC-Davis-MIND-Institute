pm = sqlFetch(med_con,"Prescription Medication")

pm = id_visit_changing_into_char(pm)
pm = subset(pm,entry_status==2)