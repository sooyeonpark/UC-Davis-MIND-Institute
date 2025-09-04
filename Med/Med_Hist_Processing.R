#importing med hist variable codes spreadsheet
med_hist_codes <- read_excel("~/Jobs/MIND/Med_Hist_Var_Categorizations (for coding).xlsx")

#getting descriptives
#t1
med_hist_var_index = med_hist_codes[!is.na(med_hist_codes$`Category for Coding`) & med_hist_codes$Timepoint=="T1",]$`Var Order`
appending_var_descriptives(med_hist,med_hist_var_index,"Med/med_hist_descriptives.csv","med_hist_")
#t3
med_hist_var_index = med_hist_codes[!is.na(med_hist_codes$`Category for Coding`) & med_hist_codes$Timepoint=="T3",]$`Var Order`
appending_var_descriptives(med_hist_t3,med_hist_var_index,"Med/med_hist_t3_descriptives.csv","med_hist_T3_")
#t4
med_hist_var_index = med_hist_codes[!is.na(med_hist_codes$`Category for Coding`) & med_hist_codes$Timepoint=="T4",]$`Var Order`
appending_var_descriptives(med_hist_t4,med_hist_var_index,"Med/med_hist_t4_descriptives.csv","med_hist_T4_")

#cleaning up
rm(med_hist_var_index)
