#make sure to use preexisting csv file with this function
appending_var_descriptives = function(dt,var_index,file_name,prefix){
  #dt = which data frame the variable(s) of interest is/are from
  #var_index = should be in a vector form
  #file_name = the name of the file where you want to save the descriptives into
  for(i in var_index){
    write.table(table(dt[,i]),file_name,row.names=F,sep=",",col.names=c(gsub(prefix,"",names(dt)[i]),"Freq"),append=T)
  }
}

#ex.
# write.csv(table(med_hist$med_hist_temperament),"med_hist_descriptives.csv",row.names=F)
# med_hist_var_index = med_hist_codes[!is.na(med_hist_codes$`Category for Coding`) & med_hist_codes$Timepoint=="T1",]$`Var Order`
# appending_var_descriptives(med_hist,med_hist_var_index,"med_hist_descriptives.csv","med_hist_")
# rm(med_hist_var_index)
