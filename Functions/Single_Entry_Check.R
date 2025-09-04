entry_flag = function(dt,measure){
  #the function is trying to flag out single-entered data that are older than 3 months
  #dt = the data table that you want to check the entry status
  #measure = the name of the data
  dt$entry_date = NULL
  names(dt) = gsub("[a-z]+_entry_status$","entry_status",names(dt))
  single_entry = subset(dt,entry_status==1 & !grepl("p$",tolower(visit)))
  if(nrow(single_entry)!= 0){
    single_entry$current_date = Sys.Date()
    single_entry$how_old = elapsed_months(single_entry$current_date,single_entry[,grep("date$",names(dt))])
    single_entry = single_entry[which(single_entry$how_old>3),]
    single_entry$current_date = NULL
    single_entry$how_old = NULL
    if(nrow(single_entry) != 0){
    single_entry$measure = measure
    return(single_entry)
    }
  }
}
