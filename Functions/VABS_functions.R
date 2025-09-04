modifying_vabs_norm_tables = function(dt,dt2){
  #dt = original table
  #dt2 = modified table
  init_col = ncol(dt2)+1
  for(i in init_col:ncol(dt)){
    dt2[,(init_col+2*(i-init_col))] = as.numeric(gsub("\\-.*","",dt[,i]))
    dt2[,(init_col+2*(i-init_col)+1)] = as.numeric(gsub(".*\\-","",dt[,i]))
    names(dt2)[init_col+2*(i-init_col)] = paste0(names(dt)[i],"_bottom")
    names(dt2)[init_col+2*(i-init_col)+1] = paste0(names(dt)[i],"_top")
  }
  return(dt2)
}

vabs_basal_ceiling = function(dt,item_list){
  for(i in 1:nrow(dt)){
    ##for ceiling, just not count NA's and it will be automatically 0
    ##checking basal & changing to max score below basal
    usually_index = which(dt[i,item_list]=='Usually'|dt[i,item_list]=='Usually or Often'|tolower(dt[i,item_list])=="yes")
    basal_end = which(rle(diff(usually_index))$values==1&rle(diff(usually_index))$lengths>2)
    if(length(basal_end)!=0){
      basal_end_sum = sum(rle(diff(usually_index))$lengths[1:basal_end[length(basal_end)]])
      basal_index = usually_index[basal_end_sum]
      dt[i,item_list[1:basal_index]]='Usually'
    }
  }
  return(dt)
}
