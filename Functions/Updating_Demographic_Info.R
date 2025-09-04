#updating info using two data tables
filling_info = function(dt,dt2){
  #dt = original table you want to keep
  #dt2 = table you want to use to update the information to the original table
  dt_id_index = which(dt$id %in% dt2$id)
  dt2_id_index = which(dt2$id %in% dt$id)
  for(i in 1:length(dt_id_index)){
    for(j in grep("^ch_raceth",names(dt))[1]:ncol(dt)){
      if(is.na(dt[dt_id_index[i],j]) & !is.na(dt2[dt2_id_index[i],j])){
        dt[dt_id_index[i],j] = dt2[dt2_id_index[i],j]
      }
      else if(!is.na(dt[dt_id_index[i],j]) & !is.na(dt2[dt2_id_index[i],j])){
        if(dt[dt_id_index[i],j] == '' & dt2[dt2_id_index[i],j] != ''){
          dt[dt_id_index[i],j] = dt2[dt2_id_index[i],j]
        }
      }
    }
  }
  #getting the rows from dt2 that are not in dt
  dt2 = dt2[-dt2_id_index,]
  #combining dt with the rows in dt2 that are not in dt
  dt = rbind(dt,dt2)
  #returning updated dt
  return(dt)
}
