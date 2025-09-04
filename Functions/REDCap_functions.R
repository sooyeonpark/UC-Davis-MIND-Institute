study_id_to_id = function(dt,prefix){
  dt = removing_prefix(dt,prefix)
  dt[,"id"] = gsub("s$","",dt[,"study_id"])
  dt[,"study_id"]=NULL
  names(dt)[grep("timestamp$",names(dt))]="date"
  return(dt)
}
