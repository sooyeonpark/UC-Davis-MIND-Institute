id_visit_changing_into_char = function(dt){
  dt[,"id"] = as.character(dt[,"id"])
  dt[,"visit"] = as.character(dt[,"visit"])
  return(dt)
}
