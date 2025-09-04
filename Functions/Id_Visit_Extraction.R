visit_tracking = function(dt){
  id_visit = dt[,c("id","visit")]
  id_visit = unique(id_visit)
  return(id_visit)
}