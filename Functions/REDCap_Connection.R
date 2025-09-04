url = "https://redcap.ucdmc.ucdavis.edu/redcap/api/"
token = "D290B40406AB0587C45B0C9E76E23322"
ace_con = redcapConnection(url,token)
rm(url,token)
arms = exportEvents(ace_con)
measures = exportInstruments(ace_con)

identify_same_data = function(rc_dt,access_dt){
  rc_dt = rc_dt[order(rc_dt$id,rc_dt$visit),]
  for(visit in unique(rc_dt$visit)){
    for(id in rc_dt[rc_dt$visit==visit,"id"]){
      if(length(which(access_dt$visit==visit&access_dt$id==id))!=0){
        access_dt = access_dt[-which(access_dt$visit==visit&access_dt$id==id),]
      }
    }
  }
  return(access_dt)
}
