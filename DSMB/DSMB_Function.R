dsmb_recruitment_count = function(dt,begin,end,save_table){
  #dt = acelog
  #begin = begin date of the month
  #end = end date of the month
  library(sqldf)
  
  #obtain the data of the month of interest
  data = dt[which(dt$consent_date>=begin & dt$consent_date<end),]
  
  #obtain the number of recruitment overall
  if(save_table==T){
    write.table(sqldf("select blind_group,count(dummy_id) from data group by blind_group;"),
                "S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/DSMB/dsmb_recruitment.csv",
                row.names=F,sep=",",append=T)
  }
}
