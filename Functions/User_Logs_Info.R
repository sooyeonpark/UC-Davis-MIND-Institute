user_log_month_info = function(dt,begin,end,save_table){
  #dt = acelog
  #begin = begin date of the month (YYYY-MM-DD)
  #end = end date of the month (YYYY-MM-DD)
  library(sqldf)
  
  #obtain the data of the month of interest
  data = dt[which(dt$session_date>=begin & dt$session_date<end),]
  
  #number of sessions total within the month
  print(paste0("Total Session #: ",length(unique(data$session_id))))
  
  #obtaining the number of sessions within the month for dataportal.explore
  data_explore = data[grepl("explore$",data$app_name),]
  if(save_table==T){
    write.csv(sqldf("select user_name, count(distinct session_id) as session_num from data_explore group by user_name order by session_num desc;"),
              "S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/User Logs/session_nums.csv",row.names=F)
  }

  #obtaining the number of sessions within the month for daataportal.proposal
  data_proposal = data[grepl("proposal$",data$app_name),]
  if(save_table==T){
    write.table(sqldf("select user_name, count(distinct session_id) as session_num from data_proposal group by user_name order by session_num desc;"),
                "S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/User Logs/session_nums.csv",row.names=F,sep=",",append=T)
  }

  #how many session_ids include either "histogram" or "pairplot"
  data_graph = data[data$process_name=="histogram" | data$process_name=="pairplot",]
  print(paste0("Plot Proportion: ",round(length(unique(data_graph$session_id))/length(unique(data$session_id)),2)))
  
  #how many session_ids included download of data
  data_download = data[grepl("download",data$process_name),]
  print(paste0("Download Proportion: ",round(length(unique(data_download$session_id))/length(unique(data$session_id)),2)))
  
  #who used data portal
  users = unique(data[,c("session_id","user_name")])
  print(table(users$user_name))
  
  #which tables were queried during this month
  data_leftjoin = data[grepl("left_join",data$process_name) & grepl("tbl_",data$table_list),4:(ncol(data)-1)]
  data_leftjoin = unique(data_leftjoin)
  for(i in 1:(nrow(data_leftjoin)-1)){
    if(data_leftjoin$session_id[i]==data_leftjoin$session_id[i+1]){
      data_leftjoin$table_list[i] = gsub("tbl_participants;tbl_visit;","",data_leftjoin$table_list[i])
      #print(any(strsplit(data_leftjoin$table_list[i],';') %in% strsplit(data_leftjoin$table_list[i+1],';')))
      data_leftjoin$table_list[i] = ifelse(all(strsplit(data_leftjoin$table_list[i],';')[[1]] %in% strsplit(data_leftjoin$table_list[i+1],';')[[1]]),"",data_leftjoin$table_list[i])
    }
  }
  data_leftjoin = data_leftjoin[-which(data_leftjoin$table_list==""),]
  if(save_table==T){
    write.csv(sort(char_lists_to_table(data_leftjoin$table_list,";"),decreasing = T),
              "S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/User Logs/table_list.csv",row.names=F)
  }
}
