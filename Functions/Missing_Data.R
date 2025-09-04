count_missing_items = function(dt,start,end){
  #dt = data table that you want to count missing items from
  #start = col name that you want to start counting from
  #end = col name that you want to end counting to
  if(is.null(dt$missing_items_count)){
    dt$missing_items_count = 0
  }
  for(i in 1:nrow(dt)){
    for(j in which(names(dt)==start):which(names(dt)==end)){
      if(class(dt[,j])=="numeric" | class(dt[,j])=="integer"){
        dt$missing_items_count[i] = ifelse(is.na(dt[i,j]),dt$missing_items_count[i]+1,dt$missing_items_count[i])
      }
    }
  }
  return(dt)
}
#ex. ados_g2 = count_missing_items(ados_g2,"a1","e3")

comment_missing_data = function(dt,item_list,item_name){
  #the function is meant to be used for data tables with (sub)section scores
  #dt = data table you want to put coments on
  #item_list = the list of items you want to analyze (can be one list or a list of multiple lists)
  #item_name = labelling the the list of items for a commenting variable (the number of names should match with the item list)
  if(is.null(dt$missing_data_comment)){
    dt$missing_items_comment = ""
  }
  total_list = c()
  for(l in 1:length(item_list)){
    total_list = c(total_list,item_list[[l]])
  }
  #making multiple item lists into one list to only run the fuction one time for each test measure!
  for(i in 1:nrow(dt)){
    if(length(unique(total_list))==length(which(is.na(dt[i,unique(total_list)])))){
      dt$missing_items_comment[i] = "All data are missing;"
    }
    else{
      for(l in 1:length(item_list)){
        missing_count = which(is.na(dt[i,item_list[[l]]]))
        if(length(missing_count) == 0){
          l = l+1
        }
        else{
          missing_perc = round(length(missing_count)/length(item_list[[l]]),2)*100
          dt$missing_items_comment[i] = paste0(dt$missing_items_comment[i],missing_perc,'% of ',item_name[[l]],' algorithm items; ')
          dt$missing_items_comment[i] = gsub("100%","All",dt$missing_items_comment[i])
        }
      }
    }
  }
  return(dt)
}
#ex. ados_g2 = comment_missing_data(ados_g2,list(mod2_sa_items,mod2_rrb_items),list('sa','rrb'))

data_all_na_status = function(dt,measure){
  missing_items_comment_col = grep("missing_items_comment$",names(dt))
  dt_na = subset(dt,grepl("All data",dt[,missing_items_comment_col]))
  dt_na_tracking = subset(assess_survey, `STS ID` %in% dt_na$id & grepl(toupper(measure),toupper(Type)))
  return(dt_na_tracking)
}

item_missing = function(dt,con,tbl,items){
  library(RODBC)
  #dt = preexisting flagged table
  #items can be single or vectors
  table = sqlFetch(con,tbl,stringsAsFactors=F)
  table = table[,c("id","visit",items)]
  extract_index = c()
  table$item = ''
  for(i in 1:nrow(table)){
    if(any(is.na(table[i,]))){
      extract_index = c(extract_index,i)
      na_index = which(is.na(table[i,]))
      for(index in na_index){
        table$item[i] = paste0(table$item[i],', ',names(table)[index])
      }
    }
  }
  dt = rbind(dt,table[extract_index,c("id","visit","item")])
  dt$item = gsub("^, ","",dt$item)
  return(dt)
}
#ex. last_items_missing = item_missing(last_items_missing,new_con,last_items$Measure[1],last_items$Variable[1:3])