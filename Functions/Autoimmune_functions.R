autoimmune_extract = function(dt,num){
  index = c()
  extract_string = c()
  for(j in num:grep("other_age_5",names(dt))){
    index = c(index,which(!is.na(dt[,j])))
  }
  dt = dt[sort(unique(index)),]
  for(i in 1:nrow(dt)){
    not_na = which(!is.na(dt[i,num:grep("other_age_5",names(dt))]))
    element = ''
    for(m in 1:length(not_na)){
      if(dt[i,(not_na[m]+num-1)] != 'No'){
        element = paste0(element,names(dt)[not_na[m]+num-1],': ',dt[i,(not_na[m]+num-1)],'; ')
      }
    }
    extract_string = c(extract_string,element)
  }
  dt$list = tolower(extract_string)
  return(dt)
}

autoimmune_count = function(dt,col_nums){
  #col_nums = column numbers that you want to count out of
  dt$count = 0
  for(j in col_nums){
    for(i in 1:nrow(dt)){
      if(!grepl("other",names(dt)[j]) & !grepl("^no$",tolower(dt[i,j])) & !is.na(dt[i,j])){
        dt$count[i] = dt$count[i]+1
      }
      else if(grepl("other",names(dt)[j]) & !is.na(dt[i,j])){
        dt$count[i] = dt$count[i]+1
      }
    }
  }
  return(dt)
}
