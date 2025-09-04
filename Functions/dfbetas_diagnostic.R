dfbeta_diagnostic = function(dt,dt3,var1,var2,dt2,check_type){
  #dt & dt3 = original data frames where variables are contained (can be an equal data frame)
  #dt2 = new data frame with lists of rows to check (ex. mullen_diagnostic)
  #var 1&2 = two variable names that are used to fit the model; type = character
  #check_type = the variable paris checked; type = character
  
  init_row = dim(dt2)[1] #check how many rows are there in the data frame with discrepant rows
  
  if(all(dim(mullen) != dim(ppvt))){
    #when dt is not the same as dt3
    #we need to merge dt and dt3
    dt_dt3 = merge(dt,dt3,by="id",all=T)
    dt = dt_dt3
    print(dim(dt))
  }
  fit = lm(dt[,var1]~dt[,var2])
  sig = unlist(summary(fit)[4])[8]
  if(sig>=0.05){
    break
  }
  else{
    dfbeta_vec = as.data.frame(dfbetas(fit)[,2])
    colnames(dfbeta_vec)="dfbeta_val"
    dfbeta_vec$row_num = rownames(dfbeta_vec)
    library(sqldf)
    flag = sqldf("select * from dfbeta_vec where abs(dfbeta_val)>=0.22")
    if(length(flag$row_num) != 0){
      library(plyr)
      dt2 = rbind.fill(dt2,dt[c(flag$row_num),])
      dt2$flag[(init_row+1):nrow(dt2)] = check_type
    }
    return(dt2)
  }
  #need to create dt2 = data.frame(dt2) before using this function
  #after running the functions several times, make sure to collapse using ddply
}
#dt2 = ddply(dt2,.(id,visit,age),summarize,flag = paste0(flag,collapse = ", "))
#-> if putting this into the function, you can't use the same function to the same data frame anymore