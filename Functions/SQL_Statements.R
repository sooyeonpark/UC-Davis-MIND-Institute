create_table_statement = function(dt, test_measure){
  #num_var = number of variables in the table you want to put in
  #test_measure in character string
  test_measure = tolower(test_measure)
  table_name= paste0("tbl_",test_measure)
  if(class(dt$id) != "character"){
    dt$id = as.character(dt$id)
  }
  var_name_type_string = ""
  for(i in 1:(ncol(dt)-1)){
    if(class(dt[,i])=="factor"){
      dt[,i] = as.character(dt[,i])
    }
    #putting variable name and class together
    var_namentype = paste0(names(dt)[i]," ",class(dt[,i]),", ")
    #concactenate the string
    var_name_type_string = paste0(var_name_type_string,var_namentype)
  }
  if(class(dt[,ncol(dt)])=="factor"){
    dt[,ncol(dt)] = as.character(dt[,ncol(dt)])
  }
  var_name_type_string = paste0(var_name_type_string,names(dt)[ncol(dt)]," ",class(dt[,ncol(dt)]),");")
  create_table_statement = paste0("create table ",table_name,"(pk_",test_measure," int auto_increment, ",
                                  var_name_type_string)
  return(create_table_statement)
}

insert_statement = function(con,test_measure,dt){
  #make sure the test_measure is in a lower case
  test_measure = tolower(test_measure)
  table_name= paste0("tbl_",test_measure)

  #delete the table first before inserting rows in
  dbExecute(con,paste0('delete from ',table_name))

  #making into sql format(factors into characters and character variables need '' around the variables)
  for(j in 1:ncol(dt)){
    if(class(dt[,j]) == "factor"){
      dt[,j] = as.character(dt[,j])
    }
    if(class(dt[,j])=="character"){
      dt[which(dt[,j]=="NA"),j] = ""
      dt[which(is.na(dt[,j])),j] = ""
      for(i in 1:nrow(dt)){
        dt[i,j] = gsub("'","''",dt[i,j])
        dt[i,j] = paste0("'",dt[i,j],"'")
      }
    }
  }
  #generating statements
  insert_statements = c()
  var_vals = c() #generating vectors with each the list of values in a row
  for(i in 1:nrow(dt)){
    for(j in 1:(ncol(dt)-1)){
      var_val = paste0(dt[i,j],", ")
      var_val = gsub("^NA","NULL",var_val)
      var_vals[i] = paste0(var_vals[i],var_val)
    }
    var_vals[i] = paste0(var_vals[i],dt[i,ncol(dt)],");")
    var_vals[i] = gsub("NA);$","NULL);",var_vals[i])

    #somehow from the 2nd statement, "NA" appears in the beginning of the statement -> clean up
    if(length(grep("^NA",var_vals[i],value=T)) != 0){
      var_vals[i] = gsub("^NA","",var_vals[i])
    }
    #print(var_vals[i])

    #generating insert statement
    insert_statements[i] = paste0("insert into ",table_name," values (",var_vals[i])
    dbExecute(con,insert_statements[i])
  }
}

updating_table = function(con,test_measure,dt){
  #make sure the test_measure is in a lower case
  test_measure = tolower(test_measure)
  table_name= paste0("tbl_",test_measure)
  
  #delete the table first before inserting rows in
  dbExecute(con,paste0('delete from ',table_name))
  
  #append table rows
  dbWriteTable(con,table_name,dt,row.names=F,append=T,overwrite=F)
}
