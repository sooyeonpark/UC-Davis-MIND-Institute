inserting_prefix_into_variables = function(df,prefix){
  id_var_index = grep("^id",names(df))
  visit_var_index = grep("^visit",names(df))
  names(df) = paste0(prefix,names(df))
  names(df)[id_var_index] = gsub(prefix,"",names(df)[id_var_index])
  names(df)[visit_var_index] = gsub(prefix,"",names(df)[visit_var_index])
  # skip any variables that already have the prefix somewhere in the name
  return(df)
}

removing_prefix = function(df,prefix){
  names(df) = gsub(prefix,"",names(df))
  return(df)
}
