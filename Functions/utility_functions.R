fv <- function(id){
  print(final_visits[which(final_visits$id==id),])
}

dfid <- function(df,id){
  print(df[which(df$id==id),c(1:5,which(grepl("*date*",names(df))))])
}