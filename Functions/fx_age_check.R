#check calculated ages against final_visits ages (fxagecheck finds age differences of given value and replaces them with the visit age, and returns a separate data frame of bad ages)
#df is the data frame to be checked and cleaned
#age_var is the quoted names of the existing age variable (usually calculated using the entered date -- which is often wrong)
#agediff is a numeric value used as a threshold for detecting bad ages

fxagecheck <- function(df,age_var,agediff){
  if(exists("final_visits")==FALSE){
    final_visits <- read.csv("S:/MIND/RESEARCH/Miller/Data Portal/Participants/final_visits_master.csv",stringsAsFactors=F)
  }
  df <- merge(df,setNames(final_visits[,c('id','visit','age_at_testing')],c('id','visit','actual_visit_age')),
              by=c('id','visit'),all.x=TRUE)
  bad_ages <- df[which(abs(df[,age_var]-df$actual_visit_age)>agediff),c('id','visit',age_var,'actual_visit_age')]
  df[,age_var] <- ifelse(abs(df[,age_var]-df$actual_visit_age)>agediff,df$actual_visit_age,df[,age_var])
  df$v_age_check <- NULL
  return(list(df,bad_ages))
}