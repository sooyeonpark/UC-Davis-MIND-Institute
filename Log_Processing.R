# set working directory #
#setwd("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/User logs")

#### get log data ####
# note:  ACE logs.tsv file needs to be downloaded manually each week from dataminder.ucdavis.edu, 
#   renamed to "ACE logs.tsv", and placed into the User logs folder (as per below)
library(sqldf)
acelog = read.table(file='S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/User Logs/ACE Logs.tsv', sep = '\t', header = FALSE,stringsAsFactors = F)
names(acelog) = c('record_id','db_name','id2','user_name','app_name','session_id','process_name','process_status','table_list','session_date')

#### Tasks ####
# Filter data by date (since last report until now)

# Filter data by db_name=="ACE" corresponding to the month
acelog = acelog[acelog$db_name=="ACE",]

# Calculate how many unique session_id there are since last report 
        # (this tells us how many times the app has been used since the last report)
#length(unique(acelog$session_id))

# Calculate how many unique user_names there are since last report 
        # (this tells us how many different users have used the dataportal)
#length(unique(acelog$user_name))

# Calculate which app_names were used:  
        # this may only be the "dataportal.explore" app until more users are using the "dataportal.proposal" app.  
        # If only a single app is being used, just note in the report that that single app is being used
#table(acelog$app_name)

# Calculate how long each session_id lasted from "app startup" to "app shutdown" within each session_id 
        # (sort by record_id and use session_date timestamp to calculate duration in minutes)
        # then calculate average length of session time across session_ids since last report
#not every session has app startup and shutdown AND sometimes shutdown happens waaaay later alone
#issue above led to append the rows of beginning and ending activities to get more accurate result
#SAVE THIS CODE
acelog_start_shut = data.frame()
for(i in 1:(nrow(acelog)-2)){
  if(acelog[i,"process_name"]=="app startup" | (acelog[i,"session_id"] != acelog[(i+2),"session_id"] & acelog[i,"process_name"] != "app shutdown")){
    acelog_start_shut = rbind(acelog_start_shut,acelog[i,])
  }
}
acelog_start_shut$session_date = as.POSIXct(acelog_start_shut$session_date)
acelog_usage_time = sqldf("select session_id,round((max(session_date) - min(session_date))/60,2) as usage_time from acelog_start_shut group by session_id")
mean(acelog_usage_time$usage_time)

#  Calculate how many session_ids include either "histogram" or "pairplot" process_name values 
        # to tell us how many sessions involved someone looking at graphs
        # express this as a proportion of the total number of session_ids since last report that included graphing
#acelog_graph = acelog[acelog$process_name=="histogram" | acelog$process_name=="pairplot",]
#length(unique(acelog_graph$session_id))

#  Calculate how many session_ids included download of data:  look for process_name values such as 
        # "download data", "writexl::write_xlsx" and "data.table::fwrite"
        # and count only one of these per session to indicate that data was downloaded during a session
#download data is data set download, strip_dict_config_columns is for data dictionary
#acelog_download = acelog[grepl("download",acelog$process_name),]
#length(unique(acelog_download$session_id))

#  Calculate what proportion of session_ids involved a Bug report -> SAVE THIS CODE
acelog_bug = acelog[acelog$process_name=="Bug Report",]
length(unique(acelog_bug$session_id))/length(unique(acelog$session_id))

#  Search through and parse the data in the variable "table_list" and, when that variable contains 
        # "tbl_*" values separated by semicolons, save a list of all unique tables queried per session.
        # We want to know the frequency of which tables are being queried.  In other words, for each
        # report, we want to know the number of times each table is queried across sessions. 
        # For example, since the last report, how many sessions involved a query of the ADOS data?
        # How many sessions invovled a query of the CBCL data?  
        # Be careful to only include a count of a table like tbl_CBCL once per session; you will notice
        # that the "table_list" variable contains redundant info, depending on the "process_name", so
        # count a table as "queried" only once per session_id.
# acelog_leftjoin = acelog[grepl("left_join",acelog$process_name) & grepl("tbl_",acelog$table_list),4:(ncol(acelog)-1)]
# acelog_leftjoin = unique(acelog_leftjoin)
# for(i in 1:(nrow(acelog_leftjoin)-1)){
#   if(acelog_leftjoin$session_id[i]==acelog_leftjoin$session_id[i+1]){
#     acelog_leftjoin$table_list[i] = gsub("tbl_participants;tbl_visit;","",acelog_leftjoin$table_list[i])
#     #print(any(strsplit(acelog_leftjoin$table_list[i],';') %in% strsplit(acelog_leftjoin$table_list[i+1],';')))
#     acelog_leftjoin$table_list[i] = ifelse(all(strsplit(acelog_leftjoin$table_list[i],';')[[1]] %in% strsplit(acelog_leftjoin$table_list[i+1],';')[[1]]),"",acelog_leftjoin$table_list[i])
#   }
# }
# acelog_leftjoin = acelog_leftjoin[-which(acelog_leftjoin$table_list==""),]
# char_lists_to_table(acelog_leftjoin$table_list,";")

#clean up
rm(acelog_start_shut,acelog_usage_time,acelog_bug)
