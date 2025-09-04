library(qualtRics)
##referral form
referral = readSurvey("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/Family Navigator/referral_form.csv")

#replacing slash to dash in the dates
split_dates = strsplit(referral$referral_date,"/")
split_dates2 = strsplit(referral$dob,"/")
for(j in c(split_dates,split_dates2)){
  for(i in 1:nrow(referral)){
    referral[i,"referral_date"] = ifelse(!is.na(referral[i,"referral_date"]),paste0(split_dates[[i]][3],'-',split_dates[[i]][1],'-',split_dates[[i]][2]),NA)
    referral[i,"dob"] = ifelse(!is.na(referral[i,"dob"]),paste0(split_dates2[[i]][3],'-',split_dates2[[i]][1],'-',split_dates2[[i]][2]),NA)
  }
}
rm(split_dates,split_dates2)

#calculate age
referral$child_age = elapsed_months(referral$referral_date,referral$dob)

#obtaining resources available
referral$resource_available = gsub("Speech, OT","Speech & OT",referral$resource_available)
char_lists_to_table(referral$resource_available,",")
table(referral$resource_available_12_TEXT)

#diagnoses
referral$asd_diagnosis = ifelse(grepl("asd",tolower(referral$diagnosis)) | grepl("autism",tolower(referral$diagnosis)),"Yes","No")
referral$adhd_disagnosis = ifelse(grepl("adhd",tolower(referral$diagnosis)),"Yes","No")
referral$delay = ifelse(grepl("delay",tolower(referral$diagnosis)) | grepl("gdd",tolower(referral$diagnosis)),"Yes","No")

##activity log
activity_log = readSurvey("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/Family Navigator/activity_log.csv")

#need to make the data in a longer format
actqs = paste0('_activity_',c('date','location','time_3','content','outcome'))
actlog = data.frame()
for(i in 1:nrow(activity_log)){
  row = removing_prefix(activity_log[i,],'1_')
  names(row) = gsub("location_2","location",names(row))
  names(row) = gsub("content_6","content",names(row))
  #print(names(row)[grep("^activity_date$",names(row)):grep("^notes$",names(row))])
  actlog = rbind(actlog,row[,c(1:2,grep("^activity_date$",names(row)):grep("^notes$",names(row)))])
  for(j in 2:7){
    if(!all(is.na(activity_log[i,paste0(j,actqs)]))){
      row = removing_prefix(activity_log[i,],paste0(j,'_'))
      names(row) = gsub("location_2","location",names(row))
      names(row) = gsub("content_6","content",names(row))
      #print(names(row)[grep("^activity_date$",names(row)):grep("^notes$",names(row))])
      actlog = rbind(actlog,row[,c(1:2,grep("^activity_date$",names(row)):grep("^notes$",names(row)))])
    }
  }
  rm(row)
}
actlog = actlog[,-(6:7)]

fn = merge(referral,actlog,all.x=T)
length(unique(actlog$family_id))
char_lists_to_table(actlog$activity_content,',')

#clean up
rm(actqs,activity_log)
