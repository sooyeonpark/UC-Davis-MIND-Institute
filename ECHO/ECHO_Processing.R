library(readxl)
library(plyr)
library(writexl)
# attendance = list.files(pattern="*.xlsx")
# attendance = lapply(attendance,read_xlsx)
# attendance_data = data.frame("FirstName","LastName")
# names(attendance_data) = attendance_data[1,]
# for(l in 1:length(attendance)){
#   attendance_data = rbind(attendance_data,attendance[[l]][c("FirstName","LastName")])
# }
# attendance_data = attendance_data[-1,]
# attendance_data = subset(attendance_data,!is.na(FirstName))
# attendance_final = data.frame(table(attendance_data$FirstName,attendance_data$LastName))
# attendance_final = attendance_data_final[attendance_data_final$Freq!=0,]
# write_xlsx(attendance_final,"attendance.xlsx")
# 
# #use in appropriate directories
# setwd("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/ECHO/Pre Surveys")
# pre_surveys = list.files(pattern = "*.xlsx")
# pre_surveys = lapply(pre_surveys,read_xlsx)
# setwd("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/ECHO/Post Surveys")
# post_surveys = list.files(pattern = "*.xlsx")
# post_surveys = lapply(post_surveys,read_xlsx)
# setwd("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/ECHO/")
# pre_survey = pre_surveys[[1]]
# post_survey = post_surveys[[1]]
# for(l in 2:length(pre_surveys)){
#   pre_survey = rbind.fill(pre_survey,pre_surveys[[l]])
# }
# for(l in 2:length(post_surveys)){
#   post_survey = rbind.fill(post_survey,post_surveys[[l]])
# }
# pre_survey = pre_survey[pre_survey$Finished=="True"
#                         & !is.na(pre_survey$RecipientFirstName),] 
# post_survey = post_survey[post_survey$Finished=="True"
#                           & !is.na(post_survey$RecipientFirstName),]
# pre_survey = inserting_prefix_into_variables(pre_survey,'pre_')
# post_survey = inserting_prefix_into_variables(post_survey,'post_')
# names(pre_survey)[c(1:7,9:11)] = gsub("^pre_","",names(pre_survey[c(1:7,9:11)]))
# names(post_survey)[c(1:7,9:11)] = gsub("^post_","",names(post_survey[c(1:7,9:11)]))
# pre_post_final = merge(pre_survey[!is.na(pre_survey$RecipientFirstName),c(grep("_RecordedDate$",names(pre_survey)),
#                                                                           grep("FirstName$",names(pre_survey)),grep("LastName$",names(pre_survey)),
#                                                                           grep("Q38_[0-9]{1}$",names(pre_survey)),
#                                                                           grep("Q41_[0-9]{1}$",names(pre_survey)))],
#                        post_survey[!is.na(post_survey$RecipientFirstName),c(grep("_RecordedDate$",names(pre_survey)),
#                                                                             grep("FirstName$",names(pre_survey)),grep("LastName$",names(pre_survey)),
#                                                                             grep("Q38_[0-9]{1}$",names(pre_survey)),
#                                                                             grep("Q41_[0-9]{1}$",names(pre_survey)))],all=T)
# pre_post_final = subset(pre_post_final,!is.na(pre_RecordedDate) & !is.na(post_RecordedDate))
# pre_post_final = unique(pre_post_final)
# write_xlsx(pre_post_final,"pre_post_final.xlsx")
# rm(pre_post_final)

#bringing cleaned pre_post survey
pre_post_final_cleaned=read_xlsx("pre_post_final.xlsx")
#importing list variables
pre_post_final2 = data.frame(1:18)
for(j in 1:ncol(pre_post_final_cleaned)){
  pre_post_final2 = cbind(pre_post_final2,pre_post_final_cleaned[,j][[1]])
  names(pre_post_final2)[ncol(pre_post_final2)] = names(pre_post_final_cleaned)[j]
}
rm(pre_post_final_cleaned)

#changing Q38 & 41'S categories to numeric scales
for(j in grep("38",names(pre_post_final2))){
  pre_post_final2[,j] = ifelse(grepl("Very Low",pre_post_final2[,j]),1,
                                      ifelse(grepl("^Low",pre_post_final2[,j]),2,
                                             ifelse(grepl("^Average",pre_post_final2[,j]),3,
                                                    ifelse(grepl("^High",pre_post_final2[,j]),4,
                                                           ifelse(grepl("Very High",pre_post_final2[,j]),5,NA)))))
  pre_post_final2[,j] = as.numeric(pre_post_final2[,j])
}
for(j in grep("41",names(pre_post_final2))){
  pre_post_final2[,j] = ifelse(grepl("^No",pre_post_final2[,j]),1,
                               ifelse(grepl("^Some",pre_post_final2[,j]),2,
                                      ifelse(grepl("^Average",pre_post_final2[,j]),3,
                                             ifelse(grepl("^Above",pre_post_final2[,j]),4,
                                                    ifelse(grepl("Expert",pre_post_final2[,j]),5,NA)))))
  pre_post_final2[,j] = as.numeric(pre_post_final2[,j])
}

#obtaining avg of Q38 and 41
for(i in 1:nrow(pre_post_final2)){
  pre_post_final2$pre_Q38_avg[i] = round(mean(unlist(pre_post_final2[i,grep("pre_Q38_[0-9]{1}$",names(pre_post_final2))]),na.rm=T),2)
  pre_post_final2$pre_Q41_avg[i] = round(mean(unlist(pre_post_final2[i,grep("pre_Q41_",names(pre_post_final2))]),na.rm=T),2)
  pre_post_final2$post_Q38_avg[i] = round(mean(unlist(pre_post_final2[i,grep("post_Q38_[0-9]{1}$",names(pre_post_final2))]),na.rm=T),2)
  pre_post_final2$post_Q41_avg[i] = round(mean(unlist(pre_post_final2[i,grep("post_Q41_",names(pre_post_final2))]),na.rm=T),2)
}
pre_post_final2 = pre_post_final2[,-1]

#importing attendance spreadsheet
attendance_edited <- read_excel("attendance_edited.xlsx")

#merge attendance information to consolidated dataset
attendance_edited$RecipientFirstName = tolower(attendance_edited$RecipientFirstName)
attendance_edited$RecipientLastName = tolower(attendance_edited$RecipientLastName)
pre_post_final2$RecipientFirstName = tolower(pre_post_final2$RecipientFirstName)
pre_post_final2$RecipientLastName = tolower(pre_post_final2$RecipientLastName)
all(pre_post_final2$RecipientFirstName %in% attendance_edited$RecipientFirstName)
all(pre_post_final2$RecipientLastName %in% attendance_edited$RecipientLastName)
pre_post_final2 = merge(pre_post_final2,attendance_edited,all.x=T)

#putting together demographic information
pre_post_demographic = merge(pre_survey[,c(10:11,grep("_RecordedDate$",names(pre_survey)),grep("_Q1$",names(pre_survey)),grep("_Q8$",names(pre_survey)),grep("_Q40$",names(pre_survey)),grep("_Q37...27$",names(pre_survey)))],
                             post_survey[,c(10:11,grep("_RecordedDate$",names(post_survey)),grep("_Q1$",names(post_survey)),grep("_Q8$",names(pre_survey)),grep("_Q40$",names(post_survey)),grep("_Q37...27$",names(post_survey)))],all=T)
pre_post_demographic$RecipientFirstName = tolower(pre_post_demographic$RecipientFirstName)
pre_post_demographic$RecipientLastName = tolower(pre_post_demographic$RecipientLastName)
all(pre_post_final2$RecipientFirstName %in% pre_post_demographic$RecipientFirstName)
all(pre_post_final2$RecipientLastName %in% pre_post_demographic$RecipientLastName)
pre_post_final2=merge(pre_post_final2,pre_post_demographic,all.x=T)

#getting rid of Ramiro participant because pre survey date is later than post
pre_post_final2 = pre_post_final2[pre_post_final2$pre_RecordedDate<pre_post_final2$post_RecordedDate,]

# Q1 -> age range
# Q40 -> national, international
# Q37 ->years of practice
# Q8 -> profession
# Q41 -> rate your knowlege
# Q38 -> rate your confidence

table(pre_post_final2$session_attended) #pretty even but bimodal
mean(pre_post_final2$session_attended) #12.12
pre_post_final2$attendance = ifelse(pre_post_final2$session_attended<12,"Low","High")
#age range -> mostly 31-40, otherwise equally distributed
table(pre_post_final2$pre_Q1)
table(pre_post_final2$post_Q1)
#profession -> mostly psychologists and behavior specialists (~10+)
table(pre_post_final2$pre_Q8)
table(pre_post_final2$post_Q8)
# table(pre_post_final2$pre_Q1,pre_post_final2$session_attended)
# table(pre_post_final2$post_Q1,pre_post_final2$session_attended)
#years of exp
table(pre_post_final2$pre_Q37...27) #-> right skewed bimodal
table(pre_post_final2$post_Q37...27) #-> equally distributed
#national vs international
table(pre_post_final2$pre_Q40) #national 11, int 6
table(pre_post_final2$post_Q40)

#attendance and post survey
mean(pre_post_final2$pre_Q38_avg) #->3.70 sd = 0.90
mean(pre_post_final2$pre_Q41_avg) #->3.08 sd = 0.70
mean(pre_post_final2$post_Q38_avg) #->3.98 sd = 0.75
mean(pre_post_final2$post_Q41_avg) #->3.10 sd = 0.57
pre_post_final2$pre_Q41 = ifelse(pre_post_final2$pre_Q41_avg<round(mean(pre_post_final2$pre_Q41_avg),2),"Low","High")
pre_post_final2$post_Q41 = ifelse(pre_post_final2$post_Q41_avg<round(mean(pre_post_final2$post_Q41_avg),2),"Low","High")
pre_post_final2$post_Q38 = ifelse(pre_post_final2$post_Q38_avg<round(mean(pre_post_final2$post_Q38_avg),2),"Low","High")
#very few low knowledge rate when attendance was high compared to pre survey
table(pre_post_final2$post_Q38,pre_post_final2$attendance)
table(pre_post_final2$pre_Q41,pre_post_final2$attendance)
table(pre_post_final2$post_Q41,pre_post_final2$attendance)

write_xlsx(pre_post_final2,"pre_post_final^2.xlsx")
#satisfaction
clinic_eval$RecipientFirstName = tolower(clinic_eval$RecipientFirstName)
clinic_eval$RecipientLastName = tolower(clinic_eval$RecipientLastName)
pre_post_final2 = merge(pre_post_final2,clinic_eval[,c(8,10:11,grep("Q6_",names(clinic_eval)))],all.x=T)
