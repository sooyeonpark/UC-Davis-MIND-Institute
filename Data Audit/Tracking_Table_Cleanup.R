behav_assess = sqlQuery(new_con2,"select * from tblBehAssess_local;",stringsAsFactors=F)
behav_surveys = sqlQuery(new_con2,"select * from tblBehSurveys_local;",stringsAsFactors=F)

#correcting ssp labels in the survey table
# ssp1_t4_id = c("101430-200",'101443-100','107330-100')
# behav_surveys$Type = ifelse(behav_surveys$Type=="SSP" & behav_surveys$`STS ID` %in% ssp[ssp$visit=="1","id"],"SSP1",
#                             ifelse(behav_surveys$Type=="SSP" & behav_surveys$`STS ID` %in% ssp2[ssp2$visit=="1","id"],"SSP2",
#                                    ifelse(behav_surveys$Type=="SSP (3)" & behav_surveys$`STS ID` %in% ssp[ssp$visit=="3","id"],"SSP1 (3)",
#                                           ifelse(behav_surveys$Type=="SSP (3)" & behav_surveys$`STS ID` %in% ssp2[ssp2$visit=="3","id"],"SSP2 (3)",
#                                                  ifelse(behav_surveys$Type=="SSP1 (4)" & !(behav_surveys$`STS ID` %in% ssp1_t4_id) & behav_surveys$`STS ID` %in% ssp2[ssp2$visit=="4","id"],"SSP2 (4)",behav_surveys$Type)))))
# rm(ssp1_t4_id)

#fixing erroneous tracking entries
# behav_assess[which(behav_assess$Status %in% "Missing"),"Status"] = "Missing Data"
# behav_assess[which(behav_assess$Status=="refused"),"Status"] = "Refused"
# behav_assess[which(behav_assess$Status=="sent"),"Status"] = "Sent"
behav_assess[which(is.na(behav_assess$Status)),"Status"] = "N/A"
# behav_assess[behav_assess$Status=="NA","Status"] = "N/A"
behav_assess[grepl("unable",tolower(behav_assess$Status)) | grepl("unable",tolower(behav_assess$Comment)),"Status"] = "Incomplete"
# behav_assess$Type = gsub("(4p)","4P",behav_assess$Type)
#behav_assess[which(behav_assess$Status=="Missing Data" | behav_assess$Status=="Incomplete"),"Missing Data"]=TRUE

#behav_surveys[which(behav_surveys$Status %in% c("missing Data","Missing")),"Status"] = "Missing Data"
#behav_surveys[which(behav_surveys$Status=="validated"),"Status"] = "Validated"
behav_surveys[which(is.na(behav_surveys$Status)),"Status"] = "N/A"
#behav_surveys$Type = gsub("^demographic","Demographic",behav_surveys$Type)
#behav_surveys[which(tolower(behav_surveys$Type)=="srs  (3)"),"Type"] = "SRS (3)"
#behav_surveys[which(behav_surveys$Type=="masc_P (4)"),"Type"] = "MASC_P (4)"
#behav_surveys[which(behav_surveys$Type=="Scq"),"Type"] = "SCQ"
#behav_surveys[which(tolower(behav_surveys$Type)=="masc_p(4)"),"Type"] = "MASC_P (4)"
#behav_surveys[which(tolower(behav_surveys$Type)=="scared_p(4)"),"Type"] = "SCARED_P (4)"
#behav_surveys[which(tolower(behav_surveys$Type)=="scared_c(4)"),"Type"] = "SCARED_C (4)"
#behav_surveys$Type = gsub("(4p)","4P",behav_surveys$Type)
#behav_surveys[which(behav_surveys$Status=="Missing Data" | behav_surveys$Status=="Incomplete"),"Missing Data"]=TRUE

#getting rid of MSEL (3) where there is DAS (3)
# msel3 = subset(behav_assess,Type=="MSEL (3)")
# msel3_das3_id = msel3[which(msel3$`STS ID` %in% behav_assess[behav_assess$Type=="DAS (3)","STS ID"]),1]
# msel3_das3 = subset(longitudinal_iq,visit=='3' & id %in% msel3_das3_id)
# for(i in grep("DAS",msel3_das3$iq_version)){
#   #print(which(behav_assess$Type=="MSEL (3)" & behav_assess$`STS ID`==msel3_das3$id[i]))
#   behav_assess = behav_assess[-which(behav_assess$Type=="MSEL (3)" & behav_assess$`STS ID`==msel3_das3$id[i]),]
# }
# rm(msel3,msel3_das3_id,msel3_das3)

#removing das rows where msel at t3 instead of das
# das3 = behav_assess[behav_assess$Type=="DAS (3)",]
# for(i in 1:nrow(das3)){
#   if(das3$`STS ID`[i] %in% mullen[mullen$visit=="3","id"]){
#     print(das3$`STS ID`[i])
#     behav_assess = behav_assess[-which(behav_assess$`STS ID` == das3$`STS ID`[i]
#                                        & behav_assess$Type == "DAS (3)"),]
#   }
# }
# rm(das3)

#dealing with SSP & MSEL rows with all NA's
behav_surveys[which(behav_surveys$`STS ID`%in% ssp[grep("All data",ssp$ssp_missing_items_comment),"id"]
                    & grepl("SSP1",behav_surveys$Type)),"Status"] = "Missing Data"
behav_assess[which(behav_surveys$`STS ID`%in% mullen[grep("All data",mullen$msel_missing_items_comment),"id"]
                   & grepl("MSEL",behav_surveys$Type)),"Status"] = "Missing Data"

#merging two tables
names(behav_assess)[which(names(behav_assess)=='Delivered Date')] = "Comp Date"
assess_survey = merge(behav_assess,behav_surveys,by=c("STS ID","Comp Date","Type","Status","Comment","Missing Data"),all=T)
assess_survey = unique(assess_survey)
assess_survey_primary = subset(assess_survey,assess_survey$`STS ID` %in% subj$subj_id)