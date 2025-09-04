merging_brain_staar_study_subjects = function(){
  #querying brain subject data
  app_cohort_subj = sqlQuery(channel = new_con2,
                        query = "SELECT t1.`SUBJ ID` AS subj_id, t1.`SUBJ GRP ID` AS study_cohort,
                        t1.`SUBJ DOB` AS dob, t1.`SUBJ GENDER` AS gender,t1.`SUBJ STATUS` as time1_subj_status,
                        t1.`SUBJ STATUS YEAR 2` as time2_subj_status,t1.`SUBJ STATUS YEAR 3` as time3_subj_status,
                        t1.`SUBJ STATUS YEAR 4` as time4_subj_status,t1.`SUBJ APP DIAG` AS app_diagnosis,t1.`APP_DIAG_Comment`as app_diagnosis_comment
                        FROM tblSubject AS t1 where t1.`SUBJ TYPE`='Primary';",stringsAsFactors=F)

  #querying staar subject data
  star_subj <- sqlQuery(channel = con5,
                        query = "SELECT t1.`SUBJ ID` AS subj_id, 'STAAR' AS study_cohort,
                        t1.`SUBJ STATUS` as subj_status,t1.`SUBJ DOB` AS dob, t1.`SUBJ GENDER` AS gender,
                        t1.`SUBJ APP DIAG` AS app_diagnosis,t1.APP_DIAG_Comment as app_diagnosis_comment,
                        t1.study_arm FROM `tblSubject_Contact` AS t1;",stringsAsFactors=F)
  for(i in 1:nrow(star_subj)){
    star_subj$study_arm[i] = ifelse(grepl("CBT",star_subj$study_arm[i]),1,ifelse(grepl("Med",star_subj$study_arm[i]) | grepl("Sertraline",star_subj$study_arm[i]) | grepl("Placebo",star_subj$study_arm[i]),2,star_subj$study_arm[i]))
  }

  #merging two data
  library(plyr)
  subj = rbind.fill(app_cohort_subj,star_subj)
  subj$gender = ifelse(subj$gender==1,"Male",ifelse(subj$gender==2,"Female",subj$gender))
  return(subj)
}
app_cohort_subj = sqlQuery(new_con2,"select * from tblSubject;",stringsAsFactors=F)
names(app_cohort_subj) = tolower(names(app_cohort_subj))
subj = merging_brain_staar_study_subjects()
subj = sqldf("select t1.*,t2.GUID as NDAR_GUID from subj t1 left join guid t2 on t1.subj_id=t2.id;")
subj = subj[!is.na(subj$study_cohort),]
parents = sqlFetch(new_con2,"tblGuardian_local",stringsAsFactors=F)[,c(1:2,4,8)]
names(parents) = c("parent_id","subj_id","resp","parent dob")
parents$`parent dob` = gsub(" ","",parents$`parent dob`)
parents$dob = gsub("([0-9]{1,2})/([0-9]{1,2})/([0-9]{4})","\\3-\\1-\\2",parents$`parent dob`)

#write.csv(subj[,c(1:3,5:9)],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/participants.csv")