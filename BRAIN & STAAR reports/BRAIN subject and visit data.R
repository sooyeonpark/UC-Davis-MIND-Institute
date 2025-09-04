#### libraries and options ####

#stick to basic R functions
#use RODBC package to connect to access databases
library(RODBC)
# defaut option ####
options(stringsAsFactors = F)
#use sqldf
library(sqldf)

#### establish open connection to tracking database ####

con5 <- odbcConnectAccess2007("S:/MIND/RESEARCH/APP/APP Database/Backend/APPDatabase2009_2009-01-15_be.accdb")

# get list of available tables in database
available_tables5 <- sqlTables(con5)

#tblBehSurveys

#### query the data ####

brain_subj <- sqlQuery(channel = con5,
                 query = "SELECT t1.`SUBJ ID` AS subj_id, t1.`SUBJ GRP ID` AS study_cohort, t1.`SUBJ DOB` AS dob, t1.`SUBJ GENDER` AS gender,
                 t1.`SUBJ STATUS` AS subj_status, t1.`SUBJ INI DIAG` AS initial_diagnosis, 
                 t1.`SUBJ APP DIAG` AS app_diagnosis, t1.`SUBJ OTHER DIAG` AS other_diagnosis, t1.`SUBJ LANGU` AS intake_lang
                 FROM `tblSubject_Contact` AS t1;")


app_proc = sqlQuery(con5,"select * from tblAPP_Protocol;")
behav_assess = sqlQuery(con5,"select * from tblBehAssess;")
behav_surveys = sqlQuery(con5,"select * from tblBehSurveys;")

names(behav_assess)[3] = "Comp Date"
brain_visits = merge(behav_assess,behav_surveys,by=c("STS ID","Comp Date","Type","Status","Comment",
                                                      "Missing Data"),all=T)
rm(behav_assess,behav_surveys)

#encounter_date, t2.encounter_type 
#### close db connection ####

odbcClose(con5) #clean up by closing connection
rm(con5, available_tables5)

#### explore and format data ####
names(brain_subj)
names(brain_visits)

# report table of frequencies for all study_cohorts.  How many are in BRAIN?
subj$study_cohort = as.factor(subj$study_cohort)
study_cohort_summary = data.frame(summary(subj$study_cohort))
colnames(study_cohort_summary) = "Study_Cohorts"
subj$study_cohort = as.character(subj$study_cohort)

# Filter subj data frame for only those where study_cohort='BRAIN'
brain = sqldf("select * from subj where study_cohort like 'BRAIN'")
brain$query_date = Sys.Date()
brain$current_age = as.Date(brain$query_date) - as.Date(brain$dob) #get the age in the number of days
brain$current_age = brain$current_age/365.25*12
brain$gender[brain$gender == "1"] = "male"
brain$gender[brain$gender == "2"] = "female"
brain[] <- lapply(brain, function(x) if(is.factor(x)) factor(x) else x)

# Join (merge) subj and visits data frames #subj left join
combined = sqldf("select * from brain left join visits on brain.subj_id = visits.subj_id")

# add new variable for query_date (i.e., current system date)
combined$query_date = Sys.Date()

# calculate each subjects current age in months (precision to 2 decimal places) using the query date; name the variable as "current_age"
combined$current_age = as.Date(combined$query_date) - as.Date(combined$dob) #get the age in the number of days
combined$current_age = combined$current_age/365.25*12
#combined$current_age = format(round(combined$current_age,2),nsmall=2)

# recode gender from numeric to a character string (all lower case) (1=male, 2=female)
combined$gender[combined$gender == "1"] = "male"
combined$gender[combined$gender == "2"] = "female"

# report table of subject status -- how many are active?  how many dropped? etc.
names(combined)
combined$subj_status = as.factor(combined$subj_status)
subj_status_summary = data.frame(summary(combined$subj_status))
colnames(subj_status_summary) = "Subject Status"
combined$subj_status = as.character(combined$subj_status)

# report table of diagnoses -- what is the diagnosis of each subject?  How many ASD?  How many typical?
#initial
combined$initial_diagnosis = as.factor(combined$initial_diagnosis)
initial_diagnosis_table = data.frame(summary(combined$initial_diagnosis))
colnames(initial_diagnosis_table) = "Diagnosis_initial"
combined$initial_diagnosis = as.character(combined$initial_diagnosis)
#app
combined$app_diagnosis = as.factor(combined$app_diagnosis)
app_diagnosis_table = data.frame(summary(combined$app_diagnosis))
colnames(app_diagnosis_table) = "Diagnosis_app"
combined$app_diagnosis = as.character(combined$app_diagnosis)

# report table for number and types of visit
combined$encounter_type = as.factor(combined$encounter_type)
visit_type_table = data.frame(summary(combined$encounter_type))
colnames(visit_type_table) = "Visit_type"
combined$app_diagnosis = as.character(combined$app_diagnosis)

# filter visits data for only actual assessment visits (e.g., delete out emails, etc. etc.)
combined$encounter_type = as.factor(combined$encounter_type)
visit_only = subset(combined, encounter_type == "Visit")
combined$encounter_type = as.character(combined$encounter_type)

# calculate ages (in months, to 2 decimal places) at each visit (name the variable as "age_at_visit")
visit_only$age_at_visit = as.Date(visit_only$encounter_date) - as.Date(visit_only$dob) #get the age in the number of days
visit_only$age_at_visit = visit_only$age_at_visit/365.25*12
#visit_only$age_at_visit = format(round(visit_only$age_at_visit,2),nsmall=2)

# erasing the time from encounter date
visit_only$encounter_date = format(as.POSIXct(visit_only$encounter_date,format='%m/%d/%Y %H:%M:%S'),format='%m/%d/%Y')

# query indicators of all available data in data entry systems for BRAIN (not actual data, just how many records in measure tables for each subject?)
str(visit_only)
summary(visit_only)

combined[] <- lapply(combined, function(x) if(is.factor(x)) factor(x) else x)
#questions
#1. only male subjects so far?
#2. diff btw app_diag and initial_diag