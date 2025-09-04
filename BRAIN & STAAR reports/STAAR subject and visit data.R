#### libraries and options ####

#use RODBC package to connect to access databases
library(RODBC)
library(sqldf)
# defaut option ####
options(stringsAsFactors = FALSE)


#### establish open connection to tracking database ####

con6 <- odbcConnectAccess2007("S:/MIND/RESEARCH/STAAR/Database docs/STAAR Subject Tracking.accdb")

# get list of available tables in database
available_tables6 <- sqlTables(con6)



#### query the data ####

star_subj <- sqlQuery(channel = con6,
                 query = "SELECT t1.`SUBJ ID` AS subj_id, 'STAAR' AS study_cohort, t1.`SUBJ DOB` AS dob, t1.`SUBJ GENDER` AS gender,
                 t1.`SUBJ STATUS` AS subj_status, 
                 t1.`SUBJ APP DIAG` AS app_diagnosis, t1.`SUBJ LANGU` AS intake_lang, 
                 t1.study_arm, t1.iq_score, t1.iq_measure, t1.date_consented, t1.pars_score, t1.treatment_day
                 FROM `tblSubject_Contact` AS t1;")

star_visits = sqlQuery(channel = con6,
                  "SELECT `SUBJ ID` AS subj_id,
                  VisitDate as encounter_date, VisitNumber as visit
                  FROM tblVisits;")

rndmz <- sqlQuery(channel=con6,
                  query = "SELECT * FROM tbl_rndmz;")


#### close db connection ####

odbcClose(con6) #clean up by closing connection
rm(con6, available_tables6)

#### explore and format data ####
names(subj)
# names(visits)
names(rndmz)

# Join (merge) subj and rndmz data frames
star = sqldf("select * from subj_star left join rndmz on subj_star.subj_id = rndmz.id")

# Join (merge) subj and visits data frames (unless no visit data yet)
star_combined = sqldf("select * from star left join visits on star.subj_id = visits.subj_id")

# add new variable for query_date (i.e., current system date)
star_combined$query_date = Sys.Date()

# calculate each subjects current age in months (precision to 2 decimal places) using the query date; name the variable as "current_age"
star_combined$current_age = as.Date(star_combined$query_date) - as.Date(star_combined$dob) #get the age in the number of days
star_combined$current_age = star_combined$current_age/365.25*12
star_combined$current_age = format(round(star_combined$current_age,2),nsmall=2)

# calculate each subjects' age at time of randomization
star_combined$age_frm_rndmz = as.Date(star_combined$date_of_randomization) - as.Date(star_combined$dob) #get the age in the number of days
star_combined$age_frm_rndmz = star_combined$age_frm_rndmz/365.25*12
star_combined$age_frm_rndmz = format(round(star_combined$age_frm_rndmz,2),nsmall=2)

# recode gender from numeric to a character string (all lower case) (1=male, 2=female)
star_combined$gender[star_combined$gender == "1"] = "male"
star_combined$gender[star_combined$gender == "2"] = "female"

# report table of subject status -- how many are active?  how many dropped? etc.
names(star_combined)
star_combined$subj_status = as.factor(star_combined$subj_status)
star_subj_status_summary = data.frame(summary(star_combined$subj_status))
colnames(star_subj_status_summary) = "Subject Status"
star_combined$subj_status = as.character(star_combined$subj_status)

# report table of diagnoses -- what is the diagnosis of each subject?
star_combined$app_diagnosis = as.factor(star_combined$app_diagnosis)
star_app_diagnosis_table = data.frame(summary(star_combined$app_diagnosis))
colnames(star_app_diagnosis_table) = "Diagnosis_app"
star_combined$app_diagnosis = as.character(star_combined$app_diagnosis)

# report table for number and types of visit (unless no visit data yet)
#no visited yet

# calculate ages (in months, to 2 decimal places) at each visit (name the variable as "age_at_visit")
#na

# strip time out of date_of_randomization.  Format should be yyyy-mm-dd
star$date_of_randomization = format(as.POSIXct(star$date_of_randomization,format='%m/%d/%Y %H:%M:%S'),format='%m/%d/%Y')
summary(star)

# query indicators of all available data in data entry systems for STAAR (not actual data, just how many records in measure tables for each subject?)