#importing necessary package and tables
library(sqldf)
library(readxl)
library(writexl)
#make sure to change the spreadsheet name and range accordingly
dsmb_dem = read_excel("DSMB/DSMB_STAAR_4th.xlsx",sheet="Demographics",range = "A3:I54",
                      col_types = c("text","text", "date", "numeric", "text","text", "text", "date", "text"))
dsmb_ae = read_excel("DSMB/DSMB_STAAR_4th.xlsx",sheet="AE Table",range="A3:K144",
                     col_types = c("text", "text", "text","date", "date", "text", "text", "text","text", "text", "text"))
names(dsmb_dem) = c("dummy_id","blind_group","dob","enrollment_age","gender","race","ethnicity","consent_date","status")
names(dsmb_ae) = c("dummy_id","ae","category","start_date","end_date","severity","frequency","action_taken","outcome","study_med_relation","serious_ae")

##Demographics##
#recruitment numbers by groups
dsmb_recruitment_count(dsmb_dem,"2018-05-01","2019-01-01",T)
dsmb_recruitment_count(dsmb_dem,"2019-01-01","2019-07-01",T)
dsmb_recruitment_count(dsmb_dem,"2019-07-01","2020-01-01",T)
dsmb_recruitment_count(dsmb_dem,"2020-01-01","2020-07-01",T)
dsmb_recruitment_count(dsmb_dem,"2020-07-01","2021-01-01",T)
dsmb_recruitment_count(dsmb_dem,"2021-01-01","2021-07-01",T)
dsmb_recruitment_count(dsmb_dem,"2021-07-01","2022-01-01",T)
dsmb_recruitment_count(dsmb_dem,"2022-01-01","2022-02-28",T)

#study status
write.table(sqldf("select status,count(status) from dsmb_dem group by status"),
            "S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/DSMB/dsmb_recruitment.csv",
            row.names=F,sep=",",append=T)

#gender
table(dsmb_dem$gender)

#ethnicity
table(dsmb_dem$ethnicity,dsmb_dem$blind_group)

#race
dsmb_dem_mixed = dsmb_dem[grepl('and',dsmb_dem$race),]
dsmb_dem_non_mixed = dsmb_dem[!grepl('and',dsmb_dem$race),]
table(dsmb_dem_non_mixed$race,dsmb_dem_non_mixed$blind_group)
table(dsmb_dem_mixed$race,dsmb_dem_mixed$blind_group)

#enrollment age & anova test
#dsmb_dem$current_age = round(elapsed_months(Sys.Date(),dsmb_dem$dob)/12,2)
dsmb_dem$enrollment_age_exact = round(elapsed_months(dsmb_dem$consent_date,dsmb_dem$dob)/12,2)
sqldf("select blind_group,avg(enrollment_age_exact) as mean_age,median(enrollment_age_exact) as median_age,
      stdev(enrollment_age_exact) as sd_age, min(enrollment_age_exact) as min_age, max(enrollment_age_exact) as max_age
      from dsmb_dem group by blind_group")
summary(aov(dsmb_dem$enrollment_age_exact~dsmb_dem$blind_group))

#fisher exact test for group differences
fisher.test(dsmb_dem$gender,dsmb_dem$blind_group)
fisher.test(dsmb_dem$ethnicity,dsmb_dem$blind_group)
fisher.test(dsmb_dem$race,dsmb_dem$blind_group)

##Adverse Events##
dsmb_ae = merge(dsmb_ae,dsmb_dem[,1:2],all.x=T)
ae_freq = sqldf("select dummy_id,count(dummy_id) as freq from dsmb_ae group by dummy_id")
ae_freq = merge(dsmb_dem[,1],ae_freq,all.x=T)
ae_freq$freq = ifelse(is.na(ae_freq$freq),0,ae_freq$freq)
mean(ae_freq$freq)
sd(ae_freq$freq)

#ae_category 
#total number of events
write.csv(table(dsmb_ae$category),"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/DSMB/ae_category.csv",row.names=F)
#ae event counts by participant
ae_summary = sqldf("select category,dummy_id,count(dummy_id) as freq_within_id from dsmb_ae group by category,dummy_id")
write.table(ae_summary,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/DSMB/ae_category.csv",row.names=F,sep=",",append=T)
#total number of unique participants experiencing different ae's
write.table(sqldf("select category,count(category) as freq_participant from ae_summary group by category"),
            "S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/DSMB/ae_category.csv",
            row.names=F,sep=",",append=T)

#ae_category by group
ae_summary_group = sqldf("select category,blind_group,count(dummy_id) as freq_within_id
                         from dsmb_ae group by category,dummy_id,blind_group order by category,blind_group")
write.table(sqldf("select category,blind_group,count(blind_group) as participant_num,sum(freq_within_id) as incident_total
                  from ae_summary_group group by category,blind_group order by category,blind_group"),
            "S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/DSMB/ae_category.csv",row.names=F,sep=",",append=T)

#severity & fisher exact test
dsmb_sev_tbl1 = unique(merge(dsmb_dem[,1:2],dsmb_ae[,c(1,6)],all.x=T))
dsmb_sev_tbl1 = sqldf("select dummy_id,blind_group,max(severity) as severity_max from dsmb_sev_tbl1
                      group by dummy_id,blind_group")
dsmb_sev_tbl1$severity_max = ifelse(is.na(dsmb_sev_tbl1$severity_max),0,dsmb_sev_tbl1$severity_max)
write.csv(table(dsmb_sev_tbl1$severity_max,dsmb_sev_tbl1$blind_group),"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/DSMB/ae_severity.csv")
write.table(table(dsmb_ae$severity,dsmb_ae$blind_group),"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/DSMB/ae_severity.csv",sep=",",append=T)
fisher.test(dsmb_sev_tbl1$severity_max,dsmb_sev_tbl1$blind_group) #1st table in the report
fisher.test(dsmb_ae$severity,dsmb_ae$blind_group) #2nd table in the report

#clean up
rm(ae_freq,ae_summary,ae_summary_group,dsmb_sev_tbl1,dsmb_dem_mixed,dsmb_dem_non_mixed,dsmb_dem,dsmb_ae)
