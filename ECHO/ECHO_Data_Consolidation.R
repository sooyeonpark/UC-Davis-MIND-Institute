library(readxl)
###Bibiana Dataset Version
#importing tables
advance_pre <- read_excel("ECHO/ECHO Autism Primary Care and Advance Topics_8_2020_through_1_2021.xlsx",
                          col_types = c("date","text","text","text","text","text","text","text","text",
                                       "text","text","text","text","text","text","text","text","text",
                                       "text","text","text","text","text","text","text","text","text",
                                       "text","text","text","text","text","text","text","text","text",
                                       "text","text","text","text","text","text","text","text","text",
                                       "text","text","text","text","text","text","text","text","text",
                                       "text","text","text","text","text","text","text","text","text",
                                       "text","text","text","text","text","text","text","text","text",
                                       "text","text","text","text","text","text","text","text","text",
                                       "text","text","text","text","text","text","text","text","text",
                                       "text","text","text","text"),sheet="ADVANCED TOPICS PRE SURVEY")
advance_detail <- read_excel("ECHO/ECHO Autism Primary Care and Advance Topics_8_2020_through_1_2021.xlsx",
                          col_types = c("date","text","text","text","text","text","text","text","text",
                                        "text","text","text","text","text","text","text"),sheet="ADVANCED TOPICS CLINIC EVAL")
advance_post <- read_excel("ECHO/ECHO Autism Primary Care and Advance Topics_8_2020_through_1_2021.xlsx",
                          col_types = c("date","skip","text","text","text","text","text","text","text","text",
                                        "text","text","text","text","text","text","text","text","text",
                                        "text","text","text","text","text","text","text","text","text",
                                        "text","text","text","text","text","text","text","text","text",
                                        "text","text","text","text","text","text","text","text","text",
                                        "text","text","text","text","text","text","text","text","text",
                                        "text","text","text","text","text","text","text","text","text",
                                        "text","text","text","text","text","text","text","text","text",
                                        "text","text","text","text","text","text","text","text","text",
                                        "text","text","text","text","text","text","text","text","text",
                                        "text","text","text","text"),sheet="ADVANCED TOPICS POST SURVEY")
primary_pre <- read_excel("ECHO/ECHO Autism Primary Care and Advance Topics_8_2020_through_1_2021.xlsx",
                          col_types = c("date","text","text","text","text","text","text","text","text",
                                        "text","text","text","text","text","text","text","text","text",
                                        "text","text","text","text","text","text","text","text","text",
                                        "text","text","text","text","text","text","text","text","text",
                                        "text","text","text","text","text","text","text","text","text",
                                        "text","text","text","text","text","text","text","text","text",
                                        "text","text","text","text","text","text","text","text","text",
                                        "text","text","text","text","text","text","text","text","text",
                                        "text","text","text","text","text","text","text","text","text",
                                        "text","text","text","text","text","text","text","text","text",
                                        "text","text","text","text"),sheet="PRIMARY CARE PRE SURVEY")
primary_detail <- read_excel("ECHO/ECHO Autism Primary Care and Advance Topics_8_2020_through_1_2021.xlsx",
                             col_types = c("date","text","text","text","skip","skip","skip","skip","text",
                                           "text","text","text","text","text","text","text","text","text",
                                           "text","text","skip","skip","skip","skip"),sheet="PRIMARY CARE CLINIC EVALUATIONS")
primary_post <- read_excel("ECHO/ECHO Autism Primary Care and Advance Topics_8_2020_through_1_2021.xlsx",
                          col_types = c("date","text","text","text","text","text","text","text","text",
                                        "text","text","text","text","text","text","text","text","text",
                                        "text","text","text","text","text","text","text","text","text",
                                        "text","text","text","text","text","text","text","text","text",
                                        "text","text","text","text","text","text","text","text","text",
                                        "text","text","text","text","text","text","text","text","text",
                                        "text","text","text","text","text","text","text","text","text",
                                        "text","text","text","text","text","text","text","text","text",
                                        "text","text","text","text","text","text","text","text","text",
                                        "text","text","text","text","text","text","text","text","text",
                                        "text","text","text","text"),sheet="PRIMARY CARE POST SURVEY")

#assigning variables before stacking
advance_pre$curriculum = "Advanced Topics"
advance_post$curriculum = "Advanced Topics"
primary_pre$curriculum = "Primary Care"
primary_post$curriculum = "Primary Care"
advance_pre$survey_type = "Pre"
advance_post$survey_type = "Post"
primary_pre$survey_type = "Pre"
primary_post$survey_type = "Post"
primary_detail$curriculum = "Primary Care"
advance_detail$curriculum = "Advanced Topics"

#stacking the tables
echo_pre_post = rbind(advance_pre[-1,],primary_pre[-1,],advance_post[-1,],primary_post[-1,])
echo_clin_eval = rbind(advance_detail[-1,],primary_detail[-1,])
rm(advance_pre,advance_detail,advance_post,primary_detail,primary_post,primary_pre)

#renaming the variables
names(echo_pre_post)[1:94] = c("start_date","last_name","first_name","email","echo_participation",
                         "age","gender","gender_describe","ethnicity","race","practice_in_us",
                         "practice_zipcode","practice_location","years_experience","practice_setting",
                         "practice_setting_describe","percentage_served_medicaid","percentage_served_medicare",
                         "percentage_served_uninsured","percentage_served_private_insurance",
                         "percentage_served_unknown","percentage_served_abroad_na","average_wait_time",
                         "primary_discipline","primary_discipline_physician","primary_discipline_other",
                         "practice_length","children_number_yearly","asd_children_number_yearly",
                         "prior_asd_training","training_received","training_received_other",
                         "reason_echo_participation","reason_echo_participation_other","seen_children_last_6months",
                         "screening_tools_access","administer_screening_tools","screening_tools_use",
                         "screening_tools_other","pfv_dst","pfv_dst_9months","pfv_dst_18months","pfv_dst_24months",
                         "pfv_dst_30months","mchat_usage","pfv_ast","pfv_ast_18months","pfv_ast_24months",
                         "mchat_ast_positive","referral_number","referral_number_dev_eval","referral_number_asd",
                         "referral_number_psychiatric","referral_number_other","asd_patient_number_referral",
                         "asd_resource_access","asd_resouce_access_describe","resource_sharing_frequent",
                         "barrier_treating_asd","barrier_treating_asd_behavior","barrier_treating_asd_medical",
                         "barrier_treating_asd_other","access_mhst","administer_mhst","typical_use_mhst",
                         "typical_use_mhst_other","asd_comorbid_patient","barrier_treating_asd_mh",
                         "barrier_treating_asd_mh_other","barrier_treating_asd_adhd","barrier_treating_asd_adhd_other",
                         "barrier_treating_asd_anx","barrier_treating_asd_anx_other","barrier_treating_asd_medical",
                         "barrier_treating_asd_medical_other","confidence_asd_screening","confidence_asd_mh_assessment",
                         "confidence_prescribe_meds","confidence_educate_asd","confidence_identify_medical_comorbs",
                         "confidence_identify_community_resources","confidence_recognize_ped_mh","confidence_discuss_mh",
                         "confidence_recommend_treatment_modals","dsa_knowledge","asd_screening_tool_knowledge",
                         "knowledge_mh_assessment","knowlege_prescribe_meds_bx","knowledge_evidence_based_intervention",
                         "knowledge_identify_med_comorbs","knowledge_access_community_resources",
                         "knowledge_support_asd_families","interdisciplinary_discussion_helpful","interdisciplinary_discussion_helpful_text")
names(echo_clin_eval) = c("start_date","last_name","first_name","email","profession",
                          "clinic_date_topic","topics_satisfaction","echo_learning_community_satisfaction",
                          "asd_screening_satisfaction","community_resources_satisfaction","echo_autism_community_satisfaction",
                          "asd_comorbidities_satisfaction","useful_info_changing_practice","future_topics_request",
                          "suggestions_echo_clinic","future_attendance","curriculum")

write.csv(echo_pre_post,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/ECHO/pre_post_surveys.csv",row.names=F)
write.csv(echo_clin_eval,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/ECHO/clinical_eval_surveys.csv",row.names=F)

###Qualtrics Version (download all the data first)
#importing data
echo_mh_curr_pre = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/ECHO/Mental Health Curr Pre-survey.csv",stringsAsFactors = F)[-(1:2),-(1:7)]
echo_mh_curr_post = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/ECHO/Mental Health Curr Post-survey.csv",stringsAsFactors = F)[-(1:2),-(1:7)]
echo_pc_pre = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/ECHO/Primary Care Pre-survey_1st.csv",stringsAsFactors=F)[-(1:2),-(1:7)]
echo_pc_post = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/ECHO/Primary Care Post-survey.csv",stringsAsFactors=F)[-(1:2),-(1:7)]
echo_at_pre = subset(echo_mh_curr_pre,RecordedDate>"2020-08-01")
echo_mh_curr_pre = subset(echo_mh_curr_pre,RecordedDate<"2020-08-01")
echo_at_post = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/ECHO/Advanced Topics Post-survey.csv",stringsAsFactors=F)[-(1:2),-(1:7)]
echo_clin_eval = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/ECHO/Clinic Evaluation Combined.csv",stringsAsFactors=F)[-1,]

#combining data
col = c("RecipientLastName","RecipientFirstName")
echo_data_summary = unique(rbind(echo_mh_curr_pre[,col],echo_mh_curr_post[,col],echo_at_pre[,col],
                                 echo_at_post[,col],echo_pc_pre[,col],echo_pc_post[,col],echo_clin_eval[,col]))
rm(col)
echo_data_summary = subset(echo_data_summary,RecipientLastName != '')
echo_data_summary$mh_pre = ifelse(paste0(echo_data_summary[,1],echo_data_summary[,2]) %in% paste0(echo_mh_curr_pre[,3],echo_mh_curr_pre[,4]),1,0)
echo_data_summary$mh_post = ifelse(paste0(echo_data_summary[,1],echo_data_summary[,2]) %in% paste0(echo_mh_curr_post[,3],echo_mh_curr_post[,4]),1,0)
echo_data_summary$pc_pre = ifelse(paste0(echo_data_summary[,1],echo_data_summary[,2]) %in% paste0(echo_pc_pre[,3],echo_pc_pre[,4]),1,0)
echo_data_summary$pc_post = ifelse(paste0(echo_data_summary[,1],echo_data_summary[,2]) %in% paste0(echo_pc_post[,3],echo_pc_post[,4]),1,0)
echo_data_summary$at_pre = ifelse(paste0(echo_data_summary[,1],echo_data_summary[,2]) %in% paste0(echo_at_pre[,3],echo_at_pre[,4]),1,0)
echo_data_summary$at_post = ifelse(paste0(echo_data_summary[,1],echo_data_summary[,2]) %in% paste0(echo_at_post[,3],echo_at_post[,4]),1,0)
echo_data_summary$clin_eval = ifelse(paste0(echo_data_summary[,1],echo_data_summary[,2]) %in% paste0(echo_clin_eval[,3],echo_clin_eval[,4]),1,0)

#_______________________________________________________
echo_data_summary = unique(echo_clin_eval_q[echo_clin_eval_q$RecipientLastName != '',3:5])
names(echo_data_summary) = c("last_name","first_name","email")
echo_data_summary = echo_data_summary[-1,]
echo_data_summary = echo_data_summary[-66,] #Conglio vs Coniglio for Desiree
echo_data_summary = echo_data_summary[-17,] #aubyn's email duplicate
echo_data_summary$clin_eval = "Yes"
library(plyr)
echo_data_summary = rbind.fill(echo_data_summary,echo_pre_post[which(!(echo_pre_post$last_name %in% echo_data_summary$last_name)),2:4])
echo_data_summary[is.na(echo_data_summary$clin_eval),"clin_eval"]="No"
echo_data_summary$post_primary = ifelse(echo_data_summary$email %in%
                                        echo_pre_post[which(echo_pre_post$survey_type=="Post"&echo_pre_post$curriculum=="Primary Care"),"email"],"Yes","No")
