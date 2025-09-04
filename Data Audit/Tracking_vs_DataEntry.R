#write a function that pulls out each measure in each visit and compare each other
#& subj$app_diagnosis=="ASD"
tracking_access_comparison=function(dt,visit_num,tracking_type,measure_name){
  #extracting relevant rows from tracking sheet and db to start comparing
  tracking = assess_survey_primary[which(tolower(assess_survey_primary$Type)==tracking_type),]
  access = dt[which(dt$visit==visit_num),]
  single_entry_flag_table = single_entry_flag_table[which(single_entry_flag_table$visit==visit_num),]
  single_entry_flag_table = single_entry_flag_table[grep(tolower(measure_name),single_entry_flag_table$flag_list),]
  
  #building up the list of rows missing in db
  access_missing = data.frame(tracking[which(tracking[,1] %in% subj[subj$study_cohort != 'STAAR',1] == T & tracking[,1] %in% access[,1] == F),1])
  if(nrow(access_missing)>0){
    access_missing$missing_from = 'ACCESS'
    access_missing$measure = measure_name
    access_missing$id = as.character(access_missing[,1])
    access_missing$visit = visit_num
    access_missing = merge(access_missing,tracking[,c(1:2,4,6)],by.x="id",by.y="STS ID",all.x=T)
    access_missing$suspected_reason = ''
    access_missing$possible_solution = ''
    #putting possible reasons for missing data in access db
    for(i in 1:nrow(access_missing)){
      status = assess_survey_primary[which(assess_survey_primary[,1]==access_missing$id[i]&tolower(assess_survey_primary$Type)==tracking_type),"Status"]
      comment = assess_survey_primary[which(assess_survey_primary[,1]==access_missing$id[i]&tolower(assess_survey_primary$Type)==tracking_type),"Comment"]
      #missing = assess_survey_primary[which(assess_survey_primary[,1]==access_missing$id[i] & tolower(assess_survey_primary$Type)==tracking_type),"Missing Data"]
      visit_status = subj[which(subj[,1]==access_missing$id[i]),paste0('time',visit_num,'_subj_status')]
      #tracked missing data for complete participants
      if((status == "Missing Data") & grepl("Completed",visit_status)){
        access_missing$suspected_reason[i] = "Tracked missing data"
        access_missing$possible_solution[i] = "NA"
      }
      #visit in progress
      else if(!grepl("Completed",visit_status)){
        if(grepl("Ineligible",visit_status)){
          access_missing$suspected_reason[i] = "Missing data: Ineligible Participant"
          access_missing$possible_solution[i] = "NA"
        }
        else{
          access_missing$suspected_reason[i] = "Missing data: Visit Incomplete"
          access_missing$possible_solution[i] = ifelse(visit_status %in% c("Consented-Active","Agreed","","Scheduled") | grepl("rescheduled",tolower(visit_status)),"Wait til the data gets collected/entered","NA")
        }
      }
      #tracking table doesn't say "validated"
      else if(!is.na(status) & grepl("Completed",visit_status)){
        if(status != "Validated"){
          if(status == "Problem with data"){
            access_missing$suspected_reason[i] = "Problem with Data"
            access_missing$possible_solution[i] = "NA"
          }
          else if(grepl("not valid",tolower(comment)) | grepl("invalid",tolower(status))){
            access_missing$suspected_reason[i] = "Invalid data"
            access_missing$possible_solution[i] = "NA"
          }
          else if(grepl("not administered",tolower(status)) | grepl("incomplete",tolower(status)) | grepl("too",tolower(comment))){
            access_missing$suspected_reason[i] = "Didn't/Couldn't collect data"
            access_missing$possible_solution[i] = "NA"
          }
          else{
            access_missing$suspected_reason[i] = "Data not validated"
            access_missing$possible_solution[i] = "Inquire Brianna to check"
          }
        }
      }
      #duplicate data check?
      if(access_missing$id[i] %in% duplicate_data_list$id
         & visit_num %in% duplicate_data_list[which(duplicate_data_list$id == access_missing$id[i]),"visit"]){
        if(tolower(measure_name) %in% duplicate_data_list[which(duplicate_data_list$id == access_missing$id[i]),"measure_list"]){
          access_missing$suspected_reason[i] = "Duplicate data"
          access_missing$possible_solution[i] = "Figure out the correct time point/age for the data"
        }
      }
      #single entry check
      if(access_missing$id[i] %in% single_entry_flag_table$id){
        access_missing$suspected_reason[i] = "Single-entered data"
        access_missing$possible_solution[i] = "Double enter the data"
      }
    }
    rm(status,visit_status)
  }
  #building up the list of rows missing in tracking tables
  tracking_missing = data.frame(access[which(access[,1] %in% tracking[,1] == F),1])
  if(nrow(tracking_missing)>0){
    tracking_missing$missing_from = 'Tracking Table'
    tracking_missing$measure = measure_name
    tracking_missing$id = as.character(tracking_missing[,1])
    tracking_missing$visit = visit_num
    tracking_missing$suspected_reason = ''
    tracking_missing$possible_solution = ''
    for(i in 1:nrow(tracking_missing)){
      if(any(c("Consented-Ineligible","Ineligible") %in% subj[which(subj[,1]==tracking_missing[i,"id"]),"app_diagnosis"])==T){
        tracking_missing$suspected_reason[i] = "Ineligible participant"
        tracking_missing$possible_solution[i] = ""
      }
      else{
        tracking_missing$suspected_reason[i] = "Not entered in the tracking table"
        tracking_missing$possible_solution[i] = "Enter corresponding info to the tracking"
      }
    }
  }
  #combining two tables
  access_columns = c("missing_from","measure","id","visit","Comp Date","Status","Missing Data","suspected_reason","possible_solution")
  if(nrow(access_missing)>0 & nrow(tracking_missing)>0){
    library(plyr)
    access_tracking_missing = rbind.fill(access_missing[,access_columns],tracking_missing[,2:ncol(tracking_missing)])
    names(access_tracking_missing)[5:7] = c("task_date","task_status","missing_data")
    access_tracking_missing[which(access_tracking_missing$missing_from=="Tracking Table"),"task_status"]="Validated"
    access_tracking_missing[which(access_tracking_missing$missing_from=="Tracking Table"),"missing_data"]=0
    return(access_tracking_missing)
  }
  else if(nrow(access_missing)>0 & nrow(tracking_missing) == 0){
    access_missing = access_missing[,access_columns]
    names(access_missing)[5:7] = c("task_date","task_status","missing_data")
    return(access_missing)
  }
  else if(nrow(tracking_missing)>0 & nrow(access_missing) == 0){
    tracking_missing$task_status="Validated"
    tracking_missing$missing_data=0
    return(tracking_missing[,-1])
  }
}
#ex. cbcl_1 = tracking_access_comparison(dt,1,'cbcl','CBCL')
#write.table(cbq_1,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/Data audit/tracking_vs_access.csv",row.names=F,col.names=F,sep=",",append=T)

###TIME5_STATUS VARIABLE NOT AVAILABLE IN SUBJ TABLE YET!###
#"subj$app_diagnosis==ASD" in line 11
adi_1 = tracking_access_comparison(adi_scored,1,'adi (1)','ADI')
adi_3 = tracking_access_comparison(adi_scored,3,'adi (3)','ADI')
#erase subj$app_diagnosis == "ASD"
adis_4 = tracking_access_comparison(adis_processed,4,'adis (4)','ADIS')
ados_1 = tracking_access_comparison(ados_scored,1,'ados (1)','ADOS')
ados_3 = tracking_access_comparison(ados_scored,3,'ados (3)','ADOS')
ados_4 = tracking_access_comparison(ados_scored,4,'ados (4)','ADOS')
# ados_5 = tracking_access_comparison(ados_scored,5,'ados (5)','ADOS')
ados_audit = rbind(ados_1,ados_3,ados_4)
ados_audit$id_num = as.numeric(gsub("-[0-2]{3}$","",ados_audit$id))
ados_audit = merge(ados_audit,subj[,c(1,9)],by.x="id",by.y="subj_id",all.x=T)
ados_audit = subset(ados_audit,app_diagnosis=="ASD" | id_num>110100)
ados_audit = ados_audit[,c(2:3,1,4:9)]
# aqc_5 = tracking_access_comparison(aqc_scored,5,'aqc (5)','AQC')
cbcl_1 = tracking_access_comparison(cbcl_scored[grep("[0-9]{6}-[0-9]{3}",cbcl_scored$id),],1,'cbcl (1)','CBCL')
cbcl_3 = tracking_access_comparison(cbcl_scored[grep("[0-9]{6}-[0-9]{3}",cbcl_scored$id),],3,'cbcl (3)','CBCL')
cbcl_4 = tracking_access_comparison(cbcl_scored[grep("[0-9]{6}-[0-9]{3}",cbcl_scored$id),],4,'cbcl (4)','CBCL')
# cbcl_5 = tracking_access_comparison(cbcl_scored[grep("[0-9]{6}-[0-9]{3}",cbcl_scored$id),],5,'cbcl (5)','CBCL')
cbq_1 = tracking_access_comparison(cbq_scored,1,'cbq (1)','CBQ')
ccc_4 = tracking_access_comparison(ccc_scored,4,'ccc (4)','CCC')
cdi_wg_1 = tracking_access_comparison(cdi_wg_scored,1,'cdi-wg (1)','CDI-WG')
cdi_ws_1 = tracking_access_comparison(cdi_ws_scored,1,'cdi-ws (1)','CDI-WS')
cdi_ws_3 = tracking_access_comparison(cdi_ws_scored,3,'cdi-ws (3)','CDI-WS')
celf_1 = tracking_access_comparison(celf_processed,1,'celf (1)','CELF')
celf_4 = tracking_access_comparison(celf_processed,4,'celf (4)','CELF')
# celf_5 = tracking_access_comparison(celf_processed,5,'celf (5)','CELF')
cshq_1 = tracking_access_comparison(cshq_scored,1,'cshq (1)','CSHQ')
cshq_3 = tracking_access_comparison(cshq_scored,3,'cshq (3)','CSHQ')
cshq_4 = tracking_access_comparison(cshq_scored,4,'cshq (4)','CSHQ')
# cshq_5 = tracking_access_comparison(cshq_scored,5,'cshq (5)','CSHQ')
das_1 = tracking_access_comparison(das_scored,1,'das (1)','DAS')
das_3 = tracking_access_comparison(das_scored,3,'das (3)','DAS')
das_4 = tracking_access_comparison(das_scored,4,'das (4)','DAS')
demograph_1 = tracking_access_comparison(demograph,1,'demographics (1)','Demographic')
demograph_3 = tracking_access_comparison(demograph,3,'demographics (3)','Demographic')
demograph_4 = tracking_access_comparison(demograph,4,'demographics (4)','Demographic')
# demograph_5 = tracking_access_comparison(demograph,5,'demographics (5)','Demographic')
dsm_3 = tracking_access_comparison(dsm_scored,3,'dsm5 (3)','DSM')
dsm_4 = tracking_access_comparison(dsm_scored,4,'dsm5 (4)','DSM')
# dsm_5 = tracking_access_comparison(dsm_scored,5,'dsm5 (5)','DSM')
edin_4 = tracking_access_comparison(edin,4,'edinburgh (4)','Edinburgh')
edq_1 = tracking_access_comparison(edq_scored,1,'edq (1)','EDQ')
edq_3 = tracking_access_comparison(edq_scored,3,'edq (3)','EDQ')
eowpvt_1 = tracking_access_comparison(eowpvt,1,'eowpvt (1)','EOWPVT')
eowpvt_3 = tracking_access_comparison(eowpvt,3,'eowpvt (3)','EOWPVT')
gort_4 = tracking_access_comparison(gort_processed,4,'gort (4)','GORT')
hpt_1 = tracking_access_comparison(handedness_scored,1,'hpt (1)','HPT')
hpt_3 = tracking_access_comparison(handedness_scored,3,'hpt (3)','HPT')
masc_4 = tracking_access_comparison(masc_scored,4,'masc-c (4)','MASC')
# masc_5 = tracking_access_comparison(masc_scored,5,'masc-c (5)','MASC')
masc_p_4 = tracking_access_comparison(masc_p_scored,4,'masc-p (4)','MASC_Parent')
# masc_p_5 = tracking_access_comparison(masc_p_scored,5,'masc-p (5)','MASC_Parent')
mullen_1 = tracking_access_comparison(mullen_scored,1,'msel (1)','MSEL')
mullen_3 = tracking_access_comparison(mullen_scored,3,'msel (3)','MSEL')
nih_4 = tracking_access_comparison(nih_processed,4,'toolbox (4)','NIH-Toolbox')
# nih_5 = tracking_access_comparison(nih_processed,5,'toolbox (5)','NIH-Toolbox')
pal_4 = tracking_access_comparison(pal_processed,4,'pal (4)','PAL')
# pras_asd_5 = tracking_access_comparison(pras_asd_processed,5,'pras-asd (5)','PRAS-ASD')
ppvt_1 = tracking_access_comparison(ppvt_scored,1,'ppvt (1)','PPVT')
ppvt_3 = tracking_access_comparison(ppvt_scored,3,'ppvt (3)','PPVT')
rbs_1 = tracking_access_comparison(rbs_scored,1,'rbs (1)','RBS')
rbs_3 = tracking_access_comparison(rbs_scored,3,'rbs (3)','RBS')
rbs_4 = tracking_access_comparison(rbs_scored,4,'rbs (4)','RBS')
# rbs_5 = tracking_access_comparison(rbs_scored,5,'rbs (5)','RBS')
scared_4 = tracking_access_comparison(scared_scored,1,'scared-c (1)','SCARED')
scared_p_4 = tracking_access_comparison(scared_p_scored,4,'scared-p (4)','SCARED_Parent')
scq_1 = tracking_access_comparison(scq_scored,1,'scq (1)','SCQ')
scq_3 = tracking_access_comparison(scq_scored,3,'scq (3)','SCQ')
scq_4 = tracking_access_comparison(scq_scored,4,'scq (4)','SCQ')
# scq_5 = tracking_access_comparison(scq_scored,5,'scq (5)','SCQ')
seq_1 = tracking_access_comparison(seq_scored,1,'seq (1)','SEQ')
seq_3 = tracking_access_comparison(seq_scored,3,'seq (3)','SEQ')
# seq_5 = tracking_access_comparison(seq_scored,5,'seq (5)','SEQ')
srs_1 = tracking_access_comparison(srs_scored,1,'srs (1)','SRS')
srs_3 = tracking_access_comparison(srs_scored,3,'srs (3)','SRS')
srs_4 = tracking_access_comparison(srs_scored,4,'srs (4)','SRS')
# srs_5 = tracking_access_comparison(srs_scored,5,'srs (5)','SRS')
#srs_fat_1 = tracking_access_comparison(srs_dad_scored,1,'srs-fat','SRS-Fat')
#srs_mth_1 = tracking_access_comparison(srs_mom_scored,1,'srs-mth','SRS-Mth')
ssp_1 = tracking_access_comparison(ssp_scored,1,'ssp','SSP')
ssp_3 = tracking_access_comparison(ssp_scored,3,'ssp1 (3)','SSP')
ssp_4 = tracking_access_comparison(ssp_scored,4,'ssp1 (4)','SSP')
# ssp_5 = tracking_access_comparison(ssp_scored,5,'ssp1 (5)','SSP')
ssp2_1 = tracking_access_comparison(ssp2_scored,1,'ssp2 (1)','SSP2')
ssp2_3 = tracking_access_comparison(ssp2_scored,3,'ssp2 (3)','SSP2')
ssp2_4 = tracking_access_comparison(ssp2_scored,4,'ssp2 (4)','SSP2')
sti_1 = tracking_access_comparison(sti_scored,1,'sti (1)','STI')
sti_3 = tracking_access_comparison(sti_scored,3,'sti (3)','STI')
sti_4 = tracking_access_comparison(sti_scored,4,'sti (4)','STI')
# sti_5 = tracking_access_comparison(sti_scored,5,'sti (5)','STI')
swan_1 = tracking_access_comparison(swan_scored,1,'swan','SWAN')
swan_3 = tracking_access_comparison(swan_scored,3,'swan (3)','SWAN')
swan_4 = tracking_access_comparison(swan_scored,4,'swan (4)','SWAN')
# swan_5 = tracking_access_comparison(swan_scored,5,'swan (5)','SWAN')
tmcq_4 = tracking_access_comparison(tmcq_scored,4,'tmcq (4)','TMCQ')
towre_4 = tracking_access_comparison(towre_processed,4,'towre (4)','TOWRE')
vabs_1 = tracking_access_comparison(vabs_scored,1,'vabs (1)','VABS')
vabs_3 = tracking_access_comparison(vabs_scored,3,'vabs (3)','VABS')
vabs_4 = tracking_access_comparison(vabs_scored,4,'vabs (4)','VABS')
# vabs_5 = tracking_access_comparison(vabs_scored,5,'vabs (5)','VABS')
wraml_4 = tracking_access_comparison(wraml_scored,4,'wraml (4)','WRAML')
# ysr_5 = tracking_access_comparison(ysr_scored,5,'ysr (5)','YSR')

data = rbind.fill(adi_1,adi_3,adis_4,ados_audit,cbcl_1,cbcl_3,cbcl_4,cbq_1,ccc_4,cdi_wg_1,
      cdi_ws_1,cdi_ws_3,celf_1,celf_4,cshq_1,cshq_3,cshq_4,das_1,das_3,das_4,demograph_1,
      demograph_3,demograph_4,dsm_3,dsm_4,edin_4,edq_1,edq_3,eowpvt_1,eowpvt_3,gort_4,hpt_1,
      hpt_3,masc_4,masc_p_4,mullen_1,mullen_3,nih_4,pal_4,ppvt_1,ppvt_3,rbs_1,rbs_3,rbs_4,scared_4,
      scared_p_4,scq_1,scq_3,scq_4,seq_1,seq_3,srs_1,srs_3,srs_4,ssp_1,ssp_3,ssp_4,ssp2_1,ssp2_3,ssp2_4,
      sti_1,sti_3,sti_4,swan_1,swan_3,swan_4,tmcq_4,towre_4,vabs_1,vabs_3,vabs_4,wraml_4)

#getting rid of the discrepancies of data that are less than 6 months old
data$age = elapsed_months(Sys.Date(),as.Date(data$task_date))
data = data[-which(data$age<6 & data$missing_from=="ACCESS"),]

#adding entries to the tracking tables
data_tracking = subset(data,missing_from=="Tracking Table")
# behav_assess = adding_rows_to_tracking(behav_assess,'adi',1,'ADI',adi)
# behav_assess = adding_rows_to_tracking(behav_assess,'adi',3,'ADI (3)',adi)
# behav_assess = adding_rows_to_tracking(behav_assess,'adis',4,'ADIS (4)',adis)
# behav_assess = adding_rows_to_tracking(behav_assess,'ados',1,'ADOS',ados2_1)
# behav_assess = adding_rows_to_tracking(behav_assess,'ados',4,'ADOS (4)',rbind.fill(ados_g3,ados2_1,ados2_3))
# behav_assess = adding_rows_to_tracking(behav_assess,'ados',3,'ADOS (3)',ados_g3)
# behav_surveys = adding_rows_to_tracking(behav_surveys,'cbcl',1,'CBCL',rbind.fill(cbcl_young,cbcl_old))
# behav_surveys = adding_rows_to_tracking(behav_surveys,'cbcl',3,'CBCL (3)',rbind.fill(cbcl_young,cbcl_old))
# behav_surveys = adding_rows_to_tracking(behav_surveys,'cbcl',4,'CBCL (4)',rbind.fill(cbcl_young,cbcl_old))
# behav_surveys = adding_rows_to_tracking(behav_surveys,'cbq',1,'CBQ',cbq)
# behav_surveys = adding_rows_to_tracking(behav_surveys,'ccc',4,'CCC (4)',ccc)
# behav_surveys = adding_rows_to_tracking(behav_surveys,'cdi-wg',1,'CDI-WG',cdi_wg)
# behav_surveys = adding_rows_to_tracking(behav_surveys,'cdi-ws',1,'CDI-WS',cdi_ws)
# behav_surveys = adding_rows_to_tracking(behav_surveys,'cdi-ws',3,'CDI-WS (3)',cdi_ws)
# behav_assess = adding_rows_to_tracking(behav_assess,'celf',4,'CELF (4)',celf)
# behav_surveys = adding_rows_to_tracking(behav_surveys,'cshq',1,'Sleep',cshq)
# behav_surveys = adding_rows_to_tracking(behav_surveys,'cshq',3,'Sleep (3)',cshq)
# behav_surveys = adding_rows_to_tracking(behav_surveys,'cshq',4,'Sleep (4)',cshq)
# behav_assess = adding_rows_to_tracking(behav_assess,'das',1,'DAS',rbind.fill(das_ey,das_sa))
# behav_assess = adding_rows_to_tracking(behav_assess,'das',3,'DAS (3)',rbind.fill(das_ey,das_sa))
# behav_assess = adding_rows_to_tracking(behav_assess,'das',4,'DAS (4)',rbind.fill(das_ey,das_sa))
# behav_surveys = adding_rows_to_tracking(behav_surveys,'demographic',1,'Demographic',demograph)
# behav_surveys = adding_rows_to_tracking(behav_surveys,'demographic',3,'Demographic (3)',demograph)
# behav_surveys = adding_rows_to_tracking(behav_surveys,'demographic',4,'Demographic (4)',demograph)
# behav_assess = adding_rows_to_tracking(behav_assess,'dsm',3,'DSM5 (3)',dsm)
# behav_assess = adding_rows_to_tracking(behav_assess,'dsm',4,'DSM5 (4)',dsm)
# behav_assess = adding_rows_to_tracking(behav_assess,'edinburgh',4,'Edinburgh (4)',edin)
# behav_surveys = adding_rows_to_tracking(behav_surveys,'edq',1,'EDQ',edq)
# behav_assess = adding_rows_to_tracking(behav_assess,'eowpvt',1,'EOWPVT',eowpvt4)
# behav_assess = adding_rows_to_tracking(behav_assess,'eowpvt',3,'EOWPVT (3)',eowpvt3)
# behav_assess = adding_rows_to_tracking(behav_assess,'gort',4,'GORT (4)',gort)
# behav_assess = adding_rows_to_tracking(behav_assess,'hpt',1,'HPT',hpt)
# behav_surveys = adding_rows_to_tracking(behav_surveys,'masc',4,'MASC_C (4)',masc)
# behav_surveys = adding_rows_to_tracking(behav_surveys,'masc_parent',4,'MASC_P (4)',masc_p)
# behav_assess = adding_rows_to_tracking(behav_assess,'msel',1,'MSEL',mullen)
# behav_assess = adding_rows_to_tracking(behav_assess,'msel',3,'MSEL (3)',mullen)
# behav_assess = adding_rows_to_tracking(behav_assess,'nih-toolbox',4,'Toolbox (4)',nih)
# behav_assess = adding_rows_to_tracking(behav_assess,'pal',4,'PAL (4)',pal)
# behav_assess = adding_rows_to_tracking(behav_assess,'ppvt',1,'PPVT',ppvt3)
# behav_assess = adding_rows_to_tracking(behav_assess,'ppvt',1,'PPVT',ppvt4)
# behav_assess = adding_rows_to_tracking(behav_assess,'ppvt',3,'PPVT (3)',ppvt3)
# behav_surveys = adding_rows_to_tracking(behav_surveys,'rbs',1,'RBS',rbs)
# behav_surveys = adding_rows_to_tracking(behav_surveys,'rbs',3,'RBS (3)',rbs)
# behav_surveys = adding_rows_to_tracking(behav_surveys,'rbs',4,'RBS (4)',rbs)
# behav_surveys = adding_rows_to_tracking(behav_surveys,'scared_parent',4,'SCARED_P (4)',scared_p)
# behav_surveys = adding_rows_to_tracking(behav_surveys,'scq',1,'SCQ',scq)
# behav_surveys = adding_rows_to_tracking(behav_surveys,'scq',3,'SCQ (3)',scq)
# behav_surveys = adding_rows_to_tracking(behav_surveys,'scq',4,'SCQ (4)',scq)
# behav_surveys = adding_rows_to_tracking(behav_surveys,'srs',1,'SRS',srs)
# behav_surveys = adding_rows_to_tracking(behav_surveys,'srs',3,'SRS (3)',srs)
# behav_surveys = adding_rows_to_tracking(behav_surveys,'srs',4,'SRS (4)',srs)
# behav_surveys = adding_rows_to_tracking(behav_surveys,'ssp',1,'SSP',ssp)
# behav_surveys = adding_rows_to_tracking(behav_surveys,'ssp',3,'SSP1 (3)',ssp)
# behav_surveys = adding_rows_to_tracking(behav_surveys,'ssp2',1,'SSP2',ssp2)
# behav_surveys = adding_rows_to_tracking(behav_surveys,'ssp2',3,'SSP2 (3)',ssp2)
# behav_surveys = adding_rows_to_tracking(behav_surveys,'ssp2',4,'SSP2 (4)',ssp2)
# behav_surveys = adding_rows_to_tracking(behav_surveys,'sti',1,'Services & Treat',sti)
# behav_surveys = adding_rows_to_tracking(behav_surveys,'sti',3,'Services & Treat (3)',sti)
# behav_surveys = adding_rows_to_tracking(behav_surveys,'sti',4,'Services & Treat (4)',sti)
# behav_surveys = adding_rows_to_tracking(behav_surveys,'tmcq',4,'TMCQ (4)',tmcq)
# behav_assess = adding_rows_to_tracking(behav_assess,'towre',4,'TOWRE (4)',towre)
# behav_surveys = adding_rows_to_tracking(behav_surveys,'vabs',1,'VABS',vabs)
# behav_surveys = adding_rows_to_tracking(behav_surveys,'vabs',3,'VABS (3)',vabs)
# behav_surveys = adding_rows_to_tracking(behav_surveys,'vabs',4,'VABS (4)',vabs)

#updating results to assess_survey
behav_assess = unique(behav_assess)
behav_surveys = unique(behav_surveys)
assess_survey = merge(behav_assess,behav_surveys,by=c("STS ID","Comp Date","Type","Status","Comment","Missing Data"),all=T)
assess_survey_primary = subset(assess_survey,assess_survey$`STS ID` %in% subj$subj_id)

#fixing "N/A" status with missing data -> "N/A" to "Missing Data" & "Missing Data" value to 1
data = subset(data,missing_from == "ACCESS")
data$missing_data = ifelse(data$possible_solution=="NA" & !grepl("Ineligible",data$suspected_reason),TRUE,
                           ifelse(grepl("Ineligible",data$suspected_reason)&data$task_status
                                  %in% c("Missing Data","Incomplete","N/A","Problem with data"),TRUE,FALSE))

rm(data_tracking,adis_4,ados_1,ados_3,ados_4,cbcl_1,cbcl_3,cbcl_4,cbq_1,ccc_4,cdi_wg_1,
   cdi_ws_1,cdi_ws_3,celf_1,celf_4,cshq_1,cshq_3,cshq_4,das_1,das_3,das_4,demograph_1,
   demograph_3,demograph_4,dsm_3,dsm_4,edin_4,edq_1,edq_3,eowpvt_1,eowpvt_3,gort_4,hpt_1,
   hpt_3,masc_4,masc_p_4,mullen_1,mullen_3,pal_4,ppvt_1,ppvt_3,rbs_1,rbs_3,rbs_4,scared_4,
   scared_p_4,scq_1,scq_3,scq_4,seq_1,srs_1,srs_3,srs_4,ssp_1,ssp_3,ssp_4,ssp2_1,ssp2_3,ssp2_4,
   sti_1,sti_3,sti_4,towre_4,vabs_1,vabs_3,vabs_4,wraml_4,ados_audit,tmcq_4,nih_4,swan_1,swan_3,swan_4)
# for(i in 1:nrow(data)){
#   assess_survey[which(assess_survey$`STS ID` == data$id[i]
#                 & assess_survey$Type == ifelse(data$visit[i]==1,data$measure[i],paste0(data$measure[i],' (',data$visit[i],')'))),"Missing Data"]=data$missing_data[i]
# }
# data_na_missing = subset(data,(is.na(task_status)|task_status=="N/A")&missing_data==1)
# data_na_missing[which(data_na_missing$measure=="CSHQ"),"measure"]="Sleep"
# data_na_missing[which(data_na_missing$measure=="DSM"),"measure"]="DSM5"
# data_na_missing[which(data_na_missing$measure=="STI"),"measure"]="Services & Treat"
# for(i in 1:nrow(data_na_missing)){
#   assess_survey[which(assess_survey$`STS ID` == data_na_missing$id[i]
#                       & assess_survey$Type == ifelse(data_na_missing$visit[i]==1,data_na_missing$measure[i],paste0(data_na_missing$measure[i],' (',data_na_missing$visit[i],')'))),"Missing Data"]=data_na_missing$missing_data[i]
#   assess_survey[which(assess_survey$`STS ID` == data_na_missing$id[i]
#                       & assess_survey$Type == ifelse(data_na_missing$visit[i]==1,data_na_missing$measure[i],paste0(data_na_missing$measure[i],' (',data_na_missing$visit[i],')'))),"Status"]="Missing Data"
# }
# data[which((is.na(data$task_status)|data$task_status=="N/A") & data$missing_data==1),"task_status"]="Missing Data"

# if(length(which(rbind(das_1,das_3)$id %in% mullen_scored[which(mullen_scored$visit=="3"),"id"]==T))>0){
#   for(i in 1:length(which(rbind(das_1,das_3)$id %in% mullen_scored[which(mullen_scored$visit=="3"),"id"]==T))){
#     assess_survey = assess_survey[-which(assess_survey$`STS ID` == rbind(das_1,das_3)[which(rbind(das_1,das_3)$id %in% mullen_scored[which(mullen_scored$visit=="3"),"id"]==T),"id"][i]
#                                          & assess_survey$Type %in% c("DAS","DAS (3)")),]
#   }
# }


#write.csv(data,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/Data audit/tracking_vs_access.csv",row.names=F)