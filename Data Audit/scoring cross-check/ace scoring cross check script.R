#### library and options ####
options(stringsAsFactors = FALSE)

#### get data ####
#dp <- read.csv('S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/Data Audit/Scoring cross-check/dataset_2020-08-06.csv',stringsAsFactors = F) # <-- dp = "data portal" ... current scoring algorithms
#msel_2016 <- read.csv('S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/Data Audit/Scoring cross-check/old_data_files/OldSQLScoring/MULLEN_SCORE_20160810.csv',stringsAsFactors = F)  # <-- scoring done in SQL Server using T-SQL
msel_scores_old = read.csv('S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/Data Audit/Scoring cross-check/old_data_files/OldSQLScoring/MULLEN_SCORE.csv',stringsAsFactors = F)  #  <-- scoring done in SQL Server using T-SQL
names(msel_scores_old)[1]="id"
names(msel_scores_old) = tolower(names(msel_scores_old))
#sqladi <- read.csv('old_data_files/OldSQLScoring/ADI_SCORE.csv')  #  <-- scoring done in SQL Server using T-SQL
adi_scores_old <- read_excel("Data Audit/scoring cross-check/old_data_files/OldSQLScoring/ADI_SCORE.xlsx")
names(adi_scores_old) = tolower(names(adi_scores_old))
#sqlcbcl <- read.csv('old_data_files/OldSQLScoring/CBCL_1_5_SCORE.csv')  #  <-- scoring done in SQL Server using T-SQL
cbcl_scores_old <- read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/Data Audit/scoring cross-check/old_data_files/OldSQLScoring/CBCL_1_5_SCORE.csv", stringsAsFactors=FALSE)
names(cbcl_scores_old)[1]="id"
names(cbcl_scores_old) = tolower(names(cbcl_scores_old))
#sqldas <- read.csv('old_data_files/OldSQLScoring/DAS_SA_SCORE_20160810.csv')  #  <-- scoring done in SQL Server using T-SQL
das_sa_scores_old <- read_excel("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/Data Audit/scoring cross-check/old_data_files/OldSQLScoring/DAS_SA_SCORE_20160810.xlsx")
names(das_sa_scores_old)[1]="id"
names(das_sa_scores_old) = tolower(names(das_sa_scores_old))
dqt <- read.csv('old_data_files/OriginalDQTScoring/dqt data 01172017.csv',stringsAsFactors = F) #  <-- scoring done in R using the original "data query tool"

# names(sqlmsel2016)[1:2] <- c('subj_id','visit')
# names(dp)

#### Check MSEL scoring, cross referencing current (dp) with earlier versions ####
mselcheck = merge(msel_scores_old,mullen_scored[,c(1:2,11:21)],all.x=T)
mselcheck = mselcheck[,c(1:2,grep("age_equiv",names(mselcheck)),grep("ae",names(mselcheck)),grep("t_score",names(mselcheck)),grep("_t$",names(mselcheck)))]
msel_discrepant = mselcheck[mselcheck$scoresumm_rl_age_equiv != mselcheck$msel_rl_ae | mselcheck$scoresumm_el_age_equiv != mselcheck$msel_el_ae
                            | mselcheck$scoresumm_vr_age_equiv != mselcheck$msel_vr_ae | mselcheck$msel_fm_ae != mselcheck$scoresumm_fm_age_equiv,]
msel_discrepant = msel_discrepant[!is.na(msel_discrepant$id),c(1:2,grep("vr",names(msel_discrepant)),grep("fm",names(msel_discrepant)),
                                                               grep("rl",names(msel_discrepant)),grep("_el_",names(msel_discrepant)))]
plot(jitter(mselcheck$msel_el_ae), jitter(mselcheck$scoresumm_el_age_equiv))
plot(jitter(mselcheck$msel_vr_ae), jitter(mselcheck$scoresumm_vr_age_equiv))
plot(jitter(mselcheck$msel_fm_ae), jitter(mselcheck$scoresumm_fm_age_equiv))
plot(jitter(mselcheck$msel_rl_ae), jitter(mselcheck$scoresumm_rl_age_equiv))

#### need to do:  DAS, Vineland, ADI, ADOS, CBCL, SCQ, SRS ####
cbclcheck = merge(cbcl_scores_old,cbcl_scored[,c(1:2,6:45)],all.x=T)
cbclcheck = cbclcheck[,c(1:5,grep("emotionally_reactive",names(cbclcheck)),grep("anxious_depressed",names(cbclcheck)),grep("somatic_complaints",names(cbclcheck)),grep("withdrawn",names(cbclcheck)),grep("sleep_problem",names(cbclcheck)),grep("attention_problem",names(cbclcheck)),grep("aggressive",names(cbclcheck)),grep("dsm",names(cbclcheck)),grep("internalizing",names(cbclcheck)),grep("externalizing",names(cbclcheck)),grep("total",names(cbclcheck)))]
cbcl_discrepant = cbclcheck[cbclcheck$internalizing_sum != cbclcheck$cbcl_internalizing_raw | cbclcheck$externalizing_sum != cbclcheck$cbcl_externalizing_raw | cbclcheck$total_sum != cbclcheck$cbcl_total_raw,c(1:2,grep("total",names(cbclcheck)))]
cbcl_discrepant = cbcl_discrepant[!is.na(cbcl_discrepant$id),]
plot(cbclcheck$total_sum,cbclcheck$cbcl_total_raw)

dassacheck = merge(das_sa_scores_old,das_sa_scored[,c(1:2,14:44)],all.x=T)
dassacheck = dassacheck[,c(1:4,grep("_as$",names(dassacheck)),grep("_ae$",names(dassacheck)),grep("_tscr$",names(dassacheck)),
                           grep("_t$",names(dassacheck)),grep("_ss$",names(dassacheck)))]
dassa_discrepant = dassacheck[dassacheck$das_vb_ss != dassacheck$dasii_sar_verbal_ss | dassacheck$das_nvr_ss != dassacheck$dasii_sar_nvr_ss
                              | dassacheck$das_gca_ss != dassacheck$dasii_sar_gca_ss | dassacheck$das_snc_ss != dassacheck$dasii_sar_snc_ss,
                              c(1:2,grep("gca",names(dassacheck)),grep("snc",names(dassacheck)))]
dassa_discrepant = dassa_discrepant[!is.na(dassa_discrepant$id),]
plot(dassacheck$dasii_sar_gca_ss,dassacheck$das_gca_ss)

adicheck = merge(adi_scores_old,adi_scored[,c(1:2,8:11)],all.x=T)
adi_discrepant = adicheck[adicheck$adi_totalA != adicheck$soct_cs | adicheck$beht_cs != adicheck$adi_totalC
                          | adicheck$adi_totalNVB != adicheck$comnvtcs | adicheck$adi_totalVB != adicheck$comvt_cs,c(1:2,26:27,29:30,35:39)]
adi_discrepant = adi_discrepant[!is.na(adi_discrepant$id),]
plot(jitter(adicheck$soct_cs),jitter(adicheck$adi_totalA))
plot(jitter(adicheck$comvt_cs),jitter(adicheck$adi_totalVB))
lines(c(2,8,10,15,30),c(2,8,10,15,30))
