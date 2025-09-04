vabs_t1 = exportRecords(ace_con,forms=c("participant_information","vineland"),events="t1_arm_1",labels=F,stringsAsFactors=F)
vabs_t1 = vabs_t1[grepl("Co",vabs_t1$vineland_complete)&!is.na(vabs_t1$vineland_timestamp),-(2:grep("participant_information_complete",names(vabs_t1)))]
vabs_t1$visit = 1
vabs_t3 = exportRecords(ace_con,forms=c("participant_information","vineland"),events="t3_arm_1",labels=F,stringsAsFactors=F)
vabs_t3 = vabs_t3[grepl("Co",vabs_t3$vineland_complete)&!is.na(vabs_t3$vineland_timestamp),-(2:grep("participant_information_complete",names(vabs_t3)))]
vabs_t3$visit = 3
vabs_t5 = exportRecords(ace_con,forms=c("participant_information","vineland_3"),events="t5_arm_1",labels=F,stringsAsFactors=F)
vabs_t5 = vabs_t5[grepl("Co",vabs_t5$vineland_3_complete)&!is.na(vabs_t5$vineland_3_timestamp),-(2:grep("participant_information_complete",names(vabs_t5)))]
vabs_t5$visit = 5
names(vabs_t5) = gsub("vabs3","vabs",names(vabs_t5))
vabs2 = rbind(vabs_t1,vabs_t3)
vabs = sqlQuery(new_con,"select * from Vineland;",stringsAsFactors=F)
names(vabs)[which(names(vabs)=="vabs_writen_raw")] = "written_raw"

#item lists
rl = grep("_a[0-9]+$",names(vabs2))
el = grep("_b[0-9]+$",names(vabs2))
wl = grep("_c[0-9]+$",names(vabs2))
personal = grep("_d[0-9]+$",names(vabs2))
domestic = grep("_e[0-9]+$",names(vabs2))
community = grep("_f[0-9]+$",names(vabs2))
interpersonal = grep("_g[0-9]+$",names(vabs2))
play = grep("_h[0-9]+$",names(vabs2))
coping = grep("_i[0-9]+$",names(vabs2))
gross = grep("_j[0-9]+$",names(vabs2))
fine = grep("_k[0-9]+$",names(vabs2))

#establishing basals and ceilings & assigning categorical values
vabs2 = vabs_basal_ceiling(vabs2,rl)
vabs2 = vabs_basal_ceiling(vabs2,el)
vabs2 = vabs_basal_ceiling(vabs2,wl)
vabs2 = vabs_basal_ceiling(vabs2,personal)
vabs2 = vabs_basal_ceiling(vabs2,domestic)
vabs2 = vabs_basal_ceiling(vabs2,community)
vabs2 = vabs_basal_ceiling(vabs2,interpersonal)
vabs2 = vabs_basal_ceiling(vabs2,play)
vabs2 = vabs_basal_ceiling(vabs2,coping)
vabs2 = vabs_basal_ceiling(vabs2,gross)
vabs2 = vabs_basal_ceiling(vabs2,fine)

#scoring vabs_rc raw scores before combining data
for(j in grep("_[a-k]{1}[0-9]+$",names(vabs2))){
  vabs2[,j] = ifelse(tolower(vabs2[,j])=="never"|tolower(vabs2[,j])=="no",0,
                     ifelse(grepl("sometimes",tolower(vabs2[,j]))|grepl("no oppor",tolower(vabs2[,j])),1,
                            ifelse(grepl("usually",tolower(vabs2[,j]))|tolower(vabs2[,j])=="yes",2,NA)))
}

vabs2 = summing_items_per_row(vabs2,list(rl,el,wl,personal,domestic,community,
                                         interpersonal,play,coping,gross,fine),
                              list('receptive_raw','expressive_raw','written_raw','personal_raw','domestic_raw',
                                   'community_raw','interpersonal_raw','playleisure_raw','coping_raw','gross_raw','fine_raw'),T)

#redefining items for vineland 3 (t5) and doing the same process
rl = grep("_a[0-9]+$",names(vabs_t5))
el = grep("_b[0-9]+$",names(vabs_t5))
wl = grep("_c[0-9]+$",names(vabs_t5))
personal = grep("_d[0-9]+$",names(vabs_t5))
domestic = grep("_e[0-9]+$",names(vabs_t5))
community = grep("_f[0-9]+$",names(vabs_t5))
interpersonal = grep("_g[0-9]+$",names(vabs_t5))
play = grep("_h[0-9]+$",names(vabs_t5))
coping = grep("_i[0-9]+$",names(vabs_t5))
gross = grep("_j[0-9]+$",names(vabs_t5))
fine = grep("_k[0-9]+$",names(vabs_t5))

#establishing basals and ceilings & assigning categorical values
for(j in 3:ncol(vabs_t5)){
  vabs_t5[,j] = as.character(vabs_t5[,j])
}
vabs_t5 = vabs_basal_ceiling(vabs_t5,rl)
vabs_t5 = vabs_basal_ceiling(vabs_t5,el)
vabs_t5 = vabs_basal_ceiling(vabs_t5,wl)
vabs_t5 = vabs_basal_ceiling(vabs_t5,personal)
vabs_t5 = vabs_basal_ceiling(vabs_t5,domestic)
vabs_t5 = vabs_basal_ceiling(vabs_t5,community)
vabs_t5 = vabs_basal_ceiling(vabs_t5,interpersonal)
vabs_t5 = vabs_basal_ceiling(vabs_t5,play)
vabs_t5 = vabs_basal_ceiling(vabs_t5,coping)
vabs_t5 = vabs_basal_ceiling(vabs_t5,gross)
vabs_t5 = vabs_basal_ceiling(vabs_t5,fine)

for(j in grep("_[a-k]{1}[0-9]+$",names(vabs_t5))){
  vabs_t5[,j] = ifelse(tolower(vabs_t5[,j])=="never"|tolower(vabs_t5[,j])=="no",0,
                       ifelse(grepl("sometimes",tolower(vabs_t5[,j]))|grepl("no oppor",tolower(vabs_t5[,j])),1,
                              ifelse(grepl("usually",tolower(vabs_t5[,j]))|tolower(vabs_t5[,j])=="yes",2,NA)))
}

vabs_t5 = summing_items_per_row(vabs_t5,list(rl,el,wl,personal,domestic,community,
                                         interpersonal,play,coping,gross,fine),
                              list('receptive_raw','expressive_raw','written_raw','personal_raw','domestic_raw',
                                   'community_raw','interpersonal_raw','playleisure_raw','coping_raw','gross_raw','fine_raw'),T)

# Load norm tables from file
vabs_v_scale <- read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/scr_Vineland V-Scale Scores_full.csv", stringsAsFactors = FALSE)
vabs_standard <- read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/scr_Vineland Standard Scores.csv", stringsAsFactors = FALSE)
vabs_ae <- read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/scr_Vineland Age Equivalents.csv", stringsAsFactors = FALSE)

#id and visit into characters
vabs = id_visit_changing_into_char(vabs)

#removing single-entered rows and prefix
vabs_entry_flag = entry_flag(vabs,'vabs')
vabs = subset(vabs,entry_status==2)
if(!is.null(vabs_entry_flag)){
  vabs = rbind(vabs,vabs_entry_flag[,-ncol(vabs_entry_flag)])
}
vabs = removing_prefix(vabs,"vabs_")
vabs_t5 = study_id_to_id(vabs_t5,"vabs_")
vabs = identify_same_data(vabs_t5,vabs)
vabs2 = study_id_to_id(vabs2,"vabs_")
vabs = identify_same_data(vabs2,vabs)
vabs = rbind.fill(vabs,vabs2[,-grep("^[a-k]{1}[0-9]+$",names(vabs2))],vabs_t5[,-grep("^[a-k]{1}[0-9]+$",names(vabs_t5))])

#calculating age
vabs = fxage(vabs,'id','date')

#modifying norm tables to obtain scores
vabs_v_scale_revised = vabs_v_scale[,1:2]
vabs_v_scale_revised = modifying_vabs_norm_tables(vabs_v_scale,vabs_v_scale_revised)
vabs_standard_revised = vabs_standard[,c(1:2,8)]
vabs_standard = vabs_standard[,c(1:2,8,3:7)]
vabs_standard_revised = modifying_vabs_norm_tables(vabs_standard,vabs_standard_revised)
vabs_ae_revised = data.frame(vabs_ae[,1])
names(vabs_ae_revised) = "vabs_ae"
vabs_ae_revised = modifying_vabs_norm_tables(vabs_ae,vabs_ae_revised)
rm(vabs_v_scale,vabs_standard,vabs_ae)

#dealing with negative raw scores
for(j in which(names(vabs)=="receptive_raw"):which(names(vabs)=="fine_raw")){
  vabs[,j] = cbraw(vabs[,j])
}

#changing written_raw scores to NA for participants younger than 3
vabs[which(vabs$age<36),"written_raw"] = 0

#missing data analysis
vabs = comment_missing_data(vabs,list(grep("_raw$",names(vabs),value = T)),list('','','','','','','','','','',''))
vabs[which(vabs$missing_items_comment != ''),"missing_items_comment"] = ifelse(vabs[which(vabs$missing_items_comment != ''),"age"]<84,"Raw score(s) missing in the data entry",
                                                                             ifelse(is.na(vabs[which(vabs$missing_items_comment != ''),"gross_raw"])|is.na(vabs[which(vabs$missing_items_comment != ''),"fine_raw"]),"","Raw score(s) missing in the data entry"))

##obtaining norm scores
#V scale scores
for(i in 1:nrow(vabs)){
  vabs$receptive_v[i] = ifelse(!is.na(vabs$receptive_raw[i]),
                               vabs_v_scale_revised[which(floor(vabs$age[i])==vabs_v_scale_revised$age
                                                          & vabs$receptive_raw[i]>=vabs_v_scale_revised$receptive_bottom
                                                          & vabs$receptive_raw[i]<=vabs_v_scale_revised$receptive_top),2],NA)
  vabs$expressive_v[i] = ifelse(!is.na(vabs$expressive_raw[i]),
                                vabs_v_scale_revised[which(floor(vabs$age[i])==vabs_v_scale_revised$age
                                                          & vabs$expressive_raw[i]>=vabs_v_scale_revised$expressive_bottom
                                                          & vabs$expressive_raw[i]<=vabs_v_scale_revised$expressive_top),2],NA)
  vabs$written_v[i] = ifelse(!is.na(vabs$written_raw[i]),
                                vabs_v_scale_revised[which(floor(vabs$age[i])==vabs_v_scale_revised$age
                                                           & vabs$written_raw[i]>=vabs_v_scale_revised$written_bottom
                                                           & vabs$written_raw[i]<=vabs_v_scale_revised$written_top),2],NA)
  vabs$personal_v[i] = ifelse(!is.na(vabs$personal_raw[i]),
                                vabs_v_scale_revised[which(floor(vabs$age[i])==vabs_v_scale_revised$age
                                                           & vabs$personal_raw[i]>=vabs_v_scale_revised$personal_bottom
                                                           & vabs$personal_raw[i]<=vabs_v_scale_revised$personal_top),2],NA)
  vabs$domestic_v[i] = ifelse(!is.na(vabs$domestic_raw[i]),
                                vabs_v_scale_revised[which(floor(vabs$age[i])==vabs_v_scale_revised$age
                                                           & vabs$domestic_raw[i]>=vabs_v_scale_revised$domestic_bottom
                                                           & vabs$domestic_raw[i]<=vabs_v_scale_revised$domestic_top),2],NA)
  vabs$community_v[i] = ifelse(!is.na(vabs$community_raw[i]),
                                vabs_v_scale_revised[which(floor(vabs$age[i])==vabs_v_scale_revised$age
                                                           & vabs$community_raw[i]>=vabs_v_scale_revised$community_bottom
                                                           & vabs$community_raw[i]<=vabs_v_scale_revised$community_top),2],NA)
  vabs$interpersonal_v[i] = ifelse(!is.na(vabs$interpersonal_raw[i]),
                                vabs_v_scale_revised[which(floor(vabs$age[i])==vabs_v_scale_revised$age
                                                           & vabs$interpersonal_raw[i]>=vabs_v_scale_revised$interpersonal_bottom
                                                           & vabs$interpersonal_raw[i]<=vabs_v_scale_revised$interpersonal_top),2],NA)
  vabs$play_v[i] = ifelse(!is.na(vabs$playleisure_raw[i]),
                                vabs_v_scale_revised[which(floor(vabs$age[i])==vabs_v_scale_revised$age
                                                           & vabs$playleisure_raw[i]>=vabs_v_scale_revised$play_bottom
                                                           & vabs$playleisure_raw[i]<=vabs_v_scale_revised$play_top),2],NA)
  vabs$coping_v[i] = ifelse(!is.na(vabs$coping_raw[i]),
                                vabs_v_scale_revised[which(floor(vabs$age[i])==vabs_v_scale_revised$age
                                                           & vabs$coping_raw[i]>=vabs_v_scale_revised$coping_bottom
                                                           & vabs$coping_raw[i]<=vabs_v_scale_revised$coping_top),2],NA)
  vabs$gross_v[i] = ifelse(!is.na(vabs$gross_raw[i]),
                                vabs_v_scale_revised[which(floor(vabs$age[i])==vabs_v_scale_revised$age
                                                           & vabs$gross_raw[i]>=vabs_v_scale_revised$gross_bottom
                                                           & vabs$gross_raw[i]<=vabs_v_scale_revised$gross_top),2],NA)
  vabs$fine_v[i] = ifelse(!is.na(vabs$fine_raw[i]),
                                vabs_v_scale_revised[which(floor(vabs$age[i])==vabs_v_scale_revised$age
                                                           & vabs$fine_raw[i]>=vabs_v_scale_revised$fine_bottom
                                                           & vabs$fine_raw[i]<=vabs_v_scale_revised$fine_top),2],NA)
}

##Standard Scores
#obtaining summation of subdomains
for(i in 1:nrow(vabs)){
  vabs$communication_raw[i] = ifelse(vabs$age[i]>=36,sum(vabs$receptive_v[i],vabs$expressive_v[i],vabs$written_v[i],na.rm=F),vabs$receptive_v[i]+vabs$expressive_v[i])
  vabs$communication_ss[i] = ifelse(!is.na(vabs$communication_raw[i]),vabs_standard_revised[which(floor(vabs$age[i])==vabs_standard_revised$age
                                                          & vabs$communication_raw[i]>=vabs_standard_revised$communication_bottom
                                                          & vabs$communication_raw[i]<=vabs_standard_revised$communication_top),2],NA)
  vabs$living_raw[i] = sum(vabs$personal_v[i],vabs$domestic_v[i],vabs$community_v[i],na.rm=F)
  vabs$living_ss[i] = ifelse(!is.na(vabs$living_raw[i]),vabs_standard_revised[which(floor(vabs$age[i])==vabs_standard_revised$age
                                                          & vabs$living_raw[i]>=vabs_standard_revised$daily_living_skills_bottom
                                                          & vabs$living_raw[i]<=vabs_standard_revised$daily_living_skills_top),2],NA)
  vabs$social_raw[i] = sum(vabs$interpersonal_v[i],vabs$play_v[i],vabs$coping_v[i],na.rm=F)
  vabs$social_ss[i] = ifelse(!is.na(vabs$social_raw[i]),vabs_standard_revised[which(floor(vabs$age[i])==vabs_standard_revised$age
                                                          & vabs$social_raw[i]>=vabs_standard_revised$socialization_bottom
                                                          & vabs$social_raw[i]<=vabs_standard_revised$socialization_top),2],NA)
  vabs$motor_raw[i] = sum(vabs$gross_v[i],vabs$fine_v[i],na.rm=F)
  vabs$motor_ss[i] = ifelse(!is.na(vabs$motor_raw[i]),vabs_standard_revised[which(floor(vabs$age[i])==vabs_standard_revised$age
                                                          & vabs$motor_raw[i]>=vabs_standard_revised$motor_skills_bottom
                                                          & vabs$motor_raw[i]<=vabs_standard_revised$motor_skills_top),2],NA)
  vabs$abc_raw[i] = ifelse(vabs$age[i]>=84,sum(vabs$communication_ss[i],vabs$living_ss[i],vabs$social_ss[i],na.rm=F),
                                vabs$communication_ss[i]+vabs$living_ss[i]+vabs$social_ss[i]+vabs$motor_ss[i])
  vabs$abc_ss[i] = ifelse(!is.na(vabs$abc_raw[i]), vabs_standard_revised[which(floor(vabs$age[i])==vabs_standard_revised$age
                                                          & vabs$abc_raw[i]>=vabs_standard_revised$adaptive_composite_bottom
                                                          & vabs$abc_raw[i]<=vabs_standard_revised$adaptive_composite_top),2],NA)
}

#obtaining age equivalents
for(i in 1:nrow(vabs)){
  vabs$receptive_ae[i] = ifelse(!is.na(vabs$receptive_raw[i]),
                               vabs_ae_revised[which(vabs$receptive_raw[i]>=vabs_ae_revised$receptive_bottom
                                                    & vabs$receptive_raw[i]<=vabs_ae_revised$receptive_top),1],NA)
  vabs$expressive_ae[i] = ifelse(!is.na(vabs$expressive_raw[i]),
                                vabs_ae_revised[which(vabs$expressive_raw[i]>=vabs_ae_revised$expressive_bottom
                                                      & vabs$expressive_raw[i]<=vabs_ae_revised$expressive_top),1],NA)
  vabs$written_ae[i] = ifelse(!is.na(vabs$written_raw[i]),
                             vabs_ae_revised[which(vabs$written_raw[i]>=vabs_ae_revised$written_bottom
                                                  & vabs$written_raw[i]<=vabs_ae_revised$written_top),1],NA)
  vabs$personal_ae[i] = ifelse(!is.na(vabs$personal_raw[i]),
                              vabs_ae_revised[which(vabs$personal_raw[i]>=vabs_ae_revised$personal_bottom
                                                    & vabs$personal_raw[i]<=vabs_ae_revised$personal_top),1],NA)
  vabs$domestic_ae[i] = ifelse(!is.na(vabs$domestic_raw[i]),
                              vabs_ae_revised[which(vabs$domestic_raw[i]>=vabs_ae_revised$domestic_bottom
                                                    & vabs$domestic_raw[i]<=vabs_ae_revised$domestic_top),1],NA)
  vabs$community_ae[i] = ifelse(!is.na(vabs$community_raw[i]),
                               vabs_ae_revised[which(vabs$community_raw[i]>=vabs_ae_revised$community_bottom
                                                    & vabs$community_raw[i]<=vabs_ae_revised$community_top),1],NA)
  vabs$interpersonal_ae[i] = ifelse(!is.na(vabs$interpersonal_raw[i]),
                                   vabs_ae_revised[which(vabs$interpersonal_raw[i]>=vabs_ae_revised$interpersonal_bottom
                                                        & vabs$interpersonal_raw[i]<=vabs_ae_revised$interpersonal_top),1],NA)
  vabs$play_ae[i] = ifelse(!is.na(vabs$playleisure_raw[i]),
                          vabs_ae_revised[which(vabs$playleisure_raw[i]>=vabs_ae_revised$play_bottom
                                                & vabs$playleisure_raw[i]<=vabs_ae_revised$play_top),1],NA)
  vabs$coping_ae[i] = ifelse(!is.na(vabs$coping_raw[i]),
                            vabs_ae_revised[which(vabs$coping_raw[i]>=vabs_ae_revised$coping_bottom
                                                  & vabs$coping_raw[i]<=vabs_ae_revised$coping_top),1],NA)
  vabs$gross_ae[i] = ifelse(!is.na(vabs$gross_raw[i]),
                           vabs_ae_revised[which(vabs$gross_raw[i]>=vabs_ae_revised$gross_bottom
                                                & vabs$gross_raw[i]<=vabs_ae_revised$gross_top),1],NA)
  vabs$fine_ae[i] = ifelse(!is.na(vabs$fine_raw[i]),
                          vabs_ae_revised[which(vabs$fine_raw[i]>=vabs_ae_revised$fine_bottom
                                                & vabs$fine_raw[i]<=vabs_ae_revised$fine_top),1],NA)
}

#putting back prefix
vabs = inserting_prefix_into_variables(vabs,"vine_")

#orphaned/duplicate_data
vabs_orphaned_data = orphaned_data_consolidate(vabs)
vabs = orphaned_data_remove(vabs)
#getting rid of 4P data
vabs=subset(vabs,visit != '4P')
vabs_duplicate_data = duplicate_data_consolidate(vabs,"vine_age")
vabs = duplicate_data_remove(vabs,"vine_age")

#outliers
vabs_outliers = vabs[,c(1:2,grep("_age$",names(vabs)),grep("missing",names(vabs)),grep("receptive_v",names(vabs)):grep("fine_ae",names(vabs)))]
vabs_outliers = outlier_list(vabs_outliers)
vabs$vine_outlier_list = vabs_outliers$outlier_list
#vabs_outlier_table = sqldf("select id,visit,outlier_list from vabs where outlier_list != ''")
rm(vabs_outliers)

#archiving the data
vabs_scored = vabs[,c(1:2,grep("_age$",names(vabs)),grep("missing",names(vabs)),grep("_raw$",names(vabs)),
                      grep("_v$",names(vabs)),grep("_ae$",names(vabs)),grep("_ss$",names(vabs)),ncol(vabs))]
vabs_scored = vabs_scored[,-grep("abc_raw",names(vabs_scored))]
write.csv(vabs_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/vabs_scored.csv",row.names=F)

#cleaning up
rm(vabs_v_scale_revised,vabs_standard_revised,vabs_ae_revised,rl,el,wl,personal,
   domestic,community,interpersonal,play,coping,gross,fine,vabs_t1,vabs_t3,vabs_t5,vabs2)
