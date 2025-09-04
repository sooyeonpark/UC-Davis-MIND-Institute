#pulling out the table
adi = sqlQuery(channel=new_con,"select * from ADI_R_2003;",stringsAsFactors=F)

#changing id and visit into characters and removing preexisting prefix & single-entered rows
adi = id_visit_changing_into_char(adi)
adi_entry_flag = entry_flag(adi,"adi")
adi = subset(adi,entry_status==2)
if(!is.null(adi_entry_flag)){
  adi = rbind(adi,adi_entry_flag[,-ncol(adi_entry_flag)])
}
adi = removing_prefix(adi,"ADI_")

#flag rows where overallang variable is -9
adi_overallang_na = adi[which((is.na(adi$overallang) | adi$overallang==-9) & adi$visit != "3"),]
adi = adi[-which((is.na(adi$overallang) | adi$overallang==-9) & adi$visit != "3"),]

#ADI age calc
adi = fxage(adi,'id','date')

##converting codes to scores
#first, identify which items belong to which sections
a_items_old = c("directgaze_mostabn","socsmile_mostabn","rangefacialexp_mostabn","imagplay_peers_mostabn",
                "intchild_mostabn","respotherchild_mostabn","grpplay_mostabn","friends_mostabn",
                "showing_mostabn","sharing_mostabn","sharedenjoy_mostabn","offercomfort_mostabn",
                "qual_social_mostabn","socialresp_mostabn","inappfacialexp_ever","usebody_ever")
a_items = gsub("_mostabn","",c("directgaze_mostabn","socsmile_mostabn","rangefacialexp_mostabn",
                               "intchild_mostabn","respotherchild_mostabn","showing_mostabn",
                               "sharing_mostabn","sharedenjoy_mostabn","offercomfort_mostabn",
                               "qual_social_mostabn","socialresp_mostabn","inappfacialexp_ever","usebody_ever"))
b_v_items_old = c("stereo_ever","socchat_ever","recipcon_ever","innappq_ever","pronounrev_ever",
                  "neologisms_ever","point_mostabn","nodding_mostabn","headshake_mostabn","conventgest_mostabn",
                  "spontimitation_mostabn","imagplay_mostabn","imitplay_mostabn")
b_v_items = gsub("_mostabn","",b_v_items_old)
b_nv_items_old = c("spontimitation_mostabn","imagplay_mostabn","imitplay_mostabn","point_mostabn",
                   "nodding_mostabn","headshake_mostabn","conventgest_mostabn")
b_nv_items = gsub("_mostabn","",b_nv_items_old)
c_items = c("verbrituals_ever","preoccupy_ever","circuminterest_ever","repobj_ever","compulsions_ever",
            "unusualsense_ever","handfingerman_ever","othermanner_ever")
c1_c2_items = c("verbrituals_ever","preoccupy_ever","circuminterest_ever","compulsions_ever")
d_items = c("sympt_onset","age_singlewords","age_firstphrases","resp_ageabn","clin_ageabn")

#subsetting the dataset based on age groups
adi_young = adi[which(adi$age<48),c("id","visit","age","overallang",a_items,b_v_items,c_items,d_items,"valid","clincom")]
adi_old = adi[which(adi$age>=48),c("id","visit","age","overallang",a_items_old,b_v_items_old,c_items,d_items,"valid","clincom")]

#converting codes to scores
adi_young = adi_codes2scores(adi_young,"directgaze","othermanner_ever")
adi_old = adi_codes2scores(adi_old,"directgaze_mostabn","othermanner_ever")

#dont forget to run the max function for items in C3n4
adi_young$C3 = finding_max(adi_young$C3,adi_young$handfingerman_ever,adi_young$othermanner_ever)
adi_young$C4 = finding_max(adi_young$C4,adi_young$repobj_ever,adi_young$unusualsense_ever)
adi_old$C3 = finding_max(adi_old$C3,adi_old$handfingerman_ever,adi_old$othermanner_ever)
adi_old$C4 = finding_max(adi_old$C4,adi_old$repobj_ever,adi_old$unusualsense_ever)

#missing data analysis
adi_young = count_missing_items(adi_young,"directgaze","clin_ageabn")
adi_young = comment_missing_data(adi_young,list(a_items,b_v_items,b_nv_items,c(c1_c2_items,"C3","C4"),d_items),
                                 list('A','B_Verbal','B_NonVerbal','C','D'))
adi_old = count_missing_items(adi_old,"directgaze_mostabn","clin_ageabn")
adi_old = comment_missing_data(adi_old,list(a_items_old,b_v_items_old,b_nv_items_old,c(c1_c2_items,"C3","C4"),d_items),
                               list('A','B_Verbal','B_NonVerbal','C','D'))

#total scores
adi_young = adi_totals(adi_young)
adi_old = adi_totals(adi_old)

# #total B
# adi_young = totalB_calc(adi_young,adi_young$totalNVB,adi_young$totalVB)
# adi_old = totalB_calc(adi_old,adi_old$totalNVB,adi_old$totalVB)

#adjusting section B scores
for(i in 1:nrow(adi_young)){
  if(!is.na(adi_young$overallang[i])){
    if(adi_young$overallang[i] == 0){
      adi_young$totalNVB[i] = NA
    }
    else{
      adi_young$totalVB[i] = NA
    }
  }
}
for(i in 1:nrow(adi_old)){
  if(!is.na(adi_old$overallang[i])){
    if(adi_old$overallang[i] == 0){
      adi_old$totalNVB[i] = NA
    }
    else{
      adi_old$totalVB[i] = NA
    }
  }
}

#classification
regression_items = c("languageloss","skillloss_socengage","onset_hindsight")
for(j in regression_items){
  adi[,j] = cbraw(adi[,j])
}
adi$regression_broad = ifelse(adi$languageloss==1 | adi$skillloss_socengage>0,1,0)
adi$regression_narrow = ifelse(adi$languageloss==1 | adi$skillloss_socengage>1,1,0)
adi$onset = ifelse(adi$onset_hindsight==0 & adi$languageloss==0 & adi$skillloss_socengage==0,1,
                  ifelse(adi$onset_hindsight==0 & adi$regression_broad==1,2,
                        ifelse(adi$onset_hindsight>0 & adi$languageloss==0 & adi$skillloss_socengage<2,3,
                              ifelse(adi$onset_hindsight>0 & adi$regression_narrow==1,4,
                                     ifelse(is.na(adi$onset_hindsight),NA,0)))))
adi$onset_4groups = ifelse(adi$onset==1,'Early Signs',
                           ifelse(adi$onset==2,'Mixed Early Signs + Loss',
                                  ifelse(adi$onset==3,'Plateau',
                                         ifelse(adi$onset==4,'Regression',''))))
adi$onset2 = ifelse(is.na(adi$onset),NA,ifelse(adi$onset==1 | adi$onset==3,1,ifelse(adi$onset==2 | adi$onset==4,2,0)))
adi$onset_2groups = ifelse(adi$onset2==1,'Early Onset',ifelse(adi$onset2==2,'Regression',''))

#putting prefix back in
adi_young = inserting_prefix_into_variables(adi_young,"adi_")
adi_old = inserting_prefix_into_variables(adi_old,"adi_")
adi = inserting_prefix_into_variables(adi,'adi_')

#extracting necessary columns
adi_young_scored = adi_young[,c(1:3,grep("valid$",names(adi_young)):grep("clincom$",names(adi_young)),grep("missing",names(adi_young)),
                                grep("totalA$",names(adi_young)),grep("totalVB$",names(adi_young)),grep("totalNVB",names(adi_young)),
                                grep("totalC$",names(adi_young)),grep("sympt_onset",names(adi_young)):grep("clin_ageabn",names(adi_young)))]
adi_young_scored = merge(adi_young_scored,adi[,c(1:2,grep("regression_broad",names(adi)):ncol(adi))],all.x=T)
adi_old_scored = adi_old[,c(1:3,grep("valid$",names(adi_old)):grep("clincom$",names(adi_old)),grep("missing",names(adi_old)),
                            grep("totalA$",names(adi_old)),grep("totalVB$",names(adi_old)),grep("totalNVB",names(adi_old)),
                            grep("totalC$",names(adi_old)),grep("sympt_onset",names(adi_old)):grep("clin_ageabn",names(adi_old)))]
adi_old_scored = merge(adi_old_scored,adi[,c(1:2,grep("regression_broad",names(adi)):ncol(adi))],all.x=T)

#generating outlier table
adi_young_outliers = adi_young_scored[,c(1:3,grep("total",names(adi_young_scored)))]
adi_old_outliers = adi_old_scored[,c(1:3,grep("total",names(adi_old_scored)))]
adi_young_outliers = outlier_list(adi_young_outliers)
adi_young_scored$adi_outlier_list = adi_young_outliers$outlier_list
adi_old_outliers = outlier_list(adi_old_outliers)
adi_old_scored$adi_outlier_list = adi_old_outliers$outlier_list

rm(adi_young_outliers,adi_old_outliers)

# ##toddler module
# adi_t = read_excel("ADI/ADI_Toddler_2003.xlsx")
# adi_t = removing_prefix(adi_t,"dbaes_")
# adi_t = adi_t[,-c(grep("_code$",names(adi_t)),grep("^[a-d]{1}[0-9]{1}$",names(adi_t)),grep("^[a-d]{1}total$",names(adi_t)))]
# 
# #calculating age
# adi_t = fxage(adi_t,'id','interview_date')
# 
# #item lists
# a_items_tod = grep("^a[0-4]{1}",names(adi_t))
# b_v_items_tod = grep("^b[0-4]{1}",names(adi_t))
# b_nv_items_tod = grep("^b[1,4]{1}",names(adi_t))
# c_items_tod = grep("^c[1-4]{1}",names(adi_t))
# d_items_tod = grep("^d",names(adi_t))
# 
# #missing data analysis
# adi_t = count_missing_items(adi_t,"a1gaze","b3neoid")
# adi_t = count_missing_items(adi_t,"c1unproc","djudgage")
# adi_t = comment_missing_data(adi_t,list(a_items_tod,b_v_items_tod,b_nv_items_tod,c_items_tod,d_items_tod),
#                              list('A','B_Verbal','B_NonVerbal','C','D'))
# 
# #summing the scores
# adi_t = summing_items_per_row(adi_t,list(a_items_tod,b_v_items_tod,b_nv_items_tod,c_items_tod),
#                               list('totalA','totalVB','totalNVB','totalC'),F)
# 
# #merging classification & putting prefix & extracting relevant columns
# adi_t = inserting_prefix_into_variables(adi_t,'adi_')
# adi_t_scored = adi_t[,c(1:2,49:ncol(adi_t),43:47)]
# adi_t_scored = merge(adi_t_scored,adi[,c(1:2,grep("regression_broad",names(adi)):ncol(adi))],all.x=T)
# names(adi_t_scored)[10:14] = names(adi_young_scored)[12:16]
# 
# #getting rid of entries in adi_young_scored that adi_t_scored has
# adi_young_scored = adi_young_scored[!(adi_young_scored$id %in% adi_t_scored$id),]
# 
# #combining both tables
# adi_t_scored$adi_outlier_list = ""
# adi_scored = rbind.fill(adi_young_scored,adi_t_scored,adi_old_scored)
adi_scored = rbind.fill(adi_young_scored,adi_old_scored)

#orphaned/duplicate data
adi_orphaned_data = orphaned_data_consolidate(adi_scored)
adi_scored = orphaned_data_remove(adi_scored)
adi_duplicate_data = duplicate_data_consolidate(adi_scored,"adi_age")
adi_scored = duplicate_data_remove(adi_scored,"adi_age")
#adi_scored = rbind(adi_scored,adi_duplicate_data[c(2,4),])

#archiving data
names(adi_scored) = gsub("_clincom$","_comment",names(adi_scored))
write.csv(adi_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/adi_scored.csv",row.names = F)
write.csv(adi[,c(1:2,7:163)],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/adi_items.csv",row.names = F)

#cleaning up
rm(a_items,a_items_old,b_v_items,b_nv_items,b_v_items_old,b_nv_items_old,c_items,d_items,c1_c2_items,regression_items)
#   a_items_tod,b_v_items_tod,b_nv_items_tod,c_items_tod,adi_t,d_items_tod,
