#Querying the data
conners = sqlQuery(con4,"select * from Conners;",stringsAsFactors=F)

#flagging old single entries and removing rows with entry_status<2
conners_entry_flag = entry_flag(conners,'conners')
conners = subset(conners,entry_status == 2)

#modifying variable classes and removing prefix for now
conners = id_visit_changing_into_char(conners)
conners = removing_prefix(conners,"conners_")

#Calculating age
conners = fxage(conners,'id','date')

for(j in paste0('',1:45)){
  conners[,j] = ifelse(conners[,j]<0,NA,conners[,j])
}

#item list for sections
in_items = paste0('',c(17,27,30,34,41))
hy_items = paste0('',c(3,5,7,13,24,28))
lp_items = paste0('',c(8,10,25,36,39))
ef_items = paste0('',c(1,15,20,32,34))
ag_items = paste0('',c(14,19,21,23,26))
pr_items = paste0('',c(4,6,18,38,43))
pi_items = paste0('',c(2,12,31,37,40,42))
ni_items = paste0('',c(9,11,16,22,29,33))

#missing data analysis
conners = count_missing_items(conners,'1','43')
conners = comment_missing_data(conners,list(in_items,hy_items,lp_items,ef_items,ag_items,pr_items,pi_items,ni_items),
                               list('inattention','hyperactivity/impulsivity','learning_problem','executive_function',
                                    'defiance/aggression','peer_relations','positive_impression','negative_impression'))

#summing up to score
conners = summing_items_per_row(conners,list(in_items,hy_items,lp_items,ef_items,ag_items,pr_items,pi_items,ni_items),
                                list('in_raw','hy_raw','lp_raw','ef_raw','ag_raw','pr_raw','pi_raw','ni_raw'),T)

#calling norm tables
conners_in = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/Conners_IN.csv")
conners_hy = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/Conners_HY.csv")
conners_lp = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/Conners_LP.csv")
conners_ef = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/Conners_EF.csv")
conners_ag = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/Conners_AG.csv")
conners_pr = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/Conners_PR.csv")

#obtaining gender
conners = sqldf("select t1.*,gender from conners as t1 left join subj on t1.id = subj.subj_id")

#obtaining T-scores
conners$gender = tolower(conners$gender)
for(i in 1:nrow(conners)){
  conners$in_t[i] = conners_in[which(conners$in_raw[i] == conners_in$raw_score 
                                     & conners$gender[i] == conners_in$gender 
                                     & floor(conners$age)[i]-conners_in$age>=0 
                                     & floor(conners$age)[i]-conners_in$age<12),"t_score"]
  conners$hy_t[i] = conners_hy[which(conners$hy_raw[i] == conners_hy$raw_score 
                                     & conners$gender[i] == conners_hy$gender 
                                     & floor(conners$age)[i]-conners_hy$age>=0 
                                     & floor(conners$age)[i]-conners_hy$age<12),"t_score"]
  conners$lp_t[i] = conners_lp[which(conners$lp_raw[i] == conners_lp$raw_score 
                                     & conners$gender[i] == conners_lp$gender 
                                     & floor(conners$age)[i]-conners_lp$age>=0 
                                     & floor(conners$age)[i]-conners_lp$age<12),"t_score"]
  conners$ef_t[i] = conners_ef[which(conners$ef_raw[i] == conners_ef$raw_score 
                                     & conners$gender[i] == conners_ef$gender 
                                     & floor(conners$age)[i]-conners_ef$age>=0 
                                     & floor(conners$age)[i]-conners_ef$age<12),"t_score"]
  conners$ag_t[i] = conners_ag[which(conners$ag_raw[i] == conners_ag$raw_score 
                                     & conners$gender[i] == conners_ag$gender 
                                     & floor(conners$age)[i]-conners_ag$age>=0 
                                     & floor(conners$age)[i]-conners_ag$age<12),"t_score"]
  conners$pr_t[i] = conners_pr[which(conners$pr_raw[i] == conners_pr$raw_score 
                                     & conners$gender[i] == conners_pr$gender 
                                     & floor(conners$age)[i]-conners_pr$age>=0 
                                     & floor(conners$age)[i]-conners_pr$age<12),"t_score"]
}

#obtaining percentile
for(i in 1:nrow(conners)){
  conners$in_percentile[i] = conners_in[which(conners$in_raw[i] == conners_in$raw_score 
                                     & conners$gender[i] == conners_in$gender 
                                     & floor(conners$age)[i]-conners_in$age>=0 
                                     & floor(conners$age)[i]-conners_in$age<12),"percentile"]
  conners$hy_percentile[i] = conners_hy[which(conners$hy_raw[i] == conners_hy$raw_score 
                                     & conners$gender[i] == conners_hy$gender 
                                     & floor(conners$age)[i]-conners_hy$age>=0 
                                     & floor(conners$age)[i]-conners_hy$age<12),"percentile"]
  conners$lp_percentile[i] = conners_lp[which(conners$lp_raw[i] == conners_lp$raw_score 
                                     & conners$gender[i] == conners_lp$gender 
                                     & floor(conners$age)[i]-conners_lp$age>=0 
                                     & floor(conners$age)[i]-conners_lp$age<12),"percentile"]
  conners$ef_percentile[i] = conners_ef[which(conners$ef_raw[i] == conners_ef$raw_score 
                                     & conners$gender[i] == conners_ef$gender 
                                     & floor(conners$age)[i]-conners_ef$age>=0 
                                     & floor(conners$age)[i]-conners_ef$age<12),"percentile"]
  conners$ag_percentile[i] = conners_ag[which(conners$ag_raw[i] == conners_ag$raw_score 
                                     & conners$gender[i] == conners_ag$gender 
                                     & floor(conners$age)[i]-conners_ag$age>=0 
                                     & floor(conners$age)[i]-conners_ag$age<12),"percentile"]
  conners$pr_percentile[i] = conners_pr[which(conners$pr_raw[i] == conners_pr$raw_score 
                                     & conners$gender[i] == conners_pr$gender 
                                     & floor(conners$age)[i]-conners_pr$age>=0 
                                     & floor(conners$age)[i]-conners_pr$age<12),"percentile"]
}

#putting back prefix
conners = inserting_prefix_into_variables(conners,"conners_")

#orphaned/duplicate data
conners_orphaned_data = orphaned_data_consolidate(conners)
conners = orphaned_data_remove(conners)
conners_duplicate_data = duplicate_data_consolidate(conners,'conners_age')
conners = duplicate_data_remove(conners,'conners_age')

#extracting relevant columns and adding prefixes
conners_scored = conners[,c(1:2,grep("age$",names(conners)),grep("missing",names(conners)),
                            grep("_raw$",names(conners)),grep("_t$",names(conners)),grep("_percentile$",names(conners)))]

#obtaining outliers
conners_scored = outlier_list(conners_scored)
conners$outlier_list = conners_scored$outlier_list
conners_scored$outlier_list = NULL
#conners_outlier_table = sqldf("select id,visit,outlier_list from conners where outlier_list != ''")

#archiving the data
write.csv(conners_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived_staar/conners_scored.csv",row.names = F)
write.csv(conners[,c(1:2,grep("_1$",names(conners)):grep("_45$",names(conners)))],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived_staar/conners_items.csv",row.names = F)

#cleaning the environment
rm(conners_ag,conners_ef,conners_hy,conners_in,conners_lp,conners_pr,in_items,hy_items,lp_items,
   ef_items,ag_items,pr_items,pi_items,ni_items)
