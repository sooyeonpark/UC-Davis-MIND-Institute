#getting norm tables
ados_sev_tbl = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/scr_ados_severity.csv")
ados_sev_dom_tbl = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/scr_ados_domain_severity.csv")

#pulling out data tables from databases
#ados_G
ados_g1 = sqlQuery(new_con, query="select * from `ADOS-G_M1`;",stringsAsFactors=F)
ados_g2 = sqlQuery(channel=new_con, query="select * from `ADOS-G_M2`;",stringsAsFactors=F)
ados_g3 = sqlQuery(channel=new_con, query="select * from `ADOS-G_M3`;",stringsAsFactors=F)
#ados2
ados2_1 = sqlQuery(channel=new_con, query="select * from `ADOS-2 M1`;",stringsAsFactors=F)
ados2_2 = sqlQuery(channel=new_con, query="select * from `ADOS-2 M2`;",stringsAsFactors=F)
ados2_3 = sqlQuery(channel=new_con, query="select * from `ADOS-2 M3`;",stringsAsFactors=F)
ados2_4 = sqlFetch(new_con,"ADOS-2 M4",stringsAsFactors=F)
#ados2 from staar
ados_staar = sqlQuery(channel=con4, query="select * from `ADOS-2 M3`;",stringsAsFactors=F)

#combining brain and staar tables
library(plyr)
#ados2_2 = rbind.fill(ados2_2,ados2_2_star)
ados2_3 = rbind.fill(ados2_3,ados_staar)

#changing id and visit into characters
ados_g1 = id_visit_changing_into_char(ados_g1)
ados_g2 = id_visit_changing_into_char(ados_g2)
ados_g3 = id_visit_changing_into_char(ados_g3)
ados2_1 = id_visit_changing_into_char(ados2_1)
ados2_2 = id_visit_changing_into_char(ados2_2)
ados2_3 = id_visit_changing_into_char(ados2_3)
ados2_4 = id_visit_changing_into_char(ados2_4)

#single entry flag
ados_g1_entry_flag = entry_flag(ados_g1,"ados")
ados_g2_entry_flag = entry_flag(ados_g2,"ados")
ados_g3_entry_flag = entry_flag(ados_g3,"ados")
ados2_1_entry_flag = entry_flag(ados2_1,'ados')
ados2_2_entry_flag = entry_flag(ados2_2,'ados')
ados2_3_entry_flag = entry_flag(ados2_3,'ados')
ados2_4_entry_flag = entry_flag(ados2_4,'ados')

#removing single-entered items & pilot data
ados_g1 = subset(ados_g1,entry_status==2 & visit != '4p')
ados_g2 = subset(ados_g2,entry_status==2 & visit != '4p')
ados_g3 = subset(ados_g3,entry_status==2 & visit != '4p')
ados2_1 = subset(ados2_1,entry_status==2 & visit != '4p')
ados2_2 = subset(ados2_2,entry_status==2 & visit != '4p')
ados2_3 = subset(ados2_3,entry_status==2 & visit != '4p')
ados2_4 = subset(ados2_4,entry_status==2 & visit != '4p')
ados_g1$entry_date = NULL
ados_g1 = rbind(ados_g1,ados_g1_entry_flag[,-ncol(ados_g1_entry_flag)])
ados_g2 = rbind(ados_g2,ados_g2_entry_flag)
ados_g3 = rbind(ados_g3,ados_g3_entry_flag)
if(!is.null(ados2_1_entry_flag)){
  ados2_1 = rbind(ados2_1,ados2_1_entry_flag[,-ncol(ados2_1_entry_flag)])
}
if(!is.null(ados2_2_entry_flag)){
  ados2_2 = rbind(ados2_2,ados2_2_entry_flag[,-ncol(ados2_2_entry_flag)])
}
if(!is.null(ados2_3_entry_flag)){
  ados2_3 = rbind(ados2_3,ados2_3_entry_flag[grep("[0-9]{6}-[0-9]{3}",ados2_3_entry_flag$id),-ncol(ados2_3_entry_flag)])
}
if(!is.null(ados2_4_entry_flag)){
  ados2_4 = rbind(ados2_4,ados2_4_entry_flag[grep("[0-9]{6}-[0-9]{3}",ados2_4_entry_flag$id),-ncol(ados2_4_entry_flag)])
}
#extracting only necessary columns (as each table is pretty messy)
ados_g1 = ados_g1[,c(1:2,9,11:39)]
ados_g2 = ados_g2[,c(1:2,31,33:60)]
ados_g3 = ados_g3[,c(1:2,31,33:60)]

#removing prefixes for convenience for now
ados_g1 = removing_prefix(ados_g1,"ad1_")
ados_g2 = removing_prefix(ados_g2,"_b$")
ados_g2 = removing_prefix(ados_g2,"coding")
ados_g3 = removing_prefix(ados_g3,"coding")
ados_g3 = removing_prefix(ados_g3,"_c$")
ados2_1 = removing_prefix(ados2_1,"ad2_mod1_")
ados2_2 = removing_prefix(ados2_2,"ad2_mod2_")
ados2_3 = removing_prefix(ados2_3,"ad2_mod3_")
ados2_4 = removing_prefix(ados2_4,"ad2_mod4_")
ados_staar = removing_prefix(ados_staar,'ad2_mod3_')

##Module 1##

#calculating age
ados_g1 = fxage(ados_g1,'id', 'date')
ados2_1 = fxage(ados2_1,'id','date')

#words vs no words
ados_g1$no_words = ifelse(ados_g1$a1 %in% c(3,8),1,0)
ados2_1$no_words = ifelse(ados2_1$a1>=3,1,0)

#version & module labelling
ados_g1$version = 'ADOSG'
ados_g1$module = 'module 1'
ados2_1$version = 'ADOS2'
ados2_1$module = 'module 1'

#converting codes to scores
ados_g1 = ados_codes2scores(ados_g1,"a2","e3")
ados2_1 = ados_codes2scores(ados2_1,"a2","e3")

#obtaining section total scores
#list of items that contribute the scores
mod1_no_words_sa_items = c(paste0("a",c(2,8)),paste0("b",c(1,3:5,9:12)))
mod1_some_words_sa_items = c(paste0("a",c(2,7:8)),paste0("b",c(1,3:5,9:10,12)))
mod1_no_words_rrb_items = c("a3",paste0("d",c(1:2,4)))
mod1_some_words_rrb_items = c("a5",paste0("d",c(1:2,4)))

#missing data analysis
ados_g1 = count_missing_items(ados_g1,"a1","e3")
ados_g1$missing_items_comment = ""
ados_g1[which(ados_g1$no_words==0),] = comment_missing_data(ados_g1[which(ados_g1$no_words==0),],
                                                            list(mod1_some_words_sa_items,mod1_some_words_rrb_items),list('sa','rrb'))
ados_g1[which(ados_g1$no_words==1),] = comment_missing_data(ados_g1[which(ados_g1$no_words==1),],
                                                            list(mod1_no_words_sa_items,mod1_no_words_rrb_items),list('sa','rrb'))
ados2_1 = count_missing_items(ados2_1,"a1","e3")
ados2_1$missing_items_comment = ""
ados2_1[which(ados2_1$no_words==0),] = comment_missing_data(ados2_1[which(ados2_1$no_words==0),],
                                                            list(mod1_some_words_sa_items,mod1_some_words_rrb_items),list('sa','rrb'))
ados2_1[which(ados2_1$no_words==1),] = comment_missing_data(ados2_1[which(ados2_1$no_words==1),],
                                                            list(mod1_no_words_sa_items,mod1_no_words_rrb_items),list('sa','rrb'))

#sa scores
ados_g1 = ados_conditional_summation(ados_g1,"sa_total","no_words",
                                     mod1_no_words_sa_items,mod1_some_words_sa_items)
#^when no_words variable is true, create the sum column, "sa_total", using first items, otherwise, use 2nd items
ados2_1 = ados_conditional_summation(ados2_1,"sa_total","no_words",
                                      mod1_no_words_sa_items,mod1_some_words_sa_items)

#rrb scores
ados_g1 = ados_conditional_summation(ados_g1,"rrb_total","no_words",
                                     mod1_no_words_rrb_items,mod1_some_words_rrb_items)
#^when no_words variable is true, create the sum column, "rrb_total", using first items, otherwise, use 2nd items
ados2_1 = ados_conditional_summation(ados2_1,"rrb_total","no_words",
                                      mod1_no_words_rrb_items,mod1_some_words_rrb_items)

# total score
ados_g1$sarrb_total = ados_g1$sa_total + ados_g1$rrb_total
ados2_1$sarrb_total = ados2_1$sa_total + ados2_1$rrb_total

#severity scores
ados_g1 = ados_severity_scores(ados_g1)
ados2_1 = ados_severity_scores(ados2_1)

##Module 2##

#calculating age
ados_g2 = fxage(ados_g2, 'id', 'interview_date')
ados2_2 = fxage(ados2_2,'id','date')

#module and version labelling
ados_g2$version = 'ADOSG'
ados_g2$module = 'module 2'
ados2_2$version = 'ADOS2'
ados2_2$module = 'module 2'

#converting codes to scores
names(ados_g2)[which(names(ados_g2)=="a_olang"):which(names(ados_g2)=="e_anxty")] = 
  c(paste0('a',c(1,0,2:7)),paste0('b',c(1:8,10:12)),paste0('c',1:2),paste0('d',1:4),paste0('e',1:3))
ados_g2 = ados_codes2scores(ados_g2,"a1","e3")
ados2_2 = ados_codes2scores(ados2_2,"a1","e3")

#obtaining total scores
#list of algorithm items
mod2_sa_items = c(paste0('a',6:7),paste0('b',c(1:3,5:6,8,11:12)))
mod2_rrb_items = c('a4',paste0('d',c(1:2,4)))

#missing data analysis
ados_g2 = count_missing_items(ados_g2,"a1","e3")
ados_g2 = comment_missing_data(ados_g2,list(mod2_sa_items,mod2_rrb_items),list('sa','rrb'))
ados2_2 = count_missing_items(ados2_2,"a1","e3")
ados2_2 = comment_missing_data(ados2_2,list(mod2_sa_items,mod2_rrb_items),list('sa','rrb'))

# obtaining total scores
for(i in 1:nrow(ados_g2)){
  ados_g2$sa_total[i] = sum(ados_g2[i,mod2_sa_items],na.rm=F)
  ados_g2$rrb_total[i] = sum(ados_g2[i,mod2_rrb_items],na.rm=F)
  ados_g2$sarrb_total[i] = ados_g2$sa_total[i] + ados_g2$rrb_total[i]
}
for(i in 1:nrow(ados2_2)){
  ados2_2$sa_total[i] = sum(ados2_2[i,mod2_sa_items],na.rm=F)
  ados2_2$rrb_total[i] = sum(ados2_2[i,mod2_rrb_items],na.rm=F)
  ados2_2$sarrb_total[i] = ados2_2$sa_total[i] + ados2_2$rrb_total[i]
}

#obtaining severity scores
ados_g2$no_words = 0
ados2_2$no_words = 0
ados_g2 = ados_severity_scores(ados_g2)
ados2_2 = ados_severity_scores(ados2_2)

##Module 3##

#calculating age
ados_g3 = fxage(ados_g3, 'id', 'interview_date')
ados2_3 = fxage(ados2_3,'id','date')

#module and version labelling
ados_g3$version = 'ADOSG'
ados_g3$module = 'module 3'
ados2_3$version = 'ADOS2'
ados2_3$module = 'module 3'

#converting codes to scores
names(ados_g3)[which(names(ados_g3)=="a_olang"):which(names(ados_g3)=="e_anxty")] = 
  c(paste0('a',1:9),paste0('b',c(1:7,9:11)),'c1',paste0('d',1:5),paste0('e',1:3))
ados_g3 = ados_codes2scores(ados_g3,"a1","e3")
ados2_3 = ados_codes2scores(ados2_3,"a1","e3")

#obtaining total scores
#list of algorithm items
mod3_sa_items = c(paste0('a',7:9),paste0('b',c(1:2,4,7,9:11)))
mod3_rrb_items = c('a4',paste0('d',c(1:2,4)))

#missing data analysis
ados_g3 = count_missing_items(ados_g3,"a1","e3")
ados_g3 = comment_missing_data(ados_g3,list(mod3_sa_items,mod3_rrb_items),list('sa','rrb'))
ados2_3 = count_missing_items(ados2_3,"a1","e3")
ados2_3 = comment_missing_data(ados2_3,list(mod3_sa_items,mod3_rrb_items),list('sa','rrb'))

#obtaining total scores
for(i in 1:nrow(ados_g3)){
  ados_g3$sa_total[i] = sum(ados_g3[i,mod3_sa_items],na.rm=F)
  ados_g3$rrb_total[i] = sum(ados_g3[i,mod3_rrb_items],na.rm=F)
  ados_g3$sarrb_total[i] = ados_g3$sa_total[i] + ados_g3$rrb_total[i]
}
for(i in 1:nrow(ados2_3)){
  ados2_3$sa_total[i] = sum(ados2_3[i,mod3_sa_items],na.rm=F)
  ados2_3$rrb_total[i] = sum(ados2_3[i,mod3_rrb_items],na.rm=F)
  ados2_3$sarrb_total[i] = ados2_3$sa_total[i] + ados2_3$rrb_total[i]
}

#obtaining severity scores
ados_g3$no_words = 0
ados2_3$no_words = 0
ados_g3 = ados_severity_scores(ados_g3)
ados2_3 = ados_severity_scores(ados2_3)

# ##Module 4##
# 
# #calculating age
# ados2_4 = fxage(ados2_4,'id','date')
# 
# #module and version labelling
# ados2_4$version = 'ADOS2'
# ados2_4$module = 'module 4'
# 
# #converting codes to scores
# ados2_4 = ados_codes2scores(ados2_4,"a1","e3")
# 
# #obtaining total scores
# #list of algorithm items
# mod4_sa_items = c(paste0('a',c(8,10)),paste0('b',c(1:2,5,7,9,11:13)))
# mod4_rrb_items = c('a4',paste0('d',c(1:2,4)))
# 
# #missing data analysis
# ados2_4 = count_missing_items(ados2_4,"a1","e3")
# ados2_4 = comment_missing_data(ados2_4,list(mod4_sa_items,mod4_rrb_items),list('sa','rrb'))
# 
# #obtaining total scores
# for(i in 1:nrow(ados2_4)){
#   ados2_4$sa_total[i] = sum(ados2_4[i,mod4_sa_items],na.rm=F)
#   ados2_4$rrb_total[i] = sum(ados2_4[i,mod4_rrb_items],na.rm=F)
#   ados2_4$sarrb_total[i] = ados2_4$sa_total[i] + ados2_4$rrb_total[i]
# }
# 
# #obtaining severity scores
# ados2_4$no_words = 0
# ados2_4 = ados_severity_scores(ados2_4)
# 
#combining tables
ados_columns = c("id","visit","version","module","age","missing_items_count","missing_items_comment","a1","no_words",
                 "sa_total","rrb_total","sarrb_total","severity","sa_severity","rrb_severity")
ados_scored = rbind(ados_g1[,ados_columns],ados_g2[,ados_columns],ados_g3[,ados_columns],
                    ados2_1[,ados_columns],ados2_2[grep("[0-9]{6}-[0-9]{3}",ados2_2$id),ados_columns],
                    ados2_3[grep("[0-9]{6}-[0-9]{3}",ados2_3$id),ados_columns])
ados_scored_staar = rbind(ados2_2[grep("^[0-9]{4}$",ados2_2$id),ados_columns],
                          ados2_3[grep("^[0-9]{4}$",ados2_3$id),ados_columns])

#putting prefixes back
ados_scored = inserting_prefix_into_variables(ados_scored,"ados_")
ados_scored_staar = inserting_prefix_into_variables(ados_scored_staar,"ados_")

#orphaned/duplicate data
ados_orphaned_data = orphaned_data_consolidate(ados_scored)
ados_scored = orphaned_data_remove(ados_scored)
ados_duplicate_data = duplicate_data_consolidate(ados_scored,"ados_age")
ados_scored = duplicate_data_remove(ados_scored,"ados_age")
ados_orphaned_data_staar = orphaned_data_consolidate(ados_scored_staar)
ados_scored_staar = orphaned_data_remove(ados_scored_staar)
ados_duplicate_data_staar = duplicate_data_consolidate(ados_scored_staar,"ados_age")
ados_scored_staar = duplicate_data_remove(ados_scored_staar,"ados_age")

#outliers for app cohort participants
ados_outliers = ados_scored[,c(1:2,grep("total$",names(ados_scored)))]
ados_outliers = outlier_list(ados_outliers)
ados_scored$ados_outlier_list = ados_outliers$outlier_list
ados_outliers = ados_scored_staar[,c(1:2,grep("total$",names(ados_scored)))]
ados_outliers = outlier_list(ados_outliers)
ados_scored_staar$ados_outlier_list = ados_outliers$outlier_list
rm(ados_outliers)

#archiving data
write.csv(ados_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/ados_scored.csv", row.names=FALSE)
write.csv(ados_scored_staar[,-(3:4)], "S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived_staar/ados_scored.csv", row.names=FALSE)
write.csv(ados_staar[,c(1:2,grep("^a1$",names(ados_staar)):grep("^e3$",names(ados_staar)))], "S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived_staar/ados_items.csv", row.names=FALSE)
write.csv(rbind.fill(ados_g1[,c(1:2,grep("^a1$",names(ados_g1)):grep("^e3$",names(ados_g1)))],
          ados_g2[,c(1:2,grep("^a1$",names(ados_g2)):grep("^e3$",names(ados_g2)))],
          ados_g3[,c(1:2,grep("^a1$",names(ados_g3)):grep("^e3$",names(ados_g3)))],
          ados2_1[,c(1:2,grep("^a1$",names(ados2_1)):grep("^e3$",names(ados2_1)))],
          ados2_2[,c(1:2,grep("^a1$",names(ados2_2)):grep("^e3$",names(ados2_2)))],
          ados2_3[,c(1:2,grep("^a1$",names(ados2_3)):grep("^e3$",names(ados2_3)))]),
          "S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/ados_items.csv",row.names=F)
#         ados2_4[,c(1:2,grep("^a1$",names(ados2_4)):grep("^e3$",names(ados2_4)))]),

          
#cleaning up
rm(mod1_no_words_rrb_items,mod1_no_words_sa_items,mod1_some_words_rrb_items,mod1_some_words_sa_items,
   mod2_rrb_items,mod2_sa_items,mod3_rrb_items,mod3_sa_items,ados_columns,ados_sev_dom_tbl,ados_sev_tbl)
