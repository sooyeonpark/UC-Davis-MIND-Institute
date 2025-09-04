#pulling data tables from db
scared = sqlQuery(new_con,"select * from SCARED_Child;",stringsAsFactors=F)
scared_p = sqlQuery(new_con,"select * from SCARED_Parent",stringsAsFactors=F)

#id, visit into characters, removing single-entered rows and prefixes
scared = id_visit_changing_into_char(scared)
scared_p = id_visit_changing_into_char(scared_p)
scared_child_entry_flag = entry_flag(scared,'scared_child')
scared_parent_entry_flag = entry_flag(scared_p,'scared_parent')
scared = subset(scared,entry_status==2)
scared_p = subset(scared_p,entry_status==2)
if(!is.null(scared_child_entry_flag)){
  scared = rbind(scared,scared_child_entry_flag[,-ncol(scared_child_entry_flag)])
}
if(!is.null(scared_parent_entry_flag)){
  scared_p = rbind(scared_p,scared_parent_entry_flag[,-ncol(scared_parent_entry_flag)])
}
scared = removing_prefix(scared,"scrd_c_")
scared_p = removing_prefix(scared_p,"scrd_p_")

##Child Scoring
#calculate age
scared = fxage(scared,'id','date')

#converting negative values to NA's and counting NA values
for(j in which(names(scared)=='1'):which(names(scared)=='41')){
  scared[,j] = cbraw(scared[,j])
}

#calculating section scores and indicators
#list of items for each section
ps_items = paste0('',c(1,seq(6,18,by=3),19,22,seq(24,30,by=3),34,38))
ga_items = paste0('',c(5,seq(7,35,by=7),23,33,37))
sep_items = paste0('',c(4,8,13,16,20,25,29,31))
soc_items = paste0('',c(3,10,26,32,39:41))
sa_items = paste0('',c(2,11,17,36))
total_items = c(ps_items,ga_items,sep_items,soc_items,sa_items)

#missing data analysis
scared = count_missing_items(scared,'1','41')
scared = comment_missing_data(scared,list(ps_items,ga_items,sep_items,soc_items,sa_items),
                              list('panic/somatic','generalized_anxiety','separation_anxiety',
                                   'social_anxiety','school_avoidance'))

#summing the items
for(i in 1:nrow(scared)){
  scared$`panic/somatic_score`[i] = sum(scared[i,ps_items],na.rm=F)
  scared$`panic/somatic`[i] = ifelse(scared$`panic/somatic_score`[i]>6,"yes","no")
  scared$generalized_anxiety_score[i] = sum(scared[i,ga_items],na.rm=F)
  scared$generalized_anxiety[i] = ifelse(scared$generalized_anxiety_score[i]>8,"yes","no")
  scared$separation_anxiety_score[i] = sum(scared[i,sep_items],na.rm=F)
  scared$separation_anxiety[i] = ifelse(scared$separation_anxiety_score[i]>4,"yes","no")
  scared$social_anxiety_score[i] = sum(scared[i,soc_items],na.rm=F)
  scared$social_anxiety[i] = ifelse(scared$social_anxiety_score[i]>7,"yes","no")
  scared$school_avoidance_score[i] = sum(scared[i,sa_items],na.rm=F)
  scared$school_avoidance[i] = ifelse(scared$school_avoidance_score[i]>2,"yes","no")
  scared$total_score[i] = sum(scared[i,total_items],na.rm=F)
  scared$total[i] = ifelse(scared$total_score[i]>=30,"yes",
                           ifelse(scared$total_score<30 & scared$total_score>=25,"possible","no"))
}

#putting back prefix
scared = inserting_prefix_into_variables(scared,'scared_')

#orphaned/duplicate data
scared_orphaned_data = orphaned_data_consolidate(scared)
scared = orphaned_data_remove(scared)
scared_duplicate_data = duplicate_data_consolidate(scared,"scared_age")
scared = duplicate_data_remove(scared,"scared_age")

#outliers
scared_outliers = scared[,c(1:2,grep("_age$",names(scared)),grep("_score$",names(scared)))]
scared_outliers = outlier_list(scared_outliers)
scared$scared_outlier_list = scared_outliers$outlier_list
#scared_outlier_table = sqldf("select id,visit,outlier_list from scared where outlier_list != ''")
rm(scared_outliers)

##Parent Scoring
#calculate age
scared_p = fxage(scared_p,'id','date')

#converting negative values to NA's and counting NA values
for(j in which(names(scared_p)=='1'):which(names(scared_p)=='41')){
  scared_p[,j] = cbraw(scared_p[,j])
}

#missing data analysis
scared_p = count_missing_items(scared_p,'1','41')
scared_p = comment_missing_data(scared_p,list(ps_items,ga_items,sep_items,soc_items,sa_items),
                              list('panic/somatic','generalized_anxiety','separation_anxiety',
                                   'social_anxiety','school_avoidance'))

#summing the items
for(i in 1:nrow(scared_p)){
  scared_p$`panic/somatic_score`[i] = sum(scared_p[i,ps_items],na.rm=F)
  scared_p$`panic/somatic`[i] = ifelse(scared_p$`panic/somatic_score`[i]>6,"yes","no")
  scared_p$generalized_anxiety_score[i] = sum(scared_p[i,ga_items],na.rm=F)
  scared_p$generalized_anxiety[i] = ifelse(scared_p$generalized_anxiety_score[i]>8,"yes","no")
  scared_p$separation_anxiety_score[i] = sum(scared_p[i,sep_items],na.rm=F)
  scared_p$separation_anxiety[i] = ifelse(scared_p$separation_anxiety_score[i]>4,"yes","no")
  scared_p$social_anxiety_score[i] = sum(scared_p[i,soc_items],na.rm=F)
  scared_p$social_anxiety[i] = ifelse(scared_p$social_anxiety_score[i]>7,"yes","no")
  scared_p$school_avoidance_score[i] = sum(scared_p[i,sa_items],na.rm=F)
  scared_p$school_avoidance[i] = ifelse(scared_p$school_avoidance_score[i]>2,"yes","no")
  scared_p$total_score[i] = sum(scared_p[i,total_items],na.rm=F)
  scared_p$total[i] = ifelse(scared_p$total_score[i]>=30,"yes",
                           ifelse(scared_p$total_score<30 & scared_p$total_score>=25,"possible","no"))
}

#putting back prefix
scared_p = inserting_prefix_into_variables(scared_p,'scared_parent_')

#orphaned/duplicate data
scared_p_orphaned_data = orphaned_data_consolidate(scared_p)
scared_p = orphaned_data_remove(scared_p)
which(table(scared_p$id,scared_p$visit)>1)

#outliers
scared_p_outliers = scared_p[,c(1:2,grep("_age$",names(scared_p)),grep("_score$",names(scared_p)))]
scared_p_outliers = outlier_list(scared_p_outliers)
scared_p$scared_parent_outlier_list = scared_p_outliers$outlier_list
#scared_p_outlier_table = sqldf("select id,visit,outlier_list from scared_p where outlier_list != ''")
rm(scared_p_outliers)

#merging the child and parent data
scared_scored = scared[,c(1:2,grep("_age$",names(scared)),grep("missing",names(scared)),
                          grep("panic/somatic_score",names(scared)):ncol(scared))]
scared_p_scored = scared_p[,c(1:2,grep("resp$",names(scared_p)),grep("_age$",names(scared_p)),grep("missing",names(scared_p)),
                              grep("panic/somatic_score",names(scared_p)):ncol(scared_p))]

#archiving data
write.csv(scared_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/scared_scored.csv",row.names = F)
write.csv(scared[,c(1:2,grep("_[0-9]+$",names(scared)))],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/scared_items.csv",row.names = F)
write.csv(scared_p_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/scared_parent_scored.csv",row.names = F)
write.csv(scared_p[,c(1:2,grep("_[0-9]+$",names(scared_p)))],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/scared_parent_items.csv",row.names = F)

#cleaning up
rm(ps_items,ga_items,sep_items,soc_items,sa_items,total_items)
