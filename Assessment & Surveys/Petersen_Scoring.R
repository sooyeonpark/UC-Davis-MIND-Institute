petersen_male = sqlFetch(con4,"Petersen Male",stringsAsFactors=F)
petersen_female = sqlFetch(con4,"Petersen Female",stringsAsFactors=F)
petersen = rbind.fill(petersen_male,petersen_female)
petersen = id_visit_changing_into_char(petersen)
petersen = subset(petersen,entry_status==2)
petersen = removing_prefix(petersen,'petersen_')

petersen$date = ifelse(is.na(petersen$m_date),as.character(petersen$f_date),as.character(petersen$m_date))
petersen = fxage(petersen,'id','date')

non_mens_items = c(paste0('m_',1:5),paste0('f_',1:4))
for(j in non_mens_items){
  petersen[,j] = ifelse(grepl("Not",petersen[,j]),1,
                        ifelse(grepl("bare",petersen[,j]),2,
                               ifelse(grepl("definite",petersen[,j]),3,
                                      ifelse(grepl("Comp",petersen[,j]),4,NA))))
}
petersen$f_5 = ifelse(grepl("y",petersen$f_5),4,1)

#missing data analysis
petersen$missing_items_count = 0
for(i in 1:nrow(petersen)){
  petersen$missing_items_count[i] = ifelse(!is.na(petersen$m_1[i]),
                                           length(which(is.na(petersen[i,grep("^m_",names(petersen))]))),
                                           length(which(is.na(petersen[i,grep("^f_",names(petersen))]))))
}

#summing up to score
petersen$total = NA
for(i in 1:nrow(petersen)){
  petersen$missing_items_count[i] = ifelse(!is.na(petersen$m_1[i]),
                                           length(which(is.na(petersen[i,grep("^m_",names(petersen))]))),
                                           length(which(is.na(petersen[i,grep("^f_",names(petersen))]))))
}

#putting back prefix
petersen = inserting_prefix_into_variables(petersen,"petersen_")

#orphaned/duplicate data
petersen_orphaned_data = orphaned_data_consolidate(petersen)
petersen = orphaned_data_remove(petersen)
petersen_duplicate_data = duplicate_data_consolidate(petersen,'petersen_age')
petersen = duplicate_data_remove(petersen,'petersen_age')

#extracting relevant columns and adding prefixes
petersen_scored = petersen[,c(1:2,grep("age$",names(petersen)),
                              grep("missing",names(petersen)),grep("_total$",names(petersen)))]

#obtaining outliers
petersen_scored = outlier_list(petersen_scored)
names(petersen_scored)[ncol(petersen_scored)] = "petersen_outlier_list"

#archiving data
write.csv(petersen_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived_staar/petersen_scored.csv",row.names=F)
write.csv(petersen[,c(1:2,grep("m_1",names(petersen)):grep("m_5",names(petersen)),
                      grep("f_1",names(petersen)):grep("f_6",names(petersen)))],
          "S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived_staar/petersen_items.csv",row.names=F)

#clean up
rm(non_mens_items,petersen_male,petersen_female)
