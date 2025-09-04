das_scored = rbind.fill(das_ey_scored,das_sa_scored)

#orphaned/duplicate data
das_orphaned_data = orphaned_data_consolidate(das_scored)
das_scored = orphaned_data_remove(das_scored)
das_duplicate_data = duplicate_data_consolidate(das_scored,"das_age")
das_scored = duplicate_data_remove(das_scored,"das_age")

#archiving data
das_scored = das_scored[,c(1:6,grep("_raw$",names(das_scored)),grep("_as$",names(das_scored)),
                           grep("_ae$",names(das_scored)),grep("_t$",names(das_scored)),grep("_ss$",names(das_scored)),
                           grep("outlier",names(das_scored)):grep("version",names(das_scored)))]
write.csv(das_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/das_scored.csv",row.names = F)

#consolidating outlier tables
#das_outlier_table = rbind(das_ey_outlier_table,das_sa_outlier_table)

#cleaning up
rm(das_as_ey,das_as_sa,das_ae,das_ae_ey,das_ae_sa,das_age,das_ss,das_ss_ey,das_subt,das_subt_ey,
   das_subt_sa,das_as,das_ss_sa,das_ss_upper,das_raw_score_vars)
