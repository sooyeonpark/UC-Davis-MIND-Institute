ai_summary = sqlFetch(med_con,'Autoimmune_part1_Summary',stringsAsFactors=F)
ai_immed_fam = sqlFetch(med_con,'Autoimmune_part2_Child-MTH-FAT-Sib',stringsAsFactors=F)
ai_rel = sqlFetch(med_con,'Autoimmune_part3_Aunts-Uncles-Grandparents',stringsAsFactors=F)

#id, visit into characters and flagging single_entered rows older than 3 months -> no "date collected" for other tables
ai_summary = id_visit_changing_into_char(ai_summary)
ai_summary_entry_flag = entry_flag(ai_summary,'autoimmune')
ai_immed_fam = id_visit_changing_into_char(ai_immed_fam)
ai_rel = id_visit_changing_into_char(ai_rel)

#removing single entries and prefixes for now
ai_summary = subset(ai_summary,entry_status==2)
ai_immed_fam = subset(ai_immed_fam,entry_status==2)
ai_rel = subset(ai_rel,entry_status==2)
ai_summary = removing_prefix(ai_summary,'autoimmune_')

#calculating age for ai_child
ai_summary = fxage(ai_summary,'id','date')

#putting back prefix
ai_summary = inserting_prefix_into_variables(ai_summary,'autoimmune_')
ai_immed_fam = inserting_prefix_into_variables(ai_immed_fam,'ai_')
ai_rel = inserting_prefix_into_variables(ai_rel,'ai_')

#orphaned_duplicated data
ai_orphaned_data = orphaned_data_consolidate(ai_summary)
ai_summary = orphaned_data_remove(ai_summary)
ai_duplicate_data = duplicate_data_consolidate(ai_summary,'autoimmune_age')
ai_summary = duplicate_data_remove(ai_summary,'autoimmune_age')
ai_immed_fam = orphaned_data_remove(ai_immed_fam)
ai_rel = orphaned_data_remove(ai_rel)

#combining immed_fam and rel tables
# ai_combined = merge(ai_immed_fam[,c(1:2,5:ncol(ai_immed_fam))],ai_rel[,c(1:2,5:ncol(ai_rel))],all.x=T)

#extracting necessary columns & archiving the data
ai_summary_processed = ai_summary[,c(1:2,grep("_age$",names(ai_summary)),
                                     grep("child_no_conditions",names(ai_summary)):grep("other_list",names(ai_summary)))]
write.csv(ai_summary_processed,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/autoimmune_processed.csv",row.names=F)
write.csv(ai_immed_fam[,-(3:4)],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/autoimmune_details_immediate_family.csv",row.names=F)
write.csv(ai_rel[,-(3:4)],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/autoimmune_details_extended_family.csv",row.names=F)

# #extracting columns without all NA values
# ai_child_nna = autoimmune_extract(ai_child,7)
# ai_fat_nna = autoimmune_extract(ai_fat,6)
# ai_mth_nna = autoimmune_extract(ai_mth,6)
# ai_mrel_nna = autoimmune_extract(ai_mrel,6)
# ai_prel_nna = autoimmune_extract(ai_prel,6)
# ai_mgp_nna = autoimmune_extract(ai_mgp,6)
# ai_pgp_nna = autoimmune_extract(ai_pgp,6)
# ai_other_nna = autoimmune_extract(ai_other,6)
# 
# #merging the list columns to the original table
# ai_child = sqldf("select t1.*,t2.list from ai_child t1 left join ai_child_nna t2 on t1.id=t2.id and t1.visit=t2.visit")
# ai_fat = sqldf("select t1.*,t2.list from ai_fat t1 left join ai_fat_nna t2 on t1.id=t2.id and t1.visit=t2.visit")
# ai_mth = sqldf("select t1.*,t2.list from ai_mth t1 left join ai_mth_nna t2 on t1.id=t2.id and t1.visit=t2.visit")
# ai_mrel = sqldf("select t1.*,t2.list from ai_mrel t1 left join ai_mrel_nna t2 on t1.id=t2.id and t1.visit=t2.visit")
# ai_prel = sqldf("select t1.*,t2.list from ai_prel t1 left join ai_prel_nna t2 on t1.id=t2.id and t1.visit=t2.visit")
# ai_mgp = sqldf("select t1.*,t2.list from ai_mgp t1 left join ai_mgp_nna t2 on t1.id=t2.id and t1.visit=t2.visit")
# ai_pgp = sqldf("select t1.*,t2.list from ai_pgp t1 left join ai_pgp_nna t2 on t1.id=t2.id and t1.visit=t2.visit")
# ai_other = sqldf("select t1.*,t2.list from ai_other t1 left join ai_other_nna t2 on t1.id=t2.id and t1.visit=t2.visit")
# 
# #count the number of risk factors
# ai_child = autoimmune_count(ai_child,c(seq(7,35,2),seq(38,111,2)))
# ai_fat = autoimmune_count(ai_fat,c(seq(6,34,2),seq(37,110,2)))
# ai_mth = autoimmune_count(ai_mth,c(seq(6,34,2),seq(37,110,2)))
# ai_mrel = autoimmune_count(ai_mrel,c(seq(6,48,3),seq(52,161,3)))
# ai_prel = autoimmune_count(ai_prel,c(seq(6,48,3),seq(52,161,3)))
# ai_mgp = autoimmune_count(ai_mgp,c(seq(6,48,3),seq(52,161,3)))
# ai_pgp = autoimmune_count(ai_pgp,c(seq(6,48,3),seq(52,161,3)))
# ai_other = autoimmune_count(ai_other,c(seq(6,48,3),seq(52,161,3)))
# 
# #putting prefixes in
# ai_child = inserting_prefix_into_variables(ai_child,'autoimmune_child_')
# ai_mth = inserting_prefix_into_variables(ai_mth,'autoimmune_mth_')
# ai_fat = inserting_prefix_into_variables(ai_fat,'autoimmune_fat_')
# ai_mrel = inserting_prefix_into_variables(ai_mrel,'autoimmune_mrel_')
# ai_prel = inserting_prefix_into_variables(ai_prel,'autoimmune_prel_')
# ai_mgp = inserting_prefix_into_variables(ai_mgp,'autoimmune_mgp_')
# ai_pgp = inserting_prefix_into_variables(ai_pgp,'autoimmune_pgp_')
# ai_other = inserting_prefix_into_variables(ai_other,'autoimmune_other_')
# 
# #merging the table
# ai_combined = ai_child[,c(1:2,112:114)]
# ai_combined = merge(ai_combined,ai_mth[,c(1:2,grep("mth_list$",names(ai_mth)),grep("mth_count$",names(ai_mth)))],all=T)
# ai_combined = merge(ai_combined,ai_fat[,c(1:2,grep("fat_list$",names(ai_fat)),grep("fat_count$",names(ai_fat)))],all=T)
# ai_combined = merge(ai_combined,ai_mrel[,c(1:2,grep("mrel_list$",names(ai_mrel)),grep("mrel_count$",names(ai_mrel)))],all=T)
# ai_combined = merge(ai_combined,ai_prel[,c(1:2,grep("prel_list$",names(ai_prel)),grep("prel_count$",names(ai_prel)))],all=T)
# ai_combined = merge(ai_combined,ai_mgp[,c(1:2,grep("mgp_list$",names(ai_mgp)),grep("mgp_count$",names(ai_mgp)))],all=T)
# ai_combined = merge(ai_combined,ai_pgp[,c(1:2,grep("pgp_list$",names(ai_pgp)),grep("pgp_count$",names(ai_pgp)))],all=T)
# # sqldf("select t1.*,t2.autoimmune_mth_list,t2.autoimmune_mth_count from
# #                     ai_combined t1 outer join ai_mth t2 on t1.id=t2.id and t1.visit=t2.visit")
# # ai_combined = sqldf("select t1.*,t2.autoimmune_fat_list,t2.autoimmune_fat_count from
# #                     ai_combined t1 outer join ai_fat t2 on t1.id=t2.id and t1.visit=t2.visit")
# # ai_combined = sqldf("select t1.*,t2.autoimmune_mrel_list,t2.autoimmune_mrel_count from
# #                     ai_combined t1 outer join ai_mrel t2 on t1.id=t2.id and t1.visit=t2.visit")
# # ai_combined = sqldf("select t1.*,t2.autoimmune_prel_list,t2.autoimmune_prel_count from
# #                     ai_combined t1 outer join ai_prel t2 on t1.id=t2.id and t1.visit=t2.visit")
# # ai_combined = sqldf("select t1.*,t2.autoimmune_mgp_list,t2.autoimmune_mgp_count from
# #                     ai_combined t1 outer join ai_mgp t2 on t1.id=t2.id and t1.visit=t2.visit")
# # ai_combined = sqldf("select t1.*,t2.autoimmune_pgp_list,t2.autoimmune_pgp_count from
# #                     ai_combined t1 outer join ai_pgp t2 on t1.id=t2.id and t1.visit=t2.visit")
# 
# #NA entries to ""
# for(j in grep("child_list",names(ai_combined)):ncol(ai_combined)){
#   if(class(ai_combined[,j])=="character"){
#     ai_combined[,j] = ifelse(is.na(ai_combined[,j]),"",ai_combined[,j])
#   }
#   if(class(ai_combined[,j])=="numeric"){
#     for(i in 1:nrow(ai_combined)){
#       ai_combined[i,(j-1)] = ifelse(is.na(ai_combined[i,j]),NA,ai_combined[i,(j-1)])
#     }
#   }
# }
# 
# #orphaned/duplicate data
# ai_orphaned_data = orphaned_data_consolidate(ai_combined)
# ai_combined = orphaned_data_remove(ai_combined)
# ai_duplicate_data = duplicate_data_consolidate(ai_combined,"autoimmune_child_age")
# ai_combined = duplicate_data_remove(ai_combined,"autoimmune_child_age")
# 
# #archiving the data
# write.csv(ai_combined,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/autoimmune_processed.csv",row.names=F)
# 
# #cleaning up
# rm(ai_child_nna,ai_fat_nna,ai_mth_nna,ai_mrel_nna,ai_prel_nna,ai_mgp_nna,ai_pgp_nna,ai_other_nna)
