# outlier_table = rbind(adi_outlier_table,ados_outlier_table,cbcl_young_outlier_table,
#                       cbcl_old_outlier_table,cbq_outlier_table,ccc_outlier_table,cdi_wg_outlier_table,
#                       cdi_ws_outlier_table,celf_outlier_table,cshq_outlier_table,das_outlier_table,
#                       edq_outlier_table,eowpvt3_outlier_table,eowpvt4_outlier_table,masc_outlier_table,
#                       masc_p_outlier_table,mullen_outlier_table,nih_outlier_table,ppvt_outlier_table,
#                       rbs_outlier_table,scared_outlier_table,scared_p_outlier_table,scq_outlier_table,
#                       srs_outlier_table,ssp_outlier_table,ssp2_outlier_table,sti_outlier_table,vabs_outlier_table)
# outlier_table = ddply(outlier_table,.(id,visit),summarize,outlier_list=paste0(outlier_list,collapse=" "))
# write.csv(outlier_table[grep("^[0-9]{6}-[0-9]{3}$",outlier_table$id),],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/Data audit/outlier_list.csv",row.names=F)

single_entry_flag_table = rbind(ai_child_entry_flag[,c("id","visit","measure")],gi_entry_flag[,c("id","visit","measure")],
                                pe_entry_flag[,c("id","visit","measure")],ts_entry_flag[,c("id","visit","measure")],
                                growth_measurement_entry_flag[,c("id","visit","measure")])
single_entry_flag_behav = rbind(adi_entry_flag[,c("id","visit","measure")],adis_entry_flag[,c("id","visit","measure")],ados_g1_entry_flag[,c("id","visit","measure")],
                                ados2_1_entry_flag[,c("id","visit","measure")],ados2_2_entry_flag[,c("id","visit","measure")],
                                ados2_3_entry_flag[,c("id","visit","measure")],cbcl_old_entry_flag[,c("id","visit","measure")],
                                cbq_entry_flag[,c("id","visit","measure")],ccc_entry_flag[,c("id","visit","measure")],
                                cdi_wg_entry_flag[,c("id","visit","measure")],cdi_ws_entry_flag[,c("id","visit","measure")],
                                celf_entry_flag[,c("id","visit","measure")],cshq_entry_flag[,c("id","visit","measure")],
                                das_ey_entry_flag[,c("id","visit","measure")],das_sa_entry_flag[,c("id","visit","measure")],
                                demograph_entry_flag[,c("id","visit","measure")],dsm_entry_flag[,c("id","visit","measure")],
                                dxcf_entry_flag[,c("id","visit","measure")],eowpvt3_entry_flag[,c("id","visit","measure")],
                                eowpvt4_entry_flag[,c("id","visit","measure")],gort_entry_flag[,c("id","visit","measure")],
                                harter_entry_flag[,c("id","visit","measure")],hpt_entry_flag[,c("id","visit","measure")],
                                masc_child_entry_flag[,c("id","visit","measure")],masc_parent_entry_flag[,c("id","visit","measure")],
                                mullen_entry_flag[,c("id","visit","measure")],pal_entry_flag[,c("id","visit","measure")],
                                ppvt3_entry_flag[,c("id","visit","measure")],ppvt4_entry_flag[,c("id","visit","measure")],
                                rbs_entry_flag[,c("id","visit","measure")],scared_child_entry_flag[,c("id","visit","measure")],
                                scared_parent_entry_flag[,c("id","visit","measure")],scq_entry_flag[,c("id","visit","measure")],
                                srs_entry_flag[,c("id","visit","measure")],ssp_entry_flag[,c("id","visit","measure")],
                                ssp2_entry_flag[,c("id","visit","measure")],sti_entry_flag[,c("id","visit","measure")],
                                tmcq_entry_flag[,c("id","visit","measure")],vabs_entry_flag[,c("id","visit","measure")])
single_entry_flag_table_staar = rbind(adis_entry_flag[,c("id","visit","measure")],ados2_3_entry_flag[,c("id","visit","measure")],
                                      cbcl_old_entry_flag[,c("id","visit","measure")],dsm_entry_flag[,c("id","visit","measure")],
                                      masc_parent_entry_flag[,c("id","visit","measure")],rbs_entry_flag[,c("id","visit","measure")],
                                      srs_entry_flag[,c("id","visit","measure")])

single_entry_flag_table = ddply(single_entry_flag_table,.(id,visit),summarize,flag_list=paste0(measure,collapse='; '))
single_entry_flag_behav = ddply(single_entry_flag_behav[grep("[0-9]{6}-[0-9]{3}",single_entry_flag_behav$id),],.(id,visit),summarize,flag_list=paste0(measure,collapse='; '))
single_entry_flag_table_staar = ddply(single_entry_flag_table_staar[grep("^[0-9]{4}$",single_entry_flag_table_staar$id),],.(id,visit),summarize,flag_list=paste0(measure,collapse = '; '))
write.csv(single_entry_flag_table,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/Data audit/single_entry_flag_app_cohorts.csv",row.names = F)
write.csv(single_entry_flag_table_staar,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/Data audit/single_entry_flag_staar.csv",row.names = F)
write.csv(char_lists_to_table(single_entry_flag_table[,"flag_list"],"; "),"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/Data audit/single_entry_freq.csv",row.names=F)
write.table(char_lists_to_table(single_entry_flag_table_staar[,"flag_list"],"; "),"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/Data audit/single_entry_freq.csv",row.names=F,col.names=F,sep=",",append=T)
write.csv(char_lists_to_table(single_entry_flag_behav[,"flag_list"],"; "),"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/Data audit/single_entry_freq_behav.csv",row.names=F)

orphaned_data_list = data.frame()
orphaned_data_list = combining_multiple_data(ados_orphaned_data,list(ai_orphaned_data,cshq_orphaned_data,dna_orphaned_data,dxcf_orphaned_data,dsm_orphaned_data,
                                                                     eowpvt4_orphaned_data,fps_orphaned_data,gort_orphaned_data,growth_measurement_orphaned_data,
                                                                     masc_p_orphaned_data,rbs_orphaned_data,scan_details_orphaned_data,scq_orphaned_data,
                                                                     srs_dad_orphaned_data,srs_mom_orphaned_data,srs_orphaned_data,ssp_orphaned_data,
                                                                     ssp2_orphaned_data,sti_orphaned_data,tcv_orphaned_data,tmcq_orphaned_data),
                                        'ados',list('autoimmune','cshq','dna_child','dxcf','dsm','eowpvt','fps','gort','growth measurement','masc_parent',
                                                    'rbs','scan_details','scq','srs_dad','srs_mom','srs_child','ssp','ssp2','sti','tcv','tmcq'))
orphaned_data_list = orphaned_data_list[-which(orphaned_data_list$id %in% app_cohort_subj$`subj id`),]
orphaned_data_checked = read.csv("S:/MIND/RESEARCH/APP/APP Database/7 - Data Auditing/orphaned_data_list_bhannotated.csv", stringsAsFactors=FALSE)
orphaned_data_list = orphaned_data_list[-which(orphaned_data_list$id %in% orphaned_data_checked$id),]
write.csv(orphaned_data_list,paste0("S:/MIND/RESEARCH/APP/APP Database/7 - Data Auditing/orphaned_data_list_",Sys.Date(),".csv"),row.names=F)
write.csv(char_lists_to_table(orphaned_data_list[,"measure_list"],','),"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/Data Audit/orphaned_data_freq.csv",row.names=F)

duplicate_data_list = data.frame()
duplicate_data_list = combining_multiple_data(scq_duplicate_data,list(cgi_duplicate_data,scan_details_duplicate_data),'scq',list('cgi','scan_details'))
write.csv(duplicate_data_list,paste0("S:/MIND/RESEARCH/APP/APP Database/7 - Data Auditing/duplicate_data_list_",Sys.Date(),".csv"),row.names=F)
write.csv(char_lists_to_table(duplicate_data_list[,"measure_list"],','),"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/Data Audit/duplicate_data_freq.csv",row.names=F)

#clean up
rm(orphaned_data_checked)
