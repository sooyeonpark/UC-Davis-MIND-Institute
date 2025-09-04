#pulling the tables
#BA
fs_ba = sqlQuery(mri_con,"select * from tbl_FS_BA",stringsAsFactors=F)
#entorhinal_exvivo
fs_entex = sqlQuery(mri_con,"select * from tbl_FS_entorhinal_exvivo",stringsAsFactors=F)
#LR
fs_area = sqlQuery(mri_con,"select * from tbl_FS_LR_area",stringsAsFactors=F)
fs_curvind = sqlQuery(mri_con,"select * from tbl_FS_LR_curvind",stringsAsFactors=F)
fs_foldind = sqlQuery(mri_con,"select * from tbl_FS_LR_foldind",stringsAsFactors=F)
fs_gauscurv = sqlQuery(mri_con,"select * from tbl_FS_LR_gauscurv",stringsAsFactors=F)
fs_meancurv = sqlQuery(mri_con,"select * from tbl_FS_LR_meancurv",stringsAsFactors=F)
fs_thick = sqlQuery(mri_con,"select * from tbl_FS_LR_thickness",stringsAsFactors=F)
fs_thick_std = sqlQuery(mri_con,"select * from tbl_FS_LR_thicknessstd",stringsAsFactors=F)
fs_vol = sqlQuery(mri_con,"select * from tbl_FS_LR_volume",stringsAsFactors=F)
#WMparc
fs_mean_wmp = sqlQuery(mri_con,"select * from tbl_FS_mean_WMparc",stringsAsFactors=F)
fs_vol_wmp = sqlQuery(mri_con,"select * from tbl_FS_volume_WMparc",stringsAsFactors=F)

#lowering the variable names
names(fs_ba) = tolower(names(fs_ba))
names(fs_entex) = tolower(names(fs_entex))
names(fs_area) = tolower(names(fs_area))
names(fs_curvind) = tolower(names(fs_curvind))
names(fs_foldind) = tolower(names(fs_foldind))
names(fs_gauscurv) = tolower(names(fs_gauscurv))
names(fs_meancurv) = tolower(names(fs_meancurv))
names(fs_thick) = tolower(names(fs_thick))
names(fs_thick_std) = tolower(names(fs_thick_std))
names(fs_vol) = tolower(names(fs_vol))
names(fs_mean_wmp) = tolower(names(fs_mean_wmp))
names(fs_vol_wmp) = tolower(names(fs_vol_wmp))

#id and visit into characters
fs_ba = id_visit_changing_into_char(fs_ba)
fs_entex = id_visit_changing_into_char(fs_entex)
fs_area = id_visit_changing_into_char(fs_area)
fs_curvind = id_visit_changing_into_char(fs_curvind)
fs_foldind = id_visit_changing_into_char(fs_foldind)
fs_gauscurv = id_visit_changing_into_char(fs_gauscurv)
fs_meancurv = id_visit_changing_into_char(fs_meancurv)
fs_thick = id_visit_changing_into_char(fs_thick)
fs_thick_std = id_visit_changing_into_char(fs_thick_std)
fs_vol = id_visit_changing_into_char(fs_vol)
fs_mean_wmp = id_visit_changing_into_char(fs_mean_wmp)
fs_vol_wmp = id_visit_changing_into_char(fs_vol_wmp)

#putting prefixes
fs_ba = inserting_prefix_into_variables(fs_ba,"fs_")
fs_entex = inserting_prefix_into_variables(fs_entex,"fs_")
fs_area = inserting_prefix_into_variables(fs_area,"fs_")
fs_curvind = inserting_prefix_into_variables(fs_curvind,"fs_")
fs_foldind = inserting_prefix_into_variables(fs_foldind,"fs_")
fs_gauscurv = inserting_prefix_into_variables(fs_gauscurv,"fs_")
fs_meancurv = inserting_prefix_into_variables(fs_meancurv,"fs_")
fs_thick = inserting_prefix_into_variables(fs_thick,"fs_")
fs_thick_std = inserting_prefix_into_variables(fs_thick_std,"fs_")
fs_vol = inserting_prefix_into_variables(fs_vol,"fs_")
fs_mean_wmp = inserting_prefix_into_variables(fs_mean_wmp,"fs_")
fs_vol_wmp = inserting_prefix_into_variables(fs_vol_wmp,"fs_")

#removing non-primary data
fs_ba = orphaned_data_remove(fs_ba)
fs_entex = orphaned_data_remove(fs_entex)
fs_area = orphaned_data_remove(fs_area)
fs_curvind = orphaned_data_remove(fs_curvind)
fs_foldind = orphaned_data_remove(fs_foldind)
fs_gauscurv = orphaned_data_remove(fs_gauscurv)
fs_meancurv = orphaned_data_remove(fs_meancurv)
fs_thick = orphaned_data_remove(fs_thick)
fs_thick_std = orphaned_data_remove(fs_thick_std)
fs_vol = orphaned_data_remove(fs_vol)
fs_mean_wmp = orphaned_data_remove(fs_mean_wmp)
fs_vol_wmp = orphaned_data_remove(fs_vol_wmp)

#quick check for duplicate data
which(table(fs_ba$id,fs_ba$visit)>1)
which(table(fs_entex$id,fs_entex$visit)>1)
which(table(fs_area$id,fs_area$visit)>1)
which(table(fs_curvind$id,fs_curvind$visit)>1)
which(table(fs_foldind$id,fs_foldind$visit)>1)
which(table(fs_gauscurv$id,fs_gauscurv$visit)>1)
which(table(fs_meancurv$id,fs_meancurv$visit)>1)
which(table(fs_thick$id,fs_thick$visit)>1)
which(table(fs_thick_std$id,fs_thick_std$visit)>1)
which(table(fs_vol$id,fs_vol$visit)>1)
which(table(fs_mean_wmp$id,fs_mean_wmp$visit)>1)
which(table(fs_vol_wmp$id,fs_vol_wmp$visit)>1)
fs_vol_wmp$age = fs_vol_wmp$visit
fs_vol_wmp_duplicate_data = duplicate_data_consolidate(fs_vol_wmp,"age") #duplicate rows with same values
fs_vol_wmp = unique(fs_vol_wmp)
fs_vol_wmp$age=NULL

#outliers?

#archiving
write.csv(fs_ba,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/fs_ba_processed.csv",row.names = F)
write.csv(fs_entex,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/fs_entorhinal_exvivo_processed.csv",row.names = F)
write.csv(fs_area,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/fs_area_processed.csv",row.names = F)
write.csv(fs_curvind,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/fs_curvind_processed.csv",row.names = F)
write.csv(fs_foldind,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/fs_foldind_processed.csv",row.names = F)
write.csv(fs_gauscurv,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/fs_gauscurv_processed.csv",row.names = F)
write.csv(fs_meancurv,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/fs_meancurv_processed.csv",row.names = F)
write.csv(fs_thick,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/fs_thick_processed.csv",row.names = F)
write.csv(fs_thick_std,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/fs_thick_std_processed.csv",row.names = F)
write.csv(fs_vol,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/fs_vol_processed.csv",row.names = F)
write.csv(fs_mean_wmp,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/fs_mean_wmp_processed.csv",row.names = F)
write.csv(fs_vol_wmp,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/fs_vol_wmp_processed.csv",row.names = F)
