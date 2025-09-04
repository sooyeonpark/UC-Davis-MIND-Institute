#importing table from ACCESS
abas = sqlQuery(con4,"select * from ABAS_3;",stringsAsFactors=F)

abas = id_visit_changing_into_char(abas)
abas = subset(abas,entry_status==2)
abas = removing_prefix(abas,"abas3_")

#calculating age
abas = fxage(abas,'id','date')

#list of items
com_items = paste0('communication_',1:24)
cu_items = paste0('community_',1:23)
fa_items = paste0('academics_',1:23)
hl_items = paste0('home_',1:25)
hs_items = paste0('health_',1:20)
ls_items = paste0('leisure_',1:20)
sc_items = paste0('selfcare_',1:25)
sd_items = paste0('selfdirection_',1:25)
soc_items = paste0('social_',1:26)

#missing data analysis
abas = count_missing_items(abas,'communication_1','social_26')
abas = comment_missing_data(abas,list(com_items,cu_items,fa_items,hl_items,hs_items,
                                      ls_items,sc_items,sd_items,soc_items),
                            list('communication','community','functional_academics',
                                 'home_living','health_safety','leisure','self-care',
                                 'self-direction','social'))

#obtaining raw total
abas = summing_items_per_row(abas,list(com_items,cu_items,fa_items,hl_items,hs_items,
                                       ls_items,sc_items,sd_items,soc_items),
                             list('communication_total','community_total','functional_academics_total',
                                  'home_living_total','health_safety_total','leisure_total','self-care_total',
                                  'self-direction_total','social_total'),F)

#importing & modifying norm tables
abas_standard = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/ABAS_5-21_standard_scores.csv", na.strings="", stringsAsFactors=F)
abas_scale = read.csv("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/norm tables/ABAS_5-21_scaled_scores.csv", na.strings="", stringsAsFactors=F)
abas_standard_revised = abas_standard[,1:2]
abas_standard_revised = modifying_vabs_norm_tables(abas_standard,abas_standard_revised)
abas_scale_revised = data.frame(abas_scale[,1])
names(abas_scale_revised) = "scaled_score"
abas_scale_revised = modifying_vabs_norm_tables(abas_scale,abas_scale_revised)
rm(abas_standard,abas_scale)

#obtaining scaled scores
for(i in 1:nrow(abas)){
  abas$communication_scaled[i] = ifelse(is.na(abas$communication_total[i]),NA,
                                        abas_scale_revised[which(abas$age[i]>=abas_scale_revised$age_months_bottom
                                                                  & abas$age[i]>=abas_scale_revised$age_months_top
                                                                  & abas$communication_total[i]>=abas_scale_revised$com_bottom
                                                                  & abas$communication_total[i]<=abas_scale_revised$com_top),"scaled_score"])
  abas$community_scaled[i] = ifelse(is.na(abas$community_total[i]),NA,
                                    abas_scale_revised[which(abas$age[i]>=abas_scale_revised$age_months_bottom
                                                              & abas$age[i]>=abas_scale_revised$age_months_top
                                                              & abas$community_total[i]>=abas_scale_revised$cu_bottom
                                                              & abas$community_total[i]<=abas_scale_revised$cu_top),"scaled_score"])
  abas$functional_academics_scaled[i] = ifelse(is.na(abas$functional_academics_total[i]),NA,
                                               abas_scale_revised[which(abas$age[i]>=abas_scale_revised$age_months_bottom
                                                                        & abas$age[i]>=abas_scale_revised$age_months_top
                                                                        & abas$functional_academics_total[i]>=abas_scale_revised$fa_bottom
                                                                        & abas$functional_academics_total[i]<=abas_scale_revised$fa_top),"scaled_score"])
  abas$home_living_scaled[i] = ifelse(is.na(abas$home_living_total[i]),NA,
                                      abas_scale_revised[which(abas$age[i]>=abas_scale_revised$age_months_bottom
                                                                & abas$age[i]>=abas_scale_revised$age_months_top
                                                                & abas$home_living_total[i]>=abas_scale_revised$hl_bottom
                                                                & abas$home_living_total[i]<=abas_scale_revised$hl_top),"scaled_score"])
  abas$health_safety_scaled[i] = ifelse(is.na(abas$health_safety_total[i]),NA,
                                        abas_scale_revised[which(abas$age[i]>=abas_scale_revised$age_months_bottom
                                                                  & abas$age[i]>=abas_scale_revised$age_months_top
                                                                  & abas$health_safety_total[i]>=abas_scale_revised$hs_bottom
                                                                  & abas$health_safety_total[i]<=abas_scale_revised$hs_top),"scaled_score"])
  abas$leisure_scaled[i] = ifelse(is.na(abas$leisure_total[i]),NA,
                                  abas_scale_revised[which(abas$age[i]>=abas_scale_revised$age_months_bottom
                                                            & abas$age[i]>=abas_scale_revised$age_months_top
                                                            & abas$leisure_total[i]>=abas_scale_revised$ls_bottom
                                                            & abas$leisure_total[i]<=abas_scale_revised$ls_top),"scaled_score"])
  abas$`self-care_scaled`[i] = ifelse(is.na(abas$`self-care_total`[i]),NA,
                                    abas_scale_revised[which(abas$age[i]>=abas_scale_revised$age_months_bottom
                                                              & abas$age[i]>=abas_scale_revised$age_months_top
                                                              & abas$`self-care_total`[i]>=abas_scale_revised$sc_bottom
                                                              & abas$`self-care_total`[i]<=abas_scale_revised$sc_top),"scaled_score"])
  abas$`self-direction_scaled`[i] = ifelse(is.na(abas$`self-direction_total`[i]),NA,
                                          abas_scale_revised[which(abas$age[i]>=abas_scale_revised$age_months_bottom
                                                                   & abas$age[i]>=abas_scale_revised$age_months_top
                                                                   & abas$`self-direction_total`[i]>=abas_scale_revised$sd_bottom
                                                                   & abas$`self-direction_total`[i]<=abas_scale_revised$sd_top),"scaled_score"])
  abas$social_scaled[i] = ifelse(is.na(abas$social_total[i]),NA,
                                 abas_scale_revised[which(abas$age[i]>=abas_scale_revised$age_months_bottom
                                                           & abas$age[i]>=abas_scale_revised$age_months_top
                                                           & abas$social_total[i]>=abas_scale_revised$soc_bottom
                                                           & abas$social_total[i]<=abas_scale_revised$soc_top),"scaled_score"])
}

#obtaining sum of scaled scores
conceptual_items = paste0(c('communication','functional_academics','self-direction'),'_scaled')
social_items = paste0(c('leisure','social'),'_scaled')
practical_items = paste0(c('community','home_living','health_safety','self-care'),'_scaled')
gac_items = c(conceptual_items,social_items,practical_items)
abas = summing_items_per_row(abas,list(conceptual_items,social_items,practical_items,gac_items),
                             list('conceptual_total','social_domain_total','practical_total','gac_total'),F)

#obtaining standard scores
for(i in 1:nrow(abas)){
  abas$conceptual_standard[i] = ifelse(is.na(abas$conceptual_total[i]),NA,
                                 abas_standard_revised[which(abas$age[i]>=abas_standard_revised$age_months_bottom
                                                             & abas$age[i]>=abas_standard_revised$age_months_top
                                                             & abas$conceptual_total[i]>=abas_standard_revised$con_bottom
                                                             & abas$conceptual_total[i]<=abas_standard_revised$con_top),"ss"])
  abas$social_standard[i] = ifelse(is.na(abas$social_domain_total[i]),NA,
                                   abas_standard_revised[which(abas$age[i]>=abas_standard_revised$age_months_bottom
                                                              & abas$age[i]>=abas_standard_revised$age_months_top
                                                              & abas$social_domain_total[i]>=abas_standard_revised$so_bottom
                                                              & abas$social_domain_total[i]<=abas_standard_revised$so_top),"ss"])
  abas$practical_standard[i] = ifelse(is.na(abas$practical_total[i]),NA,
                                      abas_standard_revised[which(abas$age[i]>=abas_standard_revised$age_months_bottom
                                                                  & abas$age[i]>=abas_standard_revised$age_months_top
                                                                  & abas$practical_total[i]>=abas_standard_revised$pr_bottom
                                                                  & abas$practical_total[i]<=abas_standard_revised$pr_top),"ss"])
  abas$gac_standard[i] = ifelse(is.na(abas$gac_total[i]),NA,
                                abas_standard_revised[which(abas$age[i]>=abas_standard_revised$age_months_bottom
                                                            & abas$age[i]>=abas_standard_revised$age_months_top
                                                            & abas$gac_total[i]>=abas_standard_revised$gac_bottom
                                                            & abas$gac_total[i]<=abas_standard_revised$gac_top),"ss"])
}

#obtaining percentile
for(i in 1:nrow(abas)){
  abas$conceptual_percentile[i] = ifelse(is.na(abas$conceptual_total[i]),NA,
                                         abas_standard_revised[which(abas$age[i]>=abas_standard_revised$age_months_bottom
                                                                     & abas$age[i]>=abas_standard_revised$age_months_top
                                                                     & abas$conceptual_standard[i] == abas_standard_revised$ss),"percentile"])
  abas$social_percentile[i] = ifelse(is.na(abas$social_domain_total[i]),NA,
                                     abas_standard_revised[which(abas$age[i]>=abas_standard_revised$age_months_bottom
                                                                 & abas$age[i]>=abas_standard_revised$age_months_top
                                                                 & abas$social_standard[i] == abas_standard_revised$ss),"percentile"])
  abas$practical_percentile[i] = ifelse(is.na(abas$practical_total[i]),NA,
                                        abas_standard_revised[which(abas$age[i]>=abas_standard_revised$age_months_bottom
                                                                    & abas$age[i]>=abas_standard_revised$age_months_top
                                                                    & abas$practical_standard[i] == abas_standard_revised$ss),"percentile"])
  abas$gac_percentile[i] = ifelse(is.na(abas$gac_total[i]),NA,
                                  abas_standard_revised[which(abas$age[i]>=abas_standard_revised$age_months_bottom
                                                              & abas$age[i]>=abas_standard_revised$age_months_top
                                                              & abas$gac_standard[i] == abas_standard_revised$ss),"percentile"])
}

#putting back prefix
abas = inserting_prefix_into_variables(abas,'abas_')

#orphaned/duplicate data
abas_orphaned_data = orphaned_data_consolidate(abas)
abas = orphaned_data_remove(abas)
abas_dupliacte_data = duplicate_data_consolidate(abas,'abas_age')
abas = duplicate_data_remove(abas,'abas_age')

#outliers
abas_outliers = abas[,c(1:2,grep("_age$",names(abas)),grep("_scaled$",names(abas)),grep("_standard",names(abas)))]
abas_outliers = outlier_list(abas_outliers)
abas$abas_outlier_list = abas_outliers$outlier_list
rm(abas_outliers)

#archiving the data
abas_scored = abas[,c(1:2,grep("_age$",names(abas)):grep("_comment$",names(abas)),
                      grep("_guessed$",names(abas)),grep("communication_total$",names(abas)):ncol(abas))]
write.csv(abas_scored,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived_staar/abas_scored.csv",row.names=F)
write.csv(abas[,c(1:2,9:228)],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived_staar/abas_items.csv",row.names=F)

#cleaning up
rm(com_items,cu_items,fa_items,hl_items,hs_items,ls_items,sc_items,sd_items,soc_items,
   abas_scale_revised,abas_standard_revised,conceptual_items,social_items,practical_items,
   gac_items)
