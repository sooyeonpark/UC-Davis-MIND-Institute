dm_norm_boys_all = subset(dm_status,gender=="Male" & app_diagnosis == "TD")
dm_norm_boys_all[which(dm_norm_boys_all$id == "108243-300"),"tcv_c/height"] = 968.588/37.0
dm_norm_girls_all = subset(dm_status,gender=="Female" & app_diagnosis=="TD")
dm_norm_girls_all[which(dm_norm_girls_all$id=='112000-100'),'tcv_c/height'] = 1046.572/38.0
dm_norm_girls_all[which(dm_norm_girls_all$id=='109012-100'),'tcv_c/height'] = 945.52/38.5
# scan_details_time1 = subset(scan_details,visit=="1" & status=="Success" & id %in% subj[subj$app_diagnosis=="TD","subj_id"])
# rm(scan_details_time1)

##comparing with all current TD participants
#male
mean(dm_norm_boys_all$`tcv_c/height`,na.rm=T) #29.45377
#current norm for 50 participants: 29.57071
sd(dm_norm_boys_all$`tcv_c/height`,na.rm=T) #2.42488
#current norm for 50 participants: 2.339164
t.test(dm_norm_boys$`tcv_c/height`,dm_norm_boys_all$`tcv_c/height`,alternative = "two.sided")

#female
mean(dm_norm_girls_all$`tcv_c/height`) #27.49207
#current norm for 50 participants: 27.55428
sd(dm_norm_girls_all$`tcv_c/height`) #2.467413
#current norm for 50 participants: 2.421489
t.test(dm_norm_girls$`tcv_c/height`,dm_norm_girls_all$`tcv_c/height`,alternative = "two.sided")

#comparing with random 50 samples
t.test(dm_norm_boys$`tcv_c/height`,sample(dm_norm_boys_all$`tcv_c/height`,50),alternative = "two.sided")
t.test(dm_norm_girls$`tcv_c/height`,sample(dm_norm_girls_all$`tcv_c/height`,50),alternative = "two.sided")

#okay, so the mean value itself stays constant, but what about each z-scores and dm-status changes?
#let's find out first with using all possible td participants
#male
dm_norm_boys_all$z_score_new = round((dm_norm_boys_all$`tcv_c/height`-mean(dm_norm_boys_all$`tcv_c/height`,na.rm=T))/sd(dm_norm_boys_all$`tcv_c/height`,na.rm=T),3)
dm_norm_boys_all$dm_new = ifelse(dm_norm_boys_all$z_score_new>=1.5,"DM",ifelse(dm_norm_boys_all$z_score_new<=-1.5,"neg1.5","N"))
dm_norm_boys_all[which(dm_norm_boys_all$id == "108243-300"),"dm"]= dm_norm_boys_all[which(dm_norm_boys_all$id == "108243-300"),"dm_new"]
dm_norm_boys_all[which(dm_norm_boys_all$dm != dm_norm_boys_all$dm_new),] #two cases

#female
dm_norm_girls_all$z_score_new = round((dm_norm_girls_all$`tcv_c/height`-mean(dm_norm_girls_all$`tcv_c/height`,na.rm=T))/sd(dm_norm_girls_all$`tcv_c/height`,na.rm=T),3)
dm_norm_girls_all$dm_new = ifelse(dm_norm_girls_all$z_score_new>=1.5,"DM",ifelse(dm_norm_girls_all$z_score_new<=-1.5,"neg1.5","N"))
dm_norm_girls_all[which(dm_norm_girls_all$id=='112000-100'),'dm'] = dm_norm_girls_all[which(dm_norm_girls_all$id=='112000-100'),'dm_new']
dm_norm_girls_all[which(dm_norm_girls_all$id=='109012-100'),'dm'] = dm_norm_girls_all[which(dm_norm_girls_all$id=='109012-100'),'dm_new']
dm_norm_girls_all[which(dm_norm_girls_all$dm != dm_norm_girls_all$dm_new),] #no case!

#how about with a random sample of 50
#male
dm_discrepancy_boys = data.frame()
for(i in 1:50){
  boys_sample = subset(dm_norm_boys_all,id %in% sample(dm_norm_boys_all$id,50))
  boys_sample$z_score_new = round((boys_sample$`tcv_c/height`-mean(boys_sample$`tcv_c/height`,na.rm=T))/sd(boys_sample$`tcv_c/height`,na.rm=T),3)
  boys_sample$dm_new = ifelse(boys_sample$z_score_new>=1.5,"DM",ifelse(boys_sample$z_score_new<=-1.5,"neg1.5","N"))
  print(boys_sample[which(boys_sample$dm != boys_sample$dm_new),c("id","dm","dm_new")])
  dm_discrepancy_boys = rbind(dm_discrepancy_boys,length(which(boys_sample$dm != boys_sample$dm_new)))
}

#female
dm_discrepancy_girls = data.frame()
for(i in 1:50){
  girls_sample = subset(dm_norm_girls_all,id %in% sample(dm_norm_girls_all$id,50))
  girls_sample$z_score_new = round((girls_sample$`tcv_c/height`-mean(girls_sample$`tcv_c/height`,na.rm=T))/sd(girls_sample$`tcv_c/height`,na.rm=T),3)
  girls_sample$dm_new = ifelse(girls_sample$z_score_new>=1.5,"DM",ifelse(girls_sample$z_score_new<=-1.5,"neg1.5","N"))
  print(girls_sample[which(girls_sample$dm != girls_sample$dm_new),c("id","dm","dm_new")])
  dm_discrepancy_girls = rbind(dm_discrepancy_girls,length(which(girls_sample$dm != girls_sample$dm_new)))
}
#108370-100*, 109531-100, 112600-100 -> id's that change dm values depending on samples
#108370-100 & 109531-100 from N to neg1.5 and 112600-100 from N to DM

td_boys = subj[which(subj$gender=="Male" & subj$app_diagnosis=="TD"),c(1,5)]
names(td_boys)[1]="id"
td_boys = merge(td_boys,gm_processed[which(gm_processed$visit=="1"),c(1,6)],all.x=T)
td_boys = merge(td_boys,tcv[which(tcv$visit=="1"),c(1,3)],all.x=T)
td_girls = data.frame(subj[which(subj$gender=="Female" & subj$app_diagnosis=="TD"),c(1,5)])
names(td_girls)[1] = "id"
td_girls = merge(td_girls,gm_processed[which(gm_processed$visit=="1"),c(1,6)],all.x=T)
td_girls = merge(td_girls,tcv[which(tcv$visit=="1"),c(1,3)],all.x=T)

rm(td_boys,td_girls)
