# ics setting -------> ASD experience and poverty level -------------> EBP use primary

hist(cptmod$ics_setting_total) # <--- respondent level
hist(cptmod$exp_asd) # <--- respondent level - MODERATOR
hist(cptmod$socio_disadvantaged_percentage) # <---- SELPA level - MODERATOR
hist(cptmod$ebp_use_primary_quality_outcome) # <---- respondent level


# identify duplicates
table(duplicated(cptmod$subjid))

fit=lm(ebp_use_primary_quality_outcome ~ ics_setting_total, data=cptmod)
summary(fit)
cor.test(cptmod$ebp_use_primary_quality_outcome, cptmod$ics_setting_total)
plot(cptmod$ebp_use_primary_quality_outcome, cptmod$ics_setting_total)

fit = lm(ebp_use_primary_quality_outcome ~ exp_asd*ics_total, data=cptmod_use)
fit2 = lm(ebp_use_primary_quality_outcome ~ exp_asd*ics_setting_total*socio_disadvantaged_percentage, data=cptmod)
summary(fit)
anova(fit)

#model fitting & plotting
library(emmeans)
emmeans(fit,~exp_asd*ics_setting_total)
emmeans(fit,~ics_setting_total*exp_asd*socio_disadvantaged_percentage)
joint_tests(fit,by="ics_setting_total")
joint_tests(fit,by="exp_asd")
joint_tests(fit,by="socio_disadvantaged_percentage")
contrast(emmeans(fit,~ics_setting_total*exp_asd),"pairwise")
contrast(emmeans(fit,~ics_setting_total*exp_asd*socio_disadvantaged_percentage))
em_fit = emmeans(fit,~exp_asd*ics_total,at=list(ics_total=c(10,50,90),exp_asd=c(0,1,2,3)))

plot(em_fit,xlab="EBP Primary Outcome")
emmip(em_fit,~ics_total|exp_asd)
emmip(em_fit,exp_asd~ics_total,xlab="ICS Total",ylab="EBP Outcome",tlab="ASD Exp")

length(which(!is.na(cptmod$ics_total) & !is.na(cptmod$exp_asd) & !is.na(cptmod$ebp_use_primary_quality_outcome)))
#v data set used for ics (total, subscales) - asd exp - ebp outcome pairs
ma_no_selpa = read_excel("Moderation Analysis/no_selpa_list PS_1.13.22.xlsx")
ma_no_selpa$district_final = ma_no_selpa$District
ma_no_selpa$selpa_final = ma_no_selpa$SELPA
cptmod_selpa_filled = merge(ma_no_selpa,cptmod[,c(1,13:38)],all.x=T)
# summary(lmer(ebp_use_primary_quality_outcome ~ 1 + ics_setting_total*exp_asd + (1|selpa_final) , data=cptmod_use)) #maybe this model to go with
summary(lmer(ebp_use_primary_quality_outcome ~ 1 + (1|district_final) + (1|selpa_final), data=cptmod_use))
# summary(lmer(ebp_use_primary_quality_outcome ~ 1 + (1|selpa_final:district_final), data=cptmod_use))
# summary(lmer(ebp_use_primary_quality_outcome ~ 1 + (1|selpa_final:district_final)+(1|selpa_final), data=cptmod_use))

#variance (district variance/total variance) -> very small
.0238/(.0238+.01152+.70074) #from model in line 52 for district
.01152/(.0238+.01152+.70074) #from model in line 52 for selpa
.0328/(.0328+.7028) # from model in line 53 for nested effect

#writing a report
length(which(!is.na(cptmod_backup$ics_total) & !is.na(cptmod_backup$exp_asd) & !is.na(cptmod_backup$ebp_use_primary_quality_outcome)))
length(which(is.na(cptmod$selpa_final) & is.na(cptmod$district_final)))
table(cptmod_use$gender)
table(cptmod_use$role)
table(cptmod_use$race)
table(cptmod_use$age)
length(table(cptmod_use$selpa_final))
range(table(cptmod_use$selpa_final))
length(table(cptmod_use$district_final))
range(table(cptmod_use$district_final))
range(cptmod_use$ics_total)
mean(cptmod_use$ics_total)
sd(cptmod_use$ics_total)
mean(cptmod_use$ics_focus_ebp)
mean(cptmod_use$ics_edu_support)
mean(cptmod_use$ics_recog_ebp)
mean(cptmod_use$ics_rewards_ebp)
mean(cptmod_use$ics_selection_ebp)
mean(cptmod_use$ics_selection_open)
mean(cptmod_use$ics_exist_sup_ebp)
mean(cptmod_use$ics_exist_use_data)
sd(cptmod_use$ics_focus_ebp)
sd(cptmod_use$ics_edu_support)
sd(cptmod_use$ics_recog_ebp)
sd(cptmod_use$ics_rewards_ebp)
sd(cptmod_use$ics_selection_ebp)
sd(cptmod_use$ics_selection_open)
sd(cptmod_use$ics_exist_sup_ebp)
sd(cptmod_use$ics_exist_use_data)
range(cptmod_use$ebp_use_primary_quality_outcome)
mean(cptmod_use$ebp_use_primary_quality_outcome)
sd(cptmod_use$ebp_use_primary_quality_outcome)
hist(cptmod_use$ebp_use_primary_quality_outcome,xlab="EBP Primary Outcome",main="EBP Outcome Histogram")
hist(cptmod_use$exp_asd,xlab="ASD Experience",main="ASD Experience Histogram")
hist(cptmod_use$ics_total,xlab="ICS Total",main="ICS Total Histogram")
hist(cptmod_use$ics_focus_ebp,xlab="ICS Focus Score",main="ICS Focus Histogram")
hist(cptmod_use$ics_edu_support,xlab="ICS Educational Support Score",main="ICS Educational Support Histogram")
hist(cptmod_use$ics_recog_ebp,xlab="ICS Recognition Score",main="ICS Recognition Histogram")
hist(cptmod_use$ics_rewards_ebp,xlab="ICS Rewards Score",main="ICS Rewards Histogram")
hist(cptmod_use$ics_selection_ebp,xlab="ICS Selection Score",main="ICS Selection Histogram")
hist(cptmod_use$ics_selection_open,xlab="ICS Selection for Opennes Score",main="ICS Selection for Opennes Histogram")
hist(cptmod_use$ics_exist_sup_ebp,xlab="ICS Existing Supports Score",main="ICS Existing Supports Histogram")
hist(cptmod_use$ics_exist_use_data,xlab="ICS Use of Data Score",main="ICS Use of Data Histogram")

#start from data set number explanation (how many data do we have, how many are missing selpas and districts)
# -> descriptives (districts,selpas,roles,gender, etc); if categories are too big, describe in ranges or mention upto 3~5 cats
# -> distributions of our variables (report our decisions on making all the vars continuous) & include histograms
# -> unconditional (without interactions) model -> first main effect (ics_setting_total) -> second main effect (exp_asd)
# -> two main effects + interaction effect -> when reporting model result, report chisq & df & p-val
# -> explaining interaction effects (using emmeans plot)

library(lme4)
#analysis 1 (ics total - exp asd - ebp outcome pair) -> remember that ics missing items were replaced with corresponding mean subscale scores
cptmod_use = subset(cptmod,!is.na(ics_total) & !is.na(exp_asd) & !is.na(ebp_use_primary_quality_outcome))
summary(fit0 <- lmer(ebp_use_primary_quality_outcome ~ 1 + (1|district_final) + (1|selpa_final), data=cptmod_use))
summary(fit1 <- lmer(ebp_use_primary_quality_outcome ~ 1 + ics_total + (1|district_final) + (1|selpa_final), data=cptmod_use)) #put random effect last
anova(fit0,fit1)
summary(fit2 <- lmer(ebp_use_primary_quality_outcome ~ 1 + ics_total + exp_asd + (1|district_final) + (1|selpa_final), data=cptmod_use)) #put random effect last
anova(fit1,fit2)
summary(fit3 <- lmer(ebp_use_primary_quality_outcome ~ 1 + ics_total*exp_asd + (1|district_final) + (1|selpa_final), data=cptmod_use)) #when putting interaction effect, main effects are defaultly added
anova(fit2,fit3) # test of the interaction -> significant moderation effect

#looking into subscales of ics -> no barriers between ics setting and district? Just combine those two and use whatever available?
#ics_focus_ebp
summary(fit1.1 <- lmer(ebp_use_primary_quality_outcome ~ 1 + exp_asd + (1|district_final) + (1|selpa_final), data=cptmod_use)) #put random effect last
summary(fit2.1 <- lmer(ebp_use_primary_quality_outcome ~ 1 + ics_focus_ebp + exp_asd + (1|district_final) + (1|selpa_final), data=cptmod_use)) #put random effect last
anova(fit1.1,fit2.1)
summary(fit3.1 <- lmer(ebp_use_primary_quality_outcome ~ 1 + ics_focus_ebp + exp_asd + ics_focus_ebp*exp_asd + (1|district_final) + (1|selpa_final), data=cptmod_use)) #when putting interaction effect, main effects are defaultly added
anova(fit2.1,fit3.1) # test of the interaction -> significant moderation effect
#ics_edu_support
summary(fit1.1 <- lmer(ebp_use_primary_quality_outcome ~ 1 + exp_asd + (1|district_final) + (1|selpa_final), data=cptmod_use)) #put random effect last
summary(fit2.1 <- lmer(ebp_use_primary_quality_outcome ~ 1 + ics_edu_support + exp_asd + (1|district_final) + (1|selpa_final), data=cptmod_use)) #put random effect last
anova(fit1.1,fit2.1)
summary(fit3.1 <- lmer(ebp_use_primary_quality_outcome ~ 1 + ics_edu_support*exp_asd + (1|district_final) + (1|selpa_final), data=cptmod_use)) #when putting interaction effect, main effects are defaultly added
anova(fit2.1,fit3.1) # test of the interaction -> significant moderation effect
#ics_recog_ebp -> moderation effect not significant
summary(fit1.1 <- lmer(ebp_use_primary_quality_outcome ~ 1 + exp_asd + (1|district_final) + (1|selpa_final), data=cptmod_use)) #put random effect last
summary(fit2.1 <- lmer(ebp_use_primary_quality_outcome ~ 1 + ics_recog_ebp + exp_asd + (1|district_final) + (1|selpa_final), data=cptmod_use)) #put random effect last
anova(fit1.1,fit2.1)
summary(fit3.1 <- lmer(ebp_use_primary_quality_outcome ~ 1 + ics_recog_ebp*exp_asd + (1|district_final) + (1|selpa_final), data=cptmod_use)) #when putting interaction effect, main effects are defaultly added
anova(fit2.1,fit3.1) # test of the interaction
#ics_rewards_ebp -> moderation effect not significant
summary(fit1.1 <- lmer(ebp_use_primary_quality_outcome ~ 1 + exp_asd + (1|district_final) + (1|selpa_final), data=cptmod_use)) #put random effect last
summary(fit2.1 <- lmer(ebp_use_primary_quality_outcome ~ 1 + ics_rewards_ebp + exp_asd + (1|district_final) + (1|selpa_final), data=cptmod_use)) #put random effect last
anova(fit1.1,fit2.1)
summary(fit3.1 <- lmer(ebp_use_primary_quality_outcome ~ 1 + ics_rewards_ebp*exp_asd + (1|district_final) + (1|selpa_final), data=cptmod_use)) #when putting interaction effect, main effects are defaultly added
anova(fit2.1,fit3.1) # test of the interaction
#ics_selection_ebp
summary(fit1.1 <- lmer(ebp_use_primary_quality_outcome ~ 1 + exp_asd + (1|district_final) + (1|selpa_final), data=cptmod_use)) #put random effect last
summary(fit2.1 <- lmer(ebp_use_primary_quality_outcome ~ 1 + ics_selection_ebp + exp_asd + (1|district_final) + (1|selpa_final), data=cptmod_use)) #put random effect last
anova(fit1.1,fit2.1)
summary(fit3.1 <- lmer(ebp_use_primary_quality_outcome ~ 1 + ics_selection_ebp*exp_asd + (1|district_final) + (1|selpa_final), data=cptmod_use)) #when putting interaction effect, main effects are defaultly added
anova(fit2.1,fit3.1) # test of the interaction -> significant moderation effect
#ics_selection_open -> moderation effect significant
summary(fit1.1 <- lmer(ebp_use_primary_quality_outcome ~ 1 + exp_asd + (1|district_final) + (1|selpa_final), data=cptmod_use)) #put random effect last
summary(fit2.1 <- lmer(ebp_use_primary_quality_outcome ~ 1 + ics_selection_open + exp_asd + (1|district_final) + (1|selpa_final), data=cptmod_use)) #put random effect last
anova(fit1.1,fit2.1)
summary(fit3.1 <- lmer(ebp_use_primary_quality_outcome ~ 1 + ics_selection_open*exp_asd + (1|district_final) + (1|selpa_final), data=cptmod_use)) #when putting interaction effect, main effects are defaultly added
anova(fit2.1,fit3.1) # test of the interaction -> significant moderation effect
#ics_exist_sup_ebp
summary(fit1.1 <- lmer(ebp_use_primary_quality_outcome ~ 1 + exp_asd + (1|district_final) + (1|selpa_final), data=cptmod_use)) #put random effect last
summary(fit2.1 <- lmer(ebp_use_primary_quality_outcome ~ 1 + ics_exist_sup_ebp + exp_asd + (1|district_final) + (1|selpa_final), data=cptmod_use)) #put random effect last
anova(fit1.1,fit2.1)
summary(fit3.1 <- lmer(ebp_use_primary_quality_outcome ~ 1 + ics_exist_sup_ebp*exp_asd + (1|district_final) + (1|selpa_final), data=cptmod_use)) #when putting interaction effect, main effects are defaultly added
anova(fit2.1,fit3.1) # test of the interaction -> significant moderation effect
#ics_exist_use_data
summary(fit1.1 <- lmer(ebp_use_primary_quality_outcome ~ 1 + exp_asd + (1|district_final) + (1|selpa_final), data=cptmod_use)) #put random effect last
summary(fit2.1 <- lmer(ebp_use_primary_quality_outcome ~ 1 + ics_exist_use_data + exp_asd + (1|district_final) + (1|selpa_final), data=cptmod_use)) #put random effect last
anova(fit1.1,fit2.1)
summary(fit3.1 <- lmer(ebp_use_primary_quality_outcome ~ 1 + ics_exist_use_data*exp_asd + (1|district_final) + (1|selpa_final), data=cptmod_use)) #when putting interaction effect, main effects are defaultly added
anova(fit2.1,fit3.1) # test of the interaction -> significant moderation effect

#analysis 2 (ics - poverty level - ebp outcome pair)
cptmod_pov_level = cptmod
cptmod_pov_level$district_final = gsub(" School District$","",cptmod_pov_level$district_final)
cptmod_pov_level$district = gsub(" ","",tolower(cptmod_pov_level$district_final))
cptmod_pov_level$district = gsub("[[:punct:]]","",cptmod_pov_level$district)
cptmod_pov_level = merge(cptmod_pov_level,poverty_district[,2:3],all.x=T)
cptmod_pov_level$poverty_rate_district[grep("burlingame",cptmod_pov_level$district)] = 0.129
cptmod_pov_level$poverty_rate_district[grep("millbrae",cptmod_pov_level$district)] = 0.248
cptmod_pov_level$poverty_rate_district[grep("woodside",cptmod_pov_level$district)] = 0.107
cptmod_district_needed = cptmod_pov_level[which(is.na(cptmod_pov_level$poverty_rate_district) & !is.na(cptmod_pov_level$district_final)),c(2,5:6,41)]
#fixing duplicates where the district name the same in different selpa's
cptmod_pov_level_duplicate = subset(cptmod_pov_level,subjid %in% names(table(cptmod_pov_level$subjid))[table(cptmod_pov_level$subjid)>1])
cptmod_pov_level = cptmod_pov_level[-which(cptmod_pov_level$subjid %in% cptmod_pov_level_duplicate$subjid),]
cptmod_pov_level_duplicate = cptmod_pov_level_duplicate[-which(cptmod_pov_level_duplicate$poverty_rate_district==.618 | cptmod_pov_level_duplicate$poverty_rate_district==.475),]
cptmod_pov_level = rbind(cptmod_pov_level,cptmod_pov_level_duplicate)
rm(cptmod_pov_level_duplicate)

cptmod_pov_level = unique(rbind.fill(cptmod_pov_level,cptmod_district_needed[!is.na(cptmod_district_needed$poverty_rate_district),]))
cptmod_pov_level = subset(cptmod_pov_level,!is.na(ics_total) & !is.na(poverty_rate_district) & !is.na(ebp_use_primary_quality_outcome))
which(table(cptmod_pov_level$subjid)>1)
hist(cptmod_pov_level$poverty_rate_district,xlab="Poverty Rate",main="Poverty Rate Histogram")
table(cptmod_pov_level$gender)
table(cptmod_pov_level$role)
table(cptmod_pov_level$age)
range(table(cptmod_pov_level$selpa_final))
range(table(cptmod_pov_level$district_final))
length(unique(cptmod_pov_level$selpa_final))
length(unique(cptmod_pov_level$district_final))
table(cptmod_pov_level$role)

summary(fit1 <- lmer(ebp_use_primary_quality_outcome ~ 1 + ics_total + (1|district_final) + (1|selpa_final), data=cptmod_pov_level)) #put random effect last
summary(fit4 <- lmer(ebp_use_primary_quality_outcome ~ 1 + ics_total + poverty_rate_district + (1|district_final) + (1|selpa_final), data=cptmod_pov_level)) #put random effect last
anova(fit1,fit4) #no fixed effect
summary(fit5 <- lmer(ebp_use_primary_quality_outcome ~ 1 + ics_total*poverty_rate_district + (1|district_final) + (1|selpa_final), data=cptmod_pov_level)) #when putting interaction effect, main effects are defaultly added
anova(fit4,fit5) # test of the interaction -> no moderation effect

#analysis 3 (ics - perf score - ebp outcome pair) -> n=10, maybe not worth looking
cptmod_perf_sco = subset(cptmod,!is.na(cptmod$ics_total) & !is.na(cptmod$performance_score_avg) & !is.na(cptmod$ebp_use_primary_quality_outcome))
cptmod_perf_sco = cptmod_perf_sco[-which(is.na(cptmod_perf_sco$district_final)),]
summary(fit1 <- lmer(ebp_use_primary_quality_outcome ~ 1 + ics_total + (1|district_final) + (1|selpa_final), data=cptmod_perf_sco)) #put random effect last
summary(fit6 <- lmer(ebp_use_primary_quality_outcome ~ 1 + ics_total + performance_score_avg + (1|district_final) + (1|selpa_final), data=cptmod_perf_sco)) #put random effect last
anova(fit1,fit6)
summary(fit7 <- lmer(ebp_use_primary_quality_outcome ~ 1 + ics_total*performance_score_avg + (1|district_final) + (1|selpa_final), data=cptmod_perf_sco)) #when putting interaction effect, main effects are defaultly added
anova(fit6,fit7) # test of the interaction

#analysis 4 (ics - captain participation - ebp outcome pair) -> use received_training_from_cadre instead
# cptmod_cap_par = subset(cptmod,!is.na(cptmod$ics_total) & !is.na(cptmod$years_participation) & !is.na(cptmod$ebp_use_primary_quality_outcome))
# hist(cptmod_cap_par$years_participation,xlab="Number of Years of CAPTAIN Participation",main="CAPTAIN Participation Histogram")
# table(cptmod_cap_par$gender)
# table(cptmod_cap_par$role)
# table(cptmod_cap_par$age)
# range(table(cptmod_cap_par$selpa_final))
# range(table(cptmod_cap_par$district_final))
# length(unique(cptmod_cap_par$selpa_final))
# length(unique(cptmod_cap_par$district_final))
# table(cptmod_cap_par$role)
# 
# summary(fit1 <- lmer(ebp_use_primary_quality_outcome ~ 1 + ics_total + (1|district_final) + (1|selpa_final), data=cptmod_cap_par)) #put random effect last
# anova(fit0,fit1)
# summary(fit8 <- lmer(ebp_use_primary_quality_outcome ~ 1 + ics_total + years_participation + (1|district_final) + (1|selpa_final), data=cptmod_cap_par)) #put random effect last
# anova(fit1,fit8)
# summary(fit9 <- lmer(ebp_use_primary_quality_outcome ~ 1 + ics_total*years_participation + (1|district_final) + (1|selpa_final), data=cptmod_cap_par)) #when putting interaction effect, main effects are defaultly added
# anova(fit8,fit9) # test of the interaction -> no moderation effect

captain_participation$cap_par = ifelse(captain_participation$received_training_from_cadre=="Yes",1,
                                       ifelse(captain_participation$received_training_from_cadre=="No",0,NA))
cptmod = unique(merge(cptmod,captain_participation[which(!is.na(captain_participation$cap_par)),c(1,34)],all.x=T))
cptmod_cap_par = subset(cptmod,!is.na(cptmod$ics_total) & !is.na(cptmod$cap_par) & !is.na(cptmod$ebp_use_primary_quality_outcome))
hist(cptmod_cap_par$cap_par,xlab="Received Training from Cadre?",main="Cadre Training Histogram")
table(cptmod_cap_par$gender)
table(cptmod_cap_par$role)
table(cptmod_cap_par$race)
table(cptmod_cap_par$age)
range(table(cptmod_cap_par$selpa_final))
range(table(cptmod_cap_par$district_final))
length(unique(cptmod_cap_par$selpa_final))
length(unique(cptmod_cap_par$district_final))

summary(fit1 <- lmer(ebp_use_primary_quality_outcome ~ 1 + ics_total + (1|district_final) + (1|selpa_final), data=cptmod_cap_par)) #put random effect last
summary(fit8 <- lmer(ebp_use_primary_quality_outcome ~ 1 + ics_total + cap_par + (1|district_final) + (1|selpa_final), data=cptmod_cap_par)) #put random effect last
anova(fit1,fit8)
summary(fit9 <- lmer(ebp_use_primary_quality_outcome ~ 1 + ics_total*cap_par + (1|district_final) + (1|selpa_final), data=cptmod_cap_par)) #when putting interaction effect, main effects are defaultly added
anova(fit8,fit9) # test of the interaction

#analysis 5 (ebp resource - poverty rate - cde 18_19) -> missing items for ebp resource were replaced by corresponding subscale mean
#constructing the dataset in district level
ma_use = subset(ma,year=='2018-19')
ma_use = ma_use[,-grep("^swd_",names(ma_use))]
for(j in grep("^aut_",names(ma_use))){
  ma_use[,j] = round(as.numeric(ma_use[,j]),3)
}
ma_use$cde_math = ma_use$aut_math_level3_pct+ma_use$aut_math_level4_pct
ma_use$cde_ela = ma_use$aut_ela_level3_pct+ma_use$aut_ela_level4_pct
ma_use$district = gsub(" ","",tolower(ma_use$district_final))
ma_use$district = gsub("[[:punct:]]","",ma_use$district)
ma_use = merge(ma_use,poverty_district[,2:3],all.x=T)
ma_use$poverty_rate_district[grep("burlingame",ma_use$district)] = 0.129
ma_use$poverty_rate_district[grep("millbrae",ma_use$district)] = 0.248
ma_use$poverty_rate_district[grep("woodside",ma_use$district)] = 0.107

#fixing duplicates where the district name the same in different selpa's
ma_use_duplicate = subset(ma_use,subjid %in% names(table(ma_use$subjid))[table(ma_use$subjid)>1])
ma_use = ma_use[-which(ma_use$subjid %in% ma_use_duplicate$subjid),]
# write_xlsx(ma_use_duplicate,"Moderation Analysis/ma_use_duplicate.xlsx")
# ma_use_duplicate_fixed = read_excel("Moderation Analysis/ma_use_duplicate.xlsx")
ma_use = rbind(ma_use,ma_use_duplicate_fixed)
rm(ma_use_duplicate)

ma_use_district = subset(ma_use,!is.na(ebp_res_total) & !is.na(poverty_rate_district))
ma_use_district = aggregate(ma_use_district[,c(28,48,33:35,44:47)],by=list(ma_use_district$district_final),mean)
names(ma_use_district)[1] = "district_final"
ma_use_district = merge(ma_use_district,unique(ma_use[,5:6]),all.x=T)
hist(ma_use_district$ebp_res_total,xlab="EBP Resources Total",main="EBP Resources Histogram")
hist(ma_use_district$poverty_rate_district,xlab="Poverty Rate",main="Poverty Rate Histogram")
hist(ma_use_district$cde_math,xlab="Math Satisfactory Pct",main="Math Satisfactory Pct Histogram")
hist(ma_use_district$cde_ela,xlab="ELA Satisfactory Pct",main="ELA Satisfactory Pct Histogram")
hist(ma_use_district$aut_rc80_pct,xlab="Regular Class above 80% Pct",main="Regular Class above 80% Histogram")
hist(ma_use_district$aut_sepschool_pct,xlab="Separate Placement Pct",main="Separate Placement Pct Histogram")
hist(ma_use_district$aut_stu_suspend_pct,xlab="Suspension Rate",main="Suspension Rate Histogram")
range(table(ma_use_district$selpa_final))
length(unique(ma_use_district$selpa_final))
length(unique(ma_use_district$district_final))

library(lme4)
library(emmeans)
#cde math
summary(fit10.0 <- lmer(cde_math ~ 1 + (1|selpa_final), data=ma_use_district)) #put random effect last
summary(fit10 <- lmer(cde_math ~ 1 + ebp_res_total + (1|selpa_final), data=ma_use_district)) #put random effect last
anova(fit10.0,fit10) #main effect for ebp resources significant
summary(fit11 <- lmer(cde_math ~ 1 + ebp_res_total + poverty_rate_district + (1|selpa_final), data=ma_use_district)) #put random effect last
anova(fit10,fit11) #no relationship between district poverty rate and cde math
summary(fit11.1 <- lmer(cde_math ~ 1 + ebp_res_total * poverty_rate_district+(1|selpa_final), data=ma_use_district)) #when putting interaction effect, main effects are defaultly added
anova(fit11,fit11.1) # test of the interaction -> significant interaction between ebp resources and district poverty rate
summary(fit11.2 <- lm(cde_math ~ 1 + ebp_res_total+poverty_rate_district+ebp_res_total * poverty_rate_district, data=ma_use_district)) #when putting interaction effect, main effects are defaultly added
em_fit2 = emmeans(fit11.2,~ebp_res_total*poverty_rate_district,at=list(ebp_res_total=c(15,40,55,100),poverty_rate_district=c(0.3,0.44,0.6,0.9)))
emmip(em_fit2,poverty_rate_district~ebp_res_total,xlab="EBP Resources Total",ylab="CDE Math",tlab="Poverty Rate")
#cde ela
summary(fit12.0 <- lmer(cde_ela ~ 1 + (1|selpa_final), data=ma_use_district)) #put random effect last
summary(fit12 <- lmer(cde_ela ~ 1 + ebp_res_total+(1|selpa_final), data=ma_use_district)) #put random effect last
anova(fit12.0,fit12) #main effect for ebp resources NOT significant
summary(fit13 <- lmer(cde_ela ~ 1 + ebp_res_total + poverty_rate_district+(1|selpa_final), data=ma_use_district)) #put random effect last
anova(fit12,fit13) #no significant relationship between district poverty rate and cde ela
summary(fit13.1 <- lmer(cde_ela ~ 1 +ebp_res_total * poverty_rate_district+(1|selpa_final), data=ma_use_district)) #when putting interaction effect, main effects are defaultly added
anova(fit13,fit13.1) # test of the interaction -> NO significant interaction between ebp resources and district poverty rate
summary(fit13.2 <- lm(cde_ela ~ 1 + ebp_res_total+poverty_rate_district+ebp_res_total * poverty_rate_district, data=ma_use_district)) #when putting interaction effect, main effects are defaultly added
em_fit3 = emmeans(fit13.2,~ebp_res_total*poverty_rate_district,at=list(ebp_res_total=c(15,40,55,100),poverty_rate_district=c(0.3,0.6,0.9)))
emmip(em_fit3,poverty_rate_district~ebp_res_total,xlab="EBP Resources Total",ylab="CDE ELA",tlab="Poverty Rate")
##cde lre
#rc80_pct
summary(fit14.0 <- lmer(aut_rc80_pct ~ 1 + (1|selpa_final), data=ma_use_district)) #put random effect last
summary(fit14 <- lmer(aut_rc80_pct ~ 1 + ebp_res_total+(1|selpa_final), data=ma_use_district)) #put random effect last
anova(fit14.0,fit14) #main effect of ebp resources NOT significant
summary(fit15 <- lmer(aut_rc80_pct ~ 1 + ebp_res_total + poverty_rate_district+(1|selpa_final), data=ma_use_district)) #put random effect last
anova(fit14,fit15) #no significant relationship between district poverty rate and rc80_pct
summary(fit15.1 <- lmer(aut_rc80_pct ~ 1 + ebp_res_total * poverty_rate_district+(1|selpa_final), data=ma_use_district)) #when putting interaction effect, main effects are defaultly added
anova(fit15,fit15.1) # test of the interaction -> NO significant interaction between ebp resources and district poverty rate
summary(fit15.2 <- lm(aut_rc80_pct ~ 1 + ebp_res_total+poverty_rate_district+ebp_res_total * poverty_rate_district, data=ma_use_district)) #when putting interaction effect, main effects are defaultly added
em_fit4 = emmeans(fit15.2,~ebp_res_total*poverty_rate_district,at=list(ebp_res_total=c(15,40,55,100),poverty_rate_district=c(0.3,0.6,0.9)))
emmip(em_fit4,poverty_rate_district~ebp_res_total,xlab="EBP Resources Total",ylab="RC80 Pct",tlab="Poverty Rate")
#sep_school
summary(fit16.0 <- lmer(aut_sepschool_pct ~ 1 + (1|selpa_final), data=ma_use_district)) #put random effect last
summary(fit16 <- lmer(aut_sepschool_pct ~ 1 + ebp_res_total+(1|selpa_final), data=ma_use_district)) #put random effect last
anova(fit16.0,fit16) #NOT significant
summary(fit17 <- lmer(aut_sepschool_pct ~ 1 + ebp_res_total + poverty_rate_district+(1|selpa_final), data=ma_use_district)) #put random effect last
anova(fit16,fit17) #significant relationship between district poverty rate and sepschool_pct
summary(fit17.1 <- lmer(aut_sepschool_pct ~ 1 + ebp_res_total * poverty_rate_district+(1|selpa_final), data=ma_use_district)) #when putting interaction effect, main effects are defaultly added
anova(fit17,fit17.1) # test of the interaction -> NO significant interaction between ebp resources and district poverty rate
summary(fit17.2 <- lm(aut_sepschool_pct ~ 1 + ebp_res_total+poverty_rate_district+ebp_res_total * poverty_rate_district, data=ma_use_district)) #when putting interaction effect, main effects are defaultly added
em_fit5 = emmeans(fit17.2,~ebp_res_total*poverty_rate_district,at=list(ebp_res_total=c(15,40,55,100),poverty_rate_district=c(0.3,0.6,0.9)))
emmip(em_fit5,poverty_rate_district~ebp_res_total,xlab="EBP Resources Total",ylab="Sep School Pct",tlab="Poverty Rate")
#cde behav
summary(fit18.0 <- lmer(aut_stu_suspend_pct ~ 1 + (1|selpa_final), data=ma_use_district)) #put random effect last
summary(fit18 <- lmer(aut_stu_suspend_pct ~ 1 + ebp_res_total+(1|selpa_final), data=ma_use_district)) #put random effect last
anova(fit18.0,fit18) #NOT significant
summary(fit19 <- lmer(aut_stu_suspend_pct ~ 1 + ebp_res_total + poverty_rate_district+(1|selpa_final), data=ma_use_district)) #put random effect last
anova(fit18,fit19) #no significant relationship between district poverty rate and cde ela
summary(fit19.1 <- lmer(aut_stu_suspend_pct ~ 1 + ebp_res_total * poverty_rate_district+(1|selpa_final), data=ma_use_district)) #when putting interaction effect, main effects are defaultly added
anova(fit19,fit19.1) # test of the interaction -> NO significant interaction between ebp resources and district poverty rate
summary(fit19.2 <- lm(aut_stu_suspend_pct ~ 1 + ebp_res_total+poverty_rate_district+ebp_res_total * poverty_rate_district, data=ma_use_district)) #when putting interaction effect, main effects are defaultly added
em_fit6 = emmeans(fit19.2,~ebp_res_total*poverty_rate_district,at=list(ebp_res_total=c(15,40,55,100),poverty_rate_district=c(0.3,0.6,0.9)))
emmip(em_fit6,poverty_rate_district~ebp_res_total,xlab="EBP Resources Total",ylab="Suspension Rate",tlab="Poverty Rate")

##extra analyses
ma_use = merge(ma_use,captain_participation[,c(1,34)],all.x=T)
#received cadre training - cde outcomes
#fix the dataset by district and re-do the analysis since cde data is district level
ma_cadre_cde = subset(ma_use,!is.na(cap_par))
ma_cadre_cde = aggregate(ma_cadre_cde[,c(33:35,44:47,49)],by=list(ma_cadre_cde$district_final),mean)
names(ma_cadre_cde)[1] = "district_final"
ma_cadre_cde = merge(ma_cadre_cde,unique(cde_district[cde_district$year=="2018-19",3:4]),all.x=T)
ma_cadre_cde = ma_cadre_cde[-which(grepl("Ventura",ma_cadre_cde$selpa_final) & grepl("iLead",ma_cadre_cde$district_final)),]

library(lme4)
summary(fit20 <- lmer(cde_math~1+(1|selpa_final),data=ma_cadre_cde)) 
summary(fit20.1 <- lmer(cde_math~1+cap_par+(1|selpa_final),data=ma_cadre_cde))
anova(fit20,fit20.1) #significant (p = 0.0504)
summary(fit21 <- lmer(cde_ela~1+(1|selpa_final),data=ma_cadre_cde)) 
summary(fit21.1 <- lmer(cde_ela~1+cap_par+(1|selpa_final),data=ma_cadre_cde))
anova(fit21,fit21.1) #significant relationship
summary(fit22 <- lmer(aut_rc80_pct~1+(1|selpa_final),data=ma_cadre_cde))
summary(fit22.1 <- lmer(aut_rc80_pct~1+cap_par+(1|selpa_final),data=ma_cadre_cde))
anova(fit22,fit22.1)
summary(fit23 <- lmer(aut_sepschool_pct~1+(1|selpa_final),data=ma_cadre_cde)) #significant relationship
summary(fit23.1 <- lmer(aut_sepschool_pct~1+cap_par+(1|selpa_final),data=ma_cadre_cde)) #significant relationship
anova(fit23,fit23.1)
summary(fit24 <- lmer(aut_stu_suspend_pct~1+(1|selpa_final),data=ma_cadre_cde)) #significant relationship
summary(fit24.1 <- lmer(aut_stu_suspend_pct~1+cap_par+(1|selpa_final),data=ma_cadre_cde)) #significant relationship
anova(fit24,fit24.1)

#RC80 with CDE Math and ELA
summary(fit30 <- lmer(cde_math~1+aut_rc80_pct+(1|selpa_final),data=ma_cadre_cde))
anova(fit20,fit30) #significant relationship btw rc80 and cde math
summary(fit31 <- lmer(cde_ela~1+aut_rc80_pct+(1|selpa_final),data=ma_cadre_cde))
anova(fit21,fit31) #significant relationship btw rc80 and cde ela
#mediation effect of rc80 btw cde math/ela and cadre_training_received?
summary(fit32 <- lmer(cde_math~1+aut_rc80_pct+cap_par+(1|selpa_final),data=ma_cadre_cde)) 
anova(fit30,fit32) #still significant main effect of cap_par
summary(fit33 <- lmer(cde_ela~1+aut_rc80_pct+cap_par+(1|selpa_final),data=ma_cadre_cde))
anova(fit31,fit33) #still significant main effect of cap_par

#ebp primary outcome - cde outome -> same
ma_ebp_cde = subset(ma_use,!is.na(ebp_use_primary_quality_outcome))
ma_ebp_cde = aggregate(ma_ebp_cde[,c(18,33:35,44:47)],by=list(ma_ebp_cde$district_final),mean)
names(ma_ebp_cde)[1] = "district_final"
ma_ebp_cde = merge(ma_ebp_cde,unique(cde_district[cde_district$year=="2018-19",3:4]),all.x=T)
ma_ebp_cde = ma_ebp_cde[-which(grepl("Ventura",ma_ebp_cde$selpa_final) & grepl("iLead",ma_ebp_cde$district_final)),]

summary(fit25 <- lmer(cde_math~1+(1|selpa_final),data=ma_ebp_cde))
summary(fit25.1 <- lmer(cde_math~1+ebp_use_primary_quality_outcome+(1|selpa_final),data=ma_ebp_cde))
anova(fit25,fit25.1)
summary(fit26 <- lmer(cde_ela~1+(1|selpa_final),data=ma_ebp_cde))
summary(fit26.1 <- lmer(cde_ela~1+ebp_use_primary_quality_outcome+(1|selpa_final),data=ma_ebp_cde))
anova(fit26,fit26.1)
summary(fit27 <- lmer(aut_rc80_pct~1+(1|selpa_final),data=ma_ebp_cde)) #significant relationship
summary(fit27.1 <- lmer(aut_rc80_pct~1+ebp_use_primary_quality_outcome+(1|selpa_final),data=ma_ebp_cde)) #significant relationship
anova(fit27,fit27.1)
summary(fit28 <- lmer(aut_sepschool_pct~1+(1|selpa_final),data=ma_ebp_cde))
summary(fit28.1 <- lmer(aut_sepschool_pct~1+ebp_use_primary_quality_outcome+(1|selpa_final),data=ma_ebp_cde))
anova(fit28,fit28.1)
summary(fit29 <- lmer(aut_stu_suspend_pct~1+(1|selpa_final),data=ma_ebp_cde))
summary(fit29.1 <- lmer(aut_stu_suspend_pct~1+ebp_use_primary_quality_outcome+(1|selpa_final),data=ma_ebp_cde))
anova(fit29,fit29.1) #significant relationship
