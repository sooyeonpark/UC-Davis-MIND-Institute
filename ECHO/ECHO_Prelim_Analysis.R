library(sqldf)
#combining all pre & post surveys
echo_mh_curr_pre$type = "Mental Health"
echo_mh_curr_post$type = "Mental Health"
echo_pc_pre$type = "Primary Care"
echo_pc_post$type = "Primary Care"
echo_at_pre$type = "Advanced Topics"
echo_at_post$type = "Advanced Topics"
echo_pre = rbind(echo_mh_curr_pre,echo_pc_pre,echo_at_pre)
echo_pre = echo_pre[,-(6:10)]
echo_post = rbind(echo_mh_curr_post,echo_pc_post,echo_at_post)
echo_post = echo_post[,-(6:10)]

#Q41 & 38
for(j in c(grep("^Q41_",names(echo_pre)),grep("^Q38_[0-9]{1}$",names(echo_pre)))){
  echo_pre[,j] = as.numeric(echo_pre[,j])
  echo_post[,j] = as.numeric(echo_post[,j])
}
for(i in 1:nrow(echo_pre)){
  echo_pre$Q38_avg[i] = round(mean(unlist(echo_pre[i,grep("^Q38_[0-9]{1}$",names(echo_pre))]),na.rm=T),2)
  echo_pre$Q41_avg[i] = round(mean(unlist(echo_pre[i,grep("^Q41_",names(echo_pre))])),2)
}
for(i in 1:nrow(echo_post)){
  echo_post$Q38_avg[i] = round(mean(unlist(echo_post[i,grep("^Q38_[0-9]{1}$",names(echo_post))]),na.rm=T),2)
  echo_post$Q41_avg[i] = round(mean(unlist(echo_post[i,grep("^Q41_",names(echo_post))])),2)
}
length(which(!is.na(echo_pre$Q41_avg)))
length(which(!is.na(echo_post$Q41_avg)))
echo_pre[is.na(echo_pre$Q41_1),"type"]
echo_post[is.na(echo_post$Q41_1),"type"]
sapply(echo_pre[,grep("pre_Q41_",names(echo_pre))],mean,na.rm=T)
sapply(echo_pre[,grep("pre_Q41_",names(echo_pre))],sd,na.rm=T)
sapply(echo_post[,grep("post_Q41_",names(echo_post))],mean,na.rm=T)
sapply(echo_post[,grep("post_Q41_",names(echo_post))],sd,na.rm=T)

sqldf("select avg(Q41_1),avg(Q41_2),avg(Q41_3),avg(Q41_4),avg(Q41_5),avg(Q41_6),
      avg(Q41_7),avg(Q41_8),avg(Q41_avg),count(type),type from echo_pre group by type;")
sqldf("select stdev(Q41_1),stdev(Q41_2),stdev(Q41_3),stdev(Q41_4),stdev(Q41_5),stdev(Q41_6),
      stdev(Q41_7),stdev(Q41_8),stdev(Q41_avg),count(type),type from echo_pre group by type;")
sqldf("select avg(Q41_1),avg(Q41_2),avg(Q41_3),avg(Q41_4),avg(Q41_5),avg(Q41_6),
      avg(Q41_7),avg(Q41_8),avg(Q41_avg),count(type),type from echo_post group by type;")
sqldf("select stdev(Q41_1),stdev(Q41_2),stdev(Q41_3),stdev(Q41_4),stdev(Q41_5),stdev(Q41_6),
      stdev(Q41_7),stdev(Q41_8),stdev(Q41_avg),count(type),type from echo_post group by type;")

write.csv(round(cor(echo_pre[,grep("^Q41_",names(echo_pre))],use="complete.obs"),2),"ECHO/cor.csv",row.names=F)
write.table(round(cor(echo_post[,grep("^Q41_",names(echo_post))],use="complete.obs"),2),"ECHO/cor.csv",sep=",",row.names=F,append=T)


length(which(!is.na(echo_pre$Q38_avg)))
length(which(!is.na(echo_post$Q38_avg)))
sapply(echo_pre[,c(grep("^Q38_[0-9]{1}$",names(echo_pre)),97)],mean,na.rm=T)
sapply(echo_pre[,c(grep("^Q38_[0-9]{1}$",names(echo_pre)),97)],sd,na.rm=T)
sapply(echo_post[,c(grep("^Q38_[0-9]{1}$",names(echo_post)),97)],mean,na.rm=T)
sapply(echo_post[,c(grep("^Q38_[0-9]{1}$",names(echo_post)),97)],sd,na.rm=T)

sqldf("select avg(Q38_1),avg(Q38_2),avg(Q38_3),avg(Q38_4),avg(Q38_5),avg(Q38_6),avg(Q38_7),
      avg(Q38_8),avg(Q38_9),avg(Q38_avg),count(type),type from echo_pre group by type;")
sqldf("select stdev(Q38_1),stdev(Q38_2),stdev(Q38_3),stdev(Q38_4),stdev(Q38_5),stdev(Q38_6),stdev(Q38_7),
      stdev(Q38_8),stdev(Q38_9),stdev(Q38_avg),count(type),type from echo_pre group by type;")
sqldf("select avg(Q38_1),avg(Q38_2),avg(Q38_3),avg(Q38_4),avg(Q38_5),avg(Q38_6),avg(Q38_7),
      avg(Q38_8),avg(Q38_9),avg(Q38_avg),count(type),type from echo_post group by type;")
sqldf("select stdev(Q38_1),stdev(Q38_2),stdev(Q38_3),stdev(Q38_4),stdev(Q38_5),stdev(Q38_6),stdev(Q38_7),
      stdev(Q38_8),stdev(Q38_9),stdev(Q38_avg),count(type),type from echo_post group by type;")

write.table(round(cor(echo_pre[,grep("^Q38_[0-9]{1}$",names(echo_pre))],use="complete.obs"),2),"ECHO/cor.csv",sep=",",row.names=F,append=T)
write.table(round(cor(echo_post[,grep("^Q38_[0-9]{1}$",names(echo_post))],use="complete.obs"),2),"ECHO/cor.csv",sep=",",row.names=F,append=T)

#paired t-test for Q38 & 41
mh_paired_t = echo_data_summary[which(echo_data_summary$mh_pre==1 & echo_data_summary$mh_post==1),1:2]
pc_paired_t = echo_data_summary[which(echo_data_summary$pc_pre==1 & echo_data_summary$pc_post==1),1:2]
mh_paired_t = merge(mh_paired_t,echo_pre[echo_pre$type=="Mental Health",c(3:4,grep("^Q38_[0-9]{1}$",names(echo_pre)),grep("^Q41_",names(echo_pre)))],all.x=T)
pc_paired_t = merge(pc_paired_t,echo_pre[echo_pre$type=="Primary Care",c(3:4,grep("^Q38_[0-9]{1}$",names(echo_pre)),grep("^Q41_",names(echo_pre)))],all.x=T)
names(mh_paired_t)[3:20] = paste0('pre_',names(mh_paired_t)[3:20]) 
names(pc_paired_t)[3:20] = paste0('pre_',names(pc_paired_t)[3:20]) 
mh_paired_t = merge(mh_paired_t,echo_post[echo_post$type=="Mental Health",c(3:4,grep("^Q38_[0-9]{1}$",names(echo_post)),grep("^Q41_",names(echo_post)))],all.x=T)
pc_paired_t = merge(pc_paired_t,echo_post[echo_post$type=="Primary Care",c(3:4,grep("^Q38_[0-9]{1}$",names(echo_post)),grep("^Q41_",names(echo_post)))],all.x=T)
names(mh_paired_t)[21:38] = paste0('post_',names(mh_paired_t)[21:38]) 
names(pc_paired_t)[21:38] = paste0('post_',names(pc_paired_t)[21:38]) 

for(i in 1:nrow(mh_paired_t)){
  mh_paired_t$pre_Q38_avg[i] = round(mean(unlist(mh_paired_t[i,grep("^pre_Q38_[0-9]{1}$",names(mh_paired_t))]),na.rm=T),2)
  mh_paired_t$post_Q38_avg[i] = round(mean(unlist(mh_paired_t[i,grep("^post_Q38_[0-9]{1}$",names(mh_paired_t))]),na.rm=T),2)
}
for(i in 1:nrow(pc_paired_t)){
  pc_paired_t$pre_Q38_avg[i] = round(mean(unlist(pc_paired_t[i,grep("^pre_Q38_[0-9]{1}$",names(pc_paired_t))]),na.rm=T),2)
  pc_paired_t$post_Q38_avg[i] = round(mean(unlist(pc_paired_t[i,grep("^post_Q38_[0-9]{1}$",names(pc_paired_t))]),na.rm=T),2)
}
t.test(mh_paired_t$pre_Q41_avg,mh_paired_t$post_Q41_avg,paired=T)
t.test(pc_paired_t$pre_Q41_avg,pc_paired_t$post_Q41_avg,paired=T)
t.test(mh_paired_t$pre_Q38_avg,mh_paired_t$post_Q38_avg,paired=T)
t.test(pc_paired_t$pre_Q38_avg,pc_paired_t$post_Q38_avg,paired=T)

#Q25
library(stringr)
echo_pre$Q25 = gsub(",11","",echo_pre$Q25)
echo_pre$Q25_count = str_count(echo_pre$Q25,",")
echo_pre$Q25_count = ifelse(echo_pre$Q25!='' & echo_pre$Q25!='11',echo_pre$Q25_count+1,echo_pre$Q25_count)
table(echo_pre$Q25_count)
q25_pre_response = c()
for(l in 1:length(strsplit(echo_pre$Q25,","))){
  q25_pre_response = c(q25_pre_response,strsplit(echo_pre$Q25,",")[[l]])
}
q25_pre_response = as.numeric(q25_pre_response)
table(q25_pre_response)

echo_post$Q25 = gsub(",11","",echo_post$Q25)
echo_post$Q25_count = str_count(echo_post$Q25,",")
echo_post$Q25_count = ifelse(echo_post$Q25!='' & echo_post$Q25!='11',echo_post$Q25_count+1,echo_post$Q25_count)
table(echo_post$Q25_count)
q25_post_response = c()
for(l in 1:length(strsplit(echo_post$Q25,","))){
  q25_post_response = c(q25_post_response,strsplit(echo_post$Q25,",")[[l]])
}
q25_post_response = as.numeric(q25_post_response)
table(q25_post_response)

#Q34
echo_pre$Q34 = gsub(",11","",echo_pre$Q34)
echo_pre$Q34_count = str_count(echo_pre$Q34,",")
echo_pre$Q34_count = ifelse(echo_pre$Q34!='' & echo_pre$Q34!='11',echo_pre$Q34_count+1,echo_pre$Q34_count)
table(echo_pre$Q34_count)
q34_pre_response = c()
for(l in 1:length(strsplit(echo_pre$Q34,","))){
  q34_pre_response = c(q34_pre_response,strsplit(echo_pre$Q34,",")[[l]])
}
q34_pre_response = as.numeric(q34_pre_response)
table(q34_pre_response)

echo_post$Q34 = gsub(",11","",echo_post$Q34)
echo_post$Q34_count = str_count(echo_post$Q34,",")
echo_post$Q34_count = ifelse(echo_post$Q34!='' & echo_post$Q34!='11',echo_post$Q34_count+1,echo_post$Q34_count)
table(echo_post$Q34_count)
q34_post_response = c()
for(l in 1:length(strsplit(echo_post$Q34,","))){
  q34_post_response = c(q34_post_response,strsplit(echo_post$Q34,",")[[l]])
}
q34_post_response = as.numeric(q34_post_response)
table(q34_post_response)

#Q48
echo_pre$Q48 = gsub(",11","",echo_pre$Q48)
echo_pre$Q48_count = str_count(echo_pre$Q48,",")
echo_pre$Q48_count = ifelse(echo_pre$Q48!='' & echo_pre$Q48!='11',echo_pre$Q48_count+1,echo_pre$Q48_count)
table(echo_pre$Q48_count)
q48_pre_response = c()
for(l in 1:length(strsplit(echo_pre$Q48,","))){
  q48_pre_response = c(q48_pre_response,strsplit(echo_pre$Q48,",")[[l]])
}
q48_pre_response = as.numeric(q48_pre_response)
table(q48_pre_response)

echo_post$Q48 = gsub(",11","",echo_post$Q48)
echo_post$Q48_count = str_count(echo_post$Q48,",")
echo_post$Q48_count = ifelse(echo_post$Q48!='' & echo_post$Q48!='11',echo_post$Q48_count+1,echo_post$Q48_count)
table(echo_post$Q48_count)
q48_post_response = c()
for(l in 1:length(strsplit(echo_post$Q48,","))){
  q48_post_response = c(q48_post_response,strsplit(echo_post$Q48,",")[[l]])
}
q48_post_response = as.numeric(q48_post_response)
table(q48_post_response)

#Quantify internal/external factors
q25_internal = c(1,4:6,9)
q25_external = c(2:3,7:8,10)
q34_48_internal = c(1,5:6,9)
q34_48_external = c(2:4,7:8,10)
for(i in 1:nrow(echo_pre)){
  echo_pre$q25_internal_count[i] = length(which(as.numeric(strsplit(echo_pre$Q25,",")[[i]]) %in% q25_internal))
  echo_pre$q25_external_count[i] = length(which(as.numeric(strsplit(echo_pre$Q25,",")[[i]]) %in% q25_external))
  echo_pre$q34_internal_count[i] = length(which(as.numeric(strsplit(echo_pre$Q34,",")[[i]]) %in% q34_48_internal))
  echo_pre$q34_external_count[i] = length(which(as.numeric(strsplit(echo_pre$Q34,",")[[i]]) %in% q34_48_external))
  echo_pre$q48_internal_count[i] = length(which(as.numeric(strsplit(echo_pre$Q48,",")[[i]]) %in% q34_48_internal))
  echo_pre$q48_external_count[i] = length(which(as.numeric(strsplit(echo_pre$Q48,",")[[i]]) %in% q34_48_external))
}
for(i in 1:nrow(echo_post)){
  echo_post$q25_internal_count[i] = length(which(as.numeric(strsplit(echo_post$Q25,",")[[i]]) %in% q25_internal))
  echo_post$q25_external_count[i] = length(which(as.numeric(strsplit(echo_post$Q25,",")[[i]]) %in% q25_external))
  echo_post$q34_internal_count[i] = length(which(as.numeric(strsplit(echo_post$Q34,",")[[i]]) %in% q34_48_internal))
  echo_post$q34_external_count[i] = length(which(as.numeric(strsplit(echo_post$Q34,",")[[i]]) %in% q34_48_external))
  echo_post$q48_internal_count[i] = length(which(as.numeric(strsplit(echo_post$Q48,",")[[i]]) %in% q34_48_internal))
  echo_post$q48_external_count[i] = length(which(as.numeric(strsplit(echo_post$Q48,",")[[i]]) %in% q34_48_external))
}

sum(echo_pre$q25_internal_count)
sum(echo_pre$q25_external_count)
sum(echo_pre$Q25_count)
sum(echo_post$q25_internal_count)
sum(echo_post$q25_external_count)
sum(echo_post$Q25_count)
sum(echo_pre$q34_internal_count)
sum(echo_pre$q34_external_count)
sum(echo_pre$Q34_count)
sum(echo_post$q34_internal_count)
sum(echo_post$q34_external_count)
sum(echo_post$Q34_count)
sum(echo_pre$q48_internal_count)
sum(echo_pre$q48_external_count)
sum(echo_pre$Q48_count)
sum(echo_post$q48_internal_count)
sum(echo_post$q48_external_count)
sum(echo_post$Q48_count)

round(cor(echo_pre[,c(99,101,103)]),2)
round(cor(echo_post[,c(99,101,103)]),2)
round(cor(echo_pre[,c(100,102,104)]),2)
round(cor(echo_post[,c(100,102,104)]),2)

#demographics
#profession -> Q8
profession_pre = c()
profession_post = c()
for(l in 1:length(strsplit(echo_pre$Q8,","))){
  profession_pre = c(profession_pre,strsplit(echo_pre$Q8,",")[[l]])
}
profession_pre = as.numeric(profession_pre)
table(profession_pre)
for(l in 1:length(strsplit(echo_post$Q8,","))){
  profession_post = c(profession_post,strsplit(echo_post$Q8,",")[[l]])
}
profession_post = as.numeric(profession_post)
table(profession_post)
#location -> Q41
table(echo_pre$Q40)
table(echo_post$Q40)
table(echo_pre[echo_pre$Q40==2,"Q41"])
table(echo_post[echo_post$Q40==2,"Q41"])
#age -> Q1
table(echo_pre$Q1)
table(echo_post$Q1)

#clean up
rm(q25_pre_response,q25_post_response,q25_external,q25_internal,q34_pre_response,
   q34_post_response,q34_48_external,q34_48_internal,q48_pre_response,q48_post_response,
   profession_pre,profession_post)
