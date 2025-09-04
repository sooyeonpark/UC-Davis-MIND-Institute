atyp_iqs = sqlQuery(new_con,"select * from IQ_AtypicalScoring;",stringsAsFactors=F)
names(atyp_iqs)[1] = "id"
names(atyp_iqs) = tolower(names(atyp_iqs))
atyp_iqs = fxage(atyp_iqs,'id','test_date')

#merging all the possible mullen and das scores
mullen_das = master_visit2[which(master_visit2$visit!="2"),1:2]
mullen_das = merge(mullen_das,mullen_scored[,c(1:2,grep("_age$",names(mullen_scored)),grep("_vdq$",names(mullen_scored)):grep("_dq$",names(mullen_scored)))],all.x=T)
mullen_das = merge(mullen_das,das_scored[,c(1:3,grep("vb_ss",names(das_scored)),grep("snc_ss",names(das_scored)),grep("gca_ss",names(das_scored)))],all.x=T)
mullen_das = merge(mullen_das,atyp_iqs[,c(1:2,grep("age",names(atyp_iqs)),grep("^viq$",names(atyp_iqs)):grep("nviq",names(atyp_iqs)),grep("^iq$",names(atyp_iqs)),grep("comment",names(atyp_iqs)))],all.x=T)
mullen_das$iq_version = ''
mullen_das$iq_comment = ifelse(is.na(mullen_das$iq_comment),'',mullen_das$iq_comment)

longitudinal_iq = mullen_das[1,c(1:2,grep("version",names(mullen_das)),grep("msel_",names(mullen_das)),grep("comment$",names(mullen_das)))]
names(longitudinal_iq)[4:7] = c("iq_age","iq_viq","iq_nviq","iq_fsiq")
longitudinal_iq$iq_version = 'MSEL'
mullen_das_copy = mullen_das
for(i in 2:nrow(mullen_das)){
  if((mullen_das$visit[i] == "1" & !(paste0(mullen_das$id[i],mullen_das$visit[i]) %in% paste0(atyp_iqs$id,atyp_iqs$visit)))| (mullen_das$id[i] %in% mullen_scored[which(mullen_scored$visit=="3"),"id"] & mullen_das$visit[i] == "3")){
    names(mullen_das)[grep("msel_",names(mullen_das))] = names(longitudinal_iq)[4:7]
    longitudinal_iq = rbind(longitudinal_iq,mullen_das[i,c(1:2,grep("version",names(mullen_das)),grep("iq_age",names(mullen_das)):grep("fsiq",names(mullen_das)),grep("comment$",names(mullen_das)))])
    longitudinal_iq[nrow(longitudinal_iq),"iq_version"] = 'MSEL'
    names(mullen_das)[grep("iq_age",names(mullen_das)):grep("fsiq",names(mullen_das))] = names(mullen_das_copy)[grep("msel_",names(mullen_das_copy))]
  }
  else{
    if(mullen_das$id[i] %in% atyp_iqs[which(atyp_iqs$visit==mullen_das$visit[i]),"id"]){
      names(mullen_das)[grep("^age$",names(mullen_das)):grep("^iq$",names(mullen_das))] = names(longitudinal_iq)[4:7]
      longitudinal_iq = rbind(longitudinal_iq,mullen_das[i,c(1:2,grep("version",names(mullen_das)),grep("iq_age",names(mullen_das)):grep("fsiq",names(mullen_das)),grep("comment",names(mullen_das)))])
      longitudinal_iq[nrow(longitudinal_iq),"iq_version"] = atyp_iqs[which(atyp_iqs$id == mullen_das$id[i]),"assessment"]
      names(mullen_das)[grep("iq_age",names(mullen_das)):grep("fsiq",names(mullen_das))] = names(mullen_das_copy)[grep("^age$",names(mullen_das_copy)):grep("^iq$",names(mullen_das_copy))]
    }
    else{
      names(mullen_das)[grep("das_",names(mullen_das))] = names(longitudinal_iq)[4:7]
      longitudinal_iq = rbind(longitudinal_iq,mullen_das[i,c(1:2,grep("version",names(mullen_das)),grep("iq_age",names(mullen_das)):grep("fsiq",names(mullen_das)),grep("comment$",names(mullen_das)))])
      longitudinal_iq[nrow(longitudinal_iq),"iq_version"] = ifelse(mullen_das$id[i] %in% das_ey_scored$id & mullen_das$visit[i]=="3",'DAS Early Years',
                                                                   ifelse(mullen_das$id[i] %in% das_sa_scored$id & mullen_das$visit[i] %in% c("3","4"),'DAS School Age','NA'))
      names(mullen_das)[grep("iq_age",names(mullen_das)):grep("fsiq",names(mullen_das))] = names(mullen_das_copy)[grep("das_",names(mullen_das_copy))]
    }
  }
}
rm(mullen_das_copy)
longitudinal_iq$iq_version = ifelse(longitudinal_iq$iq_version=="DAS-EY","DAS Early Years",ifelse(longitudinal_iq$iq_version=="DAS-SA","DAS School Age",longitudinal_iq$iq_version))

#dealing with -9 and <25
#<25
for(i in 1:nrow(longitudinal_iq)){
  longitudinal_iq[i,"iq_comment"] = ifelse(any(longitudinal_iq[i,5:7]=="<25",na.rm=T),"<25: ",longitudinal_iq[i,"iq_comment"])
  for(j in 5:7){
    if(!is.na(longitudinal_iq[i,j]) & longitudinal_iq[i,j]=="<25"){
      longitudinal_iq[i,"iq_comment"] = paste0(longitudinal_iq[i,"iq_comment"],names(longitudinal_iq)[j],", ")
      longitudinal_iq[i,j] = NA
    }
  }
}
#-9
for(j in 5:7){
  longitudinal_iq[,j] = ifelse(longitudinal_iq[,j]==-9,NA,longitudinal_iq[,j])
  longitudinal_iq[,j] = as.numeric(longitudinal_iq[,j])
}
longitudinal_iq$iq_comment = gsub(", $","",longitudinal_iq$iq_comment)

longitudinal_iq[is.na(longitudinal_iq$iq_age),"iq_version"]="NA"
longitudinal_iq[,"iq_comment"] = ifelse(paste0(longitudinal_iq$id,'-',longitudinal_iq$visit) %in% paste0(master_visit2[grep("All",master_visit2$missing_data_list),"id"],'-',master_visit2[grep("All",master_visit2$missing_data_list),"visit"]),
                                                                     "Whole Visit Missing",ifelse(is.na(longitudinal_iq$iq_age),"IQ Measure Missing",longitudinal_iq$iq_comment))
for(i in 1:nrow(longitudinal_iq)){
  longitudinal_iq[i,"iq_comment"] = ifelse(!is.na(longitudinal_iq$iq_age[i]) & all(is.na(c(longitudinal_iq$iq_viq[i],longitudinal_iq$iq_nviq[i],longitudinal_iq$iq_fsiq[i]))) & longitudinal_iq$iq_comment[i]=="","NA's in Data Entries",longitudinal_iq$iq_comment[i])
}

longitudinal_iq = unique(longitudinal_iq)
write.csv(longitudinal_iq[-which(longitudinal_iq$iq_version=="NA"&longitudinal_iq$visit=="1"),],"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/longitudinal_iq.csv",row.names = F)