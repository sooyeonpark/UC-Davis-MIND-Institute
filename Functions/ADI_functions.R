#function to convert the codes to scores
adi_codes2scores = function(dt,start,end){
  for(i in which(names(dt)==start):which(names(dt)==end)){
    for(j in 1:nrow(dt)){
      if(!is.na(dt[j,i])){
        if(dt[j,i]==0 | (dt[j,i]>=7 & dt[j,i]<=9)){
          dt[j,i] = 0
        }
        else if(dt[j,i]==2 | dt[j,i]==3){
          dt[j,i] = 2
        }
        else if(dt[j,i]==1){
          dt[j,i] = 1
        }
        else{
          dt[j,i] = NA
        }
      }
    }
    dt[,i] = as.numeric(dt[,i])
  }
  
  #section D scoring
  for(i in 1:nrow(dt)){
    for(j in which(names(dt)=="sympt_onset"):which(names(dt)=="clin_ageabn")){
      dt[i,j] = cbraw(dt[i,j])
    }
    dt$sympt_onset[i] = ifelse(is.na(dt$sympt_onset[i]),NA,ifelse(dt$sympt_onset[i]<36 | dt$sympt_onset[i]==992 |
                                                            dt$sympt_onset[i]==996,1,0))
    dt$age_singlewords[i] = ifelse(is.na(dt$age_singlewords[i]),NA,ifelse(dt$age_singlewords[i]>24 |
                                                                    dt$age_singlewords[i]==994,1,0))
    dt$age_firstphrases[i] = ifelse(is.na(dt$age_firstphrases[i]),NA,ifelse(dt$age_firstphrases[i]>33 | 
                                                                      dt$age_firstphrases[i]==994,1,0))
    dt$resp_ageabn[i] = ifelse(is.na(dt$resp_ageabn[i]),NA,ifelse(dt$resp_ageabn[i]==3 | dt$resp_ageabn[i]==4,1,0))
    dt$clin_ageabn[i] = ifelse(is.na(dt$clin_ageabn[i]),NA,ifelse(dt$clin_ageabn[i]<36,1,0))
  }
  return(dt)
}

#scoring C3 and C4
finding_max = function(new_var,var1,var2){
  new_var = NA
  for(i in 1:max(length(var1),length(var2))){
    if(!is.na(var1[i]) & !is.na(var2[i])){
      new_var[i] = max(var1[i],var2[i],na.rm=T)
    }
    else{
      i = i+1
    }
  }
  return(new_var)
}

#total_score
adi_totals = function(dt){
  for(i in 1:nrow(dt)){
    if(dt$age[i]<48){
      dt$totalA[i] = sum(dt[i,a_items],na.rm=F)
      dt$totalVB[i] = sum(dt[i,b_v_items],na.rm=F)
      dt$totalNVB[i] = sum(dt[i,b_nv_items],na.rm=F)
      dt$totalC[i] = sum(dt[i,c(c1_c2_items,"C3","C4")],na.rm=F)
      #dt$totalD[i] = sum(dt[i,d_items],na.rm=F)
    }
    else{
      dt$totalA[i] = sum(dt[i,a_items_old],na.rm=F)
      dt$totalVB[i] = sum(dt[i,b_v_items_old],na.rm=F)
      dt$totalNVB[i] = sum(dt[i,b_nv_items_old],na.rm=F)
      dt$totalC[i] = sum(dt[i,c(c1_c2_items,"C3","C4")],na.rm=F)
      #dt$totalD[i] = sum(dt[i,d_items],na.rm=F)
    }
  }
  return(dt)
}

#aligning section B NV&V scores
#var1 = NV & var2 = V
totalB_calc = function(dt,var1,var2){
  dt$totalB = NA
  dt = merge(dt,adi[,c(1:2,grep("overallang",names(adi)))],all.x=T)
  for(i in 1:nrow(dt)){
    if(!is.na(dt$overallang[i]) & dt$overallang[i]>0){
      dt$totalB[i] = var1[i] #Non-Verbal
      dt$missing_data_comment[i] = gsub("[A-Za-z]{0,3}[:space:]{0,1}[0-9]{1,3}% of B_Verbal algorithm items; ","",dt$missing_data_comment[i])
    }
    else if(!is.na(dt$overallang[i]) & dt$overallang[i]==0){
      dt$totalB[i] = var2[i] #Verbal
      dt$missing_data_comment[i] = gsub("[A-Za-z]{0,3}[:space:]{0,1}[0-9]{1,3}% of B_NonVerbal algorithm items; ","",dt$missing_data_comment[i])
    }
    # else if(!is.na(var1[i])&!is.na(var2[i])){
    #   if(var1[i] != var2[i]){
    #     dt$totalB[i] = max(var1[i],var2[i])
    #     dt$missing_data_comment[i] = gsub("[A-Za-z]{0,3}[:space:]{0,1}[0-9]{1,3}% of B_NonVerbal algorithm items; ","",dt$missing_data_comment[i])
    #   }
    #   else{
    #     dt$totalB[i] = var1[i]
    #     dt$missing_data_comment[i] = gsub("[A-Za-z]{0,3}[:space:]{0,1}[0-9]{1,3}% of B_Verbal algorithm items; ","",dt$missing_data_comment[i])
    #   }
    # }
  }
  #dt$overallang = NULL
  return(dt)
}
