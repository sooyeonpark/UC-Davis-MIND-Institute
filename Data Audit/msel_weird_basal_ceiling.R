msel_weird_basal_ceiling = data.frame(1,1)
names(msel_weird_basal_ceiling) = c("id","visit")
msel_weird_basal_ceiling$where = ''
mullen$where=''
for(i in 1:nrow(mullen)){
  vr_na_length = which(is.na(mullen[i,vr_itemset]))
  vr_omitted_length = which(diff(vr_na_length)>1)#finding an item number where there is jump in NA values
  fm_na_length = which(is.na(mullen[i,fm_itemset]))
  fm_omitted_length = which(diff(fm_na_length)>1)
  rl_na_length = which(is.na(mullen[i,rl_itemset]))
  rl_omitted_length = which(diff(rl_na_length)>1)
  el_na_length = which(is.na(mullen[i,el_itemset]))
  el_omitted_length = which(diff(el_na_length)>1)
  # print(vr_omitted_length)
  # print(fm_omitted_length)
  # print(rl_omitted_length)
  # print(el_omitted_length)
  if(length(vr_omitted_length) != 1 | length(fm_omitted_length) != 1
     | length(rl_omitted_length) != 1 | length(el_omitted_length)!=1){
    if(length(vr_omitted_length) != 1){
      mullen$where[i] = paste0(mullen$where[i],'vr, ')
    }
    if(length(fm_omitted_length) != 1){
      mullen$where[i] = paste0(mullen$where[i],'fm, ')
    }
    if(length(rl_omitted_length) != 1){
      mullen$where[i] = paste0(mullen$where[i],'rl, ')
    }
    if(length(el_omitted_length) != 1){
      mullen$where[i] = paste0(mullen$where[i],'el')
    }
    msel_weird_basal_ceiling = rbind(msel_weird_basal_ceiling,mullen[i,c(1:2,ncol(mullen))])
  }
}
msel_weird_basal_ceiling$where = gsub(", $","",msel_weird_basal_ceiling$where)
