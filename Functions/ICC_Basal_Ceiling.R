basal_ceiling = function(dt,item_list,na_index){
  if(length(item_list)!=length(na_index)){
    if(length(na_index) != 0 & 1 %in% na_index){
      if(all(diff(na_index)==1)){
        dt[i,item_list[na_index]] = 2
      }
      else{
        jump = which(diff(na_index)>1)[1]
        dt[i,item_list[1:jump]] = 2
      }
    }
  }
  return(dt)
}

basal_ceiling = function(dt,item_list){
  for(i in 1:nrow(dt)){
    dt[i,item_list] = toupper(dt[i,item_list])
    dt[i,item_list[which(dt[i,item_list]=='X')]] = 'N'
    ##checking ceiling
    #check how many N's are staying together and split apart
    n_index = which(dt[i,item_list]=='N')
    n_index_diff = diff(n_index)
    #if there are two more of n_index_diff==1, that's where the ceiling lies
    possible_ceiling = which(n_index_diff==1)
    ceiling_confirm = possible_ceiling[which.min(diff(possible_ceiling))]
    ceiling = n_index[ceiling_confirm]
    #IF there is ceiling, assigning all the items after ceiling, N
    if(length(ceiling)!=0){
      dt[i,item_list[ceiling:length(item_list)]]='N'
    }
    ##checking basal
    na_index = which(is.na(dt[i,item_list]))
    a_index = which(dt[i,item_list]=='A')
    a_index_diff = diff(a_index)
    #if there are two more of a_index_diff==1, that's where the basal lies
    possible_basal = which(a_index_diff==1)
    basal_confirm = possible_basal[which.min(diff(possible_basal))]
    basal = a_index[basal_confirm]
    if(length(basal) != 0){
      #NA items below the basal become A
      dt[i,item_list[na_index][na_index<basal]] = 'A'
    }
  }
  return(dt)
}
