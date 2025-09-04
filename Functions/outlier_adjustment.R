#generating outliers within a column
generating_outliers = function(dt){
  nums <- unlist(lapply(dt, is.numeric))
  test = data.frame(dt[ , nums])
  initial_num_col = dim(test)[2]
  #create each column with outliers
  for(i in 1:ncol(test)){
    test[,(initial_num_col+i)]=""
    for(j in 1:length(boxplot.stats(test[,i])$out)){
      test[which(test[,i]==boxplot.stats(test[,i])$out[j]),(initial_num_col+i)]=
        paste0(names(test)[i],"; ")
    }
  }
  
  #create a new column at the end with all the previous outliers
  #putting all together in outlier_list column
  test$outlier_list = ""
  for(i in (initial_num_col+1):(ncol(test)-1)){
    for(j in 1:nrow(test)){
      test$outlier_list[j] = paste0(test$outlier_list[j],test[j,i])
    }
  }
  
  dt = cbind(dt,test$outlier_list)
  names(dt)[ncol(dt)]="outlier_list"
  dt$outlier_list = as.character(dt$outlier_list)
  
  return(dt)
}

#finding outliers by adjusting visit
outlier_list = function(dt){
  dt$outlier_list = ""
  split_by_visit = split(dt,as.factor(dt[,"visit"]))
  for(i in 1:length(split_by_visit)){
    split = as.data.frame(split_by_visit[[i]])
    #getting rid of empty outlier_list column
    split$outlier_list=NULL
    #finding outliers within the visit
    split = generating_outliers(split)
    #merge dt with split table
    dt = merge(dt,split,by=names(dt)[1:(ncol(dt)-1)],all.x=T)
    #paste the outliers using outlier_list.x and outlier_list.y columns through paste0 -> new outlier_list column
    dt$outlier_list = paste0(dt$outlier_list.x,dt$outlier_list.y)
    #gsubing NA out from outlier_list col
    dt$outlier_list = gsub("NA","",dt$outlier_list)
    #getting rid of outlier_list.x and outlier_list.y cols
    dt = dt[,-c(which(names(dt)=="outlier_list.x"),which(names(dt)=="outlier_list.y"))]
  }
  return(dt)
}

outlier_proportion_calc = function(dt,dt2,prefix,prop_table){
  #dt = outlier_table
  #dt2 = reference table where all data are available
  dt$outlier_list = gsub(paste0(prefix,'_age; '),'',dt$outlier_list)
  prop_outlier = round(length(which(dt$outlier_list != ""))/nrow(dt2)*100,2)
  prop_outlier = as.data.frame(prop_outlier)
  rownames(prop_outlier)=toupper(prefix)
  prop_table = rbind(prop_table,prop_outlier)
  return(prop_table)
}

# outlier_prop = data.frame()
# outlier_prop = outlier_proportion_calc(adi_outlier_table,adi_scored,"adi",outlier_prop)
# outlier_prop = outlier_proportion_calc(rbind(cbcl_young_outlier_table,cbcl_old_outlier_table),cbcl_scored,"cbcl",outlier_prop)
# outlier_prop = outlier_proportion_calc(ados_outlier_table,ados_scored,"ados",outlier_prop)
# outlier_prop = outlier_proportion_calc(mullen_outlier_table,mullen_scored,"msel",outlier_prop)
# outlier_prop = outlier_proportion_calc(cbq_outlier_table,cbq_scored,'cbq',outlier_prop)
# outlier_prop = outlier_proportion_calc(ccc_outlier_table,ccc_scored,'ccc',outlier_prop)
# outlier_prop2 = data.frame()
# outlier_prop2 = outlier_proportion_calc(cdi_ws_outlier_table,cdi_ws_scored,'cdi_ws',outlier_prop2)
# outlier_prop2 = outlier_proportion_calc(celf_outlier_table,celf_processed,'celf',outlier_prop2)
# outlier_prop2 = outlier_proportion_calc(cshq_outlier_table,cshq_scored,'cshq',outlier_prop2)
# outlier_prop2 = outlier_proportion_calc(das_outlier_table,das_scored,'das',outlier_prop2)
# outlier_prop2 = outlier_proportion_calc(edq_outlier_table,edq_scored,'edq',outlier_prop2)
# outlier_prop2 = outlier_proportion_calc(rbind(eowpvt3_outlier_table,eowpvt4_outlier_table),eowpvt,'eowpvt',outlier_prop2)
# outlier_prop3 = data.frame()
# outlier_prop3 = outlier_proportion_calc(ppvt_outlier_table,ppvt_scored,'ppvt',outlier_prop3)
# outlier_prop3 = outlier_proportion_calc(rbs_outlier_table,rbs_scored,'rbs',outlier_prop3)
# outlier_prop3 = outlier_proportion_calc(scq_outlier_table,scq_scored,'scq',outlier_prop3)
# outlier_prop3 = outlier_proportion_calc(srs_outlier_table,srs_scored,'srs',outlier_prop3)
# outlier_prop3 = outlier_proportion_calc(ssp_outlier_table,ssp_scored,'ssp',outlier_prop3)
# outlier_prop3 = outlier_proportion_calc(ssp2_outlier_table,ssp2_scored,'ssp2',outlier_prop3)
# outlier_prop3 = outlier_proportion_calc(vabs_outlier_table,vabs_scored,'vine',outlier_prop3)
# 
# #outlier_prop = outlier_prop[-c(),,drop=F]
# barplot(t(outlier_prop),main="Proportion of Outliers in Test Measures",ylim=c(0,20),ylab="Percentage of Outliers")
# barplot(t(outlier_prop2),main="Proportion of Outliers in Test Measures",ylim=c(0,20),ylab="Percentage of Outliers")
# barplot(t(outlier_prop3),main="Proportion of Outliers in Test Measures",ylim=c(0,20),ylab="Percentage of Outliers")
# 
# outlier_prop_tot = rbind(outlier_prop,outlier_prop2,outlier_prop3)
# write.csv(outlier_prop_tot,"data core talk/outlier_proportion_table.csv")
# rm(outlier_prop,outlier_prop2,outlier_prop3,outlier_prop_tot)
