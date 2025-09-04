autoimmune_child_never_endorsed = c('child')
for(j in 7:110){
  if(all(tolower(ai_child[,j]) != "yes",na.rm=T) & !grepl("_age",names(ai_child)[j])){
    autoimmune_child_never_endorsed = c(autoimmune_child_never_endorsed,names(ai_child)[j])
  }
}
autoimmune_child_never_endorsed = autoimmune_child_never_endorsed[-1]
autoimmune_child_never_endorsed = gsub("^autoimmune_child_","",autoimmune_child_never_endorsed)

autoimmune_father_never_endorsed = c('father')
for(j in 6:110){
  if(all(tolower(ai_fat[,j]) != "yes",na.rm=T) & !grepl("_age",names(ai_fat)[j])){
    autoimmune_father_never_endorsed = c(autoimmune_father_never_endorsed,names(ai_fat)[j])
  }
}
autoimmune_father_never_endorsed = autoimmune_father_never_endorsed[-1]
autoimmune_father_never_endorsed = gsub("^autoimmune_fat_","",autoimmune_father_never_endorsed)

autoimmune_mother_never_endorsed = c('mother')
for(j in 6:110){
  if(all(tolower(ai_mth[,j]) != "yes",na.rm=T) & !grepl("_age",names(ai_mth)[j])){
    autoimmune_mother_never_endorsed = c(autoimmune_mother_never_endorsed,names(ai_mth)[j])
  }
}
autoimmune_mother_never_endorsed = autoimmune_mother_never_endorsed[-1]
autoimmune_mother_never_endorsed = gsub("^autoimmune_mth_","",autoimmune_mother_never_endorsed)

autoimmune_pgp_never_endorsed = c('pgp')
for(j in 6:162){
  if(all(tolower(ai_pgp[,j]) != "yes",na.rm=T) & !grepl("_age",names(ai_pgp)[j])){
    autoimmune_pgp_never_endorsed = c(autoimmune_pgp_never_endorsed,names(ai_pgp)[j])
  }
}
autoimmune_pgp_never_endorsed = autoimmune_pgp_never_endorsed[-1]
autoimmune_pgp_never_endorsed = gsub("^autoimmune_pgp_","",autoimmune_pgp_never_endorsed)

autoimmune_mgp_never_endorsed = c('mgp')
for(j in 6:162){
  if(all(tolower(ai_mgp[,j]) != "yes",na.rm=T) & !grepl("_age",names(ai_mgp)[j])){
    autoimmune_mgp_never_endorsed = c(autoimmune_mgp_never_endorsed,names(ai_mgp)[j])
  }
}
autoimmune_mgp_never_endorsed = autoimmune_mgp_never_endorsed[-1]
autoimmune_mgp_never_endorsed = gsub("^autoimmune_mgp_","",autoimmune_mgp_never_endorsed)

autoimmune_prel_never_endorsed = c('prel')
for(j in 6:162){
  if(all(tolower(ai_prel[,j]) != "yes",na.rm=T) & !grepl("_age",names(ai_prel)[j])){
    autoimmune_prel_never_endorsed = c(autoimmune_prel_never_endorsed,names(ai_prel)[j])
  }
}
autoimmune_prel_never_endorsed = autoimmune_prel_never_endorsed[-1]
autoimmune_prel_never_endorsed = gsub("^autoimmune_prel_","",autoimmune_prel_never_endorsed)

autoimmune_mrel_never_endorsed = c('mrel')
for(j in 6:162){
  if(all(tolower(ai_mrel[,j]) != "yes",na.rm=T) & !grepl("_age",names(ai_mrel)[j])){
    autoimmune_mrel_never_endorsed = c(autoimmune_mrel_never_endorsed,names(ai_mrel)[j])
  }
}
autoimmune_mrel_never_endorsed = autoimmune_mrel_never_endorsed[-1]
autoimmune_mrel_never_endorsed = gsub("^autoimmune_mrel_","",autoimmune_mrel_never_endorsed)

autoimmune_other_never_endorsed = c('other')
for(j in 6:162){
  if(all(tolower(ai_other[,j]) != "yes",na.rm=T) & !grepl("_age",names(ai_other)[j])){
    autoimmune_other_never_endorsed = c(autoimmune_other_never_endorsed,names(ai_other)[j])
  }
}
autoimmune_other_never_endorsed = autoimmune_other_never_endorsed[-1]
autoimmune_other_never_endorsed = gsub("^autoimmune_other_","",autoimmune_other_never_endorsed)

autoimmune_never_endorsed = autoimmune_child_never_endorsed[which(autoimmune_child_never_endorsed %in% autoimmune_father_never_endorsed
      & autoimmune_child_never_endorsed %in% autoimmune_mother_never_endorsed
      & autoimmune_child_never_endorsed %in% autoimmune_mgp_never_endorsed
      & autoimmune_child_never_endorsed %in% autoimmune_pgp_never_endorsed
      & autoimmune_child_never_endorsed %in% autoimmune_mrel_never_endorsed
      & autoimmune_child_never_endorsed %in% autoimmune_prel_never_endorsed
      & autoimmune_child_never_endorsed %in% autoimmune_other_never_endorsed)]
write.csv(autoimmune_never_endorsed,"Med/autoimmune_never_endorsed.csv",row.names=F)
rm(autoimmune_child_never_endorsed,autoimmune_father_never_endorsed,autoimmune_mgp_never_endorsed,
   autoimmune_mother_never_endorsed,autoimmune_mrel_never_endorsed,autoimmune_other_never_endorsed,
   autoimmune_pgp_never_endorsed,autoimmune_prel_never_endorsed,autoimmune_never_endorsed)
