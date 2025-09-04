cdi_wg_section_1cn2a_score = function(itemscore){
  itemscore = ifelse(is.na(itemscore), NA, ifelse(itemscore=="Often" | itemscore =="Sometimes", 1, 0))
  return(itemscore)
}

cdi_wg_section_1d_score = function(itemscore){
  itemscore <- ifelse(is.na(itemscore), NA, ifelse(itemscore>=0 & itemscore < 396, itemscore, NA))
  return(itemscore)
}

cdi_ws_section_e_score = function(itemscore){
  itemscore = ifelse(is.na(itemscore),NA,ifelse(grepl("^2.",itemscore),1,0))
  return(itemscore)
}
