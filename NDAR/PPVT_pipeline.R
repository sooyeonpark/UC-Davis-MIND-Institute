ppvt_pipeline <- function(tables, ...){
  tbl_participant = tables$tbl_participants
  tbl_visit = tables$tbl_visit 
  # 
  # tbl_visit <- dataportal::filter_table(tables$tbl_visit, 
  #                                       filter_variable = 'visit', 
  #                                       filter_values   = c(36, 24),
  #                                       update_filter_values=FALSE) %>%
  #   dataportal::filter_table(filter_variable = 'other_vaar', 
  #                            filter_values   = c('f', 's'),
  #                            update_filter_values=FALSE)
  # 
  base = dataportal::inner_join_tables_list(list(tbl_participant, tbl_visit))
  base = dataportal::left_join_tables(base, tables$tbl_ppvt)
  res = dataportal::remap_table(base, 'NDAR')
  
  res$export_write_function = function(table, ...){
    data.table::fwrite(to_local_df(table$data), file='ppvt.csv', ...)
    data.table::fwrite(to_local_df(table$data), file='ppvt_dictionary.csv', ...)
    return(c('ppvt.csv', 'ppvt_dictionary.csv'))
  }
  return(res)
}