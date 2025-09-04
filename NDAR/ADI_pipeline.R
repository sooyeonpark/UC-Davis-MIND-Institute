adi_pipeline <- function(tables, ...){
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
  base = dataportal::left_join_tables(tbl_participant, tbl_visit)
  base = dataportal::left_join_tables(base, tables$tbl_adi)

  res = dataportal::remap_table(base, 'NDAR')
  # res$data <- dplyr::mutate(res$data, 
  #                           family_member = if_else(family_member == 'mother', 'MTH',
  #                                                  'FTH'),
  #                           )
  table_data = data.frame(res$data)
  data_dic = data.frame(meta$source$data_dictionary$data)
  res$export_write_function = function(table, ...){
    data.table::fwrite(to_local_df(table_data[!is.na(table_data$subjectkey),names(table_data) %in% data_dic$EXPORT_NDAR_COLUMN_NAME]), file='adi.csv', ...)
    data.table::fwrite(to_local_df(table$data_dictionary$`data`), file='adi_dictionary.csv', sep="\t",...)
    return(c('adi.csv', 'adi_dictionary.csv'))
  }
  res$data = as_tibble(table_data[!is.na(table_data$subjectkey),names(table_data) %in% data_dic$EXPORT_NDAR_COLUMN_NAME])
  # res$selected_columns = names(table_data[!is.na(table_data$subjectkey),names(table_data) %in% data_dic$EXPORT_NDAR_COLUMN_NAME])
  # res$columns = res$selected_columns
  #print(res)
  return(res)
}

adi_pipeline <- function(tables, ...){
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
  base = dataportal::left_join_tables(base, tables$tbl_adi)
  
  res = dataportal::remap_table(base, 'NDAR')
  # res$data <- dplyr::mutate(res$data, 
  #                           family_member = if_else(family_member == 'mother', 'MTH',
  #                                                  'FTH'),
  #                           )
  res$export_write_function = function(table, ...){
    data.table::fwrite(to_local_df(table$data), file='adi.csv', ...)
    data.table::fwrite(to_local_df(table$data_dictionary$data), file='adi_dictionary.csv', ...)
    return(c('adi.csv', 'adi_dictionary.csv'))
  }
  return(res)
}