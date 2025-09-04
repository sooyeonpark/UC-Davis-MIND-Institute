# RANDO exports
RANDO1 <- function(tables, ...){
    tbl_subject <- tables$tbl_subject
    tbl_visit <- tables$tbl_visit 
    
    tbl_subject$selected_columns <- c('risk_status', 'gender', 'date_of_birth')
    
    tbl_visit$selected_columns <- c('visit', 'age_at_visit')
    
    
    tbl_visit <- dataportal::filter_table(tables$tbl_visit, 
                                          filter_variable = 'visit', 
                                          filter_values   = 36,
                                          update_filter_values=FALSE)
    
    base <- dataportal::inner_join_tables_list(list(tbl_subject, tbl_visit))
    base <- dataportal::left_join_tables_to_base(base, list(tbl_visit))
    base <- dataportal::filter_null_key(base, key_variable='subj_id')
    
    res <- dataportal::remap_table(base, 'RANDO')
    
    res$export_write_function <- function(table, ...){
        data.table::fwrite(to_local_df(table$data), file='subject_visit_list.csv', ...)
        data.table::fwrite(to_local_df(table$data_dictionary$data), file='subject_visit_list_dictionary.csv', ...)
        return(c('subject_visit_list.csv', 'subject_visit_list_dictionary.csv'))
    }
    return(res)
}

RANDO2 <- function(tables, ...){
    tbl_subject <- tables$tbl_subject
    tbl_visit <- tables$tbl_visit 
    
    tbl_visit <- dataportal::filter_table(tables$tbl_visit, 
                                          filter_variable = 'visit', 
                                          filter_values   = c(36, 24),
                                          update_filter_values=FALSE) %>%
    dataportal::filter_table(filter_variable = 'other_vaar', 
                                           filter_values   = c('f', 's'),
                                           update_filter_values=FALSE)
    
    base <- dataportal::inner_join_tables_list(list(tbl_subject, tbl_visit))
    base <- dataportal::left_join_tables(base, tables$tbl_ada)
    res <- dataportal::remap_table(base, 'RANDO')
    
    res$export_write_function <- function(table, ...){
        data.table::fwrite(to_local_df(table$data), file='ada_36mo.csv', ...)
        data.table::fwrite(to_local_df(table$data), file='ada_36mo_dictionary.csv', ...)
        return(c('ada_36mo.csv', 'ada_36mo_dictionary.csv'))
    }
    return(res)
}