library(dataportal)
options(dataportal.config.source = "C:/Users/psypa/Documents/Jobs/MIND/ACE/ace_source.yml")
options(dataportal.config.tables = "S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/ace_tables.yml")

meta = init_meta()

#checking which file creates an error
# pipeline_fx <- list.files('S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/NDAR', full.names=TRUE, pattern='.R$')
# res <- vector('list', length(pipeline_fx))
# names(res) <- pipeline_fx
# for (f in pipeline_fx){
#   .res <- tryCatch(source(f, echo=FALSE, print.eval=FALSE), 
#                    error= function(e){
#                      e
#                    })
#   res[[f]] <- .res                
# }
# names(Filter(function(x) inherits(x, 'error'), res))

#dput function -> use all the file names into a vector -> ex.object = dput(list.files(file_directory),"file_pattern")
source("S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/Functions/Pipeline_Loading.R")
pipeline_fx_list = list(adi_pipeline)
                        # adis_pipeline,ados_pipeline,cbcl_pipeline,ccc_pipeline)
                        # celf_pipeline,cshq_pipeline,das_pipeline,demographics_pipeline,
                        # edq_pipeline,eowpvt_pipeline,gort_pipeline,harter_pipeline,masc_pipeline,
                        # masc_parent_pipeline,msel_pipeline,nih_toolbox_pipeline,participant_visit_pipeline,
                        # physical_exam_pipeline,ppvt_pipeline,rbs_pipeline,scared_pipeline,scared_parent_pipeline,
                        # scq_pipeline,srs_pipeline,ssp_pipeline,ssp2_pipeline,vineland_pipeline,wraml_pipeline)
ace_ndar = execute_pipeline(pipeline_fx_list,tables=meta$tables)
write_pipeline_tables(ace_ndar)

source_disconnect(meta)
rm(pipeline_fx_list,ace_ndar,meta)
