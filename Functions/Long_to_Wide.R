
setwd('S:\\MIND\\RESEARCH\\APP Behavior Data\\Long_to_Wide\\')
library(dplyr)
library(tcltk)
library(reshape2)
#setwd('/home/cory/R/merge')


old_filename<-tk_choose.files(default='S:\\MIND\\RESEARCH\\APP Behavior Data\\Long_to_Wide\\*.csv',caption = "Choose the LONG FORM  query result.!")

old <- read.csv(old_filename)

UID_field='subj_id'
TimePoint_field='visit'

outdata=reshape(old, idvar='subj_id' , timevar = "visit" , direction="wide")



default_outfile<-paste0('WideForm_ConvertedFromLongForm_',timestamp,'.csv')

val <- tkgetSaveFile(initialfile=default_outfile, title="Save a file...", filetypes = "{ {CSV Files} {.csv} } { {All Files} * }")
outfile <- tclvalue(val)
if (outfile != "") { write.csv(outdata,outfile,na="") 
  } else print("Cancelled -- no save filename provided")

