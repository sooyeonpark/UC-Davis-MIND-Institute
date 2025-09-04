#### library and options ####
library(RMariaDB)
library(keyring)
library(readxl)
options(stringsAsFactors = FALSE)

#### read in multivariate_outlier_analysis_list ####
mvars <- read_xlsx('S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/Data Audit/multivariate_outlier_analysis_list.xlsx')
names(mvars) <- c('var1','tbl1','var2','tbl2','join_on','where_condition','test_name')

#### build list of commands ####
cmds <- vector("list",nrow(mvars))
for(i in seq(nrow(mvars))){
  sqlstr <- paste0("dbGetQuery(con,'SELECT tbl1.id, tbl1.visit, tbl1.",
                   mvars[i,]$var1," AS var1, tbl2.",
                   mvars[i,]$var2," AS var2 FROM ",
                   mvars[i,]$tbl1, " AS tbl1 LEFT JOIN ", 
                   mvars[i,]$tbl2, " AS tbl2 ON ", 
                   mvars[i,]$join_on, 
                   ifelse(!is.na(mvars[i,]$where_condition) & nchar(mvars[i,]$where_condition)>0,
                          paste0(" WHERE ",mvars[i,]$where_condition,";')"),
                          ";')")
                   )
  sqlstr <- gsub('tbl1.tbl1.','tbl1.',sqlstr)
  sqlstr <- gsub('tbl2.tbl2.','tbl2.',sqlstr)
  cmds[[i]] <- sqlstr
}

#### open connection and evaluate sql statements ####
con <- dbConnect(MariaDB(),user='young',dbname='ACE',host='localhost',port=3333,password=key_get('ACE',user='young'))

datalist <- vector("list",length(cmds)) # <-- creates list object to store data frames created through queries to the MySQL database
for(i in seq(length(cmds))){
  datalist[[i]] <- eval(parse(text=cmds[[i]]))
}

names(datalist) <- mvars$test_name # <-- name each dataframe with the name given in the mvars file

dbDisconnect(con)

# filter only for those rows with both var1 and var2 values
for(i in seq(length(datalist))){
  datalist[[i]] <- datalist[[i]][which(!is.na(datalist[[i]]$var1) & !is.na(datalist[[i]]$var2)),]
}

# change each var to integer
for(i in seq(length(datalist))){
  datalist[[i]]$var1 <- as.integer(datalist[[i]]$var1)
  datalist[[i]]$var2 <- as.integer(datalist[[i]]$var2)
}

#### run regression diagnostics for each test_name var1 by var2 in mvars list ####
lmdx <- vector("list", length(datalist)) # <-- create object to house all regression analysis objects

for(i in seq(length(datalist))){
  lmdx[[i]] <- lm(var1 ~ var2, data=datalist[[i]])
}
names(lmdx) <- names(datalist)

# get cooksd from lm models
cooksd <- vector("list",length(lmdx))
for(i in seq(length(lmdx))){
  cooksd[[i]] <- cooks.distance(lmdx[[i]])
}

names(cooksd) <- names(lmdx)

# get influential points as row numbers
infl <- vector("list",length(cooksd))
for(i in seq(length(cooksd))){
  infl[[i]] <- as.numeric(names(cooksd[[i]])[(cooksd[[i]] > (4/nrow(datalist[[i]])))])  #<-- NOTE:  rule of thumb is cooksd > 4/sample size
}

names(infl) <- names(cooksd)


# create influence indicator for data points in each dataset
for(i in seq(length(infl))){
  datalist[[i]]$infl <- ifelse(row.names(datalist[[i]]) %in% infl[[i]],1,0)
}



# create z-scores for var1 and var2 and diff between max z-score and min z-score
for(i in seq(length(datalist))){
  datalist[[i]]$var1z <- scale(datalist[[i]]$var1)
  datalist[[i]]$var2z <- scale(datalist[[i]]$var2)
  datalist[[i]]$z_diff <- apply(datalist[[i]][,c('var1z','var2z')], 1, max) - apply(datalist[[i]][,c('var1z','var2z')], 1, min)
}


for(i in seq(length(datalist))){
  datalist[[i]]$var1_name <- mvars[i,]$var1
  datalist[[i]]$var2_name <- mvars[i,]$var2
  datalist[[i]]$table1_name <- mvars[i,]$tbl1
  datalist[[i]]$table2_name <- mvars[i,]$tbl2
  datalist[[i]]$test_name <- mvars[i,]$test_name
}

for(i in seq(length(datalist))){
  if(i==1){
    within_subj_outliers <- datalist[[i]]
  } else {
      within_subj_outliers <- rbind(within_subj_outliers,datalist[[i]])
    }
}


#### clean up ####
rm(cmds,cooksd,infl,lmdx,mvars,datalist)
rm(con)
rm(i,sqlstr)

# calculate flag for z-score diff:  Start with 2
within_subj_outliers$z_flag <- ifelse(within_subj_outliers$z_diff>=2,1,0)
within_subj_outliers$outlier <- ifelse(within_subj_outliers$z_flag==1 & within_subj_outliers$infl==1,1,0)


#### plots (for exporting) ####

testname <- 'eowpvt.vine.el.ae' # <---note:  just change the testname variable here to plot a particular test

plot(within_subj_outliers[which(within_subj_outliers$test_name==testname),]$var1, 
     within_subj_outliers[which(within_subj_outliers$test_name==testname),]$var2, 
     col=factor(within_subj_outliers[which(within_subj_outliers$test_name==testname),]$outlier),
     xlab=unique(within_subj_outliers[which(within_subj_outliers$test_name==testname),]$var1_name),
     ylab=unique(within_subj_outliers[which(within_subj_outliers$test_name==testname),]$var2_name),
     main=unique(within_subj_outliers[which(within_subj_outliers$test_name==testname),]$test_name))

rm(testname)

# report outliers vs. non (handy for calculating percentage found (see above where threshold is 4/n for infl and 2 for z_diff))
total_outliers <- as.data.frame.matrix(table(within_subj_outliers$test_name, within_subj_outliers$outlier))
total_outliers$test_name <- row.names(total_outliers)
row.names(total_outliers) <- 1:nrow(total_outliers)
names(total_outliers) <- c('n_non_outliers','n_outliers', 'test_name')
total_outliers$percent_outliers <- (total_outliers$n_outliers/(total_outliers$n_non_outliers + total_outliers$n_outliers))*100
total_outliers <- total_outliers[,c('test_name','n_outliers','n_non_outliers','percent_outliers')]

#### CHECK what we have already checked!!!####
# 

#### filter and export outlier data ####

write.csv(within_subj_outliers[which(within_subj_outliers$outlier==1),],'Within_Subject_Outliers.csv', na='',row.names=F)
write.csv(total_outliers, "Within_Subject_Outliers_Summary.csv",na='',row.names=F)
