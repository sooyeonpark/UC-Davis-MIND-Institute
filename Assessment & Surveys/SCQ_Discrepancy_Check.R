library(RODBC)
library(sqldf)
con3 <- odbcConnectAccess2007("S:/MIND/RESEARCH/APP/ANALYSIS/APP Behavior Scores Jun 13 2013.accdb")
scq_score = sqlQuery(con3,"select * from SCQ_SCORE;")
scq$id %in% scq_score$ID

scq_discrepancy = sqldf("select t1.id,t1.visit,t1.scq_age,t2.AGE_SCQ,t1.total,t2.SCQ_TOTAL
                        from scq as t1 left join scq_score as t2 on t1.id = t2.ID and t1.visit = t2.VISIT
                        where (t2.AGE_SCQ <> cast(t1.scq_age as int) or t1.total <> t2.SCQ_TOTAL)")
