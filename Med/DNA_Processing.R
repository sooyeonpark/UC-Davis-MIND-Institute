dna <- read_excel("Med/APP DNA 072522.xlsx")
names(dna) = c("protocol","visit","id","sample_id","trio","date")
dna = dna[,-1]
dna$visit = gsub("^Y","",dna$visit)
dna$trio = ifelse(is.na(dna$trio),"False","True")

dna$phase = ifelse(grepl("[A-Z]+",dna$id),"Adult","Child")
dna_child = dna[dna$phase=="Child",1:5]
dna_parent = dna[dna$phase=="Adult",1:5]

dna_child = fxage(dna_child,'id','date')
dna_parent = merge(dna_parent,parents[,1:2],by.x="id",by.y="parent_id",all.x=T)
dna_parent = fxage_parent(dna_parent,'id','parent_id','date')
dna_parent = dna_parent[,-grep("^id$",names(dna_parent))] 
names(dna_parent)[grep("subj_id",names(dna_parent))] = "id"
dna_dad = dna_parent[dna_parent$resp=="Father",]
dna_mom = dna_parent[dna_parent$resp=="Mother",]

dna_child = inserting_prefix_into_variables(dna_child,'dna_')
dna_dad = inserting_prefix_into_variables(dna_dad,'dna_fat_')
dna_mom = inserting_prefix_into_variables(dna_mom,'dna_mth_')

dna_orphaned_data = orphaned_data_consolidate(dna_child)
dna_child = orphaned_data_remove(dna_child)
dna_dad_orphaned_data = orphaned_data_consolidate(dna_dad)
dna_dad = orphaned_data_remove(dna_dad)
dna_mom_orphaned_data = orphaned_data_consolidate(dna_mom)
dna_mom = orphaned_data_remove(dna_mom)

dna_child_processed = dna_child[,c(grep("^id",names(dna_child)),grep("visit",names(dna_child)),
                                   grep("sample",names(dna_child)),grep("_age$",names(dna_child)),grep("trio",names(dna_child)))]
dna_dad_processed = dna_dad[,c(grep("^id",names(dna_dad)),grep("visit",names(dna_dad)),
                                 grep("sample",names(dna_dad)),grep("_age$",names(dna_dad)),grep("trio",names(dna_dad)))]
dna_mom_processed = dna_mom[,c(grep("^id",names(dna_mom)),grep("visit",names(dna_mom)),
                               grep("sample",names(dna_mom)),grep("_age$",names(dna_mom)),grep("trio",names(dna_mom)))]
dna_processed = merge(dna_child_processed,dna_dad_processed,by=c("id","visit"),all=T)
dna_processed = merge(dna_processed,dna_mom_processed,by=c("id","visit"),all=T)
dna_processed = unique(dna_processed)
write.csv(dna_processed,"S:/MIND/RESEARCH/APP Behavior Data/ACE data R processing/archived/dna_processed.csv",row.names=F)

rm(dna_mom,dna_dad,dna_parent,dna,dna_child)
