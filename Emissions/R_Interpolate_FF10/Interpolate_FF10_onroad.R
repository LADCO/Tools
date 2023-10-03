#R Code to read in NONPOINT FF10 files and interpolate between the two files 
#DESC "country_cd","region_cd","tribal_code","census_tract_cd","shape_id","scc","emis_type","poll","ann_value","ann_pct_red","control_ids","control_measures","current_cost","cumulative_cost","projection_factor","reg_codes","calc_method","calc_year","date_updated","data_set_id","jan_value","feb_value","mar_value","apr_value","may_value","jun_value","jul_value","aug_value","sep_value","oct_value","nov_value","dec_value","jan_pctred","feb_pctred","mar_pctred","apr_pctred","may_pctred","jun_pctred","jul_pctred","aug_pctred","sep_pctred","oct_pctred","nov_pctred","dec_pctred","comment"
      
# Read CSV into R

library(dplyr)

# Set input/output directories and files
diroot <- "/Users/zac/WorkProjects/Projects/LADCO_2016_Modeling/FF10/ff10_to_interp/"
based <- paste(diroot,"2016ff_16j/inputs/nonroad/",sep="")
projd <- paste(diroot,"2023ff_16j/inputs/nonroad/",sep="")
basef <- paste(based,"2016fc_california_nonroad_07jun2017_v1.csv",sep="")
projf <- paste(projd,"2023v3_california_nonroad_annual_05nov2018_v3.csv",sep="")

outf <- paste(based,"2020_california_nonroad_annual_09Aug2019.csv",sep="")

# Read in the FF10 nonpoint inventory files and add column headers
base.df <- read.csv(file=basef,header=F,comment.char = '#',sep=",")
proj.df <- read.csv(file=projf,header=F,comment.char = '#',sep=",")
colnames(base.df)<-c("country_cd","region_cd","tribal_code","census_tract_cd","shape_id","scc","emis_type","poll","ann_value","ann_pct_red","control_ids","control_measures","current_cost","cumulative_cost","projection_factor","reg_codes","calc_method","calc_year","date_updated","data_set_id","jan_value","feb_value","mar_value","apr_value","may_value","jun_value","jul_value","aug_value","sep_value","oct_value","nov_value","dec_value","jan_pctred","feb_pctred","mar_pctred","apr_pctred","may_pctred","jun_pctred","jul_pctred","aug_pctred","sep_pctred","oct_pctred","nov_pctred","dec_pctred","comment")
colnames(proj.df)<-c("country_cd","region_cd","tribal_code","census_tract_cd","shape_id","scc","emis_type","poll","ann_value","ann_pct_red","control_ids","control_measures","current_cost","cumulative_cost","projection_factor","reg_codes","calc_method","calc_year","date_updated","data_set_id","jan_value","feb_value","mar_value","apr_value","may_value","jun_value","jul_value","aug_value","sep_value","oct_value","nov_value","dec_value","jan_pctred","feb_pctred","mar_pctred","apr_pctred","may_pctred","jun_pctred","jul_pctred","aug_pctred","sep_pctred","oct_pctred","nov_pctred","dec_pctred","comment")

# Sum annual emissions across duplicate inventory entries based on FIPs (region_cd), SCC, and pollutant
inv_base.df <- base.df %>% group_by(region_cd,scc,poll,.drop=FALSE) %>% mutate(ann16=sum(ann_value,na.rm=FALSE)) %>% data.frame() 
inv_proj.df <- proj.df %>% group_by(region_cd,scc,poll) %>% mutate(ann23=sum(ann_value,na.rm=FALSE)) %>% data.frame() 
head(inv_base.df)
head(inv_proj.df)

# natural join
inv_temp.df <- merge(x=inv_base.df,y=inv_proj.df,by=c("region_cd","scc","poll"),all=FALSE)
head(inv_temp.df)
# left join
base_only.df <- merge(x=inv_base.df,y=inv_proj.df,by=c("region_cd","scc","poll"),all.x=TRUE)

# right join
#proj_only.df <- merge(x=inv_base.df,y=inv_proj.df,by=c("region_cd","scc","poll"),all.y=TRUE)

subset(base_only.df,region.cd=='US'))

# Linear interpolate between 2016 (by) and 2023 (fy) to 2020 (target)
by = 2016
fy = 2023
target = 2020
fac = ((((inv_temp.df$ann23 - inv_temp.df$ann16)/(fy-by))*(target-by))+inv_temp.df$ann16)/inv_temp.df$ann16
fac
inv_temp.df$ann20 <- fac*inv_temp.df$ann16
inv_temp.df$jan20 <- fac*inv_temp.df$jan_value.x
inv_temp.df$feb20 <- fac*inv_temp.df$feb_value.x
inv_temp.df$mar20 <- fac*inv_temp.df$mar_value.x
inv_temp.df$apr20 <- fac*inv_temp.df$apr_value.x
inv_temp.df$may20 <- fac*inv_temp.df$may_value.x
inv_temp.df$jun20 <- fac*inv_temp.df$jun_value.x
inv_temp.df$jul20 <- fac*inv_temp.df$jul_value.x
inv_temp.df$aug20 <- fac*inv_temp.df$aug_value.x
inv_temp.df$sep20 <- fac*inv_temp.df$sep_value.x
inv_temp.df$oct20 <- fac*inv_temp.df$oct_value.x
inv_temp.df$nov20 <- fac*inv_temp.df$nov_value.x
inv_temp.df$dec20 <- fac*inv_temp.df$dec_value.x
#head(inv_temp.df)
inv_temp.df$calc_year.x <- 2020
inv_temp.df$date_updated.x <- 20190809
inv_temp.df$data_set_id.x <- "LADCO_Interp_2016-2023"

write.table(inv_temp.df[,c("country_cd.x","region_cd","tribal_code.x","census_tract_cd.x","shape_id.x","scc","emis_type.x","poll","ann20","ann_pct_red.x","control_ids.x","control_measures.x","current_cost.x","cumulative_cost.x","projection_factor.x","reg_codes.x","calc_method.x","calc_year.x","date_updated.x","data_set_id.x","jan20","feb20","mar20","apr20","may20","jun20","jul20","aug20","sep20","oct20","nov20","dec20","jan_pctred.x","feb_pctred.x","mar_pctred.x","apr_pctred.x","may_pctred.x","jun_pctred.x","jul_pctred.x","aug_pctred.x","sep_pctred.x","oct_pctred.x","nov_pctred.x","dec_pctred.x","comment.x")], sep=",",file=outf,col.names=FALSE,row.names=FALSE,na="")




