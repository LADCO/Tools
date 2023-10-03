#R Code to read in NONROAD FF10 files and interpolate between the two files 
#DESC "country_cd","region_cd","tribal_code","census_tract_cd","shape_id","scc","emis_type","poll","ann_value","ann_pct_red","control_ids","control_measures","current_cost","cumulative_cost","projection_factor","reg_codes","calc_method","calc_year","date_updated","data_set_id","jan_value","feb_value","mar_value","apr_value","may_value","jun_value","jul_value","aug_value","sep_value","oct_value","nov_value","dec_value","jan_pctred","feb_pctred","mar_pctred","apr_pctred","may_pctred","jun_pctred","jul_pctred","aug_pctred","sep_pctred","oct_pctred","nov_pctred","dec_pctred","comment"
      
library(dplyr)

# Set input/output directories and files
diroot <- "/Users/zac/WorkProjects/Projects/LADCO_2016_Modeling/FF10/ff10_to_interp/"
based <- paste(diroot,"2016ff_16j/inputs/nonroad/",sep="")
projd <- paste(diroot,"2023ff_16j/inputs/nonroad/",sep="")
basef <- paste(based,"2016fc_california_nonroad_07jun2017_v1.csv",sep="")
projf <- paste(projd,"2023v3_california_nonroad_annual_05nov2018_v3.csv",sep="")

# Output file name, write to base year inventory directory
outf <- paste(based,"2020_california_nonroad_annual_09Aug2019.csv",sep="")

# Read in the FF10 inventory files and add column headers
base.df <- read.csv(file=basef,header=T,comment.char = '#',sep=",")
proj.df <- read.csv(file=projf,header=T,comment.char = '#',sep=",")
colnames(base.df)<-c("country_cd","region_cd","tribal_code","census_tract_cd","shape_id","scc","emis_type","poll","ann_value","ann_pct_red","control_ids","control_measures","current_cost","cumulative_cost","projection_factor","reg_codes","calc_method","calc_year","date_updated","data_set_id","jan_value","feb_value","mar_value","apr_value","may_value","jun_value","jul_value","aug_value","sep_value","oct_value","nov_value","dec_value","jan_pctred","feb_pctred","mar_pctred","apr_pctred","may_pctred","jun_pctred","jul_pctred","aug_pctred","sep_pctred","oct_pctred","nov_pctred","dec_pctred","comment")
colnames(proj.df)<-c("country_cd","region_cd","tribal_code","census_tract_cd","shape_id","scc","emis_type","poll","ann_value","ann_pct_red","control_ids","control_measures","current_cost","cumulative_cost","projection_factor","reg_codes","calc_method","calc_year","date_updated","data_set_id","jan_value","feb_value","mar_value","apr_value","may_value","jun_value","jul_value","aug_value","sep_value","oct_value","nov_value","dec_value","jan_pctred","feb_pctred","mar_pctred","apr_pctred","may_pctred","jun_pctred","jul_pctred","aug_pctred","sep_pctred","oct_pctred","nov_pctred","dec_pctred","comment")

# Consolidate duplicate entries in the input inventories
# Sum annual emissions across duplicate inventory entries based on FIPs (region_cd), SCC, and pollutant
inv_base.df <- base.df %>% group_by(region_cd,scc,emis_type,poll,.drop=FALSE) %>% mutate(ann16=sum(ann_value,na.rm=FALSE)) %>% data.frame() 
inv_proj.df <- proj.df %>% group_by(region_cd,scc,emis_type,poll) %>% mutate(ann23=sum(ann_value,na.rm=FALSE)) %>% data.frame() 

# Inner join: keep records that exist in both base and future
inv_temp.df <- inner_join( inv_base.df, inv_proj.df ,by=c("region_cd","scc","poll"))

# Linear interpolate between 2016 (by) and 2023 (fy) to 2020 (target)
fac = ((inv_temp.df$ann16*3 + inv_temp.df$ann23*4)/7)/(inv_temp.df$ann16+1e-50)
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

# filter out only the columns that we need to keep for the output inventory 
inner.df <- inv_temp.df[ c("country_cd.x","region_cd","tribal_code.x","census_tract_cd.x","shape_id.x","scc","emis_type.x","poll","ann20","ann_pct_red.x","control_ids.x","control_measures.x","current_cost.x","cumulative_cost.x","projection_factor.x","reg_codes.x","calc_method.x","calc_year.x","date_updated.x","data_set_id.x","jan20","feb20","mar20","apr20","may20","jun20","jul20","aug20","sep20","oct20","nov20","dec20","jan_pctred.x","feb_pctred.x","mar_pctred.x","apr_pctred.x","may_pctred.x","jun_pctred.x","jul_pctred.x","aug_pctred.x","sep_pctred.x","oct_pctred.x","nov_pctred.x","dec_pctred.x","comment.x")]
# rename datafame columns
names(inner.df) <- c("country_cd","region_cd","tribal_code","census_tract_cd","shape_id","scc","emis_type","poll","ann20","ann_pct_red","control_ids","control_measures","current_cost","cumulative_cost","projection_factor","reg_codes","calc_method","calc_year","date_updated","data_set_id","jan20","feb20","mar20","apr20","may20","jun20","jul20","aug20","sep20","oct20","nov20","dec20","jan_pctred","feb_pctred","mar_pctred","apr_pctred","may_pctred","jun_pctred","jul_pctred","aug_pctred","sep_pctred","oct_pctred","nov_pctred","dec_pctred","comment")
rm(inv_temp.df)

# Left join: keep records that are in base year only
inv_temp.df <- anti_join( inv_base.df, inv_proj.df ,by=c("region_cd","scc","poll"))

# Linear interpolate between 2016 (by) and 2023 (fy) to 2020 (target)
fac = 3/7
inv_temp.df$ann20 <- fac*inv_temp.df$ann16
inv_temp.df$jan20 <- fac*inv_temp.df$jan_value
inv_temp.df$feb20 <- fac*inv_temp.df$feb_value
inv_temp.df$mar20 <- fac*inv_temp.df$mar_value
inv_temp.df$apr20 <- fac*inv_temp.df$apr_value
inv_temp.df$may20 <- fac*inv_temp.df$may_value
inv_temp.df$jun20 <- fac*inv_temp.df$jun_value
inv_temp.df$jul20 <- fac*inv_temp.df$jul_value
inv_temp.df$aug20 <- fac*inv_temp.df$aug_value
inv_temp.df$sep20 <- fac*inv_temp.df$sep_value
inv_temp.df$oct20 <- fac*inv_temp.df$oct_value
inv_temp.df$nov20 <- fac*inv_temp.df$nov_value
inv_temp.df$dec20 <- fac*inv_temp.df$dec_value

# filter out only the columns that we need to keep for the output inventory 
base_only.df <- inv_temp.df[c("country_cd","region_cd","tribal_code","census_tract_cd","shape_id","scc","emis_type","poll","ann20","ann_pct_red","control_ids","control_measures","current_cost","cumulative_cost","projection_factor","reg_codes","calc_method","calc_year","date_updated","data_set_id","jan20","feb20","mar20","apr20","may20","jun20","jul20","aug20","sep20","oct20","nov20","dec20","jan_pctred","feb_pctred","mar_pctred","apr_pctred","may_pctred","jun_pctred","jul_pctred","aug_pctred","sep_pctred","oct_pctred","nov_pctred","dec_pctred","comment")]
rm(inv_temp.df)

# Right join: keep records that are in future year only
inv_temp.df <- anti_join( inv_proj.df, inv_base.df ,by=c("region_cd","scc","poll"))

# Linear interpolate between 2016 (by) and 2023 (fy) to 2020 (target)
fac = 4/7
inv_temp.df$ann20 <- fac*inv_temp.df$ann23
inv_temp.df$jan20 <- fac*inv_temp.df$jan_value
inv_temp.df$feb20 <- fac*inv_temp.df$feb_value
inv_temp.df$mar20 <- fac*inv_temp.df$mar_value
inv_temp.df$apr20 <- fac*inv_temp.df$apr_value
inv_temp.df$may20 <- fac*inv_temp.df$may_value
inv_temp.df$jun20 <- fac*inv_temp.df$jun_value
inv_temp.df$jul20 <- fac*inv_temp.df$jul_value
inv_temp.df$aug20 <- fac*inv_temp.df$aug_value
inv_temp.df$sep20 <- fac*inv_temp.df$sep_value
inv_temp.df$oct20 <- fac*inv_temp.df$oct_value
inv_temp.df$nov20 <- fac*inv_temp.df$nov_value
inv_temp.df$dec20 <- fac*inv_temp.df$dec_value

# filter out only the columns that we need to keep for the output inventory 
proj_only.df <- inv_temp.df[c("country_cd","region_cd","tribal_code","census_tract_cd","shape_id","scc","emis_type","poll","ann20","ann_pct_red","control_ids","control_measures","current_cost","cumulative_cost","projection_factor","reg_codes","calc_method","calc_year","date_updated","data_set_id","jan20","feb20","mar20","apr20","may20","jun20","jul20","aug20","sep20","oct20","nov20","dec20","jan_pctred","feb_pctred","mar_pctred","apr_pctred","may_pctred","jun_pctred","jul_pctred","aug_pctred","sep_pctred","oct_pctred","nov_pctred","dec_pctred","comment")]
rm(inv_temp.df)

# concatenate the three joined dataframes
inv_temp.df <- rbind(inner.df,base_only.df,proj_only.df)

# change the year, date updated, and ID fields
inv_temp.df$calc_year <- 2020
inv_temp.df$date_updated <- 20190809
inv_temp.df$data_set_id <- "LADCO_Interp_2016-2023"

# Get headers from the base and projection files
tmp <- readLines(basef,100)
base_header <- grep("^#",tmp,value=T)
tmp <- readLines(projf,100)
proj_header <- grep("^#",tmp,value=T)

# Generate header lines
line1 <- "country_cd,region_cd,tribal_code,census_tract_cd,shape_id,scc,emis_type,poll,ann_value,ann_pct_red,control_ids,control_measures,current_cost,cumulative_cost,projection_factor,reg_codes,calc_method,calc_year,date_updated,data_set_id,jan_value,feb_value,mar_value,apr_value,may_value,jun_value,jul_value,aug_value,sep_value,oct_value,nov_value,dec_value,jan_pctred,feb_pctred,mar_pctred,apr_pctred,may_pctred,jun_pctred,jul_pctred,aug_pctred,sep_pctred,oct_pctred,nov_pctred,dec_pctred,comment"
fdesc <- "#DESC LADCO linear interpolation of 2020 from 2016 to 2023, 2016v1 National Collaborative EI"

# Write header lines to output
write.table(base_header,file=outf,quote=F,col.names=FALSE,row.names=FALSE,na="")
write.table(fdesc,file=outf,quote=F,col.names=FALSE,row.names=FALSE,na="",append=T)
write.table(line1,file=outf,quote=F,col.names=FALSE,row.names=FALSE,na="",append=T)

# write the output
write.table(inv_temp.df[,c("country_cd","region_cd","tribal_code","census_tract_cd","shape_id","scc","emis_type","poll","ann20","ann_pct_red","control_ids","control_measures","current_cost","cumulative_cost","projection_factor","reg_codes","calc_method","calc_year","date_updated","data_set_id","jan20","feb20","mar20","apr20","may20","jun20","jul20","aug20","sep20","oct20","nov20","dec20","jan_pctred","feb_pctred","mar_pctred","apr_pctred","may_pctred","jun_pctred","jul_pctred","aug_pctred","sep_pctred","oct_pctred","nov_pctred","dec_pctred","comment")], sep=",",file=outf,col.names=FALSE,row.names=FALSE,na="",append=T)

