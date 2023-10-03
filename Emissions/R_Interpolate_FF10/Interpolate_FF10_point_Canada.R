#R Code to read in NONROAD FF10 files and interpolate between the two files 
#DESC "country_cd","region_cd","tribal_code","census_tract_cd","shape_id","scc","emis_type","poll","ann_value","ann_pct_red","control_ids","control_measures","current_cost","cumulative_cost","projection_factor","reg_codes","calc_method","calc_year","date_updated","data_set_id","jan_value","feb_value","mar_value","apr_value","may_value","jun_value","jul_value","aug_value","sep_value","oct_value","nov_value","dec_value","jan_pctred","feb_pctred","mar_pctred","apr_pctred","may_pctred","jun_pctred","jul_pctred","aug_pctred","sep_pctred","oct_pctred","nov_pctred","dec_pctred","comment"
      
library(dplyr)

# Set input/output directories and files
diroot <- "/Users/zac/WorkProjects/Projects/LADCO_2016_Modeling/FF10/ff10_to_interp/"
based <- paste(diroot,"2016ff_16j/inputs/othpt/",sep="")
projd <- paste(diroot,"2023ff_16j/inputs/othpt/",sep="")
basef <- paste(based,"canada_2015_UOG_svn70_26nov2018_v0.csv",sep="")
projf <- paste(projd,"canada_2023_UOG_svn70_05apr2019_v1.csv",sep="")

basey = 2015
futurey = 2023
inty = 2020

basefac <- futurey - inty
projfac <- inty - basey
rangefac <- futurey - basey

# Set output file name, and write to base year inventory directory
outf_join <- paste(based,"2020_ptinv_canada_UOG_point_31Mar2020.csv",sep="")
outf_base <- paste(based,"base_only_2020_ptinv_canada_UOG_point_31Mar2020.csv",sep="")
outf_proj <- paste(based,"proj_only_2020_ptinv_canada_UOG_point_31Mar2020.csv",sep="")

# Read in the FF10 inventory files and add column headers
base.df <- read.csv(file=basef,header=T,comment.char = '#',sep=",")
proj.df <- read.csv(file=projf,header=T,comment.char = '#',sep=",")

colnames(base.df)<-c("country_cd","region_cd","tribal_code","facility_id","unit_id","rel_point_id","process_id","agy_facility_id","agy_unit_id","agy_rel_point_id","agy_process_id","scc","poll","ann_value","ann_pct_red","facility_name","erptype","stkhgt","stkdiam","stktemp","stkflow","stkvel","naics","longitude","latitude","ll_datum","horiz_coll_mthd","design_capacity","design_capacity_units","reg_codes","fac_source_type","unit_type_code","control_ids","control_measures","current_cost","cumulative_cost","projection_factor","submitter_id","calc_method","data_set_id","facil_category_code","oris_facility_code","oris_boiler_id","ipm_yn","calc_year","date_updated","fug_height","fug_width_xdim","fug_length_ydim","fug_angle","zipcode","annual_avg_hours_per_year","jan_value","feb_value","mar_value","apr_value","may_value","jun_value","jul_value","aug_value","sep_value","oct_value","nov_value","dec_value","jan_pctred","feb_pctred","mar_pctred","apr_pctred","may_pctred","jun_pctred","jul_pctred","aug_pctred","sep_pctred","oct_pctred","nov_pctred","dec_pctred","comment")
colnames(proj.df)<-c("country_cd","region_cd","tribal_code","facility_id","unit_id","rel_point_id","process_id","agy_facility_id","agy_unit_id","agy_rel_point_id","agy_process_id","scc","poll","ann_value","ann_pct_red","facility_name","erptype","stkhgt","stkdiam","stktemp","stkflow","stkvel","naics","longitude","latitude","ll_datum","horiz_coll_mthd","design_capacity","design_capacity_units","reg_codes","fac_source_type","unit_type_code","control_ids","control_measures","current_cost","cumulative_cost","projection_factor","submitter_id","calc_method","data_set_id","facil_category_code","oris_facility_code","oris_boiler_id","ipm_yn","calc_year","date_updated","fug_height","fug_width_xdim","fug_length_ydim","fug_angle","zipcode","annual_avg_hours_per_year","jan_value","feb_value","mar_value","apr_value","may_value","jun_value","jul_value","aug_value","sep_value","oct_value","nov_value","dec_value","jan_pctred","feb_pctred","mar_pctred","apr_pctred","may_pctred","jun_pctred","jul_pctred","aug_pctred","sep_pctred","oct_pctred","nov_pctred","dec_pctred","comment")
 
# Consolidate duplicate entries in the input inventories
# Sum annual emissions across duplicate inventory entries based on FIPs (region_cd), stack IDs, SCC, and pollutant
temp_b.df <- group_by(base.df,region_cd,facility_id,unit_id,rel_point_id,process_id,scc,poll) 
inv_base_a.df <- mutate(temp_b.df,ann16=sum(ann_value,na.rm=FALSE))

temp_p.df <- group_by(proj.df,region_cd,facility_id,unit_id,rel_point_id,process_id,scc,poll) 
inv_proj.df <- mutate(temp_p.df,ann23=sum(ann_value,na.rm=FALSE))

# Remove duplicate coordinates across a single set of IDs
inv_base_b.df <- distinct(inv_base_a.df,region_cd,facility_id,unit_id,rel_point_id,process_id,scc,poll,latitude,longitude,erptype,stkhgt,stkdiam,stktemp,stkflow,stkvel,naics)
inv_base.df <- semi_join(inv_base_a.df,inv_base_b.df,by=c("region_cd","facility_id","unit_id","rel_point_id","process_id","scc","poll"))

inv_proj.df$unit_id<-as.factor(inv_proj.df$unit_id)

# Inner join: keep records that exist in both base and future
inv_temp.df <- inner_join( inv_base.df, inv_proj.df ,by=c("region_cd","facility_id","unit_id","rel_point_id","process_id","scc","poll"))

# Linear interpolate between 2016 (by) and 2023 (fy) to 2020 (target)
fac = ((inv_temp.df$ann16*basefac + inv_temp.df$ann23*projfac)/rangefac)/(inv_temp.df$ann16+1e-50)
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
inner.df <- inv_temp.df[ c("country_cd.x","region_cd","tribal_code.x","facility_id","unit_id","rel_point_id","process_id","agy_facility_id.x","agy_unit_id.x","agy_rel_point_id.x","agy_process_id.x","scc","poll","ann20","ann_pct_red.x","facility_name.x","erptype.x","stkhgt.x","stkdiam.x","stktemp.x","stkflow.x","stkvel.x","naics.x","longitude.x","latitude.x","ll_datum.x","horiz_coll_mthd.x","design_capacity.x","design_capacity_units.x","reg_codes.x","fac_source_type.x","unit_type_code.x","control_ids.x","control_measures.x","current_cost.x","cumulative_cost.x","projection_factor.x","submitter_id.x","calc_method.x","data_set_id.x","facil_category_code.x","oris_facility_code.x","oris_boiler_id.x","ipm_yn.x","calc_year.x","date_updated.x","fug_height.x","fug_width_xdim.x","fug_length_ydim.x","fug_angle.x","zipcode.x","annual_avg_hours_per_year.x","jan20","feb20","mar20","apr20","may20","jun20","jul20","aug20","sep20","oct20","nov20","dec20","jan_pctred.x","feb_pctred.x","mar_pctred.x","apr_pctred.x","may_pctred.x","jun_pctred.x","jul_pctred.x","aug_pctred.x","sep_pctred.x","oct_pctred.x","nov_pctred.x","dec_pctred.x","comment.x")]
# rename datafame columns
names(inner.df) <- c("country_cd","region_cd","tribal_code","facility_id","unit_id","rel_point_id","process_id","agy_facility_id","agy_unit_id","agy_rel_point_id","agy_process_id","scc","poll","ann_value","ann_pct_red","facility_name","erptype","stkhgt","stkdiam","stktemp","stkflow","stkvel","naics","longitude","latitude","ll_datum","horiz_coll_mthd","design_capacity","design_capacity_units","reg_codes","fac_source_type","unit_type_code","control_ids","control_measures","current_cost","cumulative_cost","projection_factor","submitter_id","calc_method","data_set_id","facil_category_code","oris_facility_code","oris_boiler_id","ipm_yn","calc_year","date_updated","fug_height","fug_width_xdim","fug_length_ydim","fug_angle","zipcode","annual_avg_hours_per_year","jan_value","feb_value","mar_value","apr_value","may_value","jun_value","jul_value","aug_value","sep_value","oct_value","nov_value","dec_value","jan_pctred","feb_pctred","mar_pctred","apr_pctred","may_pctred","jun_pctred","jul_pctred","aug_pctred","sep_pctred","oct_pctred","nov_pctred","dec_pctred","comment")
rm(inv_temp.df)

# Left join: keep records that are in base year only: write these to a "shut down" file
inv_temp.df <- anti_join( inv_base.df, inv_proj.df ,by=c("region_cd","facility_id","unit_id","rel_point_id","process_id","scc","poll"))

# filter out only the columns that we need to keep for the output inventory 
base_only.df <- inv_temp.df[c("country_cd","region_cd","tribal_code","facility_id","unit_id","rel_point_id","process_id","agy_facility_id","agy_unit_id","agy_rel_point_id","agy_process_id","scc","poll","ann16","ann_pct_red","facility_name","erptype","stkhgt","stkdiam","stktemp","stkflow","stkvel","naics","longitude","latitude","ll_datum","horiz_coll_mthd","design_capacity","design_capacity_units","reg_codes","fac_source_type","unit_type_code","control_ids","control_measures","current_cost","cumulative_cost","projection_factor","submitter_id","calc_method","data_set_id","facil_category_code","oris_facility_code","oris_boiler_id","ipm_yn","calc_year","date_updated","fug_height","fug_width_xdim","fug_length_ydim","fug_angle","zipcode","annual_avg_hours_per_year","jan_value","feb_value","mar_value","apr_value","may_value","jun_value","jul_value","aug_value","sep_value","oct_value","nov_value","dec_value","jan_pctred","feb_pctred","mar_pctred","apr_pctred","may_pctred","jun_pctred","jul_pctred","aug_pctred","sep_pctred","oct_pctred","nov_pctred","dec_pctred","comment")]
rm(inv_temp.df)

# Right join: keep records that are in future year only
inv_temp.df <- anti_join( inv_proj.df, inv_base.df ,by=c("region_cd","facility_id","unit_id","rel_point_id","process_id","scc","poll"))

# Linear interpolate between 2016 (by) and 2023 (fy) to 2020 (target)
fac = projfac/rangefac
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
proj_only.df <- inv_temp.df[c("country_cd","region_cd","tribal_code","facility_id","unit_id","rel_point_id","process_id","agy_facility_id","agy_unit_id","agy_rel_point_id","agy_process_id","scc","poll","ann20","ann_pct_red","facility_name","erptype","stkhgt","stkdiam","stktemp","stkflow","stkvel","naics","longitude","latitude","ll_datum","horiz_coll_mthd","design_capacity","design_capacity_units","reg_codes","fac_source_type","unit_type_code","control_ids","control_measures","current_cost","cumulative_cost","projection_factor","submitter_id","calc_method","data_set_id","facil_category_code","oris_facility_code","oris_boiler_id","ipm_yn","calc_year","date_updated","fug_height","fug_width_xdim","fug_length_ydim","fug_angle","zipcode","annual_avg_hours_per_year","jan20","feb20","mar20","apr20","may20","jun20","jul20","aug20","sep20","oct20","nov20","dec20","jan_pctred","feb_pctred","mar_pctred","apr_pctred","may_pctred","jun_pctred","jul_pctred","aug_pctred","sep_pctred","oct_pctred","nov_pctred","dec_pctred","comment")]
rm(inv_temp.df)
inner.df
# change the year, date updated, and ID fields
inner.df$calc_year <- 2020
inner.df$date_updated <- 20200331
inner.df$data_set_id <- "LADCO_Interp_2016-2023"

# Get headers from the base and projection files
tmp <- readLines(basef,100)
base_header <- grep("^#",tmp,value=T)
tmp <- readLines(projf,100)
proj_header <- grep("^#",tmp,value=T)

# Generate header lines
line1 <- "country_cd,region_cd,tribal_code,facility_id,unit_id,rel_point_id,process_id,agy_facility_id,agy_unit_id,agy_rel_point_id,agy_process_id,scc,poll,ann_value,ann_pct_red,facility_name,erptype,stkhgt,stkdiam,stktemp,stkflow,stkvel,naics,longitude,latitude,ll_datum,horiz_coll_mthd,design_capacity,design_capacity_units,reg_codes,fac_source_type,unit_type_code,control_ids,control_measures,current_cost,cumulative_cost,projection_factor,submitter_id,calc_method,data_set_id,facil_category_code,oris_facility_code,oris_boiler_id,ipm_yn,calc_year,date_updated,fug_height,fug_width_xdim,fug_length_ydim,fug_angle,zipcode,annual_avg_hours_per_year,jan_value,feb_value,mar_value,apr_value,may_value,jun_value,jul_value,aug_value,sep_value,oct_value,nov_value,dec_value,jan_pctred,feb_pctred,mar_pctred,apr_pctred,may_pctred,jun_pctred,jul_pctred,aug_pctred,sep_pctred,oct_pctred,nov_pctred,dec_pctred,comment"
fdesc <- "#DESC LADCO linear interpolation of 2020 from 2016 to 2023, 2016v1 National Collaborative EI"

# Write header lines to output
write.table(base_header,file=outf_join,quote=F,col.names=FALSE,row.names=FALSE,na="")
write.table(fdesc,file=outf_join,quote=F,col.names=FALSE,row.names=FALSE,na="",append=T)
write.table(line1,file=outf_join,quote=F,col.names=FALSE,row.names=FALSE,na="",append=T)

# Write the output to three seperate files: 2020 interpolated, sources in the base only (i.e., shutdowns), and sources in the future only
write.table(inner.df[,c("country_cd","region_cd","tribal_code","facility_id","unit_id","rel_point_id","process_id","agy_facility_id","agy_unit_id","agy_rel_point_id","agy_process_id","scc","poll","ann_value","ann_pct_red","facility_name","erptype","stkhgt","stkdiam","stktemp","stkflow","stkvel","naics","longitude","latitude","ll_datum","horiz_coll_mthd","design_capacity","design_capacity_units","reg_codes","fac_source_type","unit_type_code","control_ids","control_measures","current_cost","cumulative_cost","projection_factor","submitter_id","calc_method","data_set_id","facil_category_code","oris_facility_code","oris_boiler_id","ipm_yn","calc_year","date_updated","fug_height","fug_width_xdim","fug_length_ydim","fug_angle","zipcode","annual_avg_hours_per_year","jan_value","feb_value","mar_value","apr_value","may_value","jun_value","jul_value","aug_value","sep_value","oct_value","nov_value","dec_value","jan_pctred","feb_pctred","mar_pctred","apr_pctred","may_pctred","jun_pctred","jul_pctred","aug_pctred","sep_pctred","oct_pctred","nov_pctred","dec_pctred","comment")], sep=",",file=outf_join,col.names=FALSE,row.names=FALSE,na="",append=T)
write.table(base_only.df[,c("country_cd","region_cd","tribal_code","facility_id","unit_id","rel_point_id","process_id","agy_facility_id","agy_unit_id","agy_rel_point_id","agy_process_id","scc","poll","ann16","ann_pct_red","facility_name","erptype","stkhgt","stkdiam","stktemp","stkflow","stkvel","naics","longitude","latitude","ll_datum","horiz_coll_mthd","design_capacity","design_capacity_units","reg_codes","fac_source_type","unit_type_code","control_ids","control_measures","current_cost","cumulative_cost","projection_factor","submitter_id","calc_method","data_set_id","facil_category_code","oris_facility_code","oris_boiler_id","ipm_yn","calc_year","date_updated","fug_height","fug_width_xdim","fug_length_ydim","fug_angle","zipcode","annual_avg_hours_per_year","jan_value","feb_value","mar_value","apr_value","may_value","jun_value","jul_value","aug_value","sep_value","oct_value","nov_value","dec_value","jan_pctred","feb_pctred","mar_pctred","apr_pctred","may_pctred","jun_pctred","jul_pctred","aug_pctred","sep_pctred","oct_pctred","nov_pctred","dec_pctred","comment")], sep=",",file=outf_base,col.names=FALSE,row.names=FALSE,na="")
write.table(proj_only.df[,c("country_cd","region_cd","tribal_code","facility_id","unit_id","rel_point_id","process_id","agy_facility_id","agy_unit_id","agy_rel_point_id","agy_process_id","scc","poll","ann20","ann_pct_red","facility_name","erptype","stkhgt","stkdiam","stktemp","stkflow","stkvel","naics","longitude","latitude","ll_datum","horiz_coll_mthd","design_capacity","design_capacity_units","reg_codes","fac_source_type","unit_type_code","control_ids","control_measures","current_cost","cumulative_cost","projection_factor","submitter_id","calc_method","data_set_id","facil_category_code","oris_facility_code","oris_boiler_id","ipm_yn","calc_year","date_updated","fug_height","fug_width_xdim","fug_length_ydim","fug_angle","zipcode","annual_avg_hours_per_year","jan20","feb20","mar20","apr20","may20","jun20","jul20","aug20","sep20","oct20","nov20","dec20","jan_pctred","feb_pctred","mar_pctred","apr_pctred","may_pctred","jun_pctred","jul_pctred","aug_pctred","sep_pctred","oct_pctred","nov_pctred","dec_pctred","comment")], sep=",",file=outf_proj,col.names=FALSE,row.names=FALSE,na="")
