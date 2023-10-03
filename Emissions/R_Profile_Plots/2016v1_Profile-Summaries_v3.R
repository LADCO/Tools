# Read CSV into R

# Set the pollutant to base the rankings
p = "NOX" # VOC, NOX, PM25

# Set the ranks of sources, this is the top "n" sources to plot
#r = 3
library(ggplot2)
library(reshape2)
library(scales)
library(dplyr)
library(plotly)
library(ggrepel)
library(gridExtra)
library(RColorBrewer)
library(grid)
library("M3")        # http://cran.r-project.org/web/packages/M3/
library("rasterVis") # http://cran.r-project.org/web/packages/rasterVis/
library("mapproj")
library("maptools")
library("grid")

options(scipen=999)

dirroot <- "~/WorkProjects/Modeling/Emissions/2016_Inventory_Collaborative/2016v1/profile_reports_by_scc_16dec2019/"

print("Read in data.....")
# Read in Temporal Profiles
monpro <- paste(dirroot,"2016beta_profiles/temporal/amptpro_general_2011platform_tpro_monthly_6nov2014_30nov2018_nf_v9_wOilGas",sep="")
weekpro <- paste(dirroot,"2016beta_profiles/temporal/amptpro_general_2011platform_tpro_weekly_6nov2014_09sep2016_v2",sep="")
hrpro <- paste(dirroot,"2016beta_profiles/temporal/amptpro_general_2011platform_tpro_hourly_6nov2014_24jul2017_v5",sep="")
rm(monpro.df)

monpro.df <- data.frame(read.csv(file=monpro,header=F,comment.char = '#',sep=","))
colnames(monpro.df) <- c("id","jan","feb","mar","apr","may","jun","jul","aug","sept","oct","nov","dec","desc")
rm(weekpro.df)
weekpro.df <- data.frame(read.csv(file=weekpro,header=F,comment.char = '#',sep=","))
colnames(weekpro.df) <- c("id","mon","tue","wed","thu","fri","sat","sun","desc")
rm(hrpro.df)
hrpro.df <- data.frame(read.csv(file=hrpro,header=F,comment.char = '#',sep=","))
colnames(hrpro.df) <- c("id","0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","desc")

# Read in VOC and PM2.5 Speciation Profiles
sppro <- paste(dirroot,"2016beta_profiles/speciation/gspro_cmaq_cb6_2016ff_16j_29nov2018.txt",sep="")
spprof.df <- read.csv(file=sppro,header=F,comment.char= '#',sep=";")
colnames(spprof.df) <- c("SPROF","InvPoll","Spp","SplitFac","MW","MassFrac")

# Read in Surrogate Descriptions
srgd <- paste(dirroot,"gridding/srgdesc_CONUS36_2014_beta_03dec2018_08dec2018_nf_v1.txt",sep="")
srgd2.df <- data.frame(read.csv(file=srgd,header=F,comment.char = '#',sep=";"))
colnames(srgd2.df) <- c("Country","SrgID","SrgDesc","SurrogateF")
srgdesc.df <- data.frame(Country=trimws(srgd2.df$Country),SrgID=as.character(trimws(srgd2.df$SrgID)),SrgDesc=trimws(srgd2.df$SrgDesc))

# Initialize storage lists
datalist <- list()
voclist <- list()
pmlist <- list()
srglist <- list()

# Set the sectors to summarize
#sectors <- c('afdust','ptegu','ptfire','cmv_c1c2','cmv_c3','nonpt','nonroad','np_oilgas','pt_oilgas','ptagfire','ptnonipm','rail','rwc')
sectors <- c('ag','afdust','ptegu','ptfire','cmv_c1c2','cmv_c3','nonpt','nonroad','np_oilgas','pt_oilgas','ptagfire','ptnonipm','rail','rwc')

for ( i in 14:14) {
s <- sectors[i]
print(paste("Processing sector ",s,sep=""))
rm(temp.df)
rm(temporal.df)
rm(vocspp.df)
rm(voc.df)

# Profile inputs
infile <-paste(dirroot,s,"/rep_",s,"_2016fh_16j_temporal_state_scc_temporalprof.txt",sep="")
srg_infile <-paste(dirroot,s,"/rep_",s,"_2016fh_16j_invgrid_state_scc_srgid_12US1.txt",sep="")
if (s != 'afdust') {
voc_infile <-paste(dirroot,s,"/rep_",s,"_2016ff_16j_voc_speciation_profile_state_scc_full.csv",sep="")
}
pm25_infile <-paste(dirroot,s,"/rep_",s,"_2016fh_16j_inv_state_scc_pm25prof.txt",sep="")

# Put the profiles into data frames
print("Organize dataframes.....")
if (s == 'nonroad' ){
  infile <-paste(dirroot,s,"/rep_",s,"_annual_2016fh_16j_temporal_state_scc_temporalprof.txt",sep="")
  #temp.df <- read.csv(file=infile, header=T, sep=",")
  header <- read.csv(file=infile, header=F, sep="|", skip=7,nrows=1,as.is=T)
  temp.df <- read.csv(file=infile, header=F, sep="|", skip=8)
  colnames(temp.df) <- trimws(header)
  temporal.df <- data.frame(State=trimws(temp.df$State),SCC=as.character(temp.df$SCC),SCCDesc=temp.df$'SCC Description',MonthlyTPRO=temp.df$'Monthly Prf',WeeklyTPRO=temp.df$'Weekly  Prf',DiurnalTPRO=temp.df$'Mon Diu Prf',CO=temp.df$'I-CO',NH3=temp.df$'I-NH3',NOX=temp.df$NOX,PM25=temp.df$'PM25TOTAL',SO2=temp.df$'I-SO2',VOC=temp.df$'I-VOC_INV')
  voc_infile <-paste(dirroot,s,"/rep_",s,"_2016fh_16j_voc_pm25_speciation_profile_state_scc_noCATX_annual.csv",sep="")
  vocspp.df <- read.csv(file=voc_infile,header=T,sep=",")
  voc.df <- data.frame(State=vocspp.df$state,SCC=as.character(vocspp.df$scc),SCCDesc=vocspp.df$sccdesc,SPRO=vocspp.df$profile,VOC=vocspp.df$emis)
  pm25_infile <-paste(dirroot,s,"/rep_",s,"_2016fh_16j_voc_pm25_speciation_profile_state_scc_noCATX_annual.csv",sep="")
  pmspp.df <- read.csv(file=pm25_infile,header=T,sep=",")
  pm.df <- data.frame(State=trimws(pmspp.df$state),SCC=as.character(as.numeric(pmspp.df$scc)),SCCDesc=pmspp.df$sccdesc,SPRO=pmspp.df$profile,PM25=pmspp.df$emis)
  # Read surrogate report
  srg_infile <-paste(dirroot,s,"/rep_",s,"_annual_2016fh_16j_invgrid_state_scc_srgid_12US1.txt",sep="")
  srg_header <- read.csv(file=srg_infile, header=F, sep="|", skip=7,nrows=1,as.is=T)
  srg.df <- read.csv(file=srg_infile, header=F, sep="|", skip=8)
  colnames(srg.df) <- trimws(srg_header)
  surrogates.df <- data.frame(State=trimws(srg.df$State),SCC=as.character(srg.df$SCC),SCCDesc=srg.df$'SCC Description',Surg=srg.df$'Primary Srg',CO=srg.df$'I-CO',NH3=srg.df$'I-NH3',NOX=srg.df$NOX,PM25=srg.df$PM25TOTAL,SO2=srg.df$'I-SO2',VOC=srg.df$'I-VOC_INV')
  } else if  (s == 'afdust' ) {
  header <- read.csv(file=infile, header=F, sep="|", skip=7,nrows=1,as.is=T)
  temp.df <- read.csv(file=infile, header=F, sep="|", skip=8)
  colnames(temp.df) <- trimws(header)
  temporal.df <- data.frame(State=trimws(temp.df$State),SCC=as.character(temp.df$SCC),SCCDesc=temp.df$'SCC Description',MonthlyTPRO=temp.df$'Monthly Prf',WeeklyTPRO=temp.df$'Weekly  Prf',DiurnalTPRO=temp.df$'Mon Diu Prf',PM25=temp.df$PM2_5)
  head(temporal.df)
  pmheader <- read.csv(file=pm25_infile,header=F,sep="|",skip=8,nrows=1,as.is=T)
  pmtemp.df <- read.csv(file=pm25_infile,header=F,sep="|",skip=9)
  colnames(pmtemp.df) <- trimws(pmheader)
  pm.df <- data.frame(State=trimws(pmtemp.df$State),SCC=as.character(as.numeric(pmtemp.df$SCC)),SCCDesc=pmtemp.df$'SCC Description',SPRO=pmtemp.df$'Speciation Prf',PM25=pmtemp.df$PM2_5)
  head(pm.df)
  # Read surrogate report
  srg_header <- read.csv(file=srg_infile, header=F, sep="|", skip=7,nrows=1,as.is=T)
  srg.df <- read.csv(file=srg_infile, header=F, sep="|", skip=8)
  colnames(srg.df) <- trimws(srg_header)
  surrogates.df <- data.frame(State=trimws(srg.df$State),SCC=as.character(srg.df$SCC),SCCDesc=srg.df$'SCC Description',Surg=srg.df$'Primary Srg',PM25=srg.df$PM2_5)
  
} else if ( s == 'pt_oilgas' || s == 'ptagfire' || s == 'ptegu' || s == 'ptnonipm' ){
  header <- read.csv(file=infile, header=F, sep="|", skip=7,nrows=1,as.is=T)
  temp.df <- read.csv(file=infile, header=F, sep="|", skip=8)
  colnames(temp.df) <- trimws(header)
  temporal.df <- data.frame(State=trimws(temp.df$State),SCC=as.character(temp.df$SCC),SCCDesc=temp.df$'SCC Description',MonthlyTPRO=temp.df$'Monthly Prf',WeeklyTPRO=temp.df$'Weekly  Prf',DiurnalTPRO=temp.df$'Mon Diu Prf',CO=temp.df$'I-CO',NH3=temp.df$'I-NH3',NOX=temp.df$NOX,PM25=temp.df$PM2_5,SO2=temp.df$'I-SO2',VOC=temp.df$'I-VOC_INV')
  voc_infile <-paste(dirroot,s,"/rep_",s,"_2016fh_16j_inv_state_scc_vocprof.txt",sep="")
  header <- read.csv(file=voc_infile,header=F,sep="|",skip=8,nrows=1,as.is=T)
  temp.df <- read.csv(file=voc_infile,header=F,sep="|",skip=9)
  colnames(temp.df) <- trimws(header)
  head(temp.df)
  voc.df <- data.frame(State=trimws(temp.df$State),SCC=as.character(temp.df$SCC),SCCDesc=temp.df$'SCC Description',SPRO=trimws(temp.df$'Speciation Prf'),VOC=temp.df$VOC)
  pmheader <- read.csv(file=pm25_infile,header=F,sep="|",skip=8,nrows=1,as.is=T)
  pmtemp.df <- read.csv(file=pm25_infile,header=F,sep="|",skip=9)
  colnames(pmtemp.df) <- trimws(pmheader)
  pm.df <- data.frame(State=trimws(pmtemp.df$State),SCC=as.character(as.numeric(pmtemp.df$SCC)),SCCDesc=pmtemp.df$'SCC Description',SPRO=pmtemp.df$'Speciation Prf',PM25=pmtemp.df$PM2_5)

} else if (s == 'ptfire' || s == 'cmv_c3') {
  print(infile)
  header <- read.csv(file=infile, header=F, sep="|", skip=7,nrows=1,as.is=T)
  temp.df <- read.csv(file=infile, header=F, sep="|", skip=8,as.is=T)
  colnames(temp.df) <- trimws(header)
  temporal.df <- data.frame(State=trimws(temp.df$State),SCC=as.character(temp.df$SCC),SCCDesc=temp.df$'SCC Description',MonthlyTPRO=temp.df$'Monthly Prf',WeeklyTPRO=temp.df$'Weekly  Prf',DiurnalTPRO=temp.df$'Mon Diu Prf',CO=temp.df$'I-CO',NH3=temp.df$'I-NH3',NOX=temp.df$NOX,PM25=temp.df$PM2_5,SO2=temp.df$'I-SO2',VOC=temp.df$'I-VOC_INV')
  vocspp.df <- read.csv(file=voc_infile,header=T,sep=",")
  voc.df <- data.frame(State=trimws(vocspp.df$state),SCC=as.character(vocspp.df$scc),SCCDesc=vocspp.df$sccdesc,SPRO=trimws(vocspp.df$VOC_profile),VOC=vocspp.df$VOC_INV)
  head(voc.df)
  pmheader <- read.csv(file=pm25_infile,header=F,sep="|",skip=8,nrows=1,as.is=T)
  pmtemp.df <- read.csv(file=pm25_infile,header=F,sep="|",skip=9)
  colnames(pmtemp.df) <- trimws(pmheader)
  pm.df <- data.frame(State=trimws(pmtemp.df$State),SCC=as.character(as.numeric(pmtemp.df$SCC)),SCCDesc=pmtemp.df$'SCC Description',SPRO=pmtemp.df$'Speciation Prf',PM25=pmtemp.df$PM2_5)

} else {
  header <- read.csv(file=infile, header=F, sep="|", skip=7,nrows=1,as.is=T)
  temp.df <- read.csv(file=infile, header=F, sep="|", skip=8,as.is=T)
  colnames(temp.df) <- trimws(header)
  temporal.df <- data.frame(State=trimws(temp.df$State),SCC=as.character(temp.df$SCC),SCCDesc=temp.df$'SCC Description',MonthlyTPRO=temp.df$'Monthly Prf',WeeklyTPRO=temp.df$'Weekly  Prf',DiurnalTPRO=temp.df$'Mon Diu Prf',CO=temp.df$'I-CO',NH3=temp.df$'I-NH3',NOX=temp.df$NOX,PM25=temp.df$PM2_5,SO2=temp.df$'I-SO2',VOC=temp.df$'I-VOC_INV')
  vocspp.df <- read.csv(file=voc_infile,header=T,sep=",")
  vocspp.df <- read.csv(file=voc_infile,header=T,sep=",")
  voc.df <- data.frame(State=trimws(vocspp.df$state),SCC=as.character(vocspp.df$scc),SCCDesc=vocspp.df$sccdesc,SPRO=trimws(vocspp.df$VOC_profile),VOC=vocspp.df$VOC_INV)
  pmheader <- read.csv(file=pm25_infile,header=F,sep="|",skip=8,nrows=1,as.is=T)
  pmtemp.df <- read.csv(file=pm25_infile,header=F,sep="|",skip=9)
  colnames(pmtemp.df) <- trimws(pmheader)
  pm.df <- data.frame(State=trimws(pmtemp.df$State),SCC=as.character(as.numeric(pmtemp.df$SCC)),SCCDesc=pmtemp.df$'SCC Description',SPRO=pmtemp.df$'Speciation Prf',PM25=pmtemp.df$PM2_5)
  # Read surrogate report
  srg_header <- read.csv(file=srg_infile, header=F, sep="|", skip=7,nrows=1,as.is=T)
  srg.df <- read.csv(file=srg_infile, header=F, sep="|", skip=8)
  colnames(srg.df) <- trimws(srg_header)
  surrogates.df <- data.frame(State=trimws(srg.df$State),SCC=as.character(srg.df$SCC),SCCDesc=srg.df$'SCC Description',Surg=srg.df$'Primary Srg',CO=srg.df$'I-CO',NH3=srg.df$'I-NH3',NOX=srg.df$NOX,PM25=srg.df$PM2_5,SO2=srg.df$'I-SO2',VOC=srg.df$'I-VOC_INV')
  }

temporal.df["Sector"] <- s
datalist[[i]] <- temporal.df

pm.df["Sector"] <- s
pmlist[[i]] <- pm.df

if (s != 'afdust') {
voc.df["Sector"] <- s
voclist[[i]] <- voc.df
}

if ( s != 'pt_oilgas' && s != 'ptagfire' && s != 'ptegu' && s != 'ptnonipm' && s != 'ptfire' && s != 'cmv_c3'){
  surrogates.df["Sector"] <- s
  srglist[[i]] <- surrogates.df  
}
rm(temp.df)
rm(temporal.df)
rm(srg.df)
rm(surrogates.df)
rm(pm.df)
rm(voc.df)

# Concatenate all of the temporal profile reports into a single dataframe
print("Data mashup.....")
allprofiles = do.call(rbind, datalist)

if (s != 'afdust') {
allvoc = do.call(rbind, voclist)
}
allpm = do.call(rbind, pmlist)

if ( s != 'pt_oilgas' && s != 'ptagfire' && s != 'ptegu' && s != 'ptnonipm' && s != 'ptfire' && s != 'cmv_c3' ){
  allsrgs = do.call(rbind, srglist)
}

s <- sectors[i]
print(s)

  # Filter for LADCO states
# IL, IN, MI, MN, OH, WI
print("Grab the LADCO states.....")
states<-c("Illinois","Indiana","Michigan","Minnesota","Ohio","Wisconsin")
for ( j in 1:6 ){
  a <- states[j]
  ladco.df <- subset(allprofiles,State==a & Sector==s)
  subset(allprofiles, State==a)
    if (s != 'afdust') {
    ladco_voc.df <- subset(allvoc,State==a & Sector==s)
  }
    ladco_pm.df <- subset(allpm,State==a & Sector==s)
  if ( s != 'pt_oilgas' && s != 'ptagfire' && s != 'ptegu' && s != 'ptnonipm' && s != 'ptfire' && s != 'cmv_c3' ){
    ladco_srg.df = subset(allsrgs,State==a & Sector==s)
   }
  srccnt <-nrow(ladco.df)
  print(a)

#Sort by pollutant column in temporal report and store the top X largest sources
noxsum<-sum(ladco.df$NOX)
vocsum<-sum(ladco.df$VOC)
pm25sum<-sum(ladco.df$PM25)
so2sum<-sum(ladco.df$SO2)
nh3sum<-sum(ladco.df$NH3)

print("Pick the top 10 sources.....")
if ( srccnt <= 10 ) {
  r <- srccnt
} else {
  r <- 10
}

ladco.df[order(-ladco.df[p]),]
topsrc <- head(ladco.df[order(-ladco.df[p]),],n=r)

for ( b in 1:r ) {
monplot <- topsrc[b,]

t <- monplot$SCCDesc
scc <- as.character(monplot$SCC)
state <- monplot$State

# Calculate source contributions to total sector/state emissions
noxperc<-format((monplot$NOX/noxsum)*100.0,digits=3)
vocperc<-format((monplot$VOC/vocsum)*100.0,digits=3)
pmperc<-format((monplot$PM25/pm25sum)*100.0,digits=3)
so2perc<-format((monplot$SO2/so2sum)*100.0,digits=3)
nh3perc<-format((monplot$NH3/nh3sum)*100.0,digits=3)

if ( p == 'VOC'){
voc2plot <- ladco_voc.df[which(ladco_voc.df$State==as.character(state) & ladco_voc.df$SCC==scc),]
pm2plot <- NULL
} else {
pm2plot <- ladco_pm.df[which(ladco_pm.df$State==as.character(state) & ladco_pm.df$SCC==scc),]
voc2plot <- NULL
}

print("Setup the plotting frames.....")
t1 <- textGrob(paste("Top ",p," Sources in ",state,", Sector: ",s,", SCC: ",scc,sep=""),gp=gpar(fontsize=14, col="black"))

if ( s == 'afdust'){
  t3 <- textGrob(paste("PM2.5: ",format(monplot$PM25,digits=3), " tons/yr (",pmperc,"% of total)",sep=""))
} else{
  t3 <- textGrob(paste("NOx: ",format(monplot$NOX,digits=3), " tons/yr (",noxperc,"% of total)","; VOC: ",format(monplot$VOC,digits=3), " tons/yr (",vocperc,"% of total)","; PM2.5: ",format(monplot$PM25,digits=3), " tons/yr (",pmperc,"% of total)",sep=""))#,"; SO2: ",format(monplot$SO2,digits=3), " tons/yr (",so2perc,"% of total)","; NH3: ",format(monplot$NH3,digits=3), " tons/yr (",nh3perc,"% of total)",sep=""))
}
if ( s != 'pt_oilgas' && s != 'ptagfire' && s != 'ptegu' && s != 'ptnonipm' && s != 'ptfire' ){
  srg2plot <- ladco_srg.df[which(ladco_srg.df$State==as.character(state) & ladco_srg.df$SCC==scc),]
  t_srgid <- data.frame(subset(srgdesc.df,Country == 'USA' & SrgID==trimws(as.character(srg2plot$Surg)),drop=TRUE))[,2:2]
  t_srgname <- data.frame(subset(srgdesc.df,Country == 'USA' & SrgID==trimws(as.character(srg2plot$Surg)),drop=TRUE))[,3:3]
  subset(srgdesc.df,Country == 'USA',drop=TRUE)
  # t2 <- textGrob(paste(trimws(t)," (Surrogate ",t_srgid,": ",t_srgname,")",sep=""))
  t2 <- textGrob(trimws(t),gp=gpar(fontsize=14, col="black"))
  } else {
  t2 <- textGrob(paste(trimws(t)," (Point gridding, no surrogate)",sep=""))
}
## Set up the plotting components

# Panel 1: Monthly Temporal Profile

# Month of Year Plot
#if ( s != 'ptegu' && s != 'ptfire' && s != 'cmv_c1c2' && s != 'cmv_c3') {
if ( s != 'ptegu' && s != 'ptfire' && s != 'cmv_c3') {

if ( s != 'nonroad' && s != 'rwc') {
rm(monts)
rm(monts1)
monts <- data.frame(subset(monpro.df,id == trimws(as.character(monplot$MonthlyTPRO)),drop=TRUE))[,2:13]
monts <- unique(monts)
monts1<-data.frame(t(monts))
monts1$mon<-row.names(monts1)
colnames(monts1) <-c("value","mon")
monts1$mon <- factor(monts1$mon, levels = monts1[["mon"]])
#p1<-plot_ly(monts1,x = ~mon, y = ~value, name='',type="scatter",mode = "lines") %>% layout(title = "Monthly Temporal Profile", xaxis = list(title = ""),yaxis = list (title = "Fraction Emissions/Month"))
title<-paste("Monthly Temporal Profile (",trimws(as.character(monplot$MonthlyTPRO)),")",sep="")
p1<-ggplot(monts1,aes(x=mon,y=value,group=1))+geom_line()+ theme_minimal()+labs(title=title,x="Month", y = NULL)
} else {
  p1 <- ggplot()+geom_blank()+theme_void()
  }

# Panel 2:  Day of Week Temporal Profile
if ( s != 'rwc') {
rm(weekts)
weekts <- data.frame(subset(weekpro.df, id == trimws(as.character(monplot$WeeklyTPRO)),drop=TRUE))[,2:8]
rm(weekts1)
weekts1<-data.frame(t(weekts))
weekts1$day<-row.names(weekts1)
colnames(weekts1) <-c("value","day")
weekts1$day <- factor(weekts1$day, levels = weekts1[["day"]])
title<-paste("Daily Temporal Profile (",trimws(as.character(monplot$WeeklyTPRO)),")",sep="")
p2<-ggplot(weekts1,aes(x=day,y=value,group=1))+geom_line()+ theme_minimal()+labs(title=title,x="Day", y = NULL)
} else {
  p2 <- ggplot()+geom_blank()+theme_void()
}
  
# Panel 3: Hour of Day Temporal Profile
rm(hourts)
hourts <- data.frame(subset(hrpro.df, id == trimws(as.character(monplot$DiurnalTPRO)),drop=TRUE))[,2:25]
rm(hourts1)
hourts1<-data.frame(t(hourts))
hourts1$hour<-c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23")
colnames(hourts1)<-c("value","hour")
hourts1$value<-as.numeric(as.character(hourts1$value))
hourts1$hour <- factor(hourts1$hour, levels = hourts1[["hour"]])
title<-paste("Hourly Temporal Profile (",trimws(as.character(monplot$DiurnalTPRO)),")",sep="")
p3<-ggplot(hourts1,aes(x=hour,y=value,group=1))+geom_line()+ theme_minimal()+labs(size=2,title=title,x="Hour", y = NULL)
} else {
  p1 <- ggplot()+geom_blank()+theme_void()
  p2 <- ggplot()+geom_blank()+theme_void()
  p3 <- ggplot()+geom_blank()+theme_void()
}
  
# Panel 4: Speciation Profile Bar Chart

if ( p == 'PM25'){
  pmplot <- data.frame(spprof.df[which(spprof.df$SPROF==as.character(pm2plot$SPRO) & spprof.df$InvPoll=='PM2_5' ),])
  pmproid <- as.character(pm2plot$SPRO)
  other<-subset(pmplot,SplitFac<0.001)
  newfac<-data.frame("blank","blank","Other",sum(as.numeric(other$SplitFac)),"blank","blank")
  colnames(newfac)<-c("SPROF","InvPoll","Spp","SplitFac","MW","MassFrac")
  keep<-subset(pmplot,SplitFac>=0.001)
  pmplot <-rbind(keep,newfac)
  pmplot$pos = (cumsum(c(0, pmplot$SplitFac)) + c(pmplot$SplitFac / 2, .01))[1:nrow(pmplot)]
  colourCount = length(unique(pmplot$Spp))
  getPalette = colorRampPalette(brewer.pal(9, "Set1"))
  
  title<-paste("PM2.5 Speciation, (",pmproid,")",sep="")
  p4<-ggplot(pmplot,aes(reorder(Spp, -SplitFac, sum),SplitFac,fill=Spp))+geom_col()+scale_fill_manual(values = getPalette(colourCount))+ theme_minimal()+ theme(legend.position="none") + labs(x = NULL, y = "Split Factor", fill = NULL, title = title)
  
} else if ( p == "VOC" ) {
  if ( voc2plot$SPRO == "COMBO" ){
    p4 <- ggplot()+geom_blank()+theme_void()
  }
 else {
  vocplot <- data.frame(spprof.df[which(spprof.df$SPROF==as.character(voc2plot$SPRO) & spprof.df$InvPoll=='NONHAPTOG' & substr(spprof.df$Spp,1,3)!='SOA'),])
  #vocplot <- data.frame(spprof.df[which(spprof.df$SPROF==as.character(voc2plot$SPRO) & spprof.df$InvPoll=='TOG' & substr(spprof.df$Spp,1,3)!='SOA'),])
  vocproid <- as.character(voc2plot$SPRO)
  vocplot$pos = (cumsum(c(0, vocplot$SplitFac)) + c(vocplot$SplitFac / 2, .01))[1:nrow(vocplot)]

  colourCount = length(unique(vocplot$Spp))
  getPalette = colorRampPalette(brewer.pal(9, "Set1"))
  title<-paste("VOC Speciation, (",vocproid,")",sep="")
  p4<-ggplot(vocplot,aes(reorder(Spp, -SplitFac, sum),SplitFac,fill=Spp))+geom_col()+scale_fill_manual(values = getPalette(colourCount))+ theme_minimal()+ theme(legend.position="none") + labs(x = NULL, y = "Split Factor", fill = NULL, title = title)
   }
} else {
  p4 <- ggplot()+geom_blank()+theme_void()
}

#Panel 5: Surrogate Plot
if ( s != 'ptegu' && s != 'ptfire' && s != 'cmv_c3'  && s != 'ptagfire' && s != 'pt_oilgas' && s != 'ptnonipm' ) {
# read input netCDF I/O API data ---
lcc.file <- paste("/Users/zac/WorkProjects/Modeling/Emissions/2016_Inventory_Collaborative/2016v1/Surrogates/LADCO1_1260x1170/USA_",t_srgid,"_NOFILL.txt.ncf",sep="")
lcc.proj4 <- M3::get.proj.info.M3(lcc.file)
lcc.proj4   # debugging
lcc.crs <- sp::CRS(lcc.proj4)

## Read in variable
srg.raster <- raster::raster(x=lcc.file, varname="FRACTION", crs=lcc.crs)
srg.raster@crs <- lcc.crs # why does the above assignment not take?

## Get US state boundaries in projection units.
state.map <- maps::map(
  database="state", projection="lambert", par=c(33,45), plot=FALSE)

# replace map coordinates with netCDF coordinates
metadata.coords.IOAPI.list <- M3::get.grid.info.M3(lcc.file)
metadata.coords.IOAPI.x.orig <- metadata.coords.IOAPI.list$x.orig
metadata.coords.IOAPI.y.orig <- metadata.coords.IOAPI.list$y.orig
metadata.coords.IOAPI.x.cell.width <- metadata.coords.IOAPI.list$x.cell.width
metadata.coords.IOAPI.y.cell.width <- metadata.coords.IOAPI.list$y.cell.width
map.lines <- M3::get.map.lines.M3.proj(
  file=lcc.file, database="state", units="m")
map.lines.coords.IOAPI.x <-
  (map.lines$coords[,1] - metadata.coords.IOAPI.x.orig)/metadata.coords.IOAPI.x.cell.width
map.lines.coords.IOAPI.y <-
  (map.lines$coords[,2] - metadata.coords.IOAPI.y.orig)/metadata.coords.IOAPI.y.cell.width
map.lines.coords.IOAPI <- 
  cbind(map.lines.coords.IOAPI.x, map.lines.coords.IOAPI.y)

state.map.IOAPI <- state.map # copy
state.map.IOAPI$x <- map.lines.coords.IOAPI.x
state.map.IOAPI$y <- map.lines.coords.IOAPI.y
state.map.IOAPI$range <- c(
  min(map.lines.coords.IOAPI.x),
  max(map.lines.coords.IOAPI.x),
  min(map.lines.coords.IOAPI.y),
  max(map.lines.coords.IOAPI.y))
state.map.IOAPI.shp <-
  maptools::map2SpatialLines(state.map.IOAPI, proj4string=lcc.crs)

# Build plot title
plottitle <- paste("Surrogate Fraction for surrogate ",t_srgid,": ",t_srgname,sep="")

# Set up and create the plot
cutpts <- c(0,.001,.002,.003,.004,.005,.006,.007,.008)
pal <- colorRampPalette(c("white","blue","green","yellow","orange","red"))
plot.new()
ncuts=15
p5 <- levelplot(srg.raster, margin=FALSE, at=seq(0, 0.008,length.out=ncuts), cuts=ncuts, pretty=T, 
          col.regions=pal(ncuts), xlab="Column", ylab="Row", main=list(plottitle,side=1,font=1,cex=.8,col="black")) + 
  latticeExtra::layer(sp::sp.lines(state.map.IOAPI.shp, lwd=0.8, col='black'))
} else {
  p5 <- ggplot()+geom_blank()+theme_void()
}

# Panel all of the plots together into the final product
  lay <- rbind(c(1,1,2,2,2),
               c(1,1,2,2,2),
               c(3,3,2,2,2),
               c(3,3,4,4,4),
               c(5,5,4,4,4),
               c(5,5,4,4,4),
               c(6,6,6,6,6))
g<-arrangeGrob(p1, p5, p2,p4,p3,t2,layout_matrix=lay,left="Emissions Fraction/Period",top=t1,bottom=t3)
filename <- paste(a,s,"2016v1_profile_plot",p,"rank",b,"png",sep=".")
outdirroot <- "~/WorkProjects/Modeling/Emissions/2016_Inventory_Collaborative/2016v1/Profileplots/"
outdir <- paste(outdirroot,"ladco/",a,sep="")
system(paste("mkdir -p ", outdir))
ggsave(filename,g,device="png",path=outdir, scale=1.75)
} #for top sources
} #for states
} #for sectors
