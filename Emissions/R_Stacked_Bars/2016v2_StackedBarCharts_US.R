# Read CSV into R

library(ggplot2)
library(reshape2)
library(scales)
library(dplyr)
#library(wrapr)
#library(tidyverse)

emis.df <- read.csv(file="~/WorkProjects/Modeling/Emissions/2016_Inventory_Collaborative/2016v2/2016v2_county_sector_summary.csv", header=TRUE, sep=",")
#emis.df <- read.csv(file="~/WorkProjects/Modeling/Emissions/2016_Inventory_Collaborative/2016v1/2016v1_county_sector_summary_v4.csv", header=TRUE, sep=",")


# Add headers and organize fields
temp1.df <- data.frame(Sector=emis.df$Sector,FIPS=emis.df$FIPS,County=emis.df$County,State=emis.df$State,ST=emis.df$ST,Poll=emis.df$Species,'emis2016v2'=emis.df$X2016fj,'emis2023v2'=emis.df$X2023fj,'emis2026v2'=emis.df$X2026fj,'emis2032v2'=emis.df$X2032fj)
#temp1.df <- data.frame(Sector=emis.df$Sector,FIPS=emis.df$FIPS,County=emis.df$CountyName,State=emis.df$StateName,ST=emis.df$State,Poll=emis.df$Pollutant,'emis2016fh'=emis.df$X2016fh)

# Remove non-US sectors and PM10
temp.df <- filter(temp1.df,ST!='OTHER' & ST !='CANADA' & ST != 'MEXICO' & ST != 'SECA' & ST != 'OS' & Poll != "PM10" & Poll != "PEC" & Poll != "POC")
head(temp.df)

# Find unique list of sectors
sectors_us <- unique(temp.df$Sector)
print(sectors_us)

# Find unique list of states
states_us <- unique(temp.df$State)
print(states_us)

# Find unique list of pollutants
polls <- unique(temp.df$Poll)
#print(polls)

# Filter for LADCO states
# IL, IN, MI, MN, OH, WI
ladco.df <- temp.df[(temp.df$ST=="IL" | temp.df$ST=="IN" | temp.df$ST=="MI" | temp.df$ST=="MN" | temp.df$ST=="OH"| temp.df$ST=="WI"),]

# Filter for WRAP states
# CA, OR, WA, AZ, NV, ID, NM, UT, CO, WY MT, ND, SD
wrap.df <- temp.df[(temp.df$ST=="CA" | temp.df$ST=="OR" | temp.df$ST=="WA" | temp.df$ST=="AZ" | temp.df$ST=="NV" | temp.df$ST=="ID" | temp.df$ST=="NM" | temp.df$ST=="UT" | temp.df$ST=="CO" | temp.df$ST=="WY" | temp.df$ST=="MT" | temp.df$ST=="ND" | temp.df$ST=="SD"),]

# Filter for CenRAP states
# TX, OK, KS, NE, LA, AR, MO, IA
cenrap.df <- temp.df[(temp.df$ST=="TX" | temp.df$ST=="OK" | temp.df$ST=="KS" | temp.df$ST=="NE" | temp.df$ST=="LA" | temp.df$ST=="AR" | temp.df$ST=="MO" | temp.df$ST=="IA"),]

# Filter for SESARM states
# MS, AL, TN, KY FL, GA, SC, NC, VA, WV
sesarm.df <- temp.df[(temp.df$ST=="MS" | temp.df$ST=="AL" | temp.df$ST=="TN" | temp.df$ST=="KY" | temp.df$ST=="FL" | temp.df$ST=="GA" | temp.df$ST=="SC" | temp.df$ST=="NC" | temp.df$ST=="VA" | temp.df$ST=="WV"),]

# Filter for MARAMA and MANE-VU states
# MD, DE, NJ, PA, NY, CT, RI, MA, NH, VT, ME
manevu.df <- temp.df[(temp.df$ST=="MD" | temp.df$ST=="DE" | temp.df$ST=="NJ" | temp.df$ST=="PA" | temp.df$ST=="NY" | temp.df$ST=="CT" | temp.df$ST=="RI" | temp.df$ST=="MA" | temp.df$ST=="NH" | temp.df$ST=="VT" | temp.df$ST=="ME"),]

# Filter for Tribal
tribal.df <- temp.df[(temp.df$ST=="TRIBAL-CenSARA" | temp.df$ST=="TRIBAL-LADCO" | temp.df$ST=="TRIBAL-WRAP"),]

# Add column headers
colnames(temp.df)[colnames(temp.df)=="Sector"] <- "Sect"
head(temp.df)

charttitle <- paste("2016v2 Emissions Modeling Platform Summary",sep="")
chartsubt1 <-paste("Region: All US States",sep="")
outdirroot <- "~/WorkProjects/Modeling/Emissions/2016_Inventory_Collaborative/2016v2/stackedbarplots/"

#polls <- c("SO2","NH3","VOC","NOX","CO","PM10-PRI","PM25-PRI")
polls <- c("SO2","NH3","VOC","NOX","CO","PM25")

cvals=c("afdust_adj"="#996633","fertilizer"="#992b00","livestock"="#993b00","airports"="#660000","beis"="#421717","cmv_c1c2"="#FF0000","cmv_c3"="#FF99CC","nonpt"="#FF9900","nonroad"="#FFCC00","np_oilgas"="#FFFF00","onroad"="#336600","pt_oilgas"="#99FF99","ptagfire"="#003366","ptegu"="#0066CC","ptfire-rx"="#99CCFF","ptfire-wild"="#99AAFF","ptnonipm"="#660099","rail"="#9933FF","rwc"="#CC99FF","solvents"="#ddc3f7") 

#####################Plot National Totals

# Put all pollutants on one plot
outdir<-outdirroot
system(paste("mkdir -p ", outdir))
plot.df <- data.frame(aggregate(cbind(emis2016v2,emis2023v2,emis2026v2,emis2032v2) ~ Sect+Poll, data=temp.df, FUN=sum,na.rm=FALSE))

chartsubt <-paste(chartsubt1)
dev.size("px")
print(plot.df)    
# 2016v2 platform plots
ggplot(melt(plot.df,measure.vars=c('emis2016v2','emis2023v2','emis2026v2','emis2032v2'),id.vars=c('Poll','Sect')),aes(x=variable,y=value*1e-6)) + geom_col(aes(fill=Sect),na.rm=TRUE) +facet_wrap(~ Poll,ncol=3,scales="free_y") + theme_light() + labs(title=charttitle,subtitle=chartsubt,x="Platform",y="Emissions (1e6 tons/year)",fill=NULL) + scale_fill_manual(values=cvals)+scale_x_discrete(labels=c("2016v2","2023v2","2026v2","2032v2")) 
filename <- paste("US_2016v2_emis_stackedbar_freeY",".png",sep="")
ggsave(filename,plot = last_plot(),device="png",path=outdir, scale=2)

# Base Year platform plots
#    ggplot(melt(plot.df,measure.vars=c('emis2011en','emis2014fd','emis2016ff','emis2016fh'),id.vars=c('Poll','Sect')),aes(x=variable,y=value*1e-6)) + geom_col(aes(fill=Sect),na.rm=TRUE) +facet_wrap(~ Poll,ncol=3,scales="free_y") + theme_light() + labs(title=charttitle,subtitle=chartsubt,x="Platform",y="Emissions (1e6 tons/year)",fill=NULL) + scale_fill_manual(values=cvals)+scale_x_discrete(labels=c("2011en","2014fd","2016ff","2016fh")) 
#    filename <- paste("US_11v14v16_emis_stackedbar_freeY",".png",sep="")
#    ggsave(filename,plot = last_plot(),device="png",path=outdir, scale=2)
# Future Year platform plots
#    ggplot(melt(plot.df,measure.vars=c('emis2023en','emis2023fh','emis2028el','emis2028fh'),id.vars=c('Poll','Sect')),aes(x=variable,y=value*1e-6)) + geom_col(aes(fill=Sect),na.rm=TRUE) +facet_wrap(~ Poll,ncol=3,scales="free_y") + theme_light() + labs(title=charttitle,subtitle=chartsubt,x="Platform",y="Emissions (1e6 tons/year)",fill=NULL) + scale_fill_manual(values=cvals)+scale_x_discrete(labels=c("2023en","2023fh","2028el","2028fh")) 
#    filename <- paste("US_23v28_emis_stackedbar_freeY",".png",sep="")
#    ggsave(filename,plot = last_plot(),device="png",path=outdir, scale=2)

#####################Plot MJO Totals

for ( p in polls) {
  print(paste("Plotting: ",p,sep=""))
  chartsubt1 <-paste("Pollutant: ",p,sep="")
  LADCO <- melt(data.frame(aggregate(cbind(emis2016v2,emis2023v2,emis2026v2,emis2032v2) ~ Sector, data=subset(ladco.df,Poll == p),FUN=sum,na.rm=TRUE, na.action=NULL)[,c('Sector',"emis2016v2","emis2023v2","emis2026v2","emis2032v2")]))
  LADCO["MJO"] <- 'LADCO'
  CENRAP <- melt(data.frame(aggregate(cbind(emis2016v2,emis2023v2,emis2026v2,emis2032v2) ~ Sector, data=subset(cenrap.df,Poll == p),FUN=sum,na.rm=TRUE, na.action=NULL)[,c('Sector',"emis2016v2","emis2023v2","emis2026v2","emis2032v2")]))
  CENRAP["MJO"] <- 'CENRAP'
  WRAP <- melt(data.frame(aggregate(cbind(emis2016v2,emis2023v2,emis2026v2,emis2032v2) ~ Sector, data=subset(wrap.df,Poll == p),FUN=sum,na.rm=TRUE, na.action=NULL)[,c('Sector',"emis2016v2","emis2023v2","emis2026v2","emis2032v2")]))
  WRAP["MJO"] <- 'WRAP'
  SESARM <- melt(data.frame(aggregate(cbind(emis2016v2,emis2023v2,emis2026v2,emis2032v2) ~ Sector, data=subset(sesarm.df,Poll == p),FUN=sum,na.rm=TRUE, na.action=NULL)[,c('Sector',"emis2016v2","emis2023v2","emis2026v2","emis2032v2")]))
  SESARM["MJO"] <- 'SESARM'
  Northeast <- melt(data.frame(aggregate(cbind(emis2016v2,emis2023v2,emis2026v2,emis2032v2) ~ Sector, data=subset(manevu.df,Poll == p),FUN=sum,na.rm=TRUE, na.action=NULL)[,c('Sector',"emis2016v2","emis2023v2","emis2026v2","emis2032v2")]))
  Northeast["MJO"] <- 'Northeast'
  Tribal <- melt(data.frame(aggregate(cbind(emis2016v2,emis2023v2,emis2026v2,emis2032v2) ~ Sector, data=subset(tribal.df,Poll == p),FUN=sum,na.rm=TRUE, na.action=NULL)[,c('Sector',"emis2016v2","emis2023v2","emis2026v2","emis2032v2")]))
  Tribal["MJO"] <- 'Tribal'

  plot_us.df <- data.frame(rbind(LADCO,WRAP,CenRAP,SESARM,Northeast,Tribal))
  plot_us.df$MJO_f=factor(plot_us.df$MJO, levels=c('WRAP','CenRAP','LADCO','SESARM','Northeast','Tribal'))
 
   # Plots of US Regions only
  outdir <- paste(outdirroot,"us",sep="")
  system(paste("mkdir -p ", outdir))
  chartsubt <-paste(chartsubt1,", Region: US (MJOs/Regions)",sep="")
  
  # 2016v2 platform plot
  ggplot(subset(plot_us.df,variable == 'emis2016v2' | variable == 'emis2023v2' | variable == 'emis2026v2'| variable == 'emis2032v2'),aes(x=variable,y=value*1e-6)) + geom_col(aes(fill=Sector),na.rm=TRUE) +facet_grid(~ MJO_f) + theme_light() + labs(title=charttitle,subtitle=chartsubt,x="Platform",y="Emissions (1e6 tons/year)",fill=NULL) +scale_fill_manual(values=cvals)+scale_x_discrete(labels=c("2016v2","2023v2","2026v2","2032v2"))+theme(axis.text.x = element_text(size = 6))
  filename <- paste("regions_2016v2_emis_stackedbar",p,"png",sep=".")
  ggsave(filename,plot = last_plot(),device="png",path=outdir, scale=1,width=10)
  

  #####################plot LADCO
  
  outdir <- paste(outdirroot,"ladco",sep="")
  system(paste("mkdir -p ", outdir))
  plot.df <- aggregate(cbind(emis2016v2,emis2023v2,emis2026v2,emis2032v2) ~ Sector+ST, data=subset(ladco.df,Poll == p ), FUN=sum,na.rm=TRUE, na.action=NULL)
  
  # 2016v2 platform plots
  chartsubt <-paste(chartsubt1,", Region: LADCO",sep="")
  ggplot(melt(plot.df,measure.vars=c('emis2016v2','emis2023v2','emis2026v2','emis2032v2'),id.vars=c('ST','Sector')),aes(x=variable,y=value*1e-6)) + geom_col(aes(fill=Sector),na.rm=TRUE) +facet_wrap(~ ST,ncol=3) + theme_light() + labs(title=charttitle,subtitle=chartsubt,x="Platform",y="Emissions (1e6 tons/year)",fill=NULL) + scale_fill_manual(values=cvals)+scale_x_discrete(labels=c("2016v2","2023v2","2026v2","2032v2"))+ theme(axis.text.x = element_text(size = 6))
  filename <- paste("ladco_state_2016v2_emis_stackedbar",p,"png",sep=".")
  ggsave(filename,plot = last_plot(),device="png",path=outdir, scale=1)
  
  ggplot(melt(plot.df,measure.vars=c('emis2016v2','emis2023v2','emis2026v2','emis2032v2'),id.vars=c('ST','Sector')),aes(x=variable,y=value*1e-6)) + geom_col(aes(fill=Sector),na.rm=TRUE) +facet_wrap(~ ST,ncol=3,scales="free_y") + theme_light() + labs(title=charttitle,subtitle=chartsubt,x="Platform",y="Emissions (1e6 tons/year)",fill=NULL) + scale_fill_manual(values=cvals)+scale_x_discrete(labels=c("2016v2","2023v2","2026v2","2032v2"))+ theme(axis.text.x = element_text(size = 6))
  filename <- paste("ladco_state_2016v2_emis_stackedbar_freeY",p,"png",sep=".")
  ggsave(filename,plot = last_plot(),device="png",path=outdir, scale=1)
  
  #####################plot WRAP
  
  outdir <- paste(outdirroot,"wrap",sep="")
  system(paste("mkdir -p ", outdir))
  plot.df <- aggregate(cbind(emis2016v2,emis2023v2,emis2026v2,emis2032v2) ~ Sector+ST, data=subset(wrap.df,Poll == p ), FUN=sum,na.rm=TRUE, na.action=NULL)
  
  # 2016v2 platform plots
  chartsubt <-paste(chartsubt1,", Region: WRAP",sep="")
  ggplot(melt(plot.df,measure.vars=c('emis2016v2','emis2023v2','emis2026v2','emis2032v2'),id.vars=c('ST','Sector')),aes(x=variable,y=value*1e-6)) + geom_col(aes(fill=Sector),na.rm=TRUE) +facet_wrap(~ ST,ncol=3) + theme_light() + labs(title=charttitle,subtitle=chartsubt,x="Platform",y="Emissions (1e6 tons/year)",fill=NULL) + scale_fill_manual(values=cvals)+scale_x_discrete(labels=c("2016v2","2023v2","2026v2","2032v2")) + theme(axis.text.x = element_text(size = 6))
  filename <- paste("wrap_state_2016v2_emis_stackedbar",p,"png",sep=".")
  ggsave(filename,plot = last_plot(),device="png",path=outdir, scale=1)
  
  ggplot(melt(plot.df,measure.vars=c('emis2016v2','emis2023v2','emis2026v2','emis2032v2'),id.vars=c('ST','Sector')),aes(x=variable,y=value*1e-6)) + geom_col(aes(fill=Sector),na.rm=TRUE) +facet_wrap(~ ST,ncol=3,scales="free_y") + theme_light() + labs(title=charttitle,subtitle=chartsubt,x="Platform",y="Emissions (1e6 tons/year)",fill=NULL) + scale_fill_manual(values=cvals)+scale_x_discrete(labels=c("2016v2","2023v2","2026v2","2032v2")) + theme(axis.text.x = element_text(size = 6))
  filename <- paste("wrap_state_2016v2_emis_stackedbar_freeY",p,"png",sep=".")
  ggsave(filename,plot = last_plot(),device="png",path=outdir, scale=1)
  
  #####################plot CENWRAP
  
  outdir <- paste(outdirroot,"cenrap",sep="")
  system(paste("mkdir -p ", outdir))
  plot.df <- aggregate(cbind(emis2016v2,emis2023v2,emis2026v2,emis2032v2) ~ Sector+ST, data=subset(cenrap.df,Poll == p ), FUN=sum,na.rm=TRUE, na.action=NULL)
  
  # 2016v2 platform plots
  chartsubt <-paste(chartsubt1,", Region: CENRAP",sep="")
  ggplot(melt(plot.df,measure.vars=c('emis2016v2','emis2023v2','emis2026v2','emis2032v2'),id.vars=c('ST','Sector')),aes(x=variable,y=value*1e-6)) + geom_col(aes(fill=Sector),na.rm=TRUE) +facet_wrap(~ ST,ncol=3) + theme_light() + labs(title=charttitle,subtitle=chartsubt,x="Platform",y="Emissions (1e6 tons/year)",fill=NULL) + scale_fill_manual(values=cvals)+scale_x_discrete(labels=c("2016v2","2023v2","2026v2","2032v2")) + theme(axis.text.x = element_text(size = 6))
  filename <- paste("cenrap_state_2016v2_emis_stackedbar",p,"png",sep=".")
  ggsave(filename,plot = last_plot(),device="png",path=outdir, scale=1)
  
  ggplot(melt(plot.df,measure.vars=c('emis2016v2','emis2023v2','emis2026v2','emis2032v2'),id.vars=c('ST','Sector')),aes(x=variable,y=value*1e-6)) + geom_col(aes(fill=Sector),na.rm=TRUE) +facet_wrap(~ ST,ncol=3,scales="free_y") + theme_light() + labs(title=charttitle,subtitle=chartsubt,x="Platform",y="Emissions (1e6 tons/year)",fill=NULL) + scale_fill_manual(values=cvals)+scale_x_discrete(labels=c("2016v2","2023v2","2026v2","2032v2")) + theme(axis.text.x = element_text(size = 6))
  filename <- paste("cenrap_state_2016v2_emis_stackedbar_freeY",p,"png",sep=".")
  ggsave(filename,plot = last_plot(),device="png",path=outdir, scale=1)
  
  #####################plot SESARM
  
  outdir <- paste(outdirroot,"sesarm",sep="")
  system(paste("mkdir -p ", outdir))
  plot.df <- aggregate(cbind(emis2016v2,emis2023v2,emis2026v2,emis2032v2) ~ Sector+ST, data=subset(sesarm.df,Poll == p ), FUN=sum,na.rm=TRUE, na.action=NULL)
  
  # 2016v2 platform plots
  chartsubt <-paste(chartsubt1,", Region: SESARM",sep="")
  ggplot(melt(plot.df,measure.vars=c('emis2016v2','emis2023v2','emis2026v2','emis2032v2'),id.vars=c('ST','Sector')),aes(x=variable,y=value*1e-6)) + geom_col(aes(fill=Sector),na.rm=TRUE) +facet_wrap(~ ST,ncol=3) + theme_light() + labs(title=charttitle,subtitle=chartsubt,x="Platform",y="Emissions (1e6 tons/year)",fill=NULL) + scale_fill_manual(values=cvals)+scale_x_discrete(labels=c("2016v2","2023v2","2026v2","2032v2")) + theme(axis.text.x = element_text(size = 6))
  filename <- paste("sesarm_state_2016v2_emis_stackedbar",p,"png",sep=".")
  ggsave(filename,plot = last_plot(),device="png",path=outdir, scale=1)
  
  ggplot(melt(plot.df,measure.vars=c('emis2016v2','emis2023v2','emis2026v2','emis2032v2'),id.vars=c('ST','Sector')),aes(x=variable,y=value*1e-6)) + geom_col(aes(fill=Sector),na.rm=TRUE) +facet_wrap(~ ST,ncol=3,scales="free_y") + theme_light() + labs(title=charttitle,subtitle=chartsubt,x="Platform",y="Emissions (1e6 tons/year)",fill=NULL) + scale_fill_manual(values=cvals)+scale_x_discrete(labels=c("2016v2","2023v2","2026v2","2032v2")) + theme(axis.text.x = element_text(size = 6))
  filename <- paste("sesarm_state_2016v2_emis_stackedbar_freeY",p,"png",sep=".")
  ggsave(filename,plot = last_plot(),device="png",path=outdir, scale=1)
  
  #####################plot Northeast
  
  outdir <- paste(outdirroot,"northeast",sep="")
  system(paste("mkdir -p ", outdir))
  plot.df <- aggregate(cbind(emis2016v2,emis2023v2,emis2026v2,emis2032v2) ~ Sector+ST, data=subset(manevu.df,Poll == p ), FUN=sum,na.rm=TRUE, na.action=NULL)
  
  # 2016v2 platform plots
  
  chartsubt <-paste(chartsubt1,", Region: Northeast",sep="")
  ggplot(melt(plot.df,measure.vars=c('emis2016v2','emis2023v2','emis2026v2','emis2032v2'),id.vars=c('ST','Sector')),aes(x=variable,y=value*1e-6)) + geom_col(aes(fill=Sector),na.rm=TRUE) +facet_wrap(~ ST,ncol=3) + theme_light() + labs(title=charttitle,subtitle=chartsubt,x="Platform",y="Emissions (1e6 tons/year)",fill=NULL) + scale_fill_manual(values=cvals)+scale_x_discrete(labels=c("2016v2","2023v2","2026v2","2032v2")) + theme(axis.text.x = element_text(size = 6))
  filename <- paste("northeast_state_2016v2_emis_stackedbar",p,"png",sep=".")
  ggsave(filename,plot = last_plot(),device="png",path=outdir, scale=1)
  
  ggplot(melt(plot.df,measure.vars=c('emis2016v2','emis2023v2','emis2026v2','emis2032v2'),id.vars=c('ST','Sector')),aes(x=variable,y=value*1e-6)) + geom_col(aes(fill=Sector),na.rm=TRUE) +facet_wrap(~ ST,ncol=3,scales="free_y") + theme_light() + labs(title=charttitle,subtitle=chartsubt,x="Platform",y="Emissions (1e6 tons/year)",fill=NULL) + scale_fill_manual(values=cvals)+scale_x_discrete(labels=c("2016v2","2023v2","2026v2","2032v2")) + theme(axis.text.x = element_text(size = 6))
  filename <- paste("northeast_state_2016v2_emis_stackedbar_freeY",p,"png",sep=".")
  ggsave(filename,plot = last_plot(),device="png",path=outdir, scale=1)
 
}


