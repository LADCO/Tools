# Read CSV into R

library(ggplot2)
library(reshape2)
library(scales)
library(dplyr)
#library(wrapr)
#library(tidyverse)

emis.df <- read.csv(file="~/WorkProjects/Modeling/Emissions/2016_Inventory_Collaborative/2016v2/2016v2_county_sector_summary.csv", header=TRUE, sep=",")

# Add headers and organize fields
temp1.df <- data.frame(Sector=emis.df$Sector,FIPS=emis.df$FIPS,County=emis.df$County,State=emis.df$State,ST=emis.df$ST,Poll=emis.df$Species,'emis2016v2'=emis.df$X2016fj,'emis2023v2'=emis.df$X2023fj,'emis2026v2'=emis.df$X2026fj,'emis2032v2'=emis.df$X2032fj)

# Remove non-US sectors and PM10
temp.df <- filter(temp1.df,ST!='OTHER' & ST !='CANADA' & ST != 'MEXICO' & ST != 'SECA' & ST != 'OS' & Poll != "PM10" & Poll != "PEC" & Poll != "POC")

# Remove non-US sectors and PM10
temp_nocmv.df <- filter(temp1.df,ST!='OTHER' & ST !='CANADA' & ST != 'MEXICO' & ST != 'SECA' & ST != 'OS' & Poll != "PM10" & Poll != "PEC" & Poll != "POC" & Sector != "cmv_c1c2" & Sector != "cmv_c3" )

# Find unique list of sectors
sectors_us <- unique(temp.df$Sector)
sectors_us_nocmv <- unique(temp_nocmv.df$Sector)

# Find unique list of pollutants
polls <- unique(temp.df$Poll)
polls1 <- unique(temp.df$Poll)

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

charttitle <- paste("2016v2 Emissions Modeling Platform Summary",sep="")
outdirroot <- "~/WorkProjects/Modeling/Emissions/2016_Inventory_Collaborative/2016v2/stackedbarplots/"

#polls <- c("SO2","NH3","VOC","NOX","CO","PM10-PRI","PM25-PRI")
polls <- c("SO2","NH3","VOC","NOX","CO","PM25")

cvals=c("afdust_adj"="#996633","fertilizer"="#992b00","livestock"="#993b00","airports"="#660000","beis"="#421717","cmv_c1c2"="#FF0000","cmv_c3"="#FF99CC","nonpt"="#FF9900","nonroad"="#FFCC00","np_oilgas"="#FFFF00","onroad"="#336600","pt_oilgas"="#99FF99","ptagfire"="#003366","ptegu"="#0066CC","ptfire-rx"="#99CCFF","ptfire-wild"="#99AAFF","ptnonipm"="#660099","rail"="#9933FF","rwc"="#CC99FF","solvents"="#ddc3f7") 

#####################plot Counties

states <- unique(temp_nocmv.df$State)
sts <- unique(temp_nocmv.df$ST)
nstates<-length(sts)
#for ( s in states) {
ns = 8
while ( ns <= nstates ) {
  s <- sts[ns]
  print(s)
  state.df <- filter(temp.df,ST s==s)
  counties <- unique(state.df$County)
  sectors_st <-unique(state.df$Sect)
 
  for ( t in sectors_st ) {
  print(t)
  if ( t == "fertilizer") {
    polls <- "NH3"
  } else if ( t == "livestock") {
    polls <- c("NH3")
  } else if ( t == "beis") {
    polls <- c("CO","VOC","NOX")
  } else if ( t == "afdust_adj") {
    polls <- c("PM25")
  } else if ( t == "airports") {
    polls <- c("CO","NOX","SO2","VOC","PM25")
  } else if ( t == "solvents") {
    polls <- c("VOC")
  } else {
    polls <- polls1
  }
    for ( p in polls ) {
  chartsubt <-paste("Pollutant: ",p," | State: ",s," | Sector: ",t,sep="")
  print(p)
  # County bar plot: all counties in each state by sector and pollutant
  plot.df <- data.frame(aggregate(cbind(emis2016v2,emis2023v2,emis2026v2,emis2032v2) ~ County, data=subset(state.df,Poll == p & Sect == t & State == s),FUN=sum,na.rm=TRUE, na.action=NULL))
  ggplot(melt(plot.df[,c('County','emis2016v2','emis2023v2','emis2026v2','emis2032v2')],id.vars=1),aes(x=County,y=value)) + geom_bar(aes(fill=variable),stat="identity",position="dodge") + theme_light() + labs(title=charttitle,subtitle=chartsubt,x="County",y="Emissions (tons/year)",fill=NULL) + scale_fill_brewer(labels=c("2016v2","2023v2","2026v2","2032v2"),palette="Spectral")+scale_y_continuous(name="Emissions (tons/year)", labels = comma) + coord_flip() + theme(axis.text.y = element_text(size = 6))
  filename <- paste(s,t,"AllCounties_2016v2_emis_bar",p,"png",sep=".")
  outdir <- paste(outdirroot,"county-barplots/",s,"/by-sector/",sep="")
  system(paste("mkdir -p ", outdir))
  ggsave(filename,plot = last_plot(),device="png",path=outdir, scale=1,height=10)
} # p
  } # t
  for ( c in counties) {
    print(c)
    clab<-gsub(" ", "", c, fixed = TRUE)
    county.df <- filter(state.df,County==c)
    fips<-unique(state.df$FIPS[(state.df$County==c)])
    plot.df <- aggregate(cbind(emis2016v2,emis2023v2,emis2026v2,emis2032v2) ~ Sect+Poll, data=county.df, FUN=sum,na.rm=FALSE, na.action=NULL)
    chartsubt <-paste("Pollutant: All | County: ",c," | State: ",s,sep="")
    dev.size("px")
    # 2016v2 platform plots: stacked bar plots by county
    ggplot(melt(plot.df,measure.vars=c('emis2016v2','emis2023v2','emis2026v2','emis2032v2'),id.vars=c('Poll','Sect')),aes(x=variable,y=value*1e-6)) + geom_col(aes(fill=Sect),na.rm=TRUE) +facet_wrap(~ Poll,ncol=3,scales="free_y") + theme_light() + labs(title=charttitle,subtitle=chartsubt,x="Platform",y="Emissions (1e6 tons/year)",fill=NULL) + scale_fill_manual(values=cvals)+scale_x_discrete(labels=c("2016v2","2023v2","2026v2","2032v2")) 
    filename <- paste(s,"_",clab,"_2016v2_emis_stackedbar_freeY",".png",sep="")
    outdir <- paste(outdirroot,"county-barplots/",s,sep="")
    ggsave(filename,plot = last_plot(),device="png",path=outdir, scale=2)
  } # c
  ns = ns + 1
  } # s







