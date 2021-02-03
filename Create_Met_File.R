# Create_Met_File.R
# Takes data from Chimney Park in 2019 in converts it into a format that is readable by create_driver_file_CP.R.
# Author: Alex Fox, afox18@uwyo.edu

# todo:
# gap fill data (crudely)
# exise all the bad data (like when the IRGA fucks up)
# set a realistic window (like july and august)

library(tidyverse)
library(ggplot2)
library(lubridate)

# Path to soils data and Precip data, stored on the UWyo PETA library
SoilHF.files <- Sys.glob(Sys.glob('/Volumes/TempData/Bretfeld\ Mario/Chimney/Data/BB-SF/SoilHF/2019*/*SoilData_BBSF*.dat'))
Precip.files <- Sys.glob(Sys.glob('/Volumes/TempData/Bretfeld\ Mario/Chimney/Data/BB-SF/SoilHF/2019*/*MetData_BBSF*.dat'))
Met.files <- Sys.glob(Sys.glob("/Volumes/TempData/Bretfeld\ Mario/Chimney/Data/BB-SF/EC/7m/2019*/Converted/TOA5_9810.Met30Min.dat"))
Flux.files <- Sys.glob(Sys.glob("/Volumes/TempData/Bretfeld\ Mario/Chimney/Data/BB-SF/EC/7m/2019*/Converted/TOA5_9810.Flux30Min.dat"))

# given a list of file names, collect all the data from those files into one dataframe
Collect_Data <- function(.files){
  # takes a list of file names and combines their contents into a single dataframe.
  # also renames columns
  
  # read in the first data file
  # lines 1-4 contain header information, but only line 2 contains variable names
  .raw <- read.csv(.files[1], skip=c(1,3,4), stringsAsFactors=FALSE, na = c("","NAN")) 
  
  for (fn in .files[-1]){
    .f.raw <- read.csv(fn, skip=c(1,3,4), stringsAsFactors=FALSE, na = c("","NAN"))  # read the next data file
    .raw <- rbind(.raw, .f.raw)  # add it onto the full data file
  }
  
  .raw <- .raw %>% mutate(TIMESTAMP = ymd_hms(TIMESTAMP))  # convert to ymd_hms format
  .raw <- .raw[-which(is.na(.raw$TIMESTAMP)==TRUE), ]  # extraneous header lines have timestamp "NA," giving us a way to remove them
  
  .raw <- .raw[!duplicated(.raw$TIMESTAMP), ]  # for some reason it imports each file like 12 times, so remove duplicate timestamps
  
  # convert character vector columns to floats
  .raw <- .raw %>% 
    mutate_at(names(.raw[-1]), as.numeric)
  
  return(as_tibble(.raw))
}

# get data from files
SoilHF.raw <- Collect_Data(SoilHF.files)
Precip.raw <- Collect_Data(Precip.files)
Met.raw <- Collect_Data(Met.files[13:length(Met.files)])  # files have inconsistent variable names until the 13th file, so start there
Flux.raw <- Collect_Data(Flux.files)

# filter(as.numeric(as.POSIXct(TIMESTAMP)) >= as.numeric(as.POSIXct("2019-05-19 13:30:00")))  # filter out times before 20190519 13:30, since that's when Met.raw starts

# now merge them all together
Combine <- function(A.raw, B.raw){
  full.raw.long <- full_join(by = "TIMESTAMP", A.raw, B.raw) # keep all values
} 

All_Data.raw <- Combine(Combine(Combine(SoilHF.raw, Precip.raw), Met.raw), Flux.raw) %>% 
  mutate(BaPress_kPa = 72.4)  # didn't look for a pressure gauge, but maybe there is one?

# test plots for QA/QC
SoilTest.Plot <- ggplot(data = All_Data.raw, size=0.01) +
  geom_line(mapping = aes(x=TIMESTAMP, y=SoilT_PitA_5_Avg,), color='blue')+
  geom_line(mapping = aes(x=TIMESTAMP, y=SoilT_PitA_15_Avg), color='red')
# SoilTest.Plot

PrecipTest.Plot <- ggplot(data = All_Data.raw)+
  geom_col(mapping=aes(x=TIMESTAMP, y=Rain_mm_Tot))
# PrecipTest.Plot

MetTest.Plot <- ggplot(data = All_Data.raw)+
  geom_point(mapping = aes(x=TIMESTAMP, y=AirT_6m_Avg), size = 0.5)
# MetTest.Plot

###########################################
# Comments: 
# * there's a day in april or may with a massive (erroneous) spike in precip
# * probably because of melt.
# * only look at precip data after spring melt.
# * need to start in may anyway, because that's when the met data starts
###########################################

# To create the driver file, I'm going to duplicate the weather2013part.csv format,
# then feed that file into the create_driver_file.R script

# Weather2013Part.csv columns:
Year = year(All_Data.raw$TIMESTAMP)
DOY = yday(All_Data.raw$TIMESTAMP)
TIMESTAMP = All_Data.raw$TIMESTAMP
AirTemp_C <- All_Data.raw$AirT_6m_Avg

AirTemp.Plot <- ggplot() + geom_line(mapping = aes(x=TIMESTAMP, y=AirTemp_C))
# AirTemp.Plot

RH_fraction <- All_Data.raw$RH_6m/100
Vap_Press_kPa <- RH_fraction*0.6108*exp(17.27*T/(T + 237.3))
Vap_Press.Plot <- ggplot() + geom_point(aes(x=TIMESTAMP, y=Vap_Press_kPa), size = 0.1)
# Vap_Press.Plot

Qpar <- All_Data.raw$PAR_dn_Avg  # incoming PAR
Qpar.Plot <- ggplot() + geom_point(aes(x=TIMESTAMP, y=Qpar), size=0.1)
# Qpar.Plot

WindSpeed_m_s <- All_Data.raw$WS_2m_Avg
WS.Plot <- ggplot() + geom_point(aes(x=TIMESTAMP, y=WindSpeed_m_s), size = 0.1)
# WS.Plot

WindDir_Deg <- All_Data.raw$WD_2m
WD.Plot <- ggplot() + geom_point(aes(x = WindSpeed_m_s, y = WindDir_Deg, color = hour(TIMESTAMP)), size = 0.8, alpha = 0.5) + coord_polar(theta = "y")
# WD.Plot  # very cool plot, can see a slight bias to the West

Rain_Tot <- All_Data.raw$Rain_mm_Tot

SoilTemp_5cm_C <- All_Data.raw %>%
  pivot_longer(-TIMESTAMP, names_to = "pit", values_to = "temp") %>%  # longify
  filter(pit == "SoilT_PitA_5_Avg" | 
           pit == "SoilT_PitB_5_Avg" | 
           pit == "SoilT_PitC_5_Avg" | 
           pit == "SoilT_PitD_5_Avg") %>%   # want the 5cm soil pits
  group_by(TIMESTAMP) %>%  # so that we can average all four soil pits in one timestamp
  summarize(temp = mean(temp, na.rm = TRUE))  # take the mean of all four soil pits at one timestamp
SoilTemp_5cm_C <- SoilTemp_5cm_C$temp
Soil5.Plot <- ggplot() + geom_point(aes(x=TIMESTAMP, y=SoilTemp_5cm_C), size = 0.1)
# Soil5.Plot

SoilTemp_15cm_C <- All_Data.raw %>%
  pivot_longer(-TIMESTAMP, names_to = "pit", values_to = "temp") %>%
  filter(pit == "SoilT_PitA_15_Avg" | 
           pit == "SoilT_PitB_15_Avg" | 
           pit == "SoilT_PitC_15_Avg" | 
           pit == "SoilT_PitD_15_Avg") %>%  # want the 15cm pits
  group_by(TIMESTAMP) %>%  #same as above chunk
  summarize(temp = mean(temp, na.rm = TRUE))  # same as above chunk
SoilTemp_15cm_C <-  SoilTemp_15cm_C$temp
Soil15.Plot <- ggplot() + geom_point(aes(x=TIMESTAMP, y=SoilTemp_15cm_C), size = 0.1)
# Soil15.Plot

BaPress_kPa <- All_Data.raw$BaPress_kPa

CO2_atm <- All_Data.raw$rho_c_LI7500_Avg

weather <- data.frame(Year, DOY, TIMESTAMP, AirTemp_C, RH_fraction, Vap_Press_kPa, Qpar, WindSpeed_m_s, WindDir_Deg, Rain_Tot, SoilTemp_5cm_C, SoilTemp_15cm_C, BaPress_kPa, CO2_atm)



#################
# Now we can build the actual driver file
#################


#TREES dates appear as YEARDAY, e.g. 2021001 for January 1, 2021
weather$date <- as.matrix((weather$Year*1000) + (weather$DOY))

weather$TIMESTAMP <- paste(substr(weather$TIMESTAMP,1,10), substr(weather$TIMESTAMP,12,19)) # for some reason, the TIMESTAMP entries got all screwed up

nrows <- length(weather$date)
treesMet <- array(data=0,dim=c(nrows,18))
colnames(treesMet)<-c("Date","Time","u_ref","t_ref","d_ref","precip","Qpar","t_canopy","d_canopy",
                      "p_atm","CO2_atm","Ts0","Tsurf","Troot","Zw","xylemScalar","NEEobs","Ec")
treesMet <- as_tibble(treesMet)

treesMet <- treesMet %>% 
  mutate(Date = weather$date,
         Time = hour(weather$TIMESTAMP) + minute(weather$TIMESTAMP)/60,
         u_ref = if_else(weather$WindSpeed_m_s < 0.01, 0.01, weather$WindSpeed_m_s),
         t_ref = weather$AirTemp_C,
         d_ref = (1 - weather$RH_fraction)*0.61094*exp(17.625*t_ref/(t_ref+243.04)),  # saturation vapor pressure
         precip = weather$Rain_Tot,
         Qpar = weather$Qpar,
         t_canopy = t_ref,  # no canopy obs., so just use t_ref
         d_canopy = d_ref,  # same
         p_atm = weather$BaPress_kPa,
         CO2_atm = weather$CO2_atm,
         Ts0 = 0.5*(weather$SoilTemp_5cm_C + t_canopy),  # don't have soil surface, so use an average of 5cm and air
         Tsurf = weather$SoilTemp_5cm_C,
         Troot = weather$SoilTemp_15cm_C,
         Zw = -10,  # unreachable water table
         xylenScalar = 1,  # can be used to manipulate hydraulics. Default 1. 0.99 resets min. xylem pressures to sol water pressurem and remove memory of past droughts
         NEEobs = -999,  # legacy column for use with MCMC algorithm
         Ec = -999  # same
  )

write_delim(treesMet, "treesMet.txt", delim = "\t")

