# Create_Met_File.R
# Takes data from Chimney Park in 2019 in converts it into a format that is readable by create_driver_file_CP.R.
# Author: Alex Fox, afox18@uwyo.edu

library(tidyverse)
library(ggplot2)
library(lubridate)

# Path to soils data and Precip data, stored on the UWyo PETA library
SoilHF.files <- Sys.glob(Sys.glob('/Volumes/TempData/Bretfeld\ Mario/Chimney/Data/BB-SF/SoilHF/2019*/*SoilData_BBSF*.dat'))
Precip.files <- Sys.glob(Sys.glob('/Volumes/TempData/Bretfeld\ Mario/Chimney/Data/BB-SF/SoilHF/2019*/*MetData_BBSF*.dat'))
Met.files <- Sys.glob(Sys.glob("/Volumes/TempData/Bretfeld\ Mario/Chimney/Data/BB-SF/EC/7m/2019*/Converted/TOA5_9810.Met30Min.dat"))

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

# filter(as.numeric(as.POSIXct(TIMESTAMP)) >= as.numeric(as.POSIXct("2019-05-19 13:30:00")))  # filter out times before 20190519 13:30, since that's when Met.raw starts

# now merge them all together
Combine <- function(A.raw, B.raw){
  full.raw.long <- full_join(by = "TIMESTAMP", A.raw, B.raw) # keep all values
} 

All_Data.raw <- Combine(Combine(SoilHF.raw, Precip.raw), Met.raw)

# test plots for QA/QC
SoilHF.filtered <- SoilHF.raw[seq(1, length(SoilHF.raw$TIMESTAMP), 48), ]  # plot once a day
SoilTest.Plot <- ggplot(data = SoilHF.filtered, size=0.01) +
  geom_line(mapping = aes(x=TIMESTAMP, y=SoilT_PitA_5_Avg,), color='blue')+
  geom_line(mapping = aes(x=TIMESTAMP, y=SoilT_PitA_15_Avg), color='red')
# SoilTest.Plot

PrecipTest.Plot <- ggplot(data = Precip.raw)+
  geom_col(mapping=aes(x=TIMESTAMP, y=Rain_mm_Tot))
# PrecipTest.Plot

MetTest.Plot <- ggplot(data = Met.raw)+
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
Year = year(SoilHF.raw$TIMESTAMP)
DOY = yday(SoilHF.raw$TIMESTAMP)
TIMESTAMP = SoilHF.raw$TIMESTAMP
AirTemp_C <- Met.raw$AirT_6m_Avg
ggplot() + geom_line(mapping = aes(x=TIMESTAMP, y=AirTemp_C))

RH_fraction <- Met.raw$RH_6m/100

Vap_Press_kPa <- RH_fraction*0.6108*exp(17.27*T/(T + 237.3))
ggplot() +
  geom_point(aes(x=Met.raw$TIMESTAMP, y=Vap_Press_kPa), size = 0.1)

Qpar <- Met.raw$PAR_dn_Avg  # incoming PAR
ggplot()+
  geom_point(aes(x=Met.raw$TIMESTAMP, y=Qpar), size=0.1)

WindSpeed_m_s <- Met.raw$WS_2m_Avg
ggplot() +
  geom_point(aes(x=Met.raw$TIMESTAMP, y=WindSpeed_m_s), size = 0.1)

WindDir_Deg <- Met.raw$WD_2m
ggplot() +
  geom_point(aes(x = Met.raw$TIMESTAMP, y = WindDir_Deg, color = Met.raw$TIMESTAMP), size = 0.1)+
  coord_polar(theta = "y")

Rain_Tot <- Precip.raw$Rain_mm_Tot

SoilTemp_5cm_C <- SoilHF.raw %>%
  pivot_longer(-TIMESTAMP, names_to = "pit", values_to = "temp") %>%
  filter(pit == "SoilT_PitA_5_Avg" | pit == "SoilT_PitB_5_Avg" | pit == "SoilT_PitC_5_Avg" | pit == "SoilT_PitD_5_Avg") %>% 
  group_by(TIMESTAMP) %>%
  summarize(temp = mean(temp, na.rm = TRUE))
SoilTemp_5cm_C <-  SoilTemp_5cm_C$temp

SoilTemp_15cm_C <- SoilHF.raw %>%
  pivot_longer(-TIMESTAMP, names_to = "pit", values_to = "temp") %>%
  filter(pit == "SoilT_PitA_15_Avg" | pit == "SoilT_PitB_15_Avg" | pit == "SoilT_PitC_15_Avg" | pit == "SoilT_PitD_15_Avg") %>% 
  group_by(TIMESTAMP) %>% 
  summarize(temp = mean(temp, na.rm = TRUE))
SoilTemp_15cm_C <-  SoilTemp_15cm_C$temp

# BaPress_kPa

forDriver <- data.frame(Year, DOY, TIMESTAMP, AirTemp_C, RH_fraction, Vap_Press_kPa, Qpar, WindSpeed_m_s, WindDir_Deg, Rain_Tot, SoilTemp_5cm_C, SoilTemp_15cm_C)


