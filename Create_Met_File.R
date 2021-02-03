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

setwd("~/Work/UWyo/2021 Spring/TREES Workshops/Examples/CP_2019")

# AmeriFlux variable names: 
# https://ameriflux.lbl.gov/data/aboutdata/data-variables/
#
# Variables of interest: (Ameriflux -> TREES)
# TIMESTAMP_START -> Year
# TIMESTAMP_START -> DOY
# TIMESTAMP_START -> TIMESTAMP
# T_SONIC_1_1_1 -> AirTemp_C 
# RH_1_1_1 -> RH_fraction 
# RH_1_1_1 -> Vap_Press_kPa
# From Met file -> Qpar
# WS_1_1_1 -> WindSpeed_m_s
# WD_1_1_1 -> WindDir_Deg
# From precip file -> Rain_Tot
# From soil file -> SoilTemp_5cm_C
# From soil file -> SoilTemp_15cm_C
# PA_1_1_1 -> BaPress_kPa
# CO2_1_1_1 -> CO2_atm

###################################################################################################
# This Part of the Program aggregates the data. However, if you don't have access to the PETA     #
# library, you can skip this code and go down the line that loads "BBSF_2019_All_Weather.csv"     #
###################################################################################################


# Path to soils data and Precip data, stored on the UWyo PETA library
SoilHF.files <- Sys.glob(Sys.glob('/Volumes/TempData/Bretfeld\ Mario/Chimney/Data/BB-SF/SoilHF/2019*/*SoilData_BBSF*.dat'))
Precip.files <- Sys.glob(Sys.glob('/Volumes/TempData/Bretfeld\ Mario/Chimney/Data/BB-SF/SoilHF/2019*/*MetData_BBSF*.dat'))
Met.files <- Sys.glob(Sys.glob("/Volumes/TempData/Bretfeld\ Mario/Chimney/Data/BB-SF/EC/7m/2019*/Converted/TOA5_9810.Met30Min.dat"))
Flux.files <- Sys.glob(Sys.glob("AmeriFlux_CPk-BBSF-7m.csv"))  # this file can actually be used to get most of the necessary quantities

# given a list of file names, collect all the data from those files into one dataframe
Collect_Data <- function(.files){
  # takes a list of file names and combines their contents into a single dataframe.
  # also renames columns
  
  # read in the first data file
  # lines 1-4 contain header information, but only line 2 contains variable names
  .raw <- read.csv(.files[1], skip=1, stringsAsFactors=FALSE, na = c("","NAN")) 
  
  for (fn in .files[-1]){
    .f.raw <- read.csv(fn, skip=1, stringsAsFactors=FALSE, na = c("","NAN"))  # read the next data file
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

# Ameriflux file needs to be handled slightly differently
Flux.raw <- read.csv(Flux.files, stringsAsFactors = FALSE, na = c("","NAN", "NaN", "-9999"))[-1, ] %>%  # import everything but the second header line 
  mutate(TIMESTAMP = ymd_hms(paste(substr(TIMESTAMP_START, 1, 4), "-", 
                                 substr(TIMESTAMP_START, 5, 6), "-", 
                                 substr(TIMESTAMP_START, 7, 8), " ", 
                                 substr(TIMESTAMP_START, 9, 10), ":", 
                                 substr(TIMESTAMP_START, 11, 12), ":", 
                                 "00", 
                                 sep = ""))) %>%  # convert from yyymmddhhmmss to yyyy-mm-dd hh:mm:ss format
  mutate_at(names(Flux.raw[, -length(names(Flux.raw))]), as.numeric)  # convert everything but timestamp to numeric

# now merge them all together
Combine <- function(A.raw, B.raw){
  full.raw.long <- full_join(by = "TIMESTAMP", A.raw, B.raw) # keep all values
} 

All_Data.raw <- Combine(Combine(Combine(SoilHF.raw, Precip.raw), Met.raw), Flux.raw)

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

CO2Test.Plot <- ggplot(data = All_Data.raw)+
  geom_point(mapping = aes(x=TIMESTAMP, y=CO2_1_1_1), size = 0.5)
# CO2Test.Plot

write.csv(All_Data.raw, "BBSF_2019_All_Weather.csv")

####################################################################################################
# Comments: 
# * there's a day in april or may with a massive (erroneous) spike in precip
# * probably because of melt.
# * only look at precip data after spring melt.
# * need to start in may anyway, because that's when the met data starts
# * looks like the IRGA was calibrated in july 2019 and december 2018, but I'll deal with that later
#####################################################################################################

# To create the driver file, I'm going to duplicate the weather2013part.csv format,
# then feed that file into the create_driver_file.R script
# in writing this code, I realized that there are several things that I really didn't need to do,
# but they don't really screw anything up, so if anything seems redudant here, that's because it is.

All_Data.raw <- read.csv("BBSF_2019_All_Weather.csv")

# Take columns from All_Data.raw and put them into individual vectors
Year = year(All_Data.raw$TIMESTAMP)
DOY = yday(All_Data.raw$TIMESTAMP)
TIMESTAMP = All_Data.raw$TIMESTAMP
AirTemp_C <- All_Data.raw$T_SONIC_1_1_1

AirTemp.Plot <- ggplot() + geom_line(mapping = aes(x=TIMESTAMP, y=AirTemp_C))
# AirTemp.Plot

RH_fraction <- All_Data.raw$RH_1_1_1/100
Vap_Press_kPa <- RH_fraction*0.6108*exp(17.27*T/(T + 237.3))
Vap_Press.Plot <- ggplot() + geom_point(aes(x=TIMESTAMP, y=Vap_Press_kPa), size = 0.1)
# Vap_Press.Plot

Qpar <- All_Data.raw$PAR_dn_Avg  # incoming PAR
Qpar.Plot <- ggplot() + geom_point(aes(x=TIMESTAMP, y=Qpar), size=0.1)
# Qpar.Plot

WindSpeed_m_s <- All_Data.raw$WS_1_1_1
WS.Plot <- ggplot() + geom_point(aes(x=TIMESTAMP, y=WindSpeed_m_s), size = 0.1)
# WS.Plot

WindDir_Deg <- All_Data.raw$WD_1_1_1
WD.Plot <- ggplot() + 
  geom_point(aes(x = WindSpeed_m_s, y = WindDir_Deg, color = hour(TIMESTAMP)), size = 0.8, alpha = 0.05) + 
  coord_polar(theta = "y") + 
  scale_color_continuous(low="blue", high="red")
WD.Plot  # very cool plot

Rain_Tot <- All_Data.raw$Rain_mm_Tot

SoilTemp_5cm_C <- All_Data.raw %>%
  pivot_longer(-TIMESTAMP, names_to = "pit", values_to = "temp") %>%  # longify
  filter(pit == "SoilT_PitA_5_Avg" | 
           pit == "SoilT_PitB_5_Avg" | 
           pit == "SoilT_PitC_5_Avg" | 
           pit == "SoilT_PitD_5_Avg") %>%   # want the 5cm soil pits
  group_by(TIMESTAMP) %>%  # group by timestamp so that we can average all four soil pits in one timestamp
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

BaPress_kPa <- All_Data.raw$PA_1_1_1
BaPress.Plot <- ggplot() + geom_point(aes(x=TIMESTAMP, y=BaPress_kPa))
# BaPress.Plot

CO2_atm <- All_Data.raw$CO2_1_1_1
CO2.Plot <- ggplot() + geom_point(aes(x=TIMESTAMP, y=CO2_atm))
# CO2.Plot

T_canopy <- All_Data.raw$T_CANOPY_1_1_1
T_can.Plot <- ggplot() + geom_point(aes(x=TIMESTAMP, y=T_canopy))
# T_can.Plot

# write all the columns into one "weather" dataframe
weather <- data.frame(Year, DOY, TIMESTAMP, AirTemp_C, RH_fraction, Vap_Press_kPa, Qpar, WindSpeed_m_s, WindDir_Deg, Rain_Tot, SoilTemp_5cm_C, SoilTemp_15cm_C, BaPress_kPa, CO2_atm, T_canopy)


###########################################
# Now we can build the actual driver file #
###########################################


#TREES dates appear as YEARDAY, e.g. 2021001 for January 1, 2021
weather$date <- weather$Year*1000 + weather$DOY

nrows <- length(weather$date)
treesMet <- array(data=0,dim=c(nrows,18))  # create an empty array
colnames(treesMet)<-c("Date","Time","u_ref","t_ref","d_ref","precip","Qpar","t_canopy","d_canopy",
                      "p_atm","CO2_atm","Ts0","Tsurf","Troot","Zw","xylemScalar","NEEobs","Ec")  # give it these column names
treesMet <- as_tibble(treesMet)  # make it a tibble. I don't really know what an array is, except that it doesn't work with the below code

# define all the columns in the driver file
treesMet <- treesMet %>% 
  mutate(Date = weather$date,
         Time = hour(weather$TIMESTAMP) + minute(weather$TIMESTAMP)/60,
         u_ref = if_else(weather$WindSpeed_m_s < 0.01, 0.01, weather$WindSpeed_m_s),  # lower limit of 0.01 m/s
         t_ref = weather$AirTemp_C,
         d_ref = (1 - weather$RH_fraction)*0.61094*exp(17.625*t_ref/(t_ref+243.04)),  # saturation vapor pressure
         precip = weather$Rain_Tot,
         Qpar = weather$Qpar,
         t_canopy = weather$T_canopy,
         d_canopy = (1 - weather$RH_fraction)*0.61094*exp(17.625*t_canopy/(t_canopy + 243.04)),  # same
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

treesMet[treesMet == "NaN"] <- NA  # don't know what those "NaN" strings are there from.
treesMet <- treesMet %>% 
  filter(Date > 2019211 & Date < 2019246)  # dates with good chunks of continuous data

# now to gapfill, since TREES can't handle NAs.
# gap fill by going through each column and seeing where there are NAs and then filling with the average.

CO2_na <- which(is.na(treesMet$CO2_atm))  # to check later, to make sure that it worked
Date_na <- which(is.na(treesMet$Date))  # if this has NAs, you have a problem
Time_na <- which(is.na(treesMet$Time))  # if this has NAs you have a problem
if(length(Date_na) + length(Time_na) != 0) print("You have NAs in your time columns, stopping program...")
stopifnot(length(Date_na) + length(Time_na) == 0)

for (col in 1:length(treesMet)){  # loop over columns
  for (row in 1:length(treesMet[[col]])){  # loop over rows
    if (is.na(treesMet[row, col])){  # detect an NA
      last_finite <-  row - 1  # grab the index of the previous entry, which should be finite.
      next_finite <- row  # we'll be looping over this in the while loop below
      reached_end <- FALSE  # for the edge case where we reach the end without finding a finite element.
      
      while (is.na(treesMet[next_finite, col])){  # check to see if the current element is an NA
        if (next_finite == length(treesMet[[col]])){
          reached_end <- TRUE
          break  # if we've reached the end of the list, exit the loop
        }
        
        next_finite <- next_finite + 1  # if it is, check the next element. Otherwise, exit the loop
      }
      
      if (reached_end == TRUE){  # edge case (see above)
        gap_fill <- treesMet[last_finite, col]  # fill using the last finite element
        for(na_index in (last_finite + 1):length(tressMet[[col]])) treesMet[na_index, col] <- gap_fill
      }
      
      else{  # not an edge case, and there is next finite element
        gap_fill <- 0.5*(treesMet[last_finite, col] + treesMet[next_finite, col])  # take the average of the last and next finite numbers
        for(na_index in (last_finite + 1):(next_finite - 1)) treesMet[na_index, col] <- gap_fill
      }
    }
  }
}

write_delim(treesMet, "treesMet.txt", delim = "\t")

# some quick QA/QC

ggplot(data = treesMet) +
  geom_point(aes(x=Time, y=Qpar), size=0.3)  # compress all data into one day and plot, choose your favorite variable

