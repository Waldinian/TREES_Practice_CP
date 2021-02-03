#
#Script for converting meteorological data into a TREES driver file
#DSM 2019, updated 2021
#
#This file is for illustrative purposes. It takes a real meteorological file
#with typical variables, and writes out a TREES driver file. Meteorological
#data come in various forms, with different variables, different headings,
#and different units. Consequently, these files are hard to generalize.
#
#What this file does give you is a template for creating a driver file that
#can be read by TREES, a guide to the 

#
#Set working directory and read in an example meteorological data file
#The source file is structured with the following columns:
#Year	
#DOY	
#TIMESTAMP	
#AirTemp_C	
#RH_fraction	
#Vap_Press_kPa	
#HrlySolRad_kJ_m^2_min^1	
#WindSpeed_m_s^1	
#WindDir_Deg	
#WindDir_STDD_Deg	
#Rain-Tot	
#SoilTemp_5cm_C	
#SoilTemp_15cm_C	
#HWG_maxspeed_m_s^1	
#HWG_time	HWG_Dir	
#BaPress_kPa	
#ETr	
#ETo	
#ETr-Daily	
#ETo-Daily	
#Rain-Daily
#

library(tidyverse)

weather<-read.csv("CP_Met_For_Driver.csv", stringsAsFactors = FALSE)

#TREES dates appear as YEARDAY, e.g. 2021001 for January 1, 2021
weather$date <- as.matrix((weather$Year*1000) + (weather$DOY))

weather$TIMESTAMP <- paste(substr(weather$TIMESTAMP,1,10), substr(weather$TIMESTAMP,12,19)) # for some reason, the TIMESTAMP entries got all screwed up

nrows0 <- length(weather$date)
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
         airT = weather$AirTemp_C,
         svp = (1 - weather$RH_fraction)*0.61094*exp(17.625*airT/(airT+243.04)))
#d_ref is vapor pressure deficit in kPa
  airT <- weather[i,"AirTemp_C"]
  svp <- (1-weather[i,"RH_fraction"])*0.61094*exp(17.625*airT/(airT+243.04))
  treesMet[i1,"d_ref"] <- svp
  if (i < nrows0)
  {
    airT <- 0.5*(weather[i,"AirTemp_C"]+weather[i+1,"AirTemp_C"])
    rh <- 0.5*(weather[i,"RH_fraction"]+weather[i+1,"RH_fraction"])
    svp2 <- (1-rh)*0.61094*exp(17.625*airT/(airT+243.04))
    treesMet[i2,"d_ref"] <- 0.5*(svp+svp2)
  }
  else
  {
    airT <- weather[i,"AirTemp_C"]
    svp2 <- (1-weather[i,"RH_fraction"])*0.61094*exp(17.625*airT/(airT+243.04))
    treesMet[i2,"d_ref"] <- svp2
  }
#
#precip in mm
#
  treesMet[i1,"precip"] <- weather[i,"Rain.Tot"]
  treesMet[i2,"precip"] <- 0
#
#Qpar umol m-2 s-1 = kj m-2 min-1 * min s-1 * J kJ-1 * umol J-1
#                  = x * 1/60 * 1000 / 2.12766 / 0.235 = x * 33.33333
#
#Note: Pay careful attention to the units provided in the meteorological data file
#Note: 30 minute Qpar is already in the CP Met data file
#TREES assumes this input is photosynthetically active radiation
#
  # parConvert = 33.33333
  # treesMet[i1,"Qpar"] <- weather[i,"HrlySolRad_kJ_m.2_min.1"]*parConvert
  # if (i < nrows0)
  # {
  #   treesMet[i2,"Qpar"] <- 0.5*(weather[i,"HrlySolRad_kJ_m.2_min.1"]+weather[i+1,"HrlySolRad_kJ_m.2_min.1"]) * parConvert
  # }
  # else
  # {
  #   treesMet[i2,"Qpar"] <- weather[i,"HrlySolRad_kJ_m.2_min.1"] * parConvert
  # }
#
#t_canopy will have the same values as t_ref since we don't have within canopy observations
#
  treesMet[i1,"t_canopy"] <- treesMet[i1,"t_ref"]
  treesMet[i2,"t_canopy"] <- treesMet[i2,"t_ref"]
#
#d_canopy will have the same values as d_ref since we don't have within canopy observations
#
  treesMet[i1,"d_canopy"] <- treesMet[i1,"d_ref"]
  treesMet[i2,"d_canopy"] <- treesMet[i2,"d_ref"]
#
#p_atm is barometric pressure in kPa
#
  treesMet[i1,"p_atm"] <- weather[i,"BaPress_kPa"]
  treesMet[i2,"p_atm"] <- weather[i,"BaPress_kPa"]
#
#CO2_atm, no data provided so we will assign a constant, units = ppm
#
  treesMet[i1,"CO2_atm"] <- 400
  treesMet[i2,"CO2_atm"] <- 400
#
#Note: Ts0, Tsurf, and Troot are soil surface, shallow soil, and root zone average
#temperatures, respectively, which may not be provided in many cases. 
#Ts0, here we use an average of air and surface soil layer temperature
#
#This next line detects missing data
  if (!(is.finite(weather[i,"SoilTemp_5cm_C"]))) weather[i,"SoilTemp_5cm_C"] = weather[i-1,"SoilTemp_5cm_C"]
  treesMet[i1,"Ts0"] <- 0.5*(weather[i,"SoilTemp_5cm_C"]+treesMet[i1,"t_canopy"])
  treesMet[i2,"Ts0"] <- 0.5*(weather[i,"SoilTemp_5cm_C"]+treesMet[i2,"t_canopy"])
#
#Tsurf
#
  treesMet[i1,"Tsurf"] <- weather[i,"SoilTemp_5cm_C"]
  treesMet[i2,"Tsurf"] <- weather[i,"SoilTemp_5cm_C"]
#
#Troot
#
#This next line detects missing data
  if (!(is.finite(weather[i,"SoilTemp_15cm_C"]))) weather[i,"SoilTemp_15cm_C"] = weather[i-1,"SoilTemp_15cm_C"]
  treesMet[i1,"Troot"] <- weather[i,"SoilTemp_15cm_C"]
  treesMet[i2,"Troot"] <- weather[i,"SoilTemp_15cm_C"]
#
#Zw, no water table near roots so we'll make it deep, units = m
#
  treesMet[i1,"Zw"] <- 10
  treesMet[i2,"Zw"] <- 10
#
#xylemScalar, this column can be used to manipulate the hydraulic model, 
#     but by default values should be 1
#A value of 0.99 will reset the minimum xylem pressures to soil water pressure, and 
#   thus remove the memory of past droughts on xylem cavitation. This reset is 
#   immediate, and so it may be unrealistic for your system. 
#
  treesMet[i1,"xylemScalar"] <- 1
  treesMet[i2,"xylemScalar"] <- 1
#
#NEEobs, legacy column for when TREES was used at eddy covariance sites, for use with MCMC algorithm
#
  treesMet[i1,"NEEobs"] <- -999
  treesMet[i2,"NEEobs"] <- -999
#
#Ec, legacy column for when TREES was used at eddy covariance sites, for use with MCMC algorithm
#
  treesMet[i1,"Ec"] <- -999
  treesMet[i2,"Ec"] <- -999
}

#Tab separated! Be careful that you don't replace tabs with spaces or commas
write.table(treesMet, paste(subfolder,"treesMet",".txt",sep=""),
            append=FALSE,sep="\t", eol="\n", row.names=FALSE, col.names=TRUE)

####End of file####
