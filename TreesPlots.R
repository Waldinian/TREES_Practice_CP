library(ggplot2)
library(tidyverse)
library(lubridate)

setwd("~/Work/UWyo/2021 Spring/TREES Workshops/Examples/BBSF_Test/")

fname <- "BBSF_Dead.sim"

Collect_Data <- function(fname){
 df <- read.csv(fname, sep = "\t", stringsAsFactors = FALSE)
  
 # Current timestamp format is yyyy:yday:h:decimal_minute, which is incredibly annoying
 # Need to convert it to ymd_hms.
 time.df <- data.frame(do.call('rbind',  # bind column together into into one dataframe...
                                    strsplit(as.character(df$ti),':',fixed=TRUE))) %>%   # ...split into multiple columns using ":" as a delimiter
   mutate(ti = NA)  # dummy values in new timestamp column
 names(time.df) <- c("year", "yday", "hour", "dec_minute", "ti")
 
 # for some reason, everything is a factor, so here's something to fix that
 for(name in names(time.df)){
   time.df[, name] <- as.integer(as.character(time.df[, name]))
 }
 
 time.df <- time.df %>% 
   mutate(ti = ymd_hms(  # convert to ymd_hms format
     paste(  # concatenate the below strings
       as.Date(as.integer(yday), origin = ymd(paste(year, "-01-01", sep=""))),  # convert yyyy-yday to yyyy-mm-dd
       " ", # just add a space
       hour, ":",  # get the hour of the day
       dec_minute*0.6, ":",  # convert decimal minutes to minutes
       "00",  # seconds is always 0
       sep = "")
   ))
 
 df <- df %>% mutate(ti = time.df$ti)
 
 return(df)
}

BBSF_Dead <- Collect_Data("BBSF_Dead.sim")
BBSF_Pinon <- Collect_Data("BBSF_Pinon.sim")

BBSF <- full_join(by = "ti", BBSF_Dead, BBSF_Pinon) %>%  # keep all values
  pivot_longer(-ti, names_to = "key", values_to = "val") %>% 
  mutate(Params = ifelse(substr(key, nchar(key) - 1, nchar(key)) == ".x", "Dead", "Alive"),
         key = substr(key, 1, nchar(key) - 2))

ggplot(data=filter(BBSF, key=="Asun")) +
  geom_line(aes(x=ti,y=val, color=Params), size=0.3)

ggplot(data=filter(BBSF, key=="NEE")) +
  geom_line(aes(x=ti,y=val, color=Params), size=0.3) +
  ylim(0,20)
