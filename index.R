# Script for Big Data 
# 2018-05-11
# Author: Prita 

# checks for packages to make sure they are installed
pkgs <- c("ggplot2", "RCurl", "dplyr", "tidyr", "readr", "plotly", "here",
          "stringr","tidyverse", "reshape2")
for(i in pkgs){
  if(!i %in% installed.packages()){
    install.packages(i)
  }
}

# Loads up packages from library
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(plotly)
library(lubridate)
library(stringr)
library(here)
library(tidyverse)
library(reshape2)

theme_set(theme_bw())

#setting the working directory
setwd("C:/URI/Spring 18/Big Data/final/project/final")
getwd()

#------------------------------------------------------------------------------
#### 1) Indonesia Fishing Trade from Indonesia's Fish Quarantine Inspection Agency in 2015 - 2017 ####
# In this csv-file you will find:
# 1) monthly trade (export and import) from and to Indonesia
# 2) annually export data using 4 digit hscode (combine hscode 3***(fish), 16**(crab), 18**(shrimp))
# 3) monthly export data using 6 digit hscode
# 4) monthly import data using 6 digit hscode
# Data source: UN Comtrade

trade_flow <- read.csv("data/trade_flow.csv", header=TRUE,sep=",")
trade <- read.csv("data/trade.csv", header=TRUE,sep=",")
fish_trade_4_digit <- read.csv("data/uncomtrade_4_hscode.csv", header=TRUE,sep=",")
fish_trade_6_digit_export <- read.csv("data/uncomtrade_6_hscode_export.csv", header=TRUE,sep=",")
fish_trade_6_digit_import <- read.csv("data/uncomtrade_6_hscode_import.csv", header=TRUE,sep=",")
fish_trade <- read.csv("data/indonesia_fishing_trade.csv", header=TRUE,sep=",")

#### Trade Balance Flow USA and Indonesia ####

# Prepare time series trade balance flow data using ts
usa<-ts(trade_flow$Value[trade_flow$LOCATION=="USA"],start=c(1990,1),frequency=12)
ina<-ts(trade_flow$Value[trade_flow$LOCATION=="INA"],start=c(1990,1),frequency=12)
# Plot a Trade Balance Flow
par(mar=c(5,5,2,2),las=1,bty="n",cex.lab=2,cex.axis=2)
plot(usa,axes=FALSE,xlab="",ylab="",xlim=c(1990,2018.5),ylim=c(-80,120),lwd=2)
lines(ina,lty=2,col="firebrick3",lwd=2)
text(2017,ina[305],"Indonesia",cex=1.5)
text(2018.5,usa[329],"USA",cex=1.5)
text(1995,100,"Trade balance \n (in billion USD)",cex=2)
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-1)

#### Bilateral Trade USA Indonesia ####

# Prepare time series bilateral trade data using ts  
im<-ts(trade$Imports,start=c(1985,1))
ex<-ts(trade$Exports,start=c(1985,1))
tb<-ts((ex[-33]-im[-33])/1000,start=c(1985,1))
# Plot figure
plot(tb,axes=FALSE,xlab="",ylab="",type="b",lwd=2)
axis(1,tick=FALSE);axis(2,tick=FALSE,line=-2)
text(1995,-300,"US-Indonesia bilateral trade balance \n  (in billion USD, nominal)",cex=2)

#### Fisheries Trade USA Indonesia ####

#draw a graph that show a declining on 2015
ggplot(fish_trade_4_digit, aes(x = year, group=1)) +
  labs(x = NULL, y = NULL) + 
  theme(plot.title = element_text(size = 12)) + 
  geom_line(aes(y = fish), colour="#f8766d") 

#suspect it's happened because the moratorium policy begin that shutdown all the foreign fishing vessel, no competition to catch a fish at high sea

#find the monthly value of the most exported fish (tuna, lobster, crabs, shrimp)
tuna <- fish_trade_6_digit_export  %>% 
  filter(hscode %in% c(302310000, 302320000, 302330000, 302340000, 302390000,
                       303410000, 303420000, 303430000, 303440000, 303460000,
                       303490000, 304870000)) %>%
  select (hscode_desc, val_01, val_02, val_03, val_04, val_05, val_06, val_07, 
          val_08, val_09, val_10, val_11, val_12)

chart_tuna <- melt(tuna, id = 'hscode')
names(chart_tuna) <- c('x','func', 'value')

ggplot(chart_tuna, aes(x)) + geom_line(aes(func, value))

shrimp <- fish_trade_6_digit_export  %>% 
  filter(hscode %in% c(306160000, 306171010, 306171020, 306171090, 306172010, 306172030, 
                       306172090, 306173000, 306179000, 306261000, 306262000, 306263000, 
                       306264900, 306271200, 306271900, 306271200, 306271900, 306272200, 
                       306272900, 306273100, 306273200, 306273900, 306274100, 306274900, 
                       306279900)) %>% 
  select (hscode_desc, val_01, val_02, val_03, val_04, val_05, val_06, val_07, 
          val_08, val_09, val_10, val_11, val_12) 

lobster <- fish_trade_6_digit_export  %>% 
  filter(hscode %in% c(306211000,306212000,306213000,306219900,306221000,306222000,
                       306223000,306229100)) %>% 
  select (hscode_desc, val_01, val_02, val_03, val_04, val_05, val_06, val_07, 
          val_08, val_09, val_10, val_11, val_12) 

crab <- fish_trade_6_digit_export  %>% 
  filter(hscode_desc %in% c(306141000,306149000,306241000,306242000,306249100,306249910,306249990)) %>% 
  select (val_01, val_02, val_03, val_04, val_05, val_06, val_07, 
          val_08, val_09, val_10, val_11, val_12) 




