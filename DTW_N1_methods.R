#DTW N1 VIRADEL-PEG, VIRADEL-Filtration, PEG-Filtration, gc/L as an example
# Resource: https://www.datatechnotes.com/2018/11/dynamic-time-warping-example-in-r.html
# Resource: https://dtw.r-forge.r-project.org/
# Resource: https://dynamictimewarping.github.io/
###################################################################################################
# VIRADEL-PEG
#gc/L
library(dtw)
library(ggplot2)
library(openxlsx)
library(lubridate)
library(scales)
library(ggpubr)
library(tidyverse)
library(synchrony)
setwd("~/Documents/Dell F/RLanguage/Comparison2")
data1<- read.xlsx('cases_R_gcl.xlsx',sheet="VIRADEL",colNames=TRUE, rowNames=FALSE, na.strings="NA")
Case<- read.xlsx('cases_R_gcl.xlsx',sheet="PEG",colNames=TRUE, rowNames=FALSE, na.strings="NA")
Case_data1<- merge(Case, data1, by = "Date", all = TRUE)
Case_data1$Date<- as.Date(Case_data1$Date,origin = "1899-12-30")
data_1 <- data.frame(
  Date = Case_data1$Date,
  N1V = as.numeric(Case_data1$N1V),
  N1P = as.numeric(Case_data1$N1P)
)
alignment<-dtw(Case_data1$N1V,Case_data1$N1P,keep=TRUE)
plot(dtw(Case_data1$N1V,Case_data1$N1P,keep=TRUE,step=rabinerJuangStepPattern(6,"c")),type="twoway",offset=-2,xlab = "Day", ylab = "Gene concentration",
     main=paste("DTW matching"),cex.lab = 1.5,cex.axis = 1.5)
d <- alignment$distance
dis <- round(d, digits = 1)
plot(alignment,type="threeway",xlab = "VIRADEL N1 gc/L", ylab = "PEG N1 gc/L", 
     main=paste("DTW minimum path with minimum distance = ",dis,sep = ""),
     cex.lab = 1.5,cex.axis = 1.5) 

###################################################################################################
# VIRADEL-Filtration
#gc/L
library(dtw)
library(ggplot2)
library(openxlsx)
library(lubridate)
library(scales)
library(ggpubr)
library(tidyverse)
library(synchrony)
setwd("~/Documents/Dell F/RLanguage/Comparison2")
data1<- read.xlsx('cases_R_gcl.xlsx',sheet="VIRADEL",colNames=TRUE, rowNames=FALSE, na.strings="NA")
Case<- read.xlsx('cases_R_gcl.xlsx',sheet="Filtration",colNames=TRUE, rowNames=FALSE, na.strings="NA")
Case_data1<- merge(Case, data1, by = "Date", all = TRUE)
Case_data1$Date<- as.Date(Case_data1$Date,origin = "1899-12-30")
data_1 <- data.frame(
  Date = Case_data1$Date,
  N1V = as.numeric(Case_data1$N1V),
  N1M = as.numeric(Case_data1$N1M)
)
alignment<-dtw(Case_data1$N1V,Case_data1$N1M,keep=TRUE)
plot(dtw(Case_data1$N1V,Case_data1$N1M,
         keep=TRUE,step=rabinerJuangStepPattern(6,"c")),type="twoway",offset=-2,
     xlab = "Day", ylab = "Gene concentration",
     main=paste("DTW matching"),cex.lab = 1.5,cex.axis = 1.5)
d <- alignment$distance
dis <- round(d, digits = 1)
plot(alignment,type="threeway",xlab = "VIRADEL N1 gc/L", ylab = "Filtration N1 gc/L", 
     main=paste("DTW minimum path with minimum distance = ",dis,sep = ""),
     cex.lab = 1.5,cex.axis = 1.5) 

###################################################################################################
# PEG-Filtration
#gc/L
library(dtw)
library(ggplot2)
library(openxlsx)
library(lubridate)
library(scales)
library(ggpubr)
library(tidyverse)
library(synchrony)
setwd("~/Documents/Dell F/RLanguage/Comparison2")
data1<- read.xlsx('cases_R_gcl.xlsx',sheet="PEG",colNames=TRUE, rowNames=FALSE, na.strings="NA")
Case<- read.xlsx('cases_R_gcl.xlsx',sheet="Filtration",colNames=TRUE, rowNames=FALSE, na.strings="NA")
Case_data1<- merge(Case, data1, by = "Date", all = TRUE)
Case_data1$Date<- as.Date(Case_data1$Date,origin = "1899-12-30")
data_1 <- data.frame(
  Date = Case_data1$Date,
  N1P = as.numeric(Case_data1$N1P),
  N1M = as.numeric(Case_data1$N1M)
)
alignment<-dtw(Case_data1$N1P,Case_data1$N1M,keep=TRUE)
plot(dtw(Case_data1$N1P,Case_data1$N1M,
         keep=TRUE,step=rabinerJuangStepPattern(6,"c")),type="twoway",offset=-2,
     xlab = "Day", ylab = "Gene concentration",
     main=paste("DTW matching"),cex.lab = 1.5,cex.axis = 1.5)
d <- alignment$distance
dis <- round(d, digits = 1)
plot(alignment,type="threeway",xlab = "PEG N1 gc/L", ylab = "Filtration N1 gc/L", 
     main=paste("DTW minimum path with minimum distance = ",dis,sep = ""),
     cex.lab = 1.5,cex.axis = 1.5) 