#Linear interpolation of VIRADEL for unit gc/L, gc/day

#VIRADEL gc/L
library(ggplot2)
library(openxlsx)
library(lubridate)
library(scales)
library(ggpubr)
library(tidyverse)
library(imputeTS)
library(graphics)
library(dplyr)
library(zoo)
setwd("~/Documents/Dell F/RLanguage/Comparison2")
data1<- read.xlsx('VIRADELONJ.xlsx',sheet="TotalperL",colNames=TRUE, rowNames=FALSE, na.strings="NA")
Case<- read.xlsx('cases_R.xlsx',sheet="Websitecase",colNames=TRUE, rowNames=FALSE, na.strings="NA")
Case_data1<- merge(Case, data1, by = "Date", all = TRUE)
#Converting date into y-m-d
Case_data1$Date<- as.Date(Case_data1$Date,origin = "1899-12-30")
data_1 <- data.frame(
  Date = Case_data1$Date,
  Cases = as.numeric(Case_data1$Totalcases),
  N1V = as.numeric(Case_data1$N1V),
  N2V = as.numeric(Case_data1$N2V)
)

#N1 gc/L
library(imputeTS)
N1VnewL <- zoo(Case_data1$N1V,Case_data1$Date)
N1VnewL <- na.interpolation(N1VnewL, option = "linear")
plot(N1VnewL)
write.csv(N1VnewL, "~/Documents/Dell F/RLanguage/Comparison2/Linear_interpolation/N1VnewL.csv", row.names=T)

#N2 gc/L
library(imputeTS)
N2VnewL <- zoo(Case_data1$N2V,Case_data1$Date)
N2VnewL <- na.interpolation(N2VnewL, option = "linear")
plot(N2VnewL)
write.csv(N2VnewL, "~/Documents/Dell F/RLanguage/Comparison2/Linear_interpolation/N2VnewL.csv", row.names=T)

#VIRADEL gc/day
library(ggplot2)
library(openxlsx)
library(lubridate)
library(scales)
library(ggpubr)
library(tidyverse)
library(imputeTS)
library(graphics)
library(dplyr)
library(zoo)
setwd("~/Documents/Dell F/RLanguage/Comparison2")
data1<- read.xlsx('VIRADELONJ.xlsx',sheet="TotalperDay",colNames=TRUE, rowNames=FALSE, na.strings="NA")
Case<- read.xlsx('cases_R.xlsx',sheet="Websitecase",colNames=TRUE, rowNames=FALSE, na.strings="NA")
Case_data1<- merge(Case, data1, by = "Date", all = TRUE)
#Converting date into y-m-d
Case_data1$Date<- as.Date(Case_data1$Date,origin = "1899-12-30")
data_1 <- data.frame(
  Date = Case_data1$Date,
  Cases = as.numeric(Case_data1$Totalcases),
  N1V = as.numeric(Case_data1$N1V),
  N2V = as.numeric(Case_data1$N2V)
)

#N1 gc/day
library(imputeTS)
N1VnewD <- zoo(Case_data1$N1V,Case_data1$Date)
N1VnewD <- na.interpolation(N1VnewD, option = "linear")
plot(N1VnewD)
write.csv(N1VnewD, "~/Documents/Dell F/RLanguage/Comparison2/Linear_interpolation/N1VnewD.csv", row.names=T)

#N2 gc/day
library(imputeTS)
N2VnewD <- zoo(Case_data1$N2V,Case_data1$Date)
N2VnewD <- na.interpolation(N2VnewD, option = "linear")
plot(N2VnewD)
write.csv(N2VnewD, "~/Documents/Dell F/RLanguage/Comparison2/Linear_interpolation/N2VnewD.csv", row.names=T)