library(ggplot2)
library(openxlsx)
library(lubridate)
library(scales)
library(ggpubr)
library(tidyverse)
library(onewaytests)
setwd("~/Documents/Dell F/RLanguage/All_data")
case<- read.xlsx('cases_R.xlsx',sheet="Websitecase",colNames=TRUE, rowNames=FALSE, na.strings="NA")
case$Date<- as.Date(case$Date,origin = "1899-12-30")
data <- data.frame(
  Date = case$Date,
  Detroit = as.numeric(case$Detroit),
  Wayne = as.numeric(case$Wayne),
  Macomb = as.numeric(case$Macomb),
  Oakland = as.numeric(case$Oakland),
  Total = as.numeric(case$Totalcases)
)

p<- ggplot(data) +
  geom_line(aes(x= Date, y = Detroit,colour = "#66c2a5"), size=1)+
  geom_line(aes(x= Date, y = Wayne,colour = "#fc8d62"), size=1)+
  geom_line(aes(x= Date, y = Macomb,colour = "#8da0cb"), size=1)+
  geom_line(aes(x= Date, y = Oakland,colour = "#a6d854"), size=1)+
  geom_line(aes(x= Date, y = Total,colour = "#grey"), size=1)+
  theme_bw()+
  theme(panel.border = element_rect(colour="black",size=1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = 'bold',size=25,colour ="black"),
        text=element_text(size=9, family="Times New Roman"),
        strip.background = element_rect(colour="white",size=0,fill="grey"),
        strip.text = element_text(face = "bold", size=25),
        axis.title.y = element_text(face = 'bold',size=25,colour ="black"),
        axis.title.x = element_text(face ='bold',size=25,colour ="black"),
        axis.text.y = element_text(size=25,colour ="black"),
        axis.text.x = element_text(size=25,colour ="black",angle=30,hjust=1),
        legend.title = element_text(face ='bold',size=25,colour ="black"),
        legend.key.height=unit(0.8,"line"),
        legend.margin=margin(3,3,3,3),
        legend.position = c(0.9,0.8),
        legend.box.margin=margin(1,1,1,1),
        legend.text=element_text(size=25))+
  labs(colour = "City/County")+
  scale_x_date(date_breaks = "2 month",labels=date_format("%m-%d-%Y")) +
  scale_colour_manual(labels=c("Detroit","Wayne","Macomb","Oakland","Total"),
                      values=c("#66c2a5","#fc8d62","#8da0cb","#a6d854","grey"))+
  scale_y_continuous("Confirmed COVID-19 cases",limits = c(0,6000))+
  ggtitle("")
p