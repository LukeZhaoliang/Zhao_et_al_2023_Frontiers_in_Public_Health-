#Correlation: spearman N1
# VIRADEL-PEG, gc/L, gc/day, gc/day/person, gc/L/person
# VIRADEL-MM, gc/L, gc/day, gc/day/person, gc/L/person
# PEG-MM, gc/L, gc/day, gc/day/person, gc/L/person
###################################################################################################
# VIRADEL-PEG, gc/L
#spearman gc/L
library(ggplot2)
library(openxlsx)
library(lubridate)
library(scales)
library(ggpubr)
library(tidyverse)
library(graphics)
library(dplyr)
setwd("~/Documents/Dell F/RLanguage/Comparison2")
data1<- read.xlsx('cases_R_interpolation_gcl.xlsx',sheet="VIRADEL",colNames=TRUE, rowNames=FALSE, na.strings="NA")
Case<- read.xlsx('cases_R_interpolation_gcl.xlsx',sheet="PEG",colNames=TRUE, rowNames=FALSE, na.strings="NA")
Case_data1<- merge(Case, data1, by = "Date", all = TRUE)
#Converting date into y-m-d
Case_data1$Date<- as.Date(Case_data1$Date,origin = "1899-12-30")
data_1 <- data.frame(
  Date = Case_data1$Date,
  N1V = as.numeric(Case_data1$N1V),
  N1P = as.numeric(Case_data1$N1P)
)
p <- ggscatter(data_1, x = "N1V", y = "N1P", 
               add = "reg.line", conf.int = TRUE, 
               add.params = list(color = "blue", fill = "lightgray"),
               cor.coef = TRUE, cor.method = "spearman",cor.coef.size = 10,
               title = "Spearman cor. VIRADEL PEG N1 gc/L",
               xlab = "VIRADEL N1 gc/L", ylab = "PEG N1 gc/L")
p
p+
  font("title", size = 25, color = "black")+
  font("xlab", size = 25, color = "black")+
  font("ylab", size = 25, color = "black")+
  font("xy.text", size = 25, color = "black")

# VIRADEL-PEG, gc/day
#spearman gc/day
library(ggplot2)
library(openxlsx)
library(lubridate)
library(scales)
library(ggpubr)
library(tidyverse)
library(graphics)
library(dplyr)
setwd("~/Documents/Dell F/RLanguage/Comparison2")
data1<- read.xlsx('cases_R_interpolation_gcd.xlsx',sheet="VIRADEL",colNames=TRUE, rowNames=FALSE, na.strings="NA")
Case<- read.xlsx('cases_R_interpolation_gcd.xlsx',sheet="PEG",colNames=TRUE, rowNames=FALSE, na.strings="NA")
Case_data1<- merge(Case, data1, by = "Date", all = TRUE)
#Converting date into y-m-d
Case_data1$Date<- as.Date(Case_data1$Date,origin = "1899-12-30")
data_1 <- data.frame(
  Date = Case_data1$Date,
  N1V = as.numeric(Case_data1$N1V),
  N1P = as.numeric(Case_data1$N1P)
)
p <- ggscatter(data_1, x = "N1V", y = "N1P", 
               add = "reg.line", conf.int = TRUE, 
               add.params = list(color = "blue", fill = "lightgray"),
               cor.coef = TRUE, cor.method = "spearman",cor.coef.size = 10,
               title = "Spearman cor. VIRADEL PEG N1 gc/day",
               xlab = "VIRADEL N1 gc/day", ylab = "PEG N1 gc/day")
p
p+
  font("title", size = 25, color = "black")+
  font("xlab", size = 25, color = "black")+
  font("ylab", size = 25, color = "black")+
  font("xy.text", size = 25, color = "black")

# VIRADEL-PEG, gc/day/person
#spearman gc/day/person
library(ggplot2)
library(openxlsx)
library(lubridate)
library(scales)
library(ggpubr)
library(tidyverse)
library(graphics)
library(dplyr)
setwd("~/Documents/Dell F/RLanguage/Comparison2")
data1<- read.xlsx('cases_R_interpolation_gcpd.xlsx',sheet="VIRADEL",colNames=TRUE, rowNames=FALSE, na.strings="NA")
Case<- read.xlsx('cases_R_interpolation_gcpd.xlsx',sheet="PEG",colNames=TRUE, rowNames=FALSE, na.strings="NA")
Case_data1<- merge(Case, data1, by = "Date", all = TRUE)
#Converting date into y-m-d
Case_data1$Date<- as.Date(Case_data1$Date,origin = "1899-12-30")
data_1 <- data.frame(
  Date = Case_data1$Date,
  N1V = as.numeric(Case_data1$N1V),
  N1P = as.numeric(Case_data1$N1P)
)
p <- ggscatter(data_1, x = "N1V", y = "N1P", 
               add = "reg.line", conf.int = TRUE, 
               add.params = list(color = "blue", fill = "lightgray"),
               cor.coef = TRUE, cor.method = "spearman",cor.coef.size = 10,
               title = "Spearman cor. VIRADEL PEG N1 gc/day/person",
               xlab = "VIRADEL N1 gc/day/person", ylab = "PEG N1 gc/day/person")
p
p+
  font("title", size = 25, color = "black")+
  font("xlab", size = 25, color = "black")+
  font("ylab", size = 25, color = "black")+
  font("xy.text", size = 25, color = "black")

# VIRADEL-PEG, w/c ratio
#spearman w/c ratio
library(ggplot2)
library(openxlsx)
library(lubridate)
library(scales)
library(ggpubr)
library(tidyverse)
library(graphics)
library(dplyr)
setwd("~/Documents/Dell F/RLanguage/Comparison2")
data1<- read.xlsx('cases_R_interpolation_wc.xlsx',sheet="VIRADEL",colNames=TRUE, rowNames=FALSE, na.strings="NA")
Case<- read.xlsx('cases_R_interpolation_wc.xlsx',sheet="PEG",colNames=TRUE, rowNames=FALSE, na.strings="NA")
Case_data1<- merge(Case, data1, by = "Date", all = TRUE)
#Converting date into y-m-d
Case_data1$Date<- as.Date(Case_data1$Date,origin = "1899-12-30")
data_1 <- data.frame(
  Date = Case_data1$Date,
  N1V = as.numeric(Case_data1$N1V),
  N1P = as.numeric(Case_data1$N1P)
)
p <- ggscatter(data_1, x = "N1V", y = "N1P", 
               add = "reg.line", conf.int = TRUE, 
               add.params = list(color = "blue", fill = "lightgray"),
               cor.coef = TRUE, cor.method = "spearman",cor.coef.size = 10,
               title = "Spearman cor. VIRADEL PEG N1 w/c ratio",
               xlab = "VIRADEL N1 w/c ratio", ylab = "PEG N1 w/c ratio")
p
p+
  font("title", size = 25, color = "black")+
  font("xlab", size = 25, color = "black")+
  font("ylab", size = 25, color = "black")+
  font("xy.text", size = 25, color = "black")

###################################################################################################

# VIRADEL-MM, gc/L
#spearman gc/L
library(ggplot2)
library(openxlsx)
library(lubridate)
library(scales)
library(ggpubr)
library(tidyverse)
library(graphics)
library(dplyr)
setwd("~/Documents/Dell F/RLanguage/Comparison2")
data1<- read.xlsx('cases_R_interpolation_gcl.xlsx',sheet="VIRADEL",colNames=TRUE, rowNames=FALSE, na.strings="NA")
Case<- read.xlsx('cases_R_interpolation_gcl.xlsx',sheet="MM",colNames=TRUE, rowNames=FALSE, na.strings="NA")
Case_data1<- merge(Case, data1, by = "Date", all = TRUE)
#Converting date into y-m-d
Case_data1$Date<- as.Date(Case_data1$Date,origin = "1899-12-30")
data_1 <- data.frame(
  Date = Case_data1$Date,
  N1V = as.numeric(Case_data1$N1V),
  N1M = as.numeric(Case_data1$N1M)
)
p <- ggscatter(data_1, x = "N1V", y = "N1M", 
               add = "reg.line", conf.int = TRUE, 
               add.params = list(color = "blue", fill = "lightgray"),
               cor.coef = TRUE, cor.method = "spearman",cor.coef.size = 10,
               title = "Spearman cor. VIRADEL Filtration N1 gc/L",
               xlab = "VIRADEL N1 gc/L", ylab = "Filtration N1 gc/L")
p
p+
  font("title", size = 25, color = "black")+
  font("xlab", size = 25, color = "black")+
  font("ylab", size = 25, color = "black")+
  font("xy.text", size = 25, color = "black")

# VIRADEL-MM, gc/day
#spearman gc/day
library(ggplot2)
library(openxlsx)
library(lubridate)
library(scales)
library(ggpubr)
library(tidyverse)
library(graphics)
library(dplyr)
setwd("~/Documents/Dell F/RLanguage/Comparison2")
data1<- read.xlsx('cases_R_interpolation_gcd.xlsx',sheet="VIRADEL",colNames=TRUE, rowNames=FALSE, na.strings="NA")
Case<- read.xlsx('cases_R_interpolation_gcd.xlsx',sheet="MM",colNames=TRUE, rowNames=FALSE, na.strings="NA")
Case_data1<- merge(Case, data1, by = "Date", all = TRUE)
#Converting date into y-m-d
Case_data1$Date<- as.Date(Case_data1$Date,origin = "1899-12-30")
data_1 <- data.frame(
  Date = Case_data1$Date,
  N1V = as.numeric(Case_data1$N1V),
  N1M = as.numeric(Case_data1$N1M)
)
p <- ggscatter(data_1, x = "N1V", y = "N1M", 
               add = "reg.line", conf.int = TRUE, 
               add.params = list(color = "blue", fill = "lightgray"),
               cor.coef = TRUE, cor.method = "spearman",cor.coef.size = 10,
               title = "Spearman cor. VIRADEL Filtration N1 gc/day",
               xlab = "VIRADEL N1 gc/day", ylab = "Filtration N1 gc/day")
p
p+
  font("title", size = 25, color = "black")+
  font("xlab", size = 25, color = "black")+
  font("ylab", size = 25, color = "black")+
  font("xy.text", size = 25, color = "black")

# VIRADEL-MM, gc/day/person
#spearman gc/day/person
library(ggplot2)
library(openxlsx)
library(lubridate)
library(scales)
library(ggpubr)
library(tidyverse)
library(graphics)
library(dplyr)
setwd("~/Documents/Dell F/RLanguage/Comparison2")
data1<- read.xlsx('cases_R_interpolation_gcpd.xlsx',sheet="VIRADEL",colNames=TRUE, rowNames=FALSE, na.strings="NA")
Case<- read.xlsx('cases_R_interpolation_gcpd.xlsx',sheet="MM",colNames=TRUE, rowNames=FALSE, na.strings="NA")
Case_data1<- merge(Case, data1, by = "Date", all = TRUE)
#Converting date into y-m-d
Case_data1$Date<- as.Date(Case_data1$Date,origin = "1899-12-30")
data_1 <- data.frame(
  Date = Case_data1$Date,
  N1V = as.numeric(Case_data1$N1V),
  N1M = as.numeric(Case_data1$N1M)
)
p <- ggscatter(data_1, x = "N1V", y = "N1M", 
               add = "reg.line", conf.int = TRUE, 
               add.params = list(color = "blue", fill = "lightgray"),
               cor.coef = TRUE, cor.method = "spearman",cor.coef.size = 10,
               title = "Spearman cor. VIRADEL Filtration N1 gc/day/person",
               xlab = "VIRADEL N1 gc/day/person", ylab = "Filtration N1 gc/day/person")
p
p+
  font("title", size = 25, color = "black")+
  font("xlab", size = 25, color = "black")+
  font("ylab", size = 25, color = "black")+
  font("xy.text", size = 25, color = "black")

# VIRADEL-MM, w/c ratio
#spearman w/c ratio
library(ggplot2)
library(openxlsx)
library(lubridate)
library(scales)
library(ggpubr)
library(tidyverse)
library(graphics)
library(dplyr)
setwd("~/Documents/Dell F/RLanguage/Comparison2")
data1<- read.xlsx('cases_R_interpolation_wc.xlsx',sheet="VIRADEL",colNames=TRUE, rowNames=FALSE, na.strings="NA")
Case<- read.xlsx('cases_R_interpolation_wc.xlsx',sheet="MM",colNames=TRUE, rowNames=FALSE, na.strings="NA")
Case_data1<- merge(Case, data1, by = "Date", all = TRUE)
#Converting date into y-m-d
Case_data1$Date<- as.Date(Case_data1$Date,origin = "1899-12-30")
data_1 <- data.frame(
  Date = Case_data1$Date,
  N1V = as.numeric(Case_data1$N1V),
  N1M = as.numeric(Case_data1$N1M)
)
p <- ggscatter(data_1, x = "N1V", y = "N1M", 
               add = "reg.line", conf.int = TRUE, 
               add.params = list(color = "blue", fill = "lightgray"),
               cor.coef = TRUE, cor.method = "spearman",cor.coef.size = 10,
               title = "Spearman cor. VIRADEL MM N1 w/c ratio",
               xlab = "VIRADEL N1 w/c ratio", ylab = "MM N1 w/c ratio")
p
p+
  font("title", size = 25, color = "black")+
  font("xlab", size = 25, color = "black")+
  font("ylab", size = 25, color = "black")+
  font("xy.text", size = 25, color = "black")

###################################################################################################

# PEG-MM, gc/L
#spearman gc/L
library(ggplot2)
library(openxlsx)
library(lubridate)
library(scales)
library(ggpubr)
library(tidyverse)
library(graphics)
library(dplyr)
setwd("~/Documents/Dell F/RLanguage/Comparison2")
data1<- read.xlsx('cases_R_interpolation_gcl.xlsx',sheet="PEG",colNames=TRUE, rowNames=FALSE, na.strings="NA")
Case<- read.xlsx('cases_R_interpolation_gcl.xlsx',sheet="MM",colNames=TRUE, rowNames=FALSE, na.strings="NA")
Case_data1<- merge(Case, data1, by = "Date", all = TRUE)
#Converting date into y-m-d
Case_data1$Date<- as.Date(Case_data1$Date,origin = "1899-12-30")
data_1 <- data.frame(
  Date = Case_data1$Date,
  N1P = as.numeric(Case_data1$N1P),
  N1M = as.numeric(Case_data1$N1M)
)
p <- ggscatter(data_1, x = "N1P", y = "N1M", 
               add = "reg.line", conf.int = TRUE, 
               add.params = list(color = "blue", fill = "lightgray"),
               cor.coef = TRUE, cor.method = "spearman",cor.coef.size = 10,
               title = "Spearman cor. PEG Filtration N1 gc/L",
               xlab = "PEG N1 gc/L", ylab = "Filtration N1 gc/L")
p
p+
  font("title", size = 25, color = "black")+
  font("xlab", size = 25, color = "black")+
  font("ylab", size = 25, color = "black")+
  font("xy.text", size = 25, color = "black")

# PEG-MM, gc/day
#spearman gc/day
library(ggplot2)
library(openxlsx)
library(lubridate)
library(scales)
library(ggpubr)
library(tidyverse)
library(graphics)
library(dplyr)
setwd("~/Documents/Dell F/RLanguage/Comparison2")
data1<- read.xlsx('cases_R_interpolation_gcd.xlsx',sheet="PEG",colNames=TRUE, rowNames=FALSE, na.strings="NA")
Case<- read.xlsx('cases_R_interpolation_gcd.xlsx',sheet="MM",colNames=TRUE, rowNames=FALSE, na.strings="NA")
Case_data1<- merge(Case, data1, by = "Date", all = TRUE)
#Converting date into y-m-d
Case_data1$Date<- as.Date(Case_data1$Date,origin = "1899-12-30")
data_1 <- data.frame(
  Date = Case_data1$Date,
  N1P = as.numeric(Case_data1$N1P),
  N1M = as.numeric(Case_data1$N1M)
)
p <- ggscatter(data_1, x = "N1P", y = "N1M", 
               add = "reg.line", conf.int = TRUE, 
               add.params = list(color = "blue", fill = "lightgray"),
               cor.coef = TRUE, cor.method = "spearman",cor.coef.size = 10,
               title = "Spearman cor. PEG Filtration N1 gc/day",
               xlab = "PEG N1 gc/day", ylab = "Filtration N1 gc/day")
p
p+
  font("title", size = 25, color = "black")+
  font("xlab", size = 25, color = "black")+
  font("ylab", size = 25, color = "black")+
  font("xy.text", size = 25, color = "black")

# PEG-MM, gc/day/person
#spearman gc/day/person
library(ggplot2)
library(openxlsx)
library(lubridate)
library(scales)
library(ggpubr)
library(tidyverse)
library(graphics)
library(dplyr)
setwd("~/Documents/Dell F/RLanguage/Comparison2")
data1<- read.xlsx('cases_R_interpolation_gcpd.xlsx',sheet="PEG",colNames=TRUE, rowNames=FALSE, na.strings="NA")
Case<- read.xlsx('cases_R_interpolation_gcpd.xlsx',sheet="MM",colNames=TRUE, rowNames=FALSE, na.strings="NA")
Case_data1<- merge(Case, data1, by = "Date", all = TRUE)
#Converting date into y-m-d
Case_data1$Date<- as.Date(Case_data1$Date,origin = "1899-12-30")
data_1 <- data.frame(
  Date = Case_data1$Date,
  N1P = as.numeric(Case_data1$N1P),
  N1M = as.numeric(Case_data1$N1M)
)
p <- ggscatter(data_1, x = "N1P", y = "N1M", 
               add = "reg.line", conf.int = TRUE, 
               add.params = list(color = "blue", fill = "lightgray"),
               cor.coef = TRUE, cor.method = "spearman",cor.coef.size = 10,
               title = "Spearman cor. PEG Filtration N1 gc/day/person",
               xlab = "PEG N1 gc/day/person", ylab = "Filtration N1 gc/day/person")
p
p+
  font("title", size = 25, color = "black")+
  font("xlab", size = 25, color = "black")+
  font("ylab", size = 25, color = "black")+
  font("xy.text", size = 25, color = "black")

# PEG-MM, w/c ratio
#spearman w/c ratio
library(ggplot2)
library(openxlsx)
library(lubridate)
library(scales)
library(ggpubr)
library(tidyverse)
library(graphics)
library(dplyr)
setwd("~/Documents/Dell F/RLanguage/Comparison2")
data1<- read.xlsx('cases_R_interpolation_wc.xlsx',sheet="PEG",colNames=TRUE, rowNames=FALSE, na.strings="NA")
Case<- read.xlsx('cases_R_interpolation_wc.xlsx',sheet="MM",colNames=TRUE, rowNames=FALSE, na.strings="NA")
Case_data1<- merge(Case, data1, by = "Date", all = TRUE)
#Converting date into y-m-d
Case_data1$Date<- as.Date(Case_data1$Date,origin = "1899-12-30")
data_1 <- data.frame(
  Date = Case_data1$Date,
  N1P = as.numeric(Case_data1$N1P),
  N1M = as.numeric(Case_data1$N1M)
)
p <- ggscatter(data_1, x = "N1P", y = "N1M", 
               add = "reg.line", conf.int = TRUE, 
               add.params = list(color = "blue", fill = "lightgray"),
               cor.coef = TRUE, cor.method = "spearman",cor.coef.size = 10,
               title = "spearman cor. PEG MM N1 w/c ratio",
               xlab = "PEG N1 w/c ratio", ylab = "MM N1 w/c ratio")
p
p+
  font("title", size = 25, color = "black")+
  font("xlab", size = 25, color = "black")+
  font("ylab", size = 25, color = "black")+
  font("xy.text", size = 25, color = "black")