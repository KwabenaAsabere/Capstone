# Set working directory 
setwd("C:/Users/mgrabow2/Johns Hopkins/Michael Martin - longview/data")
#setwd("C:/Users/xfeng29/OneDrive - Johns Hopkins/Rakai/longview/data")

# Read in libraries 
library("haven")
library(tidyverse)
library(vroom)
library(sandwich)
library(lmtest)

# Read in raw data from rounds 15-19 and subset out selected variables 
d <- vroom('R20_15.csv')

temp<-as.data.frame(d[,c("study_id", "round", "comm_num", "hh_num", "ageyrs", "sex","locate", 
                         "locate1", "locate2", "locate3", "locate4",
                         "finalhiv", 
                         "currmarr", "evermarr", "occup1", "occup2", "religion",
                         "educate", "educyrs", "arvmed", "cuarvmed", "sexyear", "sexp1yr", 
                         "sexp1out", "sexpever", "eversex", "rltn1", "rltn2", "rltn3", "rltn4",
                         "cndever1", "cndever2","cndever3","cndever4", "rnyrcon1", "rnyrcon2",
                         "rnyrcon3", "rnyrcon4", "pabuseyr", "ppushyr", "pfistkiyr", "pweapatyr",
                         "alcrbsx", "alcpbsx", "rhivever", "hivperiod", "hivrslt", "hivcare", "arvsourc", 
                         "arvsourc1", "arvsourc2","otharvsou", "otharvsou1", "artdays", "artwks","artmos",
                         "artyrs", "artoldmed", "artoldmeds", "artcurmed", "artcurmeds", "artnew", "hivac", "artrunac", "arthoac", "artstrac",
                         "artrunbc", "arthobc", "artstrbc", "quest", "blood", "mobility", "resident", 
                         "copies", "new_copies", "locdate", "hivcare", "hivcare2")])

# Subset data to round 20 and 15-49
bigdat<-temp[which(temp$round=="R020" & temp$ageyrs<50),]

bigdat2<-bigdat[which(bigdat$finalhiv=="P" | bigdat$finalhiv=="N"),]

# Restrict to HIV seropostive participants
posdat<-bigdat2[which(bigdat2$finalhiv=="P"),]

# Number of people who ever had an HIV test 
table(posdat$rhivever, useNA="always")

#Exclude if never tested or received results
posdat2<-posdat[which(posdat$rhivever==1),]

# Number of people with postiive test results 
table(posdat2$hivrslt, useNA="always")

# Include if report being positive 
posdat3<-posdat2[which(posdat2$hivrslt==2), ]

#Number who have never been in care 
table(posdat3$hivcare2, useNA="always")

# Incldue if ever in care  
posdat4<-posdat3[which(posdat3$hivcare2==1), ]

# Incldue if ever used ART 
posdat5<-posdat4[which(posdat4$arvmed==1), ]

# Incldue if  currently useing ART 
posdat6<-posdat5[which(posdat5$cuarvmed==1), ]

#Exclude if not taking ART prior to COVID 19 # 2839 in final data set 
dat<-posdat6[which(posdat6$artoldmed<5),]


save(dat, file = "hivtxtinterupt_data_roudn20.csv")



