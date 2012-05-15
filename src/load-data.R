########################################################
#Load the data
########################################################

deaths <- read.csv("data/sinaloa-deaths.csv.bz2", fileEncoding = "utf-8")
drug.homicides <- read.csv("data/drug-homicides.csv.bz2", fileEncoding = "utf-8")
municipality.heads <- read.csv("data/municipality-heads.csv", fileEncoding = "utf-8")



deaths[which(deaths$DIAREG == 0),]$DIAREG <- 1
#median(dseconds(as.Date(str_c(deaths$ANIOREG, deaths$MESREG, deaths$DIAREG, sep="-")) -
#as.Date(str_c(deaths$ANIODEF, deaths$MESDEF, deaths$DIADEF, sep="-"))), na.rm = TRUE)/(3600
 #                                                                         *24)

#Assume that deaths without a date of registration occurred on the same date they were registered
deaths[which(deaths$ANIODEF == 0),]$ANIODEF <- deaths[which(deaths$ANIODEF == 0),]$ANIOREG
deaths[which(deaths$MESDEF == 0),]$MESDEF <- deaths[which(deaths$MESDEF == 0),]$MESREG
deaths[which(deaths$DIADEF == 0),]$DIADEF <- deaths[which(deaths$DIADEF == 0),]$DIAREG

#deaths[which(is.na(as.Date(str_c(deaths$ANIODEF, deaths$MESDEF, deaths$DIADEF, sep="-")))),] 
