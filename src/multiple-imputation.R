########################################################
# Author: Diego Valle-Jones
# Website: www.diegovalle.net
# Date Created: Tue May 15 16:57:00 2012
# Email: diegovalle at gmail.com
# Purpose: Perform a multiple imputation on missing values of the homicide characteristics 
# Copyright (c) Diego Valle-Jones. All rights reserved


#####################NOTE################################################3
###Accidents by firearm and homicides by firearm are not distinct enough to get
###reliable values on their classification. So in essence the mi is wrong!
sinaloa.mi <- subset(deaths, ABBRV %in% c("Sin") )
#No legal interventions deaths in Sinaloa
nrow(sinaloa.mi[sinaloa.mi$PRESUNTOtxt == "Legal intervention, operations of war, military operations, and terrorism",]) == 0
#sinaloa.mi <- subset(sinaloa.mi, ANIODEF <=2008)
#sinaloa.mi <- subset(sinaloa.mi, CAUSE == "Firearm" & PRESUNTOtxt %in% c("Accident", "Homicide"))


#Set NA as appropiate
sinaloa.mi$LUGLEStxt <- as.character(sinaloa.mi$LUGLEStxt)
sinaloa.mi$EDOCIVILtxt <- as.character(sinaloa.mi$EDOCIVILtxt)
sinaloa.mi$PRESUNTOtxt <- as.character(sinaloa.mi$PRESUNTOtxt)

sinaloa.mi$EDADVALOR <- ifelse(sinaloa.mi$EDADVALOR == 998, NA, sinaloa.mi$EDADVALOR)
sinaloa.mi$EDADVALOR <- sqrt(sinaloa.mi$EDADVALOR)
sinaloa.mi$LUGLEStxt <- ifelse(sinaloa.mi$LUGLEStxt == "Unknown", NA, sinaloa.mi$LUGLEStxt)

sinaloa.mi$EDOCIVILtxt <- ifelse(is.na(sinaloa.mi$EDOCIVILtxt), "Less than 12", sinaloa.mi$EDOCIVILtxt)
sinaloa.mi$EDOCIVILtxt  <- ifelse(sinaloa.mi$EDOCIVILtxt == "Unknown", NA, sinaloa.mi$EDOCIVILtxt)
sinaloa.mi$CAUSE <- ifelse(sinaloa.mi$CAUSE == "Unspecified", NA, sinaloa.mi$CAUSE)
sinaloa.mi$PRESUNTOtxt <- ifelse(sinaloa.mi$PRESUNTOtxt == "Unknown", NA, sinaloa.mi$PRESUNTOtxt)
sinaloa.mi$SEXOtxt <- ifelse(sinaloa.mi$SEXOtxt == "0", NA, sinaloa.mi$SEXOtxt)



#Plot the missing values
cols <- c("PRESUNTOtxt","EDADVALOR","CAUSE","SEXOtxt","ANIODEF", "EDOCIVILtxt")
missm <- is.na(sinaloa.mi[,cols])
missm <- melt(missm)

missm$X2 <- reorder(missm$X2, -missm$value, sum)
p <- ggplot(missm, aes(X2, X1, fill = value)) +
  geom_tile() +
  ylab("observation number") +
  xlab("variables") +
  scale_fill_discrete("", labels = c("complete", "missing")) +
  scale_x_discrete(labels = c("Cause of\nInjury", "Injury Intent", "Marital\nStatus",
                     "Age", "Sex", "Year", "Municipality")) +
  coord_flip()
ggsave(plot = p,"graphs/missing.png", dpi= 100, w = 7, h = 5)




#Now set all accidents and homicides in 2007 and 2008 as NA
sinaloa.mi$PRESUNTOtxt[sinaloa.mi$ANIODEF %in% c(2007, 2008) &
      #                 sinaloa.mi$CAUSE == "Firearm" &
                       sinaloa.mi$PRESUNTOtxt %in% c("Accident", "Homicide")] <- NA





#Plot the missing values (with 2007 and 2008 as missing)
info <- mi.info(sinaloa.mi[,cols])

missm <- is.na(sinaloa.mi[,cols])
missm <- melt(missm)
missm$X2 <- reorder(missm$X2, -missm$value, sum)

p <- ggplot(missm, aes(X2, X1, fill = value)) +
  geom_tile() +
  ylab("observation number") +
  xlab("variables") +
  scale_fill_discrete("", labels = c("complete", "missing")) +
  scale_x_discrete(labels = c("Injury Intent", "Cause of\nInjury", "Marital\nStatus",
                     "Age", "Sex", "Year", "Municipality"))+
  coord_flip()
ggsave(plot = p,"graphs/missing-with-accidents-n-homicides.png", dpi= 100, w = 7, h = 5)




##Needed to cross validate how well the multiple imputation
##classifies accidents and homicides
fakeNA <- c()
for(i in 1:nrow(sinaloa.mi)) {
  if(sinaloa.mi[i,]$PRESUNTOtxt %in% c("Accident", "Homicide"))
    fakeNA[i] <- runif(1) > .75
  else
    fakeNA[i] <- FALSE
}
fakeFirearmAccidents <- c()
for(i in 1:nrow(sinaloa.mi)) {
  if(sinaloa.mi[i,]$PRESUNTOtxt %in% "Accident" &
     sinaloa.mi[i,]$CAUSE %in% "Firearm")
    fakeFirearmAccidents[i] <- runif(1) > .75
  else
    fakeFirearmAccidents[i] <- FALSE
}
sinaloa.mi$PRESUNTOtxt[fakeNA] <- NA
#sinaloa.mi$PRESUNTOtxt[fakeFirearmAccidents] <- NA



#Perform the multiple imputation
#Takes a long time!
if(file.exists("IMP.RData")) {
  message("Starting multiple impitation:")
  message("#############################")
  message("#############################")
  message("This will take some time")
  message("#############################")
  message("#############################")
  load(file = "IMP.RData")
} else {
  IMP <- mi(object = sinaloa.mi[,cols], seed = 1)
  IMP <- mi(IMP)
  save(IMP, file = "IMP.RData")
}

converged(IMP)


##Create a plot with the imputed data

IMP.dat1 <- mi.data.frame(IMP, m = 1)
names(IMP.dat1) <- str_c(names(IMP.dat1), 1)
IMP.dat2 <- mi.data.frame(IMP, m = 2)
names(IMP.dat2) <- str_c(names(IMP.dat2), 2)
IMP.dat3 <- mi.data.frame(IMP, m = 3)
names(IMP.dat3) <- str_c(names(IMP.dat3), 3)

sinaloa.mi$PRESUNTOtxt <- deaths$PRESUNTOtxt

imp <- cbind(sinaloa.mi, IMP.dat1, IMP.dat2, IMP.dat3)
#head(imp)
imp.hom.acc <- ddply(imp, .(ANIODEF, MESDEF), summarise,
      meanHom = (sum(ifelse(PRESUNTOtxt1 == "Homicide", 1, 0)) +
      sum(ifelse(PRESUNTOtxt2 == "Homicide", 1, 0)) +
      sum(ifelse(PRESUNTOtxt3 == "Homicide", 1, 0))) / 3,
      meanAccFir = (sum(ifelse(PRESUNTOtxt1 == "Accident" & CAUSE1 == "Firearm", 1, 0)) +
      sum(ifelse(PRESUNTOtxt2 == "Accident" & CAUSE2 == "Firearm", 1, 0)) +
      sum(ifelse(PRESUNTOtxt3== "Accident" & CAUSE3 == "Firearm", 1, 0))) / 3)

imp.hom.acc$Date <- with(imp.hom.acc, as.Date(str_c(ANIODEF, MESDEF, "15", sep = "-")))

imputed.hom.drug <- merge(imp.hom.acc[,c("Date", "meanHom", "meanAccFir")],
                          drh.sin[,c("Date", "V1")], by = "Date", all.x = TRUE)
imputed.hom.drug <- melt(imputed.hom.drug, id = "Date")


imputed.hom.drug$variable <- rep(c("All Homicides",
                     "Firearm Accidents", "Drug Homicides"), each = 84)
imputed.hom.drug$variable <- factor(imputed.hom.drug$variable,
                                    levels = c( "Drug Homicides",
                                      "All Homicides",
                                      "Firearm Accidents"))



ggplot(imputed.hom.drug, aes(Date, value, group = variable,
                             color = variable, fill = variable)) +
  geom_area(alpha = .4, position = "identity") +
  geom_line(show_guide = FALSE) +
  ylab("number of deaths") +
  xlab("date") +
  scale_fill_hue("type of\ndeath") +
  scale_color_hue("type of\ndeath") +
  opts(title = "Imputed firearm accidents, imputed total homicides, and drug-related homicides in Sinaloa",
       legend.position = c(.19,.78),
       legend.background = theme_rect(fill="white",colour="white")) 
ggsave("graphs/drh_firearm-acc_hom-mi.png", dpi = 100,
       w = 9.6, h = 5)

sum(imp$PRESUNTOtxt != imp$PRESUNTOtxt1, na.rm = TRUE)

## nrow(cbind(sinaloa.mi[sinaloa.mi$ANIODEF %in% 2007:2008,c("ANIODEF", "PRESUNTOtxt")],
##       imp[sinaloa.mi$ANIODEF %in% 2007:2008,c("PRESUNTOtxt1")]))
## sum(sinaloa.mi[!sinaloa.mi$ANIODEF %in% 2007:2008,c("PRESUNTOtxt")] !=
##       imp[!sinaloa.mi$ANIODEF %in% 2007:2008,c("PRESUNTOtxt1")])
## imp[fakeFirearmAccidents, c("ANIODEF", "PRESUNTOtxt", "PRESUNTOtxt1")]
## nrow(imp[fakeNA, c("ANIODEF", "PRESUNTOtxt", "PRESUNTOtxt1")])
## #Sensitivity
## sum(sinaloa.mi[!(sinaloa.mi$ANIODEF %in% 2007:2008) & sinaloa.mi$PRESUNTOtxt %in% c("Homicide"),c("PRESUNTOtxt")] !=
##     imp[!(sinaloa.mi$ANIODEF %in% 2007:2008) & imp$PRESUNTOtxt1 %in% c("Homicide"),c("PRESUNTOtxt1")])

performance <- function(x, imputation.num = 1){
  sum(x[[str_c("PRESUNTOtxt", imputation.num)]] == x[["PRESUNTOtxt"]])
}

avePerformance <- function(data) {
  mean(sapply(1:3, function(x) performance(data, x)))
}
sensitivity <- subset(imp[fakeNA,], PRESUNTOtxt == "Homicide")
message("the multiple imputation has sensitivity of: \n")
message(avePerformance(sensitivity) / length(sensitivity$PRESUNTOtxt))

specificity <- subset(imp[fakeNA,], PRESUNTOtxt == "Accident")
message("the multiple imputation has specifity of: \n")
message(avePerformance(specificity) / length(specificity$PRESUNTOtxt))


##Sanity checks
#ddply(sinaloa.mi, .(PRESUNTOtxt, ANIODEF), nrow)
#ddply(subset(IMP.dat1, CAUSE1 == "All Transport"), .(PRESUNTOtxt1, ANIODEF1, CAUSE1), nrow)
#ddply(subset(IMP.dat1, CAUSE1 == "Firearm"), .(PRESUNTOtxt1, ANIODEF1, CAUSE1), nrow)

#ddply(IMP.dat1, .(PRESUNTOtxt1, ANIODEF1), nrow)
#ddply(subset(sinaloa.mi, CAUSE != "Firearm"), .(PRESUNTOtxt, CAUSE, ANIODEF), nrow)
