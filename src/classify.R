########################################################
# Author: Diego Valle-Jones
# Website: www.diegovalle.net
# Date Created: Tue May 15 17:15:36 2012
# Email: diegovalle at gmail.com
# Purpose: Machine learning algorithms to classify accidents and homicides in Sinaloa 
# Copyright (c) Diego Valle-Jones. All rights reserved


#Clean the data
hom.sinaloa <- subset(deaths, ABBRV %in% c("Sin") )
#No legal interventions deaths in Sinaloa
nrow(hom.sinaloa[hom.sinaloa$PRESUNTOtxt == "Legal intervention, operations of war, military operations, and terrorism",]) == 0
#Only deaths by injury cause of Firearm
hom.sinaloa <- subset(hom.sinaloa, CAUSE == "Firearm" & PRESUNTOtxt %in% c("Accident", "Homicide"))


hom.sinaloa$LUGLEStxt <- as.character(hom.sinaloa$LUGLEStxt)
hom.sinaloa$EDOCIVILtxt <- as.character(hom.sinaloa$EDOCIVILtxt)
hom.sinaloa$PRESUNTOtxt <- as.character(hom.sinaloa$PRESUNTOtxt)
#Set NA as appropiate
hom.sinaloa$EDADVALOR <- ifelse(hom.sinaloa$EDADVALOR == 998, NA, hom.sinaloa$EDADVALOR)
hom.sinaloa$EDADVALOR <- sqrt(hom.sinaloa$EDADVALOR)
hom.sinaloa$LUGLEStxt <- ifelse(hom.sinaloa$LUGLEStxt == "Unknown", NA, hom.sinaloa$LUGLEStxt)

hom.sinaloa$EDOCIVILtxt <- ifelse(is.na(hom.sinaloa$EDOCIVILtxt), "Less than 12", hom.sinaloa$EDOCIVILtxt)
hom.sinaloa$EDOCIVILtxt  <- ifelse(hom.sinaloa$EDOCIVILtxt == "Unknown", NA, hom.sinaloa$EDOCIVILtxt)
hom.sinaloa$CAUSE <- ifelse(hom.sinaloa$CAUSE == "Unspecified", NA, hom.sinaloa$CAUSE)
hom.sinaloa$PRESUNTOtxt <- ifelse(hom.sinaloa$PRESUNTOtxt == "Unknown", NA, hom.sinaloa$PRESUNTOtxt)
hom.sinaloa$SEXOtxt <- ifelse(hom.sinaloa$SEXOtxt == "0", NA, hom.sinaloa$SEXOtxt)
 
hom.sinaloa$wday <- as.factor(wday(hom.sinaloa$date))
hom.sinaloa$MESDEF <- hom.sinaloa$MESDEF
#qplot(sqrt(hom.sinaloa$EDADVALOR), binwidth = 1)
#hom.sinaloa$EDADVALOR <- cut(hom.sinaloa$EDADVALOR, c(-1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,80, 1000))
hom.sinaloa$CAUSE <- as.factor(hom.sinaloa$CAUSE)
hom.sinaloa$SEXOtxt <- as.factor(hom.sinaloa$SEXOtxt)
hom.sinaloa$PRESUNTOtxt <- as.factor(hom.sinaloa$PRESUNTOtxt)
hom.sinaloa$LUGLEStxt <- as.factor(hom.sinaloa$LUGLEStxt)
hom.sinaloa$EDOCIVILtxt <- as.factor(hom.sinaloa$EDOCIVILtxt)


#Set 2007 and 2008 Accidents and Homicides by Injury Cause of Firearm as NA
hom.sinaloa$PRESUNTOtxt[hom.sinaloa$ANIODEF %in% c(2007, 2008) &
      #                 hom.sinaloa$CAUSE == "Firearm" &
                       hom.sinaloa$PRESUNTOtxt %in% c("Accident", "Homicide")] <- NA
hom.sinaloa <- subset(hom.sinaloa, ANIODEF <= 2008)
y <- c("PRESUNTOtxt")
x <- c("EDADVALOR", "SEXOtxt", "LUGLEStxt", "EDOCIVILtxt", "NECROPCIAtxt", "ANIODEF",
       "wday", "MESDEF")
#hom.sinaloa[,x] <- apply(hom.sinaloa[,x], 2, as.factor) 


#Use kNN to impute missing data
hom.train <- hom.sinaloa[!is.na(hom.sinaloa$PRESUNTOtxt),]
hom.train <- cbind(kNN(hom.train[,c(x)])[1:length(x)], PRESUNTOtxt = hom.train$PRESUNTOtxt)

#divide the data set into training and test
inTrain <- createDataPartition(1:nrow(hom.train), p = 4/5, list = FALSE)

train <- hom.train[inTrain,c(x,y)]
test <- hom.train[-inTrain,c(x,y)]
#trainClass <- hom.train[inTrain, y]
#testClass <- hom.train[-inTrain, y]



prop.table(table(hom.train$PRESUNTOtxt))



formula <-  PRESUNTOtxt ~ EDADVALOR + SEXOtxt + LUGLEStxt * ANIODEF + EDOCIVILtxt + NECROPCIAtxt + wday + MESDEF
bootControl <- trainControl(number = 25)
set.seed(2)
rfFit <- train(formula,
               data = train,
               method = "rf",
               trControl = bootControl)
fit.pred.rf <- predict(rfFit, test)
confusionMatrix(fit.pred.rf, test$PRESUNTOtxt)
#plot(rfFit, metric='ROC')

#formula <-  PRESUNTOtxt ~ SEXOtxt + LUGLEStxt + EDOCIVILtxt + NECROPCIAtxt
## glm.fit <- glm(formula, data = train, family = binomial)
## test$pred <- predict(glm.fit,newdata = test, type="response")

## test[order(test$pred),c("PRESUNTOtxt", "pred")]

## hom.train2 <- hom.train
## hom.train2$PRESUNTOtxt <- as.numeric(hom.train2$PRESUNTOtxt)
## hom.train2$EDADVALOR
## hom.train2 <- as.data.frame(apply(hom.train2, 2, as.numeric))
## zelig(as.numeric(PRESUNTOtxt) ~ as.numeric(SEXOtxt),
##                  data = test,
##                  model = "relogit")

## data(mid)
## z.out1 <- zelig(PRESUNTOtxt ~ ANIODEF, data = hom.train2, model = "relogit", tau = .07)
## x.out1 <- setx(z.out1)
## s.out <- sim(z.out1, x = x.out1)
## summary(s.out)
