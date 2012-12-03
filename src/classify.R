########################################################
# Author: Diego Valle-Jones
# Website: www.diegovalle.net
# Date Created: Tue May 15 17:15:36 2012
# Email: diegovalle at gmail.com
# Purpose: Machine learning algorithms to classify accidents and homicides in Sinaloa 
# Copyright (c) Diego Valle-Jones. All rights reserved


#Clean the data
hom.sinaloa <- subset(deaths, ABBRV %in% c("Sin") )
##Add the accidental deaths by firearm in the rest of Mexico
hom.sinaloa <- rbind(hom.sinaloa, accidents.mx)
#No legal interventions deaths in Sinaloa
nrow(hom.sinaloa[hom.sinaloa$PRESUNTOtxt == "Legal intervention, operations of war, military operations, and terrorism",]) == 0
#Only deaths by injury cause of Firearm
hom.sinaloa <- subset(hom.sinaloa, CAUSE == "Firearm" & PRESUNTOtxt %in% c("Accident", "Homicide"))
nrow(hom.sinaloa)


hom.sinaloa$LUGLEStxt <- as.character(hom.sinaloa$LUGLEStxt)
hom.sinaloa$EDOCIVILtxt <- as.character(hom.sinaloa$EDOCIVILtxt)
hom.sinaloa$PRESUNTOtxt <- as.character(hom.sinaloa$PRESUNTOtxt)
#Set NA as appropiate
hom.sinaloa$EDADVALOR <- ifelse(hom.sinaloa$EDADVALOR == 998, NA, hom.sinaloa$EDADVALOR)
hom.sinaloa$EDADVALOR <- sqrt(hom.sinaloa$EDADVALOR)
hom.sinaloa$LUGLEStxt <- ifelse(hom.sinaloa$LUGLEStxt == "Unknown", NA, hom.sinaloa$LUGLEStxt)
hom.sinaloa$OCUPACIONtxt <- as.factor(as.character(hom.sinaloa$OCUPACIONtxt))
hom.sinaloa$HORADEF <- ifelse(hom.sinaloa$HORADEF == 24, NA, hom.sinaloa$HORADEF)

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


hom.sinaloa <- subset(hom.sinaloa, !(PRESUNTOtxt == "Homicide" & ANIODEF %in% 2007:2008))
#Set 2007 and 2008 Accidents and Homicides by Injury Cause of Firearm as NA
hom.sinaloa$PRESUNTOtxt[hom.sinaloa$ANIODEF %in% c(2007, 2008) &
      #                 hom.sinaloa$CAUSE == "Firearm" &
                       hom.sinaloa$PRESUNTOtxt %in% c("Accident") &
                        hom.sinaloa$ABBRV == "Sin"] <- NA

y <- c("PRESUNTOtxt")
x <- c("EDADVALOR", "SEXOtxt", "LUGLEStxt", "ANIODEF", "EDOCIVILtxt", 
       "wday", "MESDEF", "NECROPCIAtxt")
#hom.sinaloa[,x] <- apply(hom.sinaloa[,x], 2, as.factor) 


#Use kNN to impute missing data
hom.train <- hom.sinaloa[!is.na(hom.sinaloa$PRESUNTOtxt),]
##hom.train <- cbind(kNN(hom.train[,c(x)])[1:length(x)], PRESUNTOtxt = hom.train$PRESUNTOtxt)

#divide the data set into training and test sets
inTrain <- createDataPartition(1:nrow(hom.train), p = 7/10, list = FALSE)

train <- na.omit(hom.train[inTrain,c(x,y)])
test <- na.omit(hom.train[-inTrain,c(x,y)])
#trainClass <- hom.train[inTrain, y]
#testClass <- hom.train[-inTrain, y]
nrow(train)
nrow(test)

prop.table(table(hom.train$PRESUNTOtxt))



formula <-  PRESUNTOtxt ~ EDADVALOR * SEXOtxt * LUGLEStxt * ANIODEF * EDOCIVILtxt + NECROPCIAtxt
bootControl <- trainControl(method='repeatedcv', 
    number=3, 
    repeats=1,
    classProbs=TRUE)
set.seed(2)
#multinom, plr, logitBoost, LMT

if(file.exists("rfFit.RData")) {
  load("rfFit.RData")
} else {
  message("Starting random forest:")
  message("#############################")
  message("#############################")
  message("This will take some time")
  message("#############################")
  message("#############################")
  rfFit <- train(formula,
               data = train,
               method = "glmnet",
               trControl = bootControl,
               preProcess = c("center", "scale"),
               metric = "AUC")
  save(rfFit, file = "rfFit.RData")
}
fit.pred.rf <- predict(rfFit, test)
message("Classifying accidents and homicides by firearm with a random forest:")
print(confusionMatrix(fit.pred.rf, test$PRESUNTOtxt))





hom.unknown <- hom.sinaloa[is.na(hom.sinaloa$PRESUNTOtxt),]
ddply(hom.unknown, .(ANIODEF), nrow)
##write.csv(hom.unknown, "t.csv")
hom.unknown[,c(x)] <- kNN(hom.unknown[,c(x)], k = 15)[1:length(x)]
  
fit.unknown <- predict(rfFit, hom.unknown[,c(x)])
print(table(fit.unknown))
hom.unknown$intent.imputed <- fit.unknown

ddply(hom.unknown, .(ANIODEF, intent.imputed), nrow)

  df.pred <- rbind.fill([!is.na(hom.sinaloa$intent),], hom.unknown)
  df.pred$intent.imputed <- with(df.pred,
                                 ifelse(is.na(intent),
                                        as.character(intent.imputed),
                                        as.character(intent)))
  original <- ddply(df.pred, .(year(date2), month(date2), intent), nrow)
  
  imputed <- ddply(df.pred, .(year(date2), month(date2), intent.imputed), nrow)
  

set.seed(2)
glmnetFit <- train(formula,
               data = train,
               method = "svmLinear",
               trControl = bootControl)
fit.pred.glmnet <- predict(glmnetFit, test)
message("Classifying accidents and homicides by firearm with regularized regression:")
print(confusionMatrix(fit.pred.glmnet, test$PRESUNTOtxt))




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
