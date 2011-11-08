
########################################################
#Analyze the data
########################################################

#Aggregate the drug war deaths by state
drug.homicides$Date <- as.Date(str_c(drug.homicides$Date, "15", sep = "-"),
                    format = "%b-%Y-%d")
drug.homicides$Year <- year(drug.homicides$Date)

#A quick table of homicides by year
ascii(ddply(subset(drug.homicides, StateCode == 25), .(Year),
                 function(df) sum(df$Total, na.rm = TRUE)))


#The state of Sinaloa corresponds to the StateCode 25
drh.sin <- ddply(subset(drug.homicides, StateCode == 25), .(Date),
                 function(df) sum(df$Total, na.rm = TRUE))
drh.sin$type <- "Drug Homicides"
drh.sin$Date <- as.Date(drh.sin$Date)

#Aggragate accidental deaths by firearm
acc.sin <- ddply(subset(deaths, CAUSE == "Firearm" &
                        PRESUNTOtxt == "Accident" &
                        ABBRV== "Sin" & MESDEF != 0),
                .(ANIODEF, MESDEF),
                nrow)
acc.sin$Date <- as.Date(str_c(acc.sin$ANIODEF, acc.sin$MESDEF, 15, sep = "-") )
acc.sin$type <- "Firearm Accidents"

#Aggregate homicides
hom.sin <- ddply(subset(deaths, 
             PRESUNTOtxt == "Homicide" &
             ABBRV== "Sin" & MESDEF != 0),
            .(ANIODEF, MESDEF),
            nrow)
hom.sin$Date <- as.Date(str_c(hom.sin$ANIODEF, hom.sin$MESDEF, 15, sep = "-") )
hom.sin$type <- "All Homicides"

#rbind drug-related, accidents by arms and total homicides
sin <- rbind.fill(drh.sin, acc.sin)
sin <- rbind.fill(sin, hom.sin)


sin$type <- factor(factor(sin$type),
                   levels = c( "Drug Homicides",
                     "All Homicides",
                     "Firearm Accidents"))
ggplot(sin, aes(Date, V1, group = type, color = type, fill = type)) +
  geom_area(alpha = .4, position = "identity") +
  geom_line() +
  ylab("number of deaths") +
  xlab("date") +
  scale_fill_hue("type of\ndeath") +
  scale_color_hue("type of\ndeath") +
  opts(title = "Firearm accidents, total homicides, and drug-related homicides in Sinaloa",
       legend.position = c(.19,.78),
       legend.background = theme_rect(fill="white",colour="white")) +
  scale_x_date(minor = "6 months", limits = c(as.Date("2004-01-01"),
                                     as.Date("2009-12-15")))
ggsave("graphs/drh_firearm-acc_hom.png", dpi = 100,
       w = 9.6, h = 5)

#Create a chart with the excess deaths
m <- merge(drh.sin, hom.sin, by = "Date", all.y = TRUE)
m$diff <- m$V1.x - m$V1.y
m <- merge(m, acc.sin)
background <-  median(acc.sin[c(1:34, 58:69),]$V1)
m$V1 <- m$V1 - background

ggplot(m, aes(Date, diff)) +
  geom_line(aes(color = "Unexplained drug-related homicides (drug-related homicides - all homicides)")) +
  geom_line(aes(Date, V1, color = "Excess firearm accidents (firearm accidents - median firearm accidents excluding 2007 and 2008)")) +
  scale_color_hue("type of death") +
  opts(legend.position = "bottom") +
  geom_abline(intercept = 0, slope = 0, color = "black") +
  xlab("date") +
  ylab("difference in deaths") +
  opts(title = "Drug-related homicides and accidental deaths by firearm in Sinaloa",
       legend.position = c(.4,.82),
       legend.background = theme_rect(fill="white",colour="white"))+
  scale_x_date(minor = "6 months",)
ggsave("graphs/diffplot.png", dpi = 100,
       w = 9.6, h = 5)

#Create a plot with the first differences of drug related
#homicides and total homicides. Since accidental deaths
#by firearm went down in 2009 I'm not really worried
#about autocorrelation, but still...
m <- subset(m, Date <= as.Date("2009-01-01"))[35:57,]
first.diff <- data.frame(diff = diff(m$diff), v = diff(m$V1))
cor.test(first.diff$diff, first.diff$v)

ggplot(m, aes(diff, v)) +
  geom_point() +
  geom_smooth(method = lm)


#Deaths by external injury (legal intervention, accidents and unspecified) that were not homicides in Sinaloa
acc <- ddply(subset(deaths, CAUSE %in% c("Firearm", "Unspecified",
                                         "Cut/pierce",
                                         "Struck by or against") &
                        PRESUNTOtxt != "Homicide" & 
                    MESDEF != 0),
                .(ANIODEF, CAUSE),
                nrow)



#acc$Date <- as.Date(str_c(acc$ANIODEF, acc$MESDEF, 15, sep = "-") )
p <- ggplot(acc, aes(ANIODEF, V1, group = CAUSE, color = CAUSE)) +
  geom_line() +
  scale_y_log10() +
  ylab("number of deaths (log scale)") +
  xlab("date") +
  opts(title = "Deaths by external injury that were not homicides in Sinaloa, by selected causes") +
  xlim(2004, 2009.9)
  #scale_x_date(minor = "6 months", limits = c(as.Date("2004-01-01"),
   #                                  as.Date("2011-07-01")))
direct.label(p, "last.bumpup")
ggsave("graphs/injury-intent.png", dpi = 100,
       w = 9.6, h = 5)

#Stupid shit
ddply(subset(deaths, 
             PRESUNTOtxt == "Homicide" &
             ABBRV== "Sin"),
      .(ANIODEF),
      nrow)

ddply(subset(deaths, 
             PRESUNTOtxt == "Accident" &
             ABBRV== "Sin"),
      .(ANIODEF, CAUSE),
      nrow)

ddply(subset(deaths, 
             !PRESUNTOtxt %in% c("Homicide", "Suicide") & CAUSE == "Firearm" &
             ABBRV== "Sin"),
      .(ANIODEF),
      nrow)$V1 - round(background*12)

ddply(subset(map, ENTOCU == 25), .(Year), function(df) sum(df$Total))

#If I'm right and homicides were misclassified as accidental deaths should have happened out in the open
fir.acc <- ddply(subset(deaths, CAUSE == "Firearm" &
                         PRESUNTOtxt == "Accident" &
                         ABBRV == "Sin" &
                        ANIODEF != 0),
                  .(ANIODEF, CAUSE, LUGLEStxt),
                  nrow)
fir.acc <- merge(unique(expand.grid(fir.acc[,1:3])), fir.acc, all.x = TRUE)
fir.acc[is.na(fir.acc)] <- 0
p <- ggplot(fir.acc, aes(ANIODEF, V1, color = LUGLEStxt)) +
  geom_line(alpha = .8) +
  ylab("number of accidents") +
  xlab("year") +
  opts(title = "Accidental deaths by firearm in Sinaloa, by place of occurrence")
direct.label(p, "top.bumpup")
ggsave("graphs/accident-place.png", dpi = 100,
       w = 9.6, h = 5)

#Homicides by firearm occur in public places
p <- ggplot(ddply(subset(deaths, CAUSE == "Firearm" &
                         PRESUNTOtxt == "Homicide" &
                         ABBRV== "Sin" ),
                  .(ANIODEF, CAUSE, LUGLEStxt),
                  nrow), aes(ANIODEF, V1, color = LUGLEStxt)) +
  geom_line() +
  ylab("number of homicides") +
  xlab("year") +
  opts(title = "Homicides by firearm in Sinaloa, by place of occurrence") +
  xlim(2004, 2011)
direct.label(p, "last.bumpup")
ggsave("graphs/homicide-place.png", dpi = 100,
       w = 9.6, h = 5)



#with(fir.acc, expand.grid(ANIODEF, CAUSE, LUGLEStxt))

#more stupid shit
ddply(subset(deaths, 
             PRESUNTOtxt == "Homicide" ),
             #!is.na(MVTPER)),
      .(ANIODEF),
      nrow)



ddply(subset(deaths, 
             PRESUNTOtxt == "Unknown" &
             ABBRV == "Sin"),
             #!is.na(MVTPER)),
      .(ANIODEF),
      nrow)


ddply(subset(deaths, 
             PRESUNTOtxt == "Accident"&
             ABBRV == "Sin" ),
             #!is.na(MVTPER)),
      .(ANIODEF, CAUSE),
      nrow)
