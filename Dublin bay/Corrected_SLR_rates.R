rm(list = ls())
library(lubridate)
library(zoo)
library(tidyverse)
library(dplyr)
library(readr)
library(reshape2)

dublin_port <- read_csv('IrishSeaComplete.csv') 
arklow <- read.delim("arklow.txt", stringsAsFactors=FALSE)
port_oriel <- read.delim("port_oriel.txt", stringsAsFactors=FALSE)
howth_harbour <- read_csv("howth_harbour.csv")
dly532 <- read_csv("dly532.csv") #Atmospher data from Dublin Airport

#Doodson's filter function:
fDoodson <- function(x) {
  #Defining the Doodson filter's constants:
  f <- c(2, 1, 1, 2, 0, 1, 1, 0, 2, 0, 1, 1, 0, 1, 0, 0, 1, 0,1)
  n <- NULL #defining a variable
  xf <- rep(NA, length(x)) #defining the filtered variable
  
  for (t in 20:(length(x) - 19)) { #starting from the 20th obvervation to be filtered (excluding the first ones)
    for (i in 1:19) { #The sigma function
      
      n[i] <- f[i] * (x[t + i] + x[t - i])
      
    }
    
    xf[t]  <- sum(n)
    
    
  }
  return(xf/30)
}

## Data pre-processing
arklow <- arklow %>% select(-Quality) %>% 
  mutate(time = as.POSIXct(time, format = "%Y/%m/%d %H:%M:%S", tz = "UTC"),
         level = level + 0.5)

port_oriel <- port_oriel %>% select(-Quality) %>% 
  mutate(time = as.POSIXct(time, format = "%Y/%m/%d %H:%M:%S", tz = "UTC"),
         level = level + 2)


howth_harbour <- howth_harbour %>% select(time, level = Water_Level_OD_Malin) %>% 
  mutate(time = as.POSIXct(time, format = "%Y/%m/%d %H:%M:%S", tz = "UTC"),
         level = level + 1.5)

####### Subsetting hourly:
#Howth_harbour
t1 <- as.POSIXct("2006-10-24 12:00:00", tz = "UTC")
t2 <- as.POSIXct("2019-02-20 15:50:00", tz = "UTC")
tseq <- as.data.frame(seq(from = t1, to = t2, by = "hour"))
names(tseq) <- 'time'
x <- full_join(tseq, howth_harbour)
howth <- dplyr::arrange(x, time)
howth_hourly <- inner_join(tseq, howth)

#Arklow
t1 <- as.POSIXct("2003-08-26 00:00:00", tz = "UTC")
t2 <- as.POSIXct("2019-02-01 00:00:00", tz = "UTC")
tseq <- as.data.frame(seq(from = t1, to = t2, by = "hour"))
names(tseq) <- 'time'
ark_hourly <- left_join(tseq, arklow)

#Port_Oriel
t1 <- as.POSIXct("2007-04-17 09:00:00", tz = "UTC")
t2 <- as.POSIXct("2019-02-01 00:00:00", tz = "UTC")
tseq <- as.data.frame(seq(from = t1, to = t2, by = "hour"))
names(tseq) <- 'time'
oriel_hourly <- left_join(tseq, port_oriel)

#Applying Doodson's filter:
Filtered_level <- fDoodson(howth_hourly$level)
howth_hourly$filtered_level <- Filtered_level

Filtered_level <- fDoodson(ark_hourly$level)
ark_hourly$filtered_level <- Filtered_level

Filtered_level <- fDoodson(oriel_hourly$level)
oriel_hourly$filtered_level <- Filtered_level

####### Calculating daily means:
howth_daily <- howth_hourly %>%
  group_by(time = as.Date(format(time, "%Y-%m-%d"))) %>%
  filter(sum(is.na(filtered_level)) < 5) %>%
  summarise(filtered_level = mean(filtered_level, na.rm = T)) %>% 
  mutate(id = "Howth_harbour")

ark_daily <- ark_hourly %>%
  group_by(time = as.Date(format(time, "%Y-%m-%d"))) %>%
  filter(sum(is.na(filtered_level)) < 5) %>%
  summarise(filtered_level = mean(filtered_level, na.rm = T)) %>% 
  mutate(id = "Arklow")

oriel_daily <- oriel_hourly %>%
  group_by(time = as.Date(format(time, "%Y-%m-%d"))) %>%
  filter(sum(is.na(filtered_level)) < 5) %>%
  summarise(filtered_level = mean(filtered_level, na.rm = T)) %>% 
  mutate(id = "Oriel_port")

## Calculating monthly means:
dublin_monthly <- dublin_port %>% filter(time > 2003.710) %>% 
                  mutate(date = time, time = seq.Date(as.Date("2003-10-01"), 
                                         as.Date("2018-12-01"), by = "1 month"),
                         Dublin = dublin/1000 - 1.56) %>% # Just a Re-scaling to meter and shift
                  select(time, Dublin, date)


howth_monthly <- howth_daily %>% 
  group_by(time = floor_date(time, "month")) %>% 
  filter(length(filtered_level) > 15) %>% 
  summarise(filtered_level = mean(filtered_level), id = max(id)) %>% ungroup() %>% 
  filter(time > as.Date("2003-03-01") & time < as.Date("2020-02-01")) %>% 
  complete(time = seq.Date(min(time), max(time), by = "1 month")) %>% 
  rename(Howth = filtered_level)

ark_monthly <- ark_daily %>% 
  group_by(time = floor_date(time, "month")) %>% 
  filter(length(filtered_level) > 15) %>% 
  summarise(filtered_level = mean(filtered_level), id = max(id)) %>% ungroup() %>% 
  filter(time > as.Date("2003-09-01") & time < as.Date("2020-01-01")) %>% 
  complete(time = seq.Date(min(time), max(time), by = "1 month")) %>% 
  rename(Arklow = filtered_level)

oriel_monthly <- oriel_daily %>% 
  group_by(time = floor_date(time, "month")) %>% 
  filter(length(filtered_level) > 15) %>% 
  summarise(filtered_level = mean(filtered_level), id = max(id))  %>% ungroup() %>% 
  filter(time > as.Date("2002-03-01") & time < as.Date("2020-02-01")) %>% 
  complete(time = seq.Date(min(time), max(time), by = "1 month")) %>% 
  rename(Oriel = filtered_level)

oriel_monthly$Oriel[which.min(oriel_monthly$Oriel)] <- NA

#Imputing missing values using Linear Regression: 
newData1 <- full_join(ark_monthly[,-3],dublin_monthly)
newData1 <- full_join(newData1,howth_monthly[,-3])
newData1 <- full_join(newData1, oriel_monthly[,-3])
newData1 <-  dplyr::arrange(newData1, time)

lm.howth <- lm(Howth ~ Arklow + time , data = newData1) 
lm.arklow <- lm(Arklow ~  Howth + time, data = newData1)
lm.dub <- lm(Dublin ~ Howth + time, data = newData1)
lm.oriel <- lm(Oriel ~ Arklow + time, data = newData1)

lm.od <- lm(Oriel ~ Dublin + time, data = newData1)
lm.ad <- lm(Arklow ~ Dublin + time, data = newData1)
lm.hd <- lm(Howth ~ Dublin + time, data = newData1)

newData1$Howth[is.na(newData1$Howth)] <- predict(lm.howth, newdata = newData1[which(is.na(newData1$Howth)), ])
newData1$Dublin[is.na(newData1$Dublin)] <- predict(lm.dub, newdata = newData1[which(is.na(newData1$Dublin)), ])
newData1$Arklow[is.na(newData1$Arklow)] <- predict(lm.arklow, newdata = newData1[which(is.na(newData1$Arklow)), ])
newData1$Oriel[is.na(newData1$Oriel)] <- predict(lm.oriel, newdata = newData1[which(is.na(newData1$Oriel)), ])
newData1$Oriel[is.na(newData1$Oriel)] <- predict(lm.od, newdata = newData1[which(is.na(newData1$Oriel)), ])
newData1$Arklow[is.na(newData1$Arklow)] <- predict(lm.ad, newdata = newData1[which(is.na(newData1$Arklow)), ])
newData1$Howth[is.na(newData1$Howth)] <- predict(lm.hd, newdata = newData1[which(is.na(newData1$Howth)), ])

## Plotting raw time series (not corrected for atmospheric and seasonaity effects)
newData1 %>% select(-date) %>% 
              melt(., id.vars="time") %>% 
 ggplot() +
 geom_line(aes(time, value, group = variable)) +
    labs(x = "Year", y = "Sea Level (m)") +
    theme_bw() + 
    scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
    annotate(geom = "text",x=as.Date("2020-01-01"),
             y=2,label="Port Oriel", size = 4, fontface="bold", color = "red") +
    annotate(geom = "text",x=as.Date("2019-12-01"),
             y=1.5,label="Howth harbor", size = 4, fontface="bold",color = "red") +
    annotate(geom = "text",x=as.Date("2020-01-01"),
             y=1,label="Dublin port", size = 4, fontface="bold",color = "red") +
    annotate(geom = "text",x=as.Date("2020-01-01"),
             y=0.5,label="Port Arklow", size = 4, fontface="bold",color = "red") +
    theme(axis.title=element_text(size=15,face="bold"),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12))


#Calculating SLC rate between 2003-2016 using raw time series:
newData1 <- newData1 %>% filter(year(time) >= 2003 & year(time) < 2016) %>% 
  mutate_at(names(.)[c(2:3,5:6)], ~ .*1000) %>% 
  select(names(.)[c(1:3,5,6,4)])

lm.fit1 <- lm(Dublin ~ date, data = newData1)
lm.fit4 <- lm(Arklow ~ date, data = newData1)
lm.fit3 <- lm(Oriel ~  date, data = newData1)
lm.fit2 <- lm(Howth ~  date, data = newData1)


d=data.frame(location=c("Dublin port","Howth harbour","Oriel port", "Arklow"), 
             mean=c(coefficients(lm.fit1)[2],coefficients(lm.fit2)[2],coefficients(lm.fit3)[2],coefficients(lm.fit4)[2]), 
             lower=c(confint(lm.fit1)[2],confint(lm.fit2)[2],confint(lm.fit3)[2],confint(lm.fit4)[2]), 
             upper=c(confint(lm.fit1)[4],confint(lm.fit2)[4],confint(lm.fit3)[4],confint(lm.fit4)[4]))

(p1 <- ggplot() + 
    geom_errorbarh(data= d, mapping=aes(y = location, x=upper, xmin=upper, xmax=lower), height=0.2, size=1, color="blue") + 
    geom_point(data=d, mapping=aes(y=location, x=mean), size=4, shape=21, fill="white") +
    theme_bw() +
    labs(x = "SLR rate (mm yr-1)", y = NULL) +
    theme(axis.title=element_text(size=15,face="bold")))

##############################################################################
#################### Atmospheric effect removal ##############################
##############################################################################
wind_pressure_monthly <- dly532 %>% select(date,wdsp, ddhm,cbl) %>% 
  mutate(date = as.Date(dmy(date))) %>% 
  filter(year(date) < 2017 & year(date) > 1945) %>% 
  complete(date = seq.Date(min(date), max(date), by = "1 day")) %>% 
  mutate(Tx = wdsp * cos(ddhm* pi/180), Ty = wdsp * sin(ddhm* pi/180)) %>% 
  select(-wdsp , -ddhm) %>% group_by(date = floor_date(date, "month")) %>% 
  summarise_all(~ mean(., na.rm = T)) %>% ungroup() %>% rename(time = date)

df_atmo <- left_join(newData1, wind_pressure_monthly)
df_atmo <- df_atmo %>% mutate(across(names(.)[c(2:5,7:9)], ~ detrend(.,tt = 'constant')))

fit1 <- lm(Arklow ~ Tx + Ty + cbl ,  data=df_atmo) 
fit2 <- lm(Dublin ~ Tx + Ty + cbl , data=df_atmo) 
fit3 <- lm(Howth ~ Tx + Ty + cbl ,   data=df_atmo)
fit4 <- lm(Oriel ~ Tx + Ty + cbl ,   data=df_atmo) 

df_atmo <- df_atmo %>% mutate(arklow_corrected = fit1$residuals,
                              dublin_corrected = fit2$residuals,
                              howth_corrected = fit3$residuals,
                              oriel_corrected = fit4$residuals)

##############################################################################
#################### Seasonality   removal      ##############################
##############################################################################
df_atmo_seas_removed <- df_atmo %>% 
  mutate(month = month(time)) %>% 
  group_by(month) %>%  
  mutate_at(names(.)[c(10:13)], ~ . - mean(.,na.rm = T)) %>% ungroup()


#################### re-calculating SLR rates with corrected values ##############################
# Using atmo_removed
lm.fit1_2 <- lm(dublin_corrected ~ date, data = df_atmo)
lm.fit4_2 <- lm(arklow_corrected ~ date, data = df_atmo)
lm.fit3_2 <- lm(oriel_corrected ~  date, data = df_atmo)
lm.fit2_2 <- lm(howth_corrected ~  date, data = df_atmo)


d=data.frame(location=c("Dublin port","Howth harbour","Oriel port", "Arklow"), 
             mean=c(coefficients(lm.fit1_2)[2],coefficients(lm.fit2_2)[2],coefficients(lm.fit3_2)[2],coefficients(lm.fit4_2)[2]), 
             lower=c(    confint(lm.fit1_2)[2],     confint(lm.fit2_2)[2],     confint(lm.fit3_2)[2],     confint(lm.fit4_2)[2]), 
             upper=c(    confint(lm.fit1_2)[4],     confint(lm.fit2_2)[4],     confint(lm.fit3_2)[4],     confint(lm.fit4_2)[4]))

(p1 <- ggplot() + 
    geom_errorbarh(data= d, mapping=aes(y = location, x=upper, xmin=upper, xmax=lower), height=0.2, size=1, color="blue") + 
    geom_point(data=d, mapping=aes(y=location, x=mean), size=4, shape=21, fill="white") +
    theme_bw() +
    labs(x = "SLR rate (mm yr-1)", y = NULL) +
    theme(axis.title=element_text(size=15,face="bold")))



plot(df_atmo$time,df_atmo$Dublin,main = 'Dublin',xlab = 'Year',
     ylab = 'Elevation (mm)',type = 'l')
lines(df_atmo$time,df_atmo$dublin_corrected, col="blue")

# Using atmo_seas_removed
lm.fit1_2 <- lm(dublin_corrected ~ date, data = df_atmo_seas_removed)
lm.fit4_2 <- lm(arklow_corrected ~ date, data = df_atmo_seas_removed)
lm.fit3_2 <- lm(oriel_corrected ~  date, data = df_atmo_seas_removed)
lm.fit2_2 <- lm(howth_corrected ~  date, data = df_atmo_seas_removed)


d=data.frame(location=c("Dublin port","Howth harbour","Oriel port", "Arklow"), 
             mean=c(coefficients(lm.fit1_2)[2],coefficients(lm.fit2_2)[2],coefficients(lm.fit3_2)[2],coefficients(lm.fit4_2)[2]), 
             lower=c(    confint(lm.fit1_2)[2],     confint(lm.fit2_2)[2],     confint(lm.fit3_2)[2],     confint(lm.fit4_2)[2]), 
             upper=c(    confint(lm.fit1_2)[4],     confint(lm.fit2_2)[4],     confint(lm.fit3_2)[4],     confint(lm.fit4_2)[4]))

(p1 <- ggplot() + 
    geom_errorbarh(data= d, mapping=aes(y = location, x=upper, xmin=upper, xmax=lower), height=0.2, size=1, color="blue") + 
    geom_point(data=d, mapping=aes(y=location, x=mean), size=4, shape=21, fill="white") +
    theme_bw() +
    labs(title = "Atmospheric effects and seasonality are removed", x = "SLR rate (mm yr-1)", y = NULL) +
    theme(axis.title=element_text(size=15,face="bold")))



plot(df_atmo_seas_removed$time,df_atmo_seas_removed$Dublin,main = 'Dublin',xlab = 'Year',
     ylab = 'Elevation (mm)',type = 'l')
lines(df_atmo_seas_removed$time,df_atmo_seas_removed$dublin_corrected, col="blue")


#################### Plotting corrected time series ##############################
df_atmo_seas_removed %>% select(names(.)[c(1,10:13)]) %>% 
  mutate(dublin_corrected = dublin_corrected + 200,
         howth_corrected = howth_corrected + 400,
         arklow_corrected = arklow_corrected + 600) %>% 
  melt(., id.vars="time") %>%
  ggplot() +
  geom_line(aes(time, value, group = variable)) +
  labs(title = "Atmospheric effects and seasonality are removed", x = "Year", y = "Sea Level (m)") +
  theme_bw() + 
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  annotate(geom = "text",x=as.Date("2017-01-01"),
           y=0,label="Port Oriel", size = 4, fontface="bold", color = "red") +
  annotate(geom = "text",x=as.Date("2016-12-01"),
           y=400,label="Howth harbor", size = 4, fontface="bold",color = "red") +
  annotate(geom = "text",x=as.Date("2017-01-01"),
           y=200,label="Dublin port", size = 4, fontface="bold",color = "red") +
  annotate(geom = "text",x=as.Date("2017-01-01"),
           y=600,label="Port Arklow", size = 4, fontface="bold",color = "red") +
  theme(axis.title=element_text(size=15,face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12))
