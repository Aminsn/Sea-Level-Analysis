rm(list = ls())
library(ggpubr)
library(readr)
library(zoo)
library(dplyr)
library(lubridate)
library(tidyverse)
library(R2jags)
library(pracma)
library(imputeTS)
require(ggplot2)
require(ggmap)


# ======================== Reading data + fixes  ======================== 
#Part I
howth_harbour <- read_csv("Dublin_Bay_Project-master/howth_harbour.csv")

arklow <- read.delim("Dublin_Bay_Project-master/arklow.txt", stringsAsFactors=FALSE)
arklow <- arklow %>% select(-Quality) %>% 
  mutate(time = as.POSIXct(time, format = "%Y/%m/%d %H:%M:%S", tz = "UTC"))

dub_mon = read_csv('Dublin_Bay_Project-master/dub_mon_1987_to_2017.csv')

#Part II
brest_hour <- read_csv("Dublin_Bay_Project-master/Brest hourly.csv")
newlyn_hour <- read_csv("Dublin_Bay_Project-master/Newlyn hourly.csv")
dub_ann = read_csv('Dublin_Bay_Project-master/dub_ann_1938_to_2017.csv')
irish_sea <- read_csv("IrishSeaComplete.csv")
irish_sea_2 <- irish_sea %>% #This is for extracting newlyn and brest MSL
  group_by(year = floor(time)) %>% 
  summarise_at(vars(bangor:north_shields), mean, na.rm = TRUE) 

#Part III
dly532 <- read_csv("~/Sea Level Analysis/Dublin_Bay_Project-master/dly532.csv")
atmosphere_data <- read_csv("atmosphere_data.csv")
p0_data <- read_csv("NCEP_global_pressure.csv")
p0_data <- p0_data %>% select(Year, p0) %>% rename(year = Year) %>% filter(year < 2017)


# ================ Mapping Dublin, Arklow and Howth Locations =================== 

base = get_stamenmap(c(-11, 45, 10, 60), zoom=6, maptype="terrain-background")
(map1 <- ggmap(base))


locs <-  data.frame( lon_dec = c(-6.138,-6.216, -6.071, -5.54, -4.49), # Dublin, Arklow & Harbour coordinates
                     lat_dec = c(52.795,53.344, 53.391, 50.1, 48.37))

locs2 <-  data.frame( lon_dec = c(-6.138,-6.216, -6.071), # Dublin, Arklow & Harbour coordinates
                      lat_dec = c(52.795,53.344, 53.391))

bigger_map <- map1 +
  geom_point(data=locs, aes(x=lon_dec, y=lat_dec), shape= "square" ,color="#A01502", cex=4)  +
  labs(x="Longitude", y="Latitude") +
  annotate(geom = "text", x = 3, y = 57, label = "North Sea", 
           color = "grey22", size = 4) +
  annotate(geom = "text", x = -9, y = 49, label = "Celtic Sea", 
           color = "grey22", size = 4) +
  annotate(geom = "text", x = -5.54, y = 49.575, label = "Newlyn", 
           , fontface="bold",family= "Serif",size = 4,color = "#A01502") +
  annotate(geom = "text", x = -4.49, y = 47.845, label = "Brest", 
           fontface="bold",family= "Serif",size = 4,color = "#A01502") +
  geom_rect(xmin = -6.5, ymin = 52.5, xmax = -5.6, ymax = 53.6, fill = NA,  colour = "black",
            size = 0.6) + 
  theme(legend.position="bottom", axis.text = element_text(size = rel(0.75)), 
        legend.key = element_rect(colour = "white"), 
        axis.text.x = element_text( vjust=0.5, size = 10, family = "Serif"),
        axis.text.y = element_text( size = 10, family = "Serif"),
        axis.title.x = element_text(family = "Serif"),
        axis.title.y = element_text(family = "Serif"))


base2 = get_stamenmap(c(-6.397, 52.75, -5.894, 53.426), zoom=10, maptype="terrain-background")
(map2 <- ggmap(base2))

smaller_map <- map2 + geom_point(data=locs2, aes(x=lon_dec, y=lat_dec), shape= "square" ,color="#A01502", cex=4) + # plot the points
  labs(x="", y="") + # label the axes
  annotate(geom = "text",x=-6.138,
           y=52.771,label="Arklow", fontface="bold",family= "Serif",size = 4,color = "#A01502") +
  annotate(geom = "text",x=-6.2,
           y=53.32,label="Dublin Port", fontface="bold", family= "Serif", size = 4,color = "#A01502") +
  annotate(geom = "text",x=-6.05,
           y=53.37,label="Howth Harbour", fontface="bold",family= "Serif", size = 4,color = "#A01502") +
  theme_bw() + 
  theme(legend.position="bottom", axis.text = element_text(size = rel(0.75)), 
        legend.key = element_rect(colour = "white"), 
        axis.text.x = element_text( vjust=0.5, size = 10, family = "Serif"),
        axis.text.y = element_text( size = 10, family = "Serif"),
        axis.title.x = element_text(family = "Serif"),
        axis.title.y = element_text(family = "Serif"))

arrow_up <- data.frame(x1 = 14.1, x2 = 7.8, y1 = 11.05, y2 = 16.91)
arrow_down <- data.frame(x1 = 14.1, x2 = 7.8, y1 = 9.67,  y2 =  3.12 )


fig_tide_locs <- ggplot() +
  coord_equal(xlim = c(0, 28), ylim = c(0, 20), expand = FALSE) +
  annotation_custom(ggplotGrob(bigger_map), xmin = 8, xmax = 28, ymin = 0, 
                    ymax = 20) +
  annotation_custom(ggplotGrob(smaller_map), xmin = 0, xmax = 8, ymin = 0, 
                    ymax = 19) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = arrow_up, 
               lineend = "round") +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = arrow_down, 
               lineend = "round") +
  scale_x_continuous(breaks = seq(1,28,1)) +
  scale_y_continuous(breaks = seq(1,20,1)) +
  theme_void()


# ================ Calculating MLW - MHW for ARKLOW and HOWTH =================== 
# ======================== Howth
howth_monthly <- howth_harbour %>% 
  mutate(block = ifelse(hour(time) < 13, 1, 2),
         time = as.Date(time)) %>% 
  group_by(time, block) %>% # Group by day
  summarise(mlw = min(Water_Level_OD_Malin, na.rm = T),
            mhw = max(Water_Level_OD_Malin, na.rm = T),
            msl = mean(Water_Level_OD_Malin, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(time = as.Date(as.yearmon(paste(year(time), month(time)), "%Y %m"))) %>% # Group by month
  summarise(mlw = mean(mlw, na.rm = T),
            mhw = mean(mhw, na.rm = T),
            msl = mean(msl, na.rm = T)) %>% 
  ungroup() %>% 
  filter(year(time) < 2017 & time >= as.Date("2006-12-01")) %>% 
  mutate(Howth_mhw = mhw - mean(mhw), 
         Howth_mlw = mlw - mean(mlw),
         Howth_msl = msl - mean(msl)) %>% 
  select(time, Howth_mlw , Howth_msl, Howth_mhw)

# ======================== Arklow
arklow_monthly <- arklow %>% 
  mutate(block = ifelse(hour(time) < 13, 1, 2),
         time = as.Date(time)) %>% 
  group_by(time, block) %>% # Group by day
  summarise(mlw = min(level, na.rm = T),
            mhw = max(level, na.rm = T),
            msl = mean(level, na.rm = T)) %>% 
  ungroup() %>%
  group_by(time = as.Date(as.yearmon(paste(year(time), month(time)), "%Y %m"))) %>% # Group by month
  summarise(mlw = mean(mlw, na.rm = T),
            mhw = mean(mhw, na.rm = T),
            msl = mean(msl, na.rm = T)) %>% 
  ungroup() %>% 
  filter(year(time) > 2002 & year(time) < 2017) %>% 
  mutate(Arklow_mhw = mhw - mean(mhw, na.rm = T), 
         Arklow_mlw = mlw - mean(mlw, na.rm = T),
         Arklow_msl = msl - mean(msl, na.rm = T)) %>% 
  select(time, Arklow_mlw, Arklow_msl, Arklow_mhw) %>% 
  add_row(!!!setNames(c(NA,	NA,	NA, NA), names(.)), .before = 108) 
arklow_monthly[108,1] <- as.Date("2012-07-01")

# ======================== Dublin 
dublin_monthly <- dub_mon %>% 
  add_row(!!!setNames(c(2014,	12,	2014.958,	NA,	NA,	NA,	NA), names(.)), .before = 336) %>% 
  filter(dyear > 2003.542 & year <2017) %>% 
  mutate(time = as.Date(as.yearmon(paste0(year, month), "%Y %m"))) %>% 
  mutate(Dublin_mhw = mhw -  mean(mhw, na.rm = T), 
         Dublin_mlw = mlw - mean(mlw, na.rm = T),
         Dublin_msl = msl - mean(msl, na.rm = T)) %>% 
  select(time, dyear,Dublin_mlw, Dublin_msl, Dublin_mhw)


# ========================  Combining monthly recordings of DUB-ARK-HOW ======================== 
dub_how_ark_monthly <- dublin_monthly %>% select(-dyear) %>% 
                          full_join(., arklow_monthly) %>% 
                          left_join(.,howth_monthly) %>% 
                          pivot_longer(names_to = "Location",
                                       values_to = "Meter",
                                       -time)


# ======================== Plotting Dublin VS Howth ======================== 
# ======================== MLW
dublin_geom_smooth <- dublin_monthly %>% filter(time >= min(howth_monthly$time)) #Matching time intervals
p1 <- dub_how_ark_monthly %>% filter(Location == "Dublin_mlw" |
                                 Location == "Howth_mlw" ,
                                 time >= min(howth_monthly$time)) %>%
  mutate(Location = as.factor(Location)) %>% 
  ggplot() + 
  geom_line(aes(time, Meter, color = forcats::fct_rev(Location)), size = 1) +
  geom_smooth(data = dublin_geom_smooth, aes(y = Dublin_mlw, x = time), 
              color = "blue", method = lm, se = F) +
  geom_smooth(data = howth_monthly, aes(y = Howth_mlw, x = time), alpha = 0.1,
              color = "red", method = lm, se = F) + 
  labs(x = "Time", y = "") +
  scale_color_manual(values = c("#A01502","#02509E"), name = "", labels = c("Howth Harbor","Dublin Port")) +
  scale_x_date(breaks = "1 years", date_labels = "%Y") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 10, face = "bold", margin = margin(t = 6)),
    axis.text.y = element_text(size = 10, face = "bold", margin = margin(t = 6)),
    legend.text = element_text(family = "Serif",size = 10, face = "bold"),
    legend.title = element_text(size = 15, face = "bold"),
    legend.spacing.x = unit(1,"cm"),
    axis.title.y = element_text(family = "Serif",size = 10, face = "bold"),
    axis.title.x = element_text(family = "Serif",size = 10, face = "bold", 
                                margin = margin(t = 10, r = 0, b = 0, l = 0)))

# ======================== MSL
p2 <- dub_how_ark_monthly %>% filter(Location == "Dublin_msl" |
                                 Location == "Howth_msl" ,
                               time >= min(howth_monthly$time)) %>%
  mutate(Location = as.factor(Location)) %>% 
  ggplot() + 
  geom_line(aes(time, Meter, color = forcats::fct_rev(Location)), size = 1) +
  geom_smooth(data = dublin_geom_smooth, aes(y = Dublin_msl, x = time), 
              color = "blue", method = lm, se = F) +
  geom_smooth(data = howth_monthly, aes(y = Howth_msl, x = time), 
              color = "red", method = lm, se = F) + 
  labs(x = "", y = "") +
  scale_color_manual(values = c("#A01502","#02509E"), name = "", labels = c("Howth Harbor","Dublin Port")) +
  scale_x_date(breaks = "1 years", date_labels = "%Y") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 10, face = "bold", margin = margin(t = 6)),
    axis.text.y = element_text(size = 10, face = "bold", margin = margin(t = 6)),
    legend.text = element_text( family = "Serif",size = 10, face = "bold"),
    legend.title = element_text(size = 15, face = "bold"),
    legend.box.margin = margin(1, 20, 1, 1),
    axis.title.y = element_text(family = "Serif",size = 10, face = "bold"),
    axis.title.x = element_text(family = "Serif",size = 10, face = "bold"))

# ======================== MHW
p3 <- dub_how_ark_monthly %>% filter(Location == "Dublin_mhw" |
                                 Location == "Howth_mhw" ,
                               time >= min(howth_monthly$time)) %>%
  mutate(Location = as.factor(Location)) %>% 
  ggplot() + 
  geom_line(aes(time, Meter, color = forcats::fct_rev(Location)), size = 1) +
  geom_smooth(data = dublin_geom_smooth, aes(y = Dublin_mhw, x = time), 
              color = "blue", method = lm, se = F) +
  geom_smooth(data = howth_monthly, aes(y = Howth_mhw, x = time), 
              color = "red", method = lm, se = F) + 
  labs(x = "", y = "") +
  scale_color_manual(values = c("#A01502","#02509E"), name = "", labels = c("Howth Harbor","Dublin Port")) +
  scale_x_date(breaks = "1 years", date_labels = "%Y") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 10, face = "bold", margin = margin(t = 6)),
    axis.text.y = element_text(size = 10, face = "bold", margin = margin(t = 6)),
    legend.text = element_text(family = "Serif",size = 10, face = "bold"),
    legend.title = element_text(size = 15, face = "bold"),
    legend.box.margin = margin(1, 20, 1, 1),
    axis.title.y = element_text(family = "Serif",size = 10, face = "bold"),
    axis.title.x = element_text(family = "Serif",size = 10, face = "bold"))

fig_dub_howth <- ggarrange(p3 + rremove("x.text"), p2 + rremove("x.text"), p1 , 
          ncol = 1, nrow = 3,
          common.legend = TRUE, legend = "top")

# ======================== Plotting Dublin VS Arklow ======================== 
# ======================== MLW
p1 <- dub_how_ark_monthly %>% filter(Location == "Dublin_mlw" |
                                       Location == "Arklow_mlw") %>%
  mutate(Location = as.factor(Location)) %>% 
  ggplot() + 
  geom_line(aes(time, Meter, color = Location), size = 1) +
  geom_smooth(data = dublin_monthly ,aes(y = Dublin_mlw, x = time), 
              color = "blue", method = lm, se = F) + 
  geom_smooth(data = arklow_monthly, aes(y = Arklow_mlw, x = time), 
              color = "red", method = lm, se = F)+ 
  labs(x = "Time", y = "Mean Low Water (m)") +
  scale_color_manual(values = c("#A01502","#02509E"), name = "", labels = c("Arklow","Dublin Port")) +
  scale_x_date(breaks = "1 years", date_labels = "%Y") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 10, face = "bold", margin = margin(t = 6)),
    axis.text.y = element_text(size = 10, face = "bold", margin = margin(t = 6)),
    legend.text = element_text(family = "Serif",size = 10, face = "bold"),
    legend.title = element_text(size = 15, face = "bold"),
    legend.spacing.x = unit(1,"cm"),
    axis.title.y = element_text(family = "Serif",size = 10, face = "bold"),
    axis.title.x = element_text(family = "Serif",size = 10, face = "bold", 
                                margin = margin(t = 10, r = 0, b = 0, l = 0)))

# ======================== MSL
p2 <- dub_how_ark_monthly %>% filter(Location == "Dublin_msl" |
                                       Location == "Arklow_msl" ) %>%
  mutate(Location = as.factor(Location)) %>% 
  ggplot() + 
  geom_line(aes(time, Meter, color = Location), size = 1) +
  geom_smooth(data = dublin_monthly, aes(y = Dublin_msl, x = time), 
              color = "blue", method = lm, se = F) +
  geom_smooth(data = arklow_monthly, aes(y = Arklow_msl, x = time), 
              color = "red", method = lm, se = F) + 
  labs(x = "", y = "Mean Sea Level (m)") +
  scale_color_manual(values = c("#A01502","#02509E"), name = "", labels = c("Arklow","Dublin Port")) +
  scale_x_date(breaks = "1 years", date_labels = "%Y") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 10, face = "bold", margin = margin(t = 6)),
    axis.text.y = element_text(size = 10, face = "bold", margin = margin(t = 6)),
    legend.text = element_text( family = "Serif",size = 10, face = "bold"),
    legend.title = element_text(size = 15, face = "bold"),
    legend.box.margin = margin(1, 20, 1, 1),
    axis.title.y = element_text(family = "Serif",size = 10, face = "bold"),
    axis.title.x = element_text(family = "Serif",size = 10, face = "bold"))

# ======================== MHW
p3 <- dub_how_ark_monthly %>% filter(Location == "Dublin_mhw" |
                                       Location == "Arklow_mhw" ) %>%
  mutate(Location = as.factor(Location)) %>% 
  ggplot() + 
  geom_line(aes(time, Meter, color = Location), size = 1) +
  geom_smooth(data = dublin_monthly, aes(y = Dublin_mhw, x = time), 
              color = "blue", method = lm, se = F) +
  geom_smooth(data = arklow_monthly, aes(y = Arklow_mhw, x = time), 
              color = "red", method = lm, se = F) + 
  labs(x = "", y = "Mean High Water (m)") +
  scale_color_manual(values = c("#A01502","#02509E"), name = "", labels = c("Arklow","Dublin Port")) +
  scale_x_date(breaks = "1 years", date_labels = "%Y") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 10, face = "bold", margin = margin(t = 6)),
    axis.text.y = element_text(size = 10, face = "bold", margin = margin(t = 6)),
    legend.text = element_text(family = "Serif",size = 10, face = "bold"),
    legend.title = element_text(size = 15, face = "bold"),
    legend.box.margin = margin(1, 20, 1, 1),
    axis.title.y = element_text(family = "Serif",size = 10, face = "bold"),
    axis.title.x = element_text(family = "Serif",size = 10, face = "bold"))

fig_dub_ark <- ggarrange(p3 + rremove("x.text"), p2 + rremove("x.text"), p1 , 
                               ncol = 1, nrow = 3,
                         common.legend = TRUE, legend = "top")


fig_dub_ark_howth <- ggarrange(fig_dub_ark + rremove("x.text"), fig_dub_howth + rremove("x.text"), 
          ncol = 2, nrow = 1)

#annotate_figure(test, bottom = text_grob("Time", family = "Serif", size = 10, face = "bold"))
# ================ Calculating rate differences among Dublin-Howth-Arklow (DHA) ==============
df_for_rates_DHA <- full_join(dublin_monthly, arklow_monthly) %>% 
               left_join(.,howth_monthly) %>% 
               select(-time) %>% 
               pivot_longer(names_to = "Location",
               values_to = "Meter",
               -dyear)


# ======================== Howth vs Dublin: MLW rate
df_HD_mlw <- df_for_rates_DHA %>% 
  filter(dyear > 2006.950, 
         Location == "Dublin_mlw" | Location == "Howth_mlw") %>% 
         mutate(Meter = Meter * 1000,
                siteDublin = ifelse(Location == "Dublin_mlw", 1,0))
                      
lm.mlw.HD <- lm(Meter ~ dyear + siteDublin + dyear*siteDublin, data = df_HD_mlw)
HD_rdiff_mlw <- round(unname(lm.mlw.HD$coefficients[4]),2)

# ======================== Howth vs Dublin: MSL rate
df_HD_msl <- df_for_rates_DHA %>% 
  filter(dyear > 2006.950, 
         Location == "Dublin_msl" | Location == "Howth_msl") %>% 
  mutate(Meter = Meter * 1000,
         siteDublin = ifelse(Location == "Dublin_msl", 1,0))

lm.msl.HD <- lm(Meter ~ dyear + siteDublin + dyear*siteDublin, data = df_HD_msl)
HD_rdiff_msl <- round(unname(lm.msl.HD$coefficients[4]),2)

# ======================== Howth vs Dublin: MHW rate
df_HD_mhw <- df_for_rates_DHA %>% 
  filter(dyear > 2006.950, 
         Location == "Dublin_mhw" | Location == "Howth_mhw") %>% 
  mutate(Meter = Meter * 1000,
         siteDublin = ifelse(Location == "Dublin_mhw", 1,0))

lm.mhw.HD <- lm(Meter ~ dyear + siteDublin + dyear*siteDublin, data = df_HD_mhw)
HD_rdiff_mhw <- round(unname(lm.mhw.HD$coefficients[4]),2)



# ======================== Arklow vs Dublin: MLW rate
df_AD_mlw <- df_for_rates_DHA %>% 
  filter(Location == "Dublin_mlw" | Location == "Arklow_mlw") %>% 
  mutate(Meter = Meter * 1000,
         siteDublin = ifelse(Location == "Dublin_mlw", 1,0))

lm.mlw.AD <- lm(Meter ~ dyear + siteDublin + dyear*siteDublin, data = df_AD_mlw)
AD_rdiff_mlw <- round(unname(lm.mlw.AD$coefficients[4]),2)

# ======================== Arklow vs Dublin: MSL rate
df_AD_msl <- df_for_rates_DHA %>% 
  filter(Location == "Dublin_msl" | Location == "Arklow_msl") %>% 
  mutate(Meter = Meter * 1000,
         siteDublin = ifelse(Location == "Dublin_msl", 1,0))

lm.msl.AD <- lm(Meter ~ dyear + siteDublin + dyear*siteDublin, data = df_AD_msl)
AD_rdiff_msl <- round(unname(lm.msl.AD$coefficients[4]),2)

# ======================== Arklow vs Dublin: MHW rate
df_AD_mhw <- df_for_rates_DHA %>% 
  filter(Location == "Dublin_mhw" | Location == "Arklow_mhw") %>% 
  mutate(Meter = Meter * 1000,
         siteDublin = ifelse(Location == "Dublin_mhw", 1,0))

lm.mhw.AD <- lm(Meter ~ dyear + siteDublin + dyear*siteDublin, data = df_AD_mhw)
AD_rdiff_mhw <- round(unname(lm.mhw.AD$coefficients[4]),2)


# ================ Calculating MLW - MHW for Brest and Newlyn =================== 
# ======================== Brest
brest_yearly <- brest_hour %>% 
  group_by(time = as.Date(paste0(year,"-" ,month, "-", day)),
           block = ifelse(hour < 12, 1, 2)) %>% # Group by day and block
  summarise(mlw = min(level,na.rm = T),
            mhw = max(level,na.rm = T)) %>%
  ungroup() %>% 
  mutate(mlw = replace(mlw, mlw < -30000, NA),
         mhw = replace(mhw, mhw < -30000, NA)) %>% 
  group_by(year = year(time)) %>% # Group by year
  summarise(brest_mlw = mean(mlw, na.rm = T),
            brest_mhw = mean(mhw, na.rm = T)) %>%
  ungroup() %>% 
  right_join(irish_sea_2[,c(1,18)]) %>% 
  rename(brest_msl = Brest) %>% 
  select(year, brest_mlw, brest_msl,brest_mhw)

# ======================== Newlyn
newlyn_yearly <- newlyn_hour %>% 
  group_by(time = as.Date(paste0(year,"-" ,month, "-", day)),
           block = ifelse(hour < 12, 1, 2)) %>% # Group by day and block
  summarise(mlw = min(level, na.rm = T),
            mhw = max(level, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(mlw = replace(mlw, mlw < -30000, NA),
         mhw = replace(mhw, mhw < -30000, NA)) %>% 
  group_by(year = year(time)) %>% # Group by year
  summarise(newlyn_mlw = mean(mlw, na.rm=T),
            newlyn_mhw = mean(mhw, na.rm=T)) %>% 
  ungroup() %>% 
  right_join(irish_sea_2[,c(1,17)]) %>% 
  rename(newlyn_msl = Newlyn) %>% 
  select(year, newlyn_mlw, newlyn_msl, newlyn_mhw)


# ========================  Combining yearly recordings of DUB-NEW-BRE ======================== 
dub_new_bre_yearly <- left_join(dub_ann, newlyn_yearly) %>% 
  left_join(.,brest_yearly) %>% 
  mutate(across(c(newlyn_mlw:brest_mhw), ~ (.-7000)/1000)) %>% 
  mutate(across(c(newlyn_mlw,brest_mlw), ~ (. - mean(., na.rm =T)) + mean(mlw,na.rm = T))) %>% 
  mutate(across(c(newlyn_mhw,brest_mhw), ~ (. - mean(., na.rm =T)) + mean(mhw, na.rm = T))) %>% 
  mutate(across(c(newlyn_msl,brest_msl), ~ (. - mean(., na.rm =T)) + mean(mtl, na.rm = T))) %>% 
  mutate(across(c(mhw:brest_mhw), ~ (. - mean(., na.rm =T)))) %>% 
  filter(year < 2017) %>% 
  rename(dublin_mlw = mlw,
         dublin_msl = mtl,
         dublin_mhw = mhw)

# ======================== Plotting Dublin VS Brest VS Newlyn ======================== 
# ======================== MSL
fig_dub_bre_new <- dub_new_bre_yearly %>% select(year, newlyn_msl, brest_msl, dublin_msl) %>% 
  pivot_longer(names_to = "Location",
               values_to = "msl",
               -year) %>% 
  ggplot() + 
  annotate(geom = "rect", xmin=1939, xmax=1977, ymin= -Inf,
           ymax= Inf,
           fill = "palegreen", colour = "transparent", alpha = 0.5) +
  geom_line(aes(year, msl, color = Location), size = 1) +
  labs(x = "Time", y = "Mean Sea Level (m)") +
  scale_color_manual(values = c("#A01502","#02509E", "#fbc200"), name = "", labels = c("Brest","Dublin Port", "Newlyn")) +
  scale_x_continuous(breaks = seq(min(dub_new_bre_yearly$year), max(dub_new_bre_yearly$year),7)) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 10, face = "bold", margin = margin(t = 6)),
    axis.text.y = element_text(size = 10, face = "bold", margin = margin(t = 6)),
    legend.text = element_text( family = "Serif",size = 10, face = "bold"),
    legend.title = element_text(size = 15, face = "bold"),
    legend.box.margin = margin(1, 20, 1, 1),
    axis.title.y = element_text(family = "Serif",size = 10, face = "bold"),
    axis.title.x = element_text(family = "Serif",size = 10, face = "bold")) 


# ================ Predicting Dublin MSL from MLW in JAGS =================== 
df_jags <- dub_ann %>% filter(year < 2017) %>% 
  mutate(mtl = replace(mtl, year > 1977, NA),
         mlw = na_interpolation(mlw)) %>% 
  mutate(mtl = mtl - 2.549)
 

model_code <- "
model
{
  # Likelihood
  for (i in 1:N) {
    y[i] ~ dnorm(mu[i], sigma^-2)
    mu[i] <- alpha + beta_1* mlw[i] + 
    beta_2 * sin(2 * pi * year[i] / (18.6)) +
    beta_3 * cos(2 * pi * year[i] / (18.6)) + 
    beta_4 * sin(2 * pi * year[i] / (4.4)) + 
    beta_5 * cos(2 * pi * year[i] / (4.4)) 
    
    
    # beta_3 * cos(sin(2 * pi * year[i] / (18.6))) + 
    # beta_4 * sin(pi * year[i] / (18.6)) + 
    # beta_5 * cos(sin(pi * year[i] / (18.6))) + 
    # beta_6 * sin(3 * pi * year[i] / (18.6)) +
    # beta_7 * cos(sin(3 * pi * year[i] / (18.6))) +
    # beta_8 * sin(5 * pi * year[i] / (18.6)) +
    # beta_9 * cos(sin(5 * pi * year[i] / (18.6)))
    
    y_pred[i] ~ dnorm(mu[i], sigma^-2)  
  }
  # Priors
  alpha ~ dnorm(0, 100^-2)
  beta_1 ~ dnorm(0, 100^-2)
  beta_2 ~ dnorm(0, 100^-2)
  beta_3 ~ dnorm(0, 100^-2)
  beta_4 ~ dnorm(0, 100^-2)
  beta_5 ~ dnorm(0, 100^-2)
  # beta_6 ~ dnorm(0, 100^-2)
  # beta_7 ~ dnorm(0, 100^-2)
  # beta_8 ~ dnorm(0, 100^-2)
  # beta_9 ~ dnorm(0, 100^-2)
  sigma ~ dunif(0, 10)
}
"  

model_data <- list(N = nrow(df_jags), 
                   y = df_jags$mtl, 
                   year = df_jags$year, 
                   mlw = df_jags$mlw,
                   pi = pi)

# Choose the parameters to watch
model_parameters <- c("y_pred", "mu")

# Run the model
model_run <- jags(
  data = model_data,
  parameters.to.save = model_parameters,
  model.file = textConnection(model_code)
)

# df_mean <- data.frame(t(model_run$BUGSoutput$sims.list$mu))
# df_mean <- df_mean[,sample(3000, 100)]
# df_mean <- df_mean %>% mutate(time = df_jags$year) %>% pivot_longer(names_to = "index",
#                                                                       values_to = "response",
#                                                                       -time)

df_y_wide <- data.frame(t(model_run$BUGSoutput$sims.list$y_pred))
df_y_wide <- df_y_wide[,sample(3000, 100)]
df_y <- df_y_wide %>% mutate(time = df_jags$year) %>% pivot_longer(names_to = "index",
                                                                values_to = "response",
                                                                -time)

mu_mean <- data.frame(pred = model_run$BUGSoutput$mean$mu, year = df_jags$year)


# ================ Plotting Dublin's new MSL & Howth and Arklow =================== 
howth_yearly <- howth_harbour %>% 
  mutate(time = as.Date(time)) %>% 
  group_by(time) %>% # Group by day
  summarise(msl = mean(Water_Level_OD_Malin, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(time = as.Date(as.yearmon(paste(year(time), month(time)), "%Y %m"))) %>% # Group by month
  summarise(msl = mean(msl, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(time = year(time)) %>% # Group by year
  mutate(msl = mean(msl, na.rm = T)) %>% 
  select(time, msl) %>% 
  filter(time > 2006 & time < 2017)

arklow_yearly <- arklow %>% 
  mutate(time = as.Date(time)) %>% 
  group_by(time) %>% # Group by day
  summarise(msl = mean(level, na.rm = T)) %>% 
  ungroup() %>%
  group_by(time = as.Date(as.yearmon(paste(year(time), month(time)), "%Y %m"))) %>% # Group by month
  summarise(msl = mean(msl, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(time = year(time)) %>% # Group by year
  summarise(msl = mean(msl, na.rm = T)) %>% 
  select(time, msl) %>% 
  filter(time > 2003 & time < 2017)

dub_ann_old <- dub_ann %>% mutate(mtl = mtl - 2.549)


fig_newdub_ark_how <- ggplot() +
  geom_line(data = df_y, aes(x = time, y = response, 
                             group = index, linetype = "Model uncertainty "),color = "#004D95",
            size = 1/4, alpha = 1/4) +
  geom_line(data = dub_ann_old, aes(year, mtl, linetype = "Dublin port's old"), color = "red", size = 1) + 
  geom_line(data = mu_mean, aes(year, pred, linetype = "Dublin port's new"), color = "blue", size = 1) +
  geom_line(data = howth_yearly, aes( time, msl, linetype = "Howth"), color = "yellow", size = 1) +
  geom_line(data = arklow_yearly, aes(time, msl, linetype = "Arklow"), color = "#61FA57", size = 1) +
  scale_x_continuous(breaks = seq(min(df_y$time), max(df_y$time), 5)) +
  scale_linetype_manual(name = "", values = c(1,1,1,1,1), 
                        guide = guide_legend(override.aes = 
                                               list(color = c("#61FA57", "blue", "red", "yellow",  "#004D95"),
                                                    linetype = c("solid", "solid","solid","solid","solid")))) +
  labs(y = "Mean Sea Level (m)", x = "Time") +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 10, face = "bold", margin = margin(t = 6)),
    axis.text.y = element_text(size = 10, face = "bold", margin = margin(t = 6)),
    legend.text = element_text( family = "Serif",size = 10, face = "bold"),
    legend.title = element_text(size = 15, face = "bold"),
    legend.box.margin = margin(1, 20, 1, 1),
    axis.title.y = element_text(family = "Serif",size = 10, face = "bold"),
    axis.title.x = element_text(family = "Serif",size = 10, face = "bold")) 



# ================ Removing atmospheric effects from the new Dublin data =================== 
#Yearly wind + pressure values
# atmo_data <- dly532 %>% select(date,wdsp, ddhm,cbl) %>%
#   mutate(date = as.Date(dmy(date))) %>%
#   filter(year(date) < 2017) %>%
#   complete(date = seq.Date(min(date), max(date), by = "1 day")) %>%
#   mutate(Tx = wdsp * cos(ddhm* pi/180), Ty = wdsp * sin(ddhm* pi/180)) %>%
#   select(-wdsp , -ddhm) %>%
#   group_by(year = year(date),date = month(date)) %>%
#   summarise_all(~ mean(., na.rm = T)) %>%
#   ungroup() %>%
#   select(-date) %>%
#   group_by(year) %>%
#   summarise_all(~ mean(., na.rm = T)) %>%
#   right_join(.,p0_data) %>%
#   na.omit() %>%
#   mutate(across(c(cbl:p0), detrend)) %>%
#   mutate(p = 100*cbl - p0)
  
  
# df_for_jags <- left_join(atmo_data, mu_mean)
# 
# model_code <- "
# model
# {
#  # Likelihood
#  for (i in 1:N) {
#   y[i] ~ dnorm(mu[i], (sigma^2 + known_sigma[i]^2)^-1)
#   
#   mu[i] <- beta_1* year[i] + 
#            beta_2 * uwind[i] + 
#            beta_3 * vwind[i] + 
#            beta_4 * pressure[i] 
#            
#  y_2[i] <- y[i] - (beta_2 * uwind[i] +
#                   beta_3 * vwind[i] +
#                   beta_4 * pressure[i])
#  }
#  # Priors
#  beta_1 ~ dnorm(0, 100^-2)
#  beta_2 ~ dnorm(0, 100^-2)
#  beta_3 ~ dnorm(0, 100^-2)
#  beta_4 ~ dnorm(0, 100^-2)
#  sigma ~ dunif(0, 10)
# }
# "  
# # In this step the inputs should be chnged for each location 
# model_data <- list(N = nrow(df_for_jags), 
#                    y = df_for_jags$pred, 
#                    year = df_for_jags$year, 
#                    uwind =    df_for_jags$Tx,
#                    vwind =    df_for_jags$Ty,
#                    pressure = df_for_jags$p,
#                    known_sigma = newdata_variance$std)
# 
# # Choose the parameters to watch
# model_parameters <- c("y_2", "mu")
# 
# # Run the model
# model_run <- jags(
#   data = model_data,
#   parameters.to.save = model_parameters,
#   model.file = textConnection(model_code)
# )
# 
# 
# df_y_atmo <- data.frame(t(model_run$BUGSoutput$sims.list$y_2))
# df_y_atmo <- df_y_atmo[,sample(3000, 100)]
# df_y_atmo <- df_y_atmo %>% mutate(time = df_for_jags$year) %>% pivot_longer(names_to = "index",
#                                                               values_to = "response",
#                                                               -time)
# 
# mu_mean_atmo <- data.frame(pred = model_run$BUGSoutput$mean$y_2, year = df_for_jags$year)
# 
# ggplot() +
#   geom_line(data = df_y_atmo, aes(x = time, y = response, 
#                              group = index, linetype = "Posterior Predictive"),
#             size = 1/4, alpha = 1/4) +
#   geom_line(data = mu_mean_atmo, aes(year, pred, linetype = "Predicted"), color = "blue", size = 1) + 
#   geom_line(data = df_for_jags, aes(year, pred, linetype = "Observed"), color = "red", size = 1) + 
#   scale_x_continuous(breaks = seq(min(df_for_jags$year), max(df_for_jags$year), 5)) +
#   scale_linetype_manual(name = "Legend", values = c(1,1,1), 
#                         guide = guide_legend(override.aes = 
#                                                list(color = c("red", "black", "blue"),
#                                                     linetype = c("solid", "solid","solid")))) +
#   labs(y = "MTL", x = "Time") 
atmo_data <- atmosphere_data %>% filter(year < 2017) %>% #This modifies pressure values
  mutate(p0 =  p0_data$p0) %>% 
  mutate(across(c(dublin_pressure, newlyn_pressure, brest_pressure), ~ . - p0)) %>% 
  mutate(across(c(dublin_pressure, newlyn_pressure, brest_pressure), ~ ./1000))

df_unc <- df_y_wide[10:78,] #choosing new data samples from 1948 onwards to match atmo data
atmospheric_corrected_values <- df_unc #allocating memory 

for (i in 1:ncol(df_unc)) {
  
  lm.fit <- lm(df_unc[,i] ~ atmo_data$dublin_uwind + atmo_data$dublin_vwind + atmo_data$dublin_pressure)
  atmospheric_corrected_values[,i] <- lm.fit$residuals
  
}  

mu_mean_corrected <- data.frame(pred_correct = rowMeans(atmospheric_corrected_values), year = atmo_data$year)
df_y_corrected <- atmospheric_corrected_values %>% mutate(time = atmo_data$year) %>% pivot_longer(names_to = "index",
                                                              values_to = "response",
                                                              -time)


fig_dub_withoutATMO <- ggplot() +
  geom_line(data = df_y_corrected, aes(x = time, y = response, 
                             group = index, linetype = "Posterior Predictive"), color = "#004D95",
            size = 1/4, alpha = 1/4) +
  geom_line(data = mu_mean_corrected, aes(year, pred_correct, linetype = "Predicted"), color = "blue", size = 1) + 
  scale_x_continuous(breaks = seq(min(df_y_corrected$time), max(df_y_corrected$time), 5)) +
  scale_linetype_manual(name = "", values = c(1,1), 
                        guide = guide_legend(override.aes = 
                                               list(color = c("#004D95", "blue"),
                                                    linetype = c("solid", "solid")))) +
  labs(y = "Mean Sea Level (m)", x = "Time")  +
  theme_classic()

# ========= Extracting New Dublin data uncertainty after ===================
# ========= removing atmospheric effects(Epistemic + Aleatory) ============= 

newdata_variance <- df_y_corrected %>% #To be counted for atmospheric removal model uncertainty
  group_by(year = time) %>% 
  summarise(std = sd(response))

# ========= Estimating Dublin's SLR rate and measure its uncertainity ===================
mu_mean_corrected_for_rate <- mu_mean_corrected %>% 
  filter(year > 1996) %>% left_join(.,newdata_variance)
                                  
model_code <- "
 model
 {
  # Likelihood
  for (i in 1:N) {
  
  y[i] ~ dnorm(mu[i], (sigma^2 + known_sigma[i]^2)^-1)
   
   mu[i] <- alpha + beta_1* year[i] +
    beta_2 * sin(2 * pi * year[i] / (18.6)) +
    beta_3 * cos(2 * pi * year[i] / (18.6)) +
    beta_4 * sin(2 * pi * year[i] / (4.4)) +
    beta_5 * cos(2 * pi * year[i] / (4.4))
    
  }
  # Priors
  alpha ~ dnorm(0, 100^-2)
  beta_1 ~ dnorm(0, 100^-2)
  beta_2 ~ dnorm(0, 100^-2)
  beta_3 ~ dnorm(0, 100^-2)
  beta_4 ~ dnorm(0, 100^-2)
  beta_5 ~ dnorm(0, 100^-2)
  sigma ~ dunif(0, 10)
 }
 "  
 # In this step the inputs should be chnged for each location 
model_data <- list(N = nrow(mu_mean_corrected_for_rate), 
                   y = mu_mean_corrected_for_rate$pred_correct, 
                   year = mu_mean_corrected_for_rate$year,
                   known_sigma = mu_mean_corrected_for_rate$std,
                   pi = pi
                  )
 
 # Choose the parameters to watch
 model_parameters <- c("beta_1")
 
 # Run the model
 model_run <- jags(
   data = model_data,
   parameters.to.save = model_parameters,
   model.file = textConnection(model_code)
 )

slr_rate <- model_run$BUGSoutput$sims.list$beta_1 * 1000 #convert to millimeter
dublin_rate <- quantile(slr_rate, probs = c(0.05,.5,.95))


# ========= Estimating SLR rates of Brest and Newlyn ===================
corrected_dub_new_brest <- dub_new_bre_yearly %>% right_join(., atmo_data)

lm.fit <- lm(corrected_dub_new_brest$newlyn_msl ~ corrected_dub_new_brest$newlyn_uwind + 
               corrected_dub_new_brest$newlyn_vwind + 
               corrected_dub_new_brest$newlyn_pressure)

corrected_dub_new_brest$newlyn_corrected <- lm.fit$residuals

lm.fit <- lm(corrected_dub_new_brest$brest_msl ~ corrected_dub_new_brest$brest_uwind + 
               corrected_dub_new_brest$brest_vwind + 
               corrected_dub_new_brest$brest_pressure)

corrected_dub_new_brest$brest_corrected <- c(NA,NA,NA,NA, lm.fit$residuals)
corrected_dub_new_brest$dublin_corrected <- mu_mean_corrected$pred_correct

corrected_new_brest_for_rate <- corrected_dub_new_brest %>% filter(year > 1996)

# ========= Brest
model_code <- "
 model
 {
  # Likelihood
  for (i in 1:N) {
  
  y[i] ~ dnorm(mu[i], sigma^-2)
   
   mu[i] <- alpha + beta_1* year[i] +
    beta_2 * sin(2 * pi * year[i] / (18.6)) +
    beta_3 * cos(2 * pi * year[i] / (18.6)) +
    beta_4 * sin(2 * pi * year[i] / (4.4)) +
    beta_5 * cos(2 * pi * year[i] / (4.4))
    
  }
  # Priors
  alpha ~ dnorm(0, 100^-2)
  beta_1 ~ dnorm(0, 100^-2)
  beta_2 ~ dnorm(0, 100^-2)
  beta_3 ~ dnorm(0, 100^-2)
  beta_4 ~ dnorm(0, 100^-2)
  beta_5 ~ dnorm(0, 100^-2)
  sigma ~ dunif(0, 10)
 }
 "  
# In this step the inputs should be chnged for each location 
model_data <- list(N = nrow(corrected_new_brest_for_rate), 
                   y = corrected_new_brest_for_rate$brest_corrected, 
                   year = corrected_new_brest_for_rate$year,
                   pi = pi
)

# Choose the parameters to watch
model_parameters <- c("beta_1")

# Run the model
model_run <- jags(
  data = model_data,
  parameters.to.save = model_parameters,
  model.file = textConnection(model_code)
)

slr_rate <- model_run$BUGSoutput$sims.list$beta_1 * 1000 #convert to millimeter
brest_rate <- quantile(slr_rate, probs = c(0.05,.5,.95))

# ========= Newlyn
model_code <- "
 model
 {
  # Likelihood
  for (i in 1:N) {
  
  y[i] ~ dnorm(mu[i], sigma^-2)
   
   mu[i] <- alpha + beta_1* year[i] +
    beta_2 * sin(2 * pi * year[i] / (18.6)) +
    beta_3 * cos(2 * pi * year[i] / (18.6)) +
    beta_4 * sin(2 * pi * year[i] / (4.4)) +
    beta_5 * cos(2 * pi * year[i] / (4.4))
    
  }
  # Priors
  alpha ~ dnorm(0, 100^-2)
  beta_1 ~ dnorm(0, 100^-2)
  beta_2 ~ dnorm(0, 100^-2)
  beta_3 ~ dnorm(0, 100^-2)
  beta_4 ~ dnorm(0, 100^-2)
  beta_5 ~ dnorm(0, 100^-2)
  sigma ~ dunif(0, 10)
 }
 "  
# In this step the inputs should be chnged for each location 
model_data <- list(N = nrow(corrected_new_brest_for_rate), 
                   y = corrected_new_brest_for_rate$newlyn_corrected, 
                   year = corrected_new_brest_for_rate$year,
                   pi = pi
)

# Choose the parameters to watch
model_parameters <- c("beta_1")

# Run the model
model_run <- jags(
  data = model_data,
  parameters.to.save = model_parameters,
  model.file = textConnection(model_code)
)

slr_rate <- model_run$BUGSoutput$sims.list$beta_1 * 1000 #convert to millimeter
newlyn_rate <- quantile(slr_rate, probs = c(0.05,.5,.95))

rates <- rbind(`Dublin port` = dublin_rate, Newlyn = newlyn_rate, Brest = brest_rate)

# ========= Plotting Corrected Dublin VS Brest VS Newlyn ===================

fig_corrected_DBN <- corrected_dub_new_brest %>% select(year, newlyn_corrected, brest_corrected, dublin_corrected) %>% 
  pivot_longer(names_to = "Location",
               values_to = "msl",
               -year) %>% 
  ggplot() + 
  geom_line(aes(year, msl, color = Location), size = 1) +
  labs(x = "Time", y = "Mean Sea Level (m)") +
  scale_color_manual(values = c("#A01502","#02509E", "#fbc200"), name = "", labels = c("Brest","Dublin Port", "Newlyn")) +
  scale_x_continuous(breaks = seq(min(corrected_dub_new_brest$year), max(corrected_dub_new_brest$year),7)) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 10, face = "bold", margin = margin(t = 6)),
    axis.text.y = element_text(size = 10, face = "bold", margin = margin(t = 6)),
    legend.text = element_text( family = "Serif",size = 10, face = "bold"),
    legend.title = element_text(size = 15, face = "bold"),
    legend.box.margin = margin(1, 20, 1, 1),
    axis.title.y = element_text(family = "Serif",size = 10, face = "bold"),
    axis.title.x = element_text(family = "Serif",size = 10, face = "bold")) 
