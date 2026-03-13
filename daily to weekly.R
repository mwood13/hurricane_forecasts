# THis script convert the daily hurricane variables to weekly by county
# the week begins on sunday and ends on Saturday to match Nielsen
# need to watch on fips and week_end variables

library(pacman)

p_load(raster, grid, tidyverse, stringr, viridis, data.table,
       reshape2, ggmosaic, GISTools, sf, tmap, maps, 
       mapdata, 'tigris', lubridate)

storms_08 <- fread('Hur_Var_2008.csv')
storms_09 <- fread('Hur_Var_2009.csv')
storms_10 <- fread('Hur_Var_2010.csv')
storms_11 <- fread('Hur_Var_2011.csv')
storms_12 <- fread('Hur_Var_2012.csv')
storms_13 <- fread('Hur_Var_2013.csv')
storms_14 <- fread('Hur_Var_2014.csv')
storms_15 <- fread('Hur_Var_2015.csv')
storms_16 <- fread('Hur_Var_2016.csv')
storms_17 <- fread('Hur_Var_2017.csv')
storms_18 <- fread('Hur_Var_2018.csv')
storms_19 <- fread('Hur_Var_2019.csv')


storms_08 <- rename(storms_08, c(Threat_24 = Threat24, Threat_48 = Threat48, Threat_72 = Threat72, Threat_96 = Threat96))
storms_08 <- storms_08[,c(1:14)]

storms_13 <- rename(storms_13, c(Threat_24 = Threat24, Threat_48 = Threat48, Threat_72 = Threat72, Threat_96 = Threat96))
storms_13 <- storms_13[,c(1:14)]

storms_14 <- rename(storms_14, c(Threat_24 = Threat24, Threat_48 = Threat48, Threat_72 = Threat72, Threat_96 = Threat96))
storms_15 <- rename(storms_15, c(Threat_24 = Threat24, Threat_48 = Threat48, Threat_72 = Threat72, Threat_96 = Threat96))
storms_16 <- rename(storms_16, c(Threat_24 = Threat24, Threat_48 = Threat48, Threat_72 = Threat72, Threat_96 = Threat96))
storms_17 <- rename(storms_17, c(Threat_24 = Threat24, Threat_48 = Threat48, Threat_72 = Threat72, Threat_96 = Threat96))
storms_18 <- rename(storms_18, c(Threat_24 = Threat24, Threat_48 = Threat48, Threat_72 = Threat72, Threat_96 = Threat96))
storms_19 <- rename(storms_19, c(Threat_24 = Threat24, Threat_48 = Threat48, Threat_72 = Threat72, Threat_96 = Threat96))


storms_all <- bind_rows(storms_08, storms_09)
storms_all <- bind_rows(storms_all, storms_10)
storms_all <- bind_rows(storms_all, storms_11)
storms_all <- bind_rows(storms_all, storms_12)
storms_all <- bind_rows(storms_all, storms_13)
storms_all <- bind_rows(storms_all, storms_14)
storms_all <- bind_rows(storms_all, storms_15)
storms_all <- bind_rows(storms_all, storms_16)
storms_all <- bind_rows(storms_all, storms_17)
storms_all <- bind_rows(storms_all, storms_18)
storms_all <- bind_rows(storms_all, storms_19)
storms_all <- storms_all[,c(2:14)]

# add in wind speed ---------------------------------------------------------------

rm(storms_08, storms_09, storms_10, storms_11, storms_12, storms_13,
   storms_14, storms_15, storms_16, storms_17, storms_18, storms_19)

# 2008
hur_now <- read_sf('Centers\\points_08.shp')

hur_now <- hur_now %>% mutate(
  #date = str_sub(FLDATELBL, end = 11), # leat years 2017 and on
  date = str_sub(ADVDATE,end=6), # early years
  #date = as.Date(date),
  date = str_c('20', date, sep=""),# early years
  date=as.Date(gsub("(\\d{4})(\\d{2})(\\d{2})$","\\1-\\2-\\3",date))# early years
)

hur_now <- hur_now[,c("TAU", "MAXWIND", "date")]

hur_now <- st_drop_geometry(hur_now)

hur_all <- hur_now

# 2009

hur_now <- read_sf('Centers\\points_09.shp')


hur_now <- hur_now %>% mutate(
  #date = str_sub(FLDATELBL, end = 11), # leat years 2017 and on
  date = str_sub(ADVDATE,end=6), # early years
  #date = as.Date(date),
  date = str_c('20', date, sep=""),# early years
  date=as.Date(gsub("(\\d{4})(\\d{2})(\\d{2})$","\\1-\\2-\\3",date))# early years
)

hur_now <- hur_now[,c("TAU", "MAXWIND", "date")]

hur_now <- st_drop_geometry(hur_now)

hur_all <- rbind(hur_all, hur_now)

# 2010

hur_now <- read_sf('Centers\\points_10.shp')


hur_now <- hur_now %>% mutate(
  #date = str_sub(FLDATELBL, end = 11), # leat years 2017 and on
  date = str_sub(ADVDATE,end=6), # early years
  #date = as.Date(date),
  date = str_c('20', date, sep=""),# early years
  date=as.Date(gsub("(\\d{4})(\\d{2})(\\d{2})$","\\1-\\2-\\3",date))# early years
)

hur_now <- hur_now[,c("TAU", "MAXWIND", "date")]

hur_now <- st_drop_geometry(hur_now)

hur_all <- rbind(hur_all, hur_now)

#2011

hur_now <- read_sf('Centers\\points_11.shp')


hur_now <- hur_now %>% mutate(
  #date = str_sub(FLDATELBL, end = 11), # leat years 2017 and on
  date = str_sub(ADVDATE,end=6), # early years
  #date = as.Date(date),
  date = str_c('20', date, sep=""),# early years
  date=as.Date(gsub("(\\d{4})(\\d{2})(\\d{2})$","\\1-\\2-\\3",date))# early years
)

hur_now <- hur_now[,c("TAU", "MAXWIND", "date")]

hur_now <- st_drop_geometry(hur_now)

hur_all <- rbind(hur_all, hur_now)

#2012

hur_now <- read_sf('Centers\\points_12.shp')


hur_now <- hur_now %>% mutate(
  #date = str_sub(FLDATELBL, end = 11), # leat years 2017 and on
  date = str_sub(ADVDATE,end=6), # early years
  #date = as.Date(date),
  date = str_c('20', date, sep=""),# early years
  date=as.Date(gsub("(\\d{4})(\\d{2})(\\d{2})$","\\1-\\2-\\3",date))# early years
)

hur_now <- hur_now[,c("TAU", "MAXWIND", "date")]

hur_now <- st_drop_geometry(hur_now)

hur_all <- rbind(hur_all, hur_now)

# 2013

hur_now <- read_sf('Centers\\points_13.shp')

hur_now <- hur_now %>% mutate(
  #date = str_sub(FLDATELBL, end = 11), # leat years 2017 and on
  date = str_sub(ADVDATE,end=6), # early years
  #date = as.Date(date),
  date = str_c('20', date, sep=""),# early years
  date=as.Date(gsub("(\\d{4})(\\d{2})(\\d{2})$","\\1-\\2-\\3",date))# early years
)

hur_now <- hur_now[,c("TAU", "MAXWIND", "date")]

hur_now <- st_drop_geometry(hur_now)

hur_all <- rbind(hur_all, hur_now)

# 2014

hur_now <- read_sf('Centers\\points_14.shp')


hur_now <- hur_now %>% mutate(
  #date = str_sub(FLDATELBL, end = 11), # leat years 2017 and on
  date = str_sub(ADVDATE,end=6), # early years
  #date = as.Date(date),
  date = str_c('20', date, sep=""),# early years
  date=as.Date(gsub("(\\d{4})(\\d{2})(\\d{2})$","\\1-\\2-\\3",date))# early years
)

hur_now <- hur_now[,c("TAU", "MAXWIND", "date")]

hur_now <- st_drop_geometry(hur_now)

hur_all <- rbind(hur_all, hur_now)

# 2015

hur_now <- read_sf('Centers\\points_15.shp')


hur_now <- hur_now %>% mutate(
  #date = str_sub(FLDATELBL, end = 11), # leat years 2017 and on
  date = str_sub(ADVDATE,end=6), # early years
  #date = as.Date(date),
  date = str_c('20', date, sep=""),# early years
  date=as.Date(gsub("(\\d{4})(\\d{2})(\\d{2})$","\\1-\\2-\\3",date))# early years
)

hur_now <- hur_now[,c("TAU", "MAXWIND", "date")]

hur_now <- st_drop_geometry(hur_now)

hur_all <- rbind(hur_all, hur_now)

# 2016

hur_now <- read_sf('Centers\\points_16.shp')


hur_now <- hur_now %>% mutate(
  #date = str_sub(FLDATELBL, end = 11), # leat years 2017 and on
  date = str_sub(ADVDATE,end=6), # early years
  #date = as.Date(date),
  date = str_c('20', date, sep=""),# early years
  date=as.Date(gsub("(\\d{4})(\\d{2})(\\d{2})$","\\1-\\2-\\3",date))# early years
)

hur_now <- hur_now[,c("TAU", "MAXWIND", "date")]

hur_now <- st_drop_geometry(hur_now)

hur_all <- rbind(hur_all, hur_now)

# 2017

hur_now <- read_sf('Centers\\points_17.shp')

hur_now <- hur_now %>% mutate(
  date = str_sub(FLDATELBL, end = 11), # leat years 2017 and on
  date=as.Date(gsub("(\\d{4})(\\d{2})(\\d{2})$","\\1-\\2-\\3",date))# early years
)

hur_now <- hur_now[,c("TAU", "MAXWIND", "date")]

hur_now <- st_drop_geometry(hur_now)

hur_all <- rbind(hur_all, hur_now)

# 2018

hur_now <- read_sf('Centers\\points_18.shp')

hur_now <- hur_now %>% mutate(
  date = str_sub(FLDATELBL, end = 11), # leat years 2017 and on
  date=as.Date(gsub("(\\d{4})(\\d{2})(\\d{2})$","\\1-\\2-\\3",date))# early years
)

hur_now <- hur_now[,c("TAU", "MAXWIND", "date")]

hur_now <- st_drop_geometry(hur_now)

hur_all <- rbind(hur_all, hur_now)

#2019

hur_now <- read_sf('Centers\\points_19.shp')

hur_now <- hur_now %>% mutate(
  date = str_sub(FLDATELBL, end = 11), # leat years 2017 and on
  date=as.Date(gsub("(\\d{4})(\\d{2})(\\d{2})$","\\1-\\2-\\3",date))# early years
)

hur_now <- hur_now[,c("TAU", "MAXWIND", "date")]

hur_now <- st_drop_geometry(hur_now)

hur_all <- rbind(hur_all, hur_now)


# pivot wider

hur_wide <- pivot_wider(hur_all, names_from = 'TAU', values_from = "MAXWIND", values_fn = max)

hur_wide <- hur_wide[,c(1:8)]


hur_wide[is.na(hur_wide)] <- 0

hur_wide <- hur_wide[,c(1,2,4:8)]

hur_wide <- rename(hur_wide, c(landfall_wind = '0',Pred24_wind = '24',Pred36_wind = '36',
                   Pred48_wind = '48',  Pred72_wind = '72',  Pred96_wind = '96'))

storms_all <- storms_all[,c(1:13)]
storms_all <- as.data.frame(storms_all)
hur_wide <- as.data.frame(hur_wide)

storms_all <- rename(storms_all, date = Date)

storms_all <- storms_all %>% mutate(
  date = as.Date(date)
)

# merge data sets

storms_all <- left_join(storms_all, hur_wide, by = 'date')

storms_all <- storms_all %>% mutate(
  landfall_wind = ifelse(is.na(landfall_wind), 0 , landfall_wind),
  Pred24_wind = ifelse(is.na(Pred24_wind), 0, Pred24_wind),
  Pred36_wind = ifelse(is.na(Pred36_wind), 0, Pred36_wind),
  Pred48_wind = ifelse(is.na(Pred48_wind), 0, Pred48_wind),
  Pred72_wind = ifelse(is.na(Pred72_wind), 0, Pred72_wind),
  Pred96_wind = ifelse(is.na(Pred96_wind), 0, Pred96_wind)
)

# Round date down to week -----------------------------------------------------------
storms_all$week <- ceiling_date(storms_all$date, "week")

# Create week_end variable for matching
storms_all$week_end <- storms_all$week - 1

# Create days until end of week variable
storms_all <- storms_all %>% mutate(
  days_until_end = difftime(week_end, date, units = 'days'),
  days_until_end = as.numeric(days_until_end),
  fips = str_pad(fips, 5,'left', '0')
)

# Group by week_end and Fips to create variables
# create dummy and county of struck and threatened variables
# Create days from the actaul landfall until week end
# 0 means struck on saturday
# 1 mean friday etc.

test_df <- storms_all[,
                      .(Struck_c = sum(Struck, na.rm = TRUE),
                        Struck_d = ifelse(sum(Struck, na.rm = TRUE) > 0, 1, 0),
                        Landfall_c = sum(Landfall, na.rm = TRUE),
                        Landfall_d = ifelse(sum(Landfall, na.rm = TRUE) >0, 1, 0),
                        After1 = ifelse(sum(After1, na.rm = TRUE)>0, 1, 0),
                        After2 = ifelse(sum(After2, na.rm = TRUE)>0, 1, 0),
                        After3 = ifelse(sum(After3, na.rm = TRUE)>0, 1, 0),
                        After4 = ifelse(sum(After4, na.rm = TRUE)>0, 1, 0),
                        Threat_24_c = sum(Threat_24, na.rm = TRUE),
                        Threat_24_d = ifelse(sum(Threat_24, na.rm = TRUE)>0, 1, 0),
                        Threat_48_c = sum(Threat_48, na.rm = TRUE),
                        Threat_48_d = ifelse(sum(Threat_48, na.rm = TRUE)>0, 1, 0),
                        Threat_72_c = sum(Threat_72, na.rm = TRUE),
                        Threat_72_d = ifelse(sum(Threat_72, na.rm = TRUE)>0, 1, 0)),
                      max_landfall_wind = max(landfall_wind),
                      max_Pred24_wind = max(Pred24_wind), 
                      max_Pred48_wind = max(Pred48_wind), 
                      max_Pred72_wind = max(Pred72_wind),
                      max_Pred96_wind = max(Pred96_wind),
                      by=.(fips, week_end)]




# Save df 
write.csv(test_df, 'Hur_weekly.csv', row.names = FALSE)
write.csv(storms_all, 'Hur_daily.csv', row.names = FALSE)
