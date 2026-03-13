# Try at faster compilation

# Figure out how to get the variables for lanfall, threatened, and after using just florida and August 2008
library(doParallel)
registerDoParallel(cores = 4)
library(pacman)

p_load(raster, grid, tidyverse, stringr, viridis,
       reshape2, ggmosaic, GISTools, sf, tmap, maps, mapdata, 'tigris', foreach)

# Load in Counties
us_counties <- tigris::counties(state = c('01', '09', '10', '11', '12', '13',
                                          '22', '23', '24', '25', '28', '33',
                                          '34', '36', '37', '42', '44', '45',
                                          '48', '50', '51'))

us_counties <- us_counties %>% mutate(
  fips = paste(STATEFP, COUNTYFP, sep= "")
)

# Load in storms
storms <- read_sf('Centers\\points_19.shp')

# Set up dates
dates<-seq(as.Date("2019-01-01"),as.Date("2019-12-31"),by = 1)

#week_end <- dates[weekdays(dates) %in%  c('Saturday')]

# create dataframe to hold variables
test_df <- data.frame(matrix(ncol=9,nrow=length(dates)*length(us_counties$NAME), 
                             dimnames=list(NULL, c("fips", "County", "Date", 
                                                   "Landfall","Threat24", 
                                                   "Threat48", "Threat72", "Threat96", "Threat120"
                             ))))


test_df <- test_df %>% mutate(
  fips = rep(us_counties$fips, each = length(dates)),
  County = rep(us_counties$NAME, each = length(dates)),
  Date = rep(dates, length(us_counties$NAME))
)


# Create a date column
storms <- storms %>% mutate(
  date = str_sub(FLDATELBL, end = 11), # leat years 2017 and on
  #date = str_sub(ADVDATE,end=6), # early years
  date = as.Date(date),
  #date = str_c('20', date, sep=""),# early years
  #date=as.Date(gsub("(\\d{4})(\\d{2})(\\d{2})$","\\1-\\2-\\3",date))# early years
)

storms <- st_transform(storms, '+proj=utm +zone=19 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')
us_counties <- st_transform(us_counties, '+proj=utm +zone=19 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')

# Filter storms just for actual location
storm_loc = storms %>% filter(TAU <12)


# create variable for if the hurricane is over land period
storm_loc <- storm_loc %>% mutate(
  over_land = st_intersects(storm_loc, us_counties),
  over_land = as.integer(over_land),
  over_land = ifelse(is.na(over_land), 0, 1)
)

# create variable for the first over land
storm_loc <- storm_loc %>% mutate(
  landfall = ifelse(over_land - lag(over_land) == 1, 1 ,0),
  landfall = ifelse(is.na(landfall), 0, landfall)
)


# Make a 100 nautical mile buffer for each points
storms_buf <- st_buffer(storm_loc, dist=185200)


# Create a landfall variable by definition in paper ------------------------------
storms_buf <- storms_buf %>% filter(landfall==1)

cond1 <- test_df$Date %in% storms_buf$date

system.time({
  results <-  foreach(i = (1:nrow(test_df))[cond1], .combine = 'c')%dopar%{
    library(sf)
    # get polygons to test intersection
    df_s <- subset(storms_buf, date == test_df$Date[i])
    df_c <- subset(us_counties, fips == test_df$fips[i])
    
    res <- as.integer(st_intersects(df_s, df_c))
    res <- ifelse(is.na(res), 0, 1)
    res <- ifelse(sum(as.integer(res))>0, 1, 0)
    
    return(res)
    gc()
  }
})

ans <- integer(nrow(test_df))
ans = as.data.frame(ans)
ans$n <- seq(1,length(ans$ans), by=1)
want = which(cond1 == TRUE)
ans$ans[which(ans$n %in% want)] <- results

test_df$Landfall <- ans$ans

sub_df <- subset(test_df, Landfall == 1)

sub_df <- sub_df[,c("fips", "Date")]

storms_buf <- storms_buf[,c("MAXWIND", "date")]%>% st_drop_geometry()
storms_buf <- storms_buf  %>% rename("Land_MaxWind" = "MAXWIND"
)

sub_df <- left_join(sub_df, storms_buf, by = c("Date"= "date"))

test_df <- left_join(test_df, sub_df, by = c("fips", "Date")) %>% as.data.frame()

test_df  <- test_df %>% mutate(
  Land_MaxWind = ifelse(is.na(Land_MaxWind), 0, Land_MaxWind),
  
)

test_df <- test_df %>% st_drop_geometry()

rm(ans, results, want)
gc()

#### Tau = 24 ####
# change tau and buffer distance and some var names all else the same

# Filter storms just for actual location
storm_loc = storms %>% filter(TAU == 24)

storms_buf <- st_buffer(storm_loc, dist=185200)

# Indicate which counties are threateed 1 day out counties and their areas ----
cond1 <- test_df$Date %in% storms_buf$date

system.time({
  results <-  foreach(i = (1:nrow(test_df))[cond1], .combine = 'c')%dopar%{
    library(sf)
    # get polygons to test intersection
    df_s <- subset(storms_buf, date == test_df$Date[i])
    df_c <- subset(us_counties, fips == test_df$fips[i])
    
    res <- as.integer(st_intersects(df_s, df_c))
    res <- ifelse(is.na(res), 0, 1)
    res <- ifelse(sum(as.integer(res))>0, 1, 0)
    
    return(res)
    gc()
  }
})

ans <- integer(nrow(test_df))
ans = as.data.frame(ans)
ans$n <- seq(1,length(ans$ans), by=1)
want = which(cond1 == TRUE)
ans$ans[which(ans$n %in% want)] <- results

test_df$Threat24 <- ans$ans

sub_df <- subset(test_df, Threat24 == 1)

sub_df <- sub_df[,c("fips", "Date")]

storms_buf <- storms_buf %>% group_by(date)%>%
  summarize(
    MAXWIND = max(MAXWIND)
  )%>% st_drop_geometry()

storms_buf <- storms_buf  %>% rename("Pred_Wind_24" = "MAXWIND")

sub_df <- left_join(sub_df, storms_buf, by = c("Date"= "date"))

test_df <- left_join(test_df, sub_df, by = c("fips", "Date")) 

test_df  <- test_df %>% mutate(
  Pred_Wind_24 = ifelse(is.na( Pred_Wind_24), 0,  Pred_Wind_24)
)

rm(ans, results, want)
gc()


#### Tau = 48 ####
# change tau and buffer distance and some var names all else the same

# Filter storms just for actual location
storm_loc = storms %>% filter(TAU == 48)


storms_buf <- st_buffer(storm_loc, dist=185200*1.5)


# Indicate which counties are threatened 2 days out and their areas ------------

cond1 <- test_df$Date %in% storms_buf$date

system.time({
  results <-  foreach(i = (1:nrow(test_df))[cond1], .combine = 'c')%dopar%{
    library(sf)
    # get polygons to test intersection
    df_s <- subset(storms_buf, date == test_df$Date[i])
    df_c <- subset(us_counties, fips == test_df$fips[i])
    
    res <- as.integer(st_intersects(df_s, df_c))
    res <- ifelse(is.na(res), 0, 1)
    res <- ifelse(sum(as.integer(res))>0, 1, 0)
    
    return(res)
    gc()
  }
})

ans <- integer(nrow(test_df))
ans = as.data.frame(ans)
ans$n <- seq(1,length(ans$ans), by=1)
want = which(cond1 == TRUE)
ans$ans[which(ans$n %in% want)] <- results


test_df$Threat48 <- ans$ans


sub_df <- subset(test_df, Threat48 == 1)

sub_df <- sub_df[,c("fips", "Date")]

storms_buf <- storms_buf %>% group_by(date)%>%
  summarize(
    MAXWIND = max(MAXWIND)
  )%>% st_drop_geometry()

storms_buf <- storms_buf  %>% rename("Pred_Wind_48" = "MAXWIND")

sub_df <- left_join(sub_df, storms_buf, by = c("Date"= "date"))

test_df <- left_join(test_df, sub_df, by = c("fips", "Date")) 

test_df  <- test_df %>% mutate(
  Pred_Wind_48 = ifelse(is.na( Pred_Wind_48), 0,  Pred_Wind_48)
)

rm(ans, results, want)
gc()

#### Tau = 72 ####
# change tau and buffer distance and some var names all else the same

# Filter storms just for actual location
storm_loc = storms %>% filter(TAU == 72)


storms_buf <- st_buffer(storm_loc, dist=185200*2)


# Indicate which counties are threatened 3 days out counties and their areas -------------

cond1 <- test_df$Date %in% storms_buf$date

system.time({
  results <-  foreach(i = (1:nrow(test_df))[cond1], .combine = 'c')%dopar%{
    library(sf)
    # get polygons to test intersection
    df_s <- subset(storms_buf, date == test_df$Date[i])
    df_c <- subset(us_counties, fips == test_df$fips[i])
    
    res <- as.integer(st_intersects(df_s, df_c))
    res <- ifelse(is.na(res), 0, 1)
    res <- ifelse(sum(as.integer(res))>0, 1, 0)
    
    return(res)
    gc()
  }
})

ans <- integer(nrow(test_df))
ans = as.data.frame(ans)
ans$n <- seq(1,length(ans$ans), by=1)
want = which(cond1 == TRUE)
ans$ans[which(ans$n %in% want)] <- results


test_df$Threat72 <- ans$ans

sub_df <- subset(test_df, Threat72 == 1)

sub_df <- sub_df[,c("fips", "Date")]

storms_buf <- storms_buf %>% group_by(date)%>%
  summarize(
    MAXWIND = max(MAXWIND)
  )%>% st_drop_geometry()

storms_buf <- storms_buf  %>% rename("Pred_Wind_72" = "MAXWIND")

sub_df <- left_join(sub_df, storms_buf, by = c("Date"= "date"))

test_df <- left_join(test_df, sub_df, by = c("fips", "Date")) 

test_df  <- test_df %>% mutate(
  Pred_Wind_72 = ifelse(is.na( Pred_Wind_72), 0,  Pred_Wind_72)
)

rm(ans, results, want)
gc()

#### Tau = 96 ####
# change tau and buffer distance and some var names all else the same

# Filter storms just for actual location
storm_loc = storms %>% filter(TAU == 96)


storms_buf <- st_buffer(storm_loc, dist=185200*2.5)


# Indicate which counties are threatened 4 days out and their areas -------------

cond1 <- test_df$Date %in% storms_buf$date

system.time({
  results <-  foreach(i = (1:nrow(test_df))[cond1], .combine = 'c')%dopar%{
    library(sf)
    # get polygons to test intersection
    df_s <- subset(storms_buf, date == test_df$Date[i])
    df_c <- subset(us_counties, fips == test_df$fips[i])
    
    res <- as.integer(st_intersects(df_s, df_c))
    res <- ifelse(is.na(res), 0, 1)
    res <- ifelse(sum(as.integer(res))>0, 1, 0)
    
    return(res)
    gc()
  }
})

ans <- integer(nrow(test_df))
ans = as.data.frame(ans)
ans$n <- seq(1,length(ans$ans), by=1)
want = which(cond1 == TRUE)
ans$ans[which(ans$n %in% want)] <- results


test_df$Threat96 <- ans$ans

sub_df <- subset(test_df, Threat96 == 1)

sub_df <- sub_df[,c("fips", "Date")]

storms_buf <- storms_buf %>% group_by(date)%>%
  summarize(
    MAXWIND = max(MAXWIND)
  )%>% st_drop_geometry()

storms_buf <- storms_buf  %>% rename("Pred_Wind_96" = "MAXWIND")

sub_df <- left_join(sub_df, storms_buf, by = c("Date"= "date"))

test_df <- left_join(test_df, sub_df, by = c("fips", "Date")) 

test_df  <- test_df %>% mutate(
  Pred_Wind_96 = ifelse(is.na( Pred_Wind_96), 0,  Pred_Wind_96)
)

rm(ans, results, want)
gc()



#### Tau = 120 ####
# change tau and buffer distance and some var names all else the same

# Filter storms just for actual location
storm_loc = storms %>% filter(TAU == 120)


storms_buf <- st_buffer(storm_loc, dist=185200*3)


# Indicate which counties are threatened 5 days out and their areas -------------

cond1 <- test_df$Date %in% storms_buf$date

system.time({
  results <-  foreach(i = (1:nrow(test_df))[cond1], .combine = 'c')%dopar%{
    library(sf)
    # get polygons to test intersection
    df_s <- subset(storms_buf, date == test_df$Date[i])
    df_c <- subset(us_counties, fips == test_df$fips[i])
    
    res <- as.integer(st_intersects(df_s, df_c))
    res <- ifelse(is.na(res), 0, 1)
    res <- ifelse(sum(as.integer(res))>0, 1, 0)
    
    return(res)
    gc()
  }
})

ans <- integer(nrow(test_df))
ans = as.data.frame(ans)
ans$n <- seq(1,length(ans$ans), by=1)
want = which(cond1 == TRUE)
ans$ans[which(ans$n %in% want)] <- results


test_df$Threat120 <- ans$ans

sub_df <- subset(test_df, Threat120 == 1)

sub_df <- sub_df[,c("fips", "Date")]

storms_buf <- storms_buf %>% group_by(date)%>%
  summarize(
    MAXWIND = max(MAXWIND)
  )%>% st_drop_geometry()

storms_buf <- storms_buf  %>% rename("Pred_Wind_120" = "MAXWIND")

sub_df <- left_join(sub_df, storms_buf, by = c("Date"= "date"))

test_df <- left_join(test_df, sub_df, by = c("fips", "Date")) 

test_df  <- test_df %>% mutate(
  Pred_Wind_120 = ifelse(is.na( Pred_Wind_120), 0,  Pred_Wind_120)
)

rm(ans, results, want)
gc()



# add in hurricane classes -----------------------------------------------------
# TD, TS, H1, H2, H3, H4, H5
# knots
# < 34, 34, 64, 83, 96, 113, >=137

test_df <- test_df %>% mutate(
  Storm_Type = ifelse(Land_MaxWind == 0, "None", 
                      ifelse(Land_MaxWind< 34, "TD",
                             ifelse(Land_MaxWind>= 34 & Land_MaxWind<64, "TS", 
                                    ifelse(Land_MaxWind>= 64 & Land_MaxWind<83, "H1",
                                           ifelse(Land_MaxWind>= 83 & Land_MaxWind<96, "H2",
                                                  ifelse(Land_MaxWind>= 96 & Land_MaxWind<113, "H3",
                                                         ifelse(Land_MaxWind>= 113 & Land_MaxWind<137, "H4", "H5"))))))),
  
  Pred_Type_24 = ifelse(Pred_Wind_24 == 0, "None", 
                        ifelse(Pred_Wind_24< 34, "TD",
                               ifelse(Pred_Wind_24>= 34 & Pred_Wind_24<64, "TS", 
                                      ifelse(Pred_Wind_24>= 64 & Pred_Wind_24<83, "H1",
                                             ifelse(Pred_Wind_24>= 83 & Pred_Wind_24<96, "H2",
                                                    ifelse(Pred_Wind_24>= 96 & Pred_Wind_24<113, "H3",
                                                           ifelse(Pred_Wind_24>= 113 & Pred_Wind_24<137, "H4", "H5"))))))),
  
  Pred_Type_48 = ifelse(Pred_Wind_48 == 0, "None", 
                        ifelse(Pred_Wind_48< 34, "TD",
                               ifelse(Pred_Wind_48>= 34 & Pred_Wind_48<64, "TS", 
                                      ifelse(Pred_Wind_48>= 64 & Pred_Wind_48<83, "H1",
                                             ifelse(Pred_Wind_48>= 83 & Pred_Wind_48<96, "H2",
                                                    ifelse(Pred_Wind_48>= 96 & Pred_Wind_48<113, "H3",
                                                           ifelse(Pred_Wind_48>= 113 & Pred_Wind_48<137, "H4", "H5"))))))),
  
  Pred_Type_72 = ifelse(Pred_Wind_72 == 0, "None", 
                        ifelse(Pred_Wind_72< 34, "TD",
                               ifelse(Pred_Wind_72>= 34 & Pred_Wind_72<64, "TS", 
                                      ifelse(Pred_Wind_72>= 64 & Pred_Wind_72<83, "H1",
                                             ifelse(Pred_Wind_72>= 83 & Pred_Wind_72<96, "H2",
                                                    ifelse(Pred_Wind_72>= 96 & Pred_Wind_72<113, "H3",
                                                           ifelse(Pred_Wind_72>= 113 & Pred_Wind_72<137, "H4", "H5"))))))),
  
  Pred_Type_96 = ifelse(Pred_Wind_96 == 0, "None", 
                        ifelse(Pred_Wind_96< 34, "TD",
                               ifelse(Pred_Wind_96>= 34 & Pred_Wind_96<64, "TS", 
                                      ifelse(Pred_Wind_96>= 64 & Pred_Wind_96<83, "H1",
                                             ifelse(Pred_Wind_96>= 83 & Pred_Wind_96<96, "H2",
                                                    ifelse(Pred_Wind_96>= 96 & Pred_Wind_96<113, "H3",
                                                           ifelse(Pred_Wind_96>= 113 & Pred_Wind_96<137, "H4", "H5"))))))),
  
  Pred_Type_120 = ifelse(Pred_Wind_120 == 0, "None", 
                         ifelse(Pred_Wind_120< 34, "TD",
                                ifelse(Pred_Wind_120>= 34 & Pred_Wind_120<64, "TS", 
                                       ifelse(Pred_Wind_120>= 64 & Pred_Wind_120<83, "H1",
                                              ifelse(Pred_Wind_120>= 83 & Pred_Wind_120<96, "H2",
                                                     ifelse(Pred_Wind_120>= 96 & Pred_Wind_120<113, "H3",
                                                            ifelse(Pred_Wind_120>= 113 & Pred_Wind_120<137, "H4", "H5")))))))
)


write.csv(test_df, 'Daily Data/Daily2019.csv')
