# compile data to one df
library(pacman)

p_load(raster, grid, tidyverse, stringr, viridis,
       reshape2, ggmosaic, GISTools, sf, tmap, maps, mapdata, 'tigris', foreach)


df <- read.csv("Daily Data/Daily2008.csv")
new_df <- read.csv("Daily Data/Daily2009.csv")

df <- rbind(df, new_df)


new_df <- read.csv("Daily Data/Daily2010.csv")
df <- rbind(df, new_df)

new_df <- read.csv("Daily Data/Daily2011.csv")
df <- rbind(df, new_df)
new_df <- read.csv("Daily Data/Daily2012.csv")
df <- rbind(df, new_df)
new_df <- read.csv("Daily Data/Daily2013.csv")
df <- rbind(df, new_df)
new_df <- read.csv("Daily Data/Daily2014.csv")
df <- rbind(df, new_df)
new_df <- read.csv("Daily Data/Daily2015.csv")
df <- rbind(df, new_df)
new_df <- read.csv("Daily Data/Daily2016.csv")
df <- rbind(df, new_df)
new_df <- read.csv("Daily Data/Daily2017.csv")
df <- rbind(df, new_df)
new_df <- read.csv("Daily Data/Daily2018.csv")
df <- rbind(df, new_df)
new_df <- read.csv("Daily Data/Daily2019.csv")
df <- rbind(df, new_df)

test <- subset(df, is.na(Land_MaxWind))

write.csv(df, "Daily_Hurricanes.csv", row.names = F)
