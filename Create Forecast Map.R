# build a map to test forecasting patterns

library(pacman)

p_load(tidyverse, stringr, viridis, reshape2, jtools, sf, tmap,
       data.table, dtplyr, lubridate, plm, estimatr, fixest, magick)


us_counties <- tigris::counties(state = c('01', '09', '10', '11', '12', '13',
                                          '22', '23', '24', '25', '28', '33',
                                          '34', '36', '37', '42', '44', '45',
                                          '48', '50', '51'))


storms_08 <- read_sf('Centers\\points_08.shp')

storms_08 <- storms_08 %>% mutate(
  #date = str_sub(FLDATELBL, end = 11), # leat years 2017 and on
  date = str_sub(ADVDATE,end=6), # early years
  #date = as.Date(date),
  date = str_c('20', date, sep=""),# early years
  date=as.Date(gsub("(\\d{4})(\\d{2})(\\d{2})$","\\1-\\2-\\3",date))# early years
)

storms_0 <- subset(storms_08, TAU == 0) %>% st_buffer(dist = 185200)
storms_24 <- subset(storms_08, TAU == 24) %>% st_buffer(dist = 1*185200)
storms_48 <- subset(storms_08, TAU == 48) %>% st_buffer(dist = 1.5*185200)
storms_72 <- subset(storms_08, TAU == 72) %>% st_buffer(dist = 2*185200)
storms_96 <- subset(storms_08, TAU == 96) %>% st_buffer(dist = 2.5*185200)
storms_120 <- subset(storms_08, TAU == 120) %>% st_buffer(dist = 3*185200)

storms <- rbind(storms_0, storms_24)
storms <- rbind(storms, storms_48)
storms <- rbind(storms, storms_72)
storms <- rbind(storms, storms_96)
storms <- rbind(storms, storms_120)

# choose a hurricane to plot
plot_storm <- subset(storms, STORMNAME == "GUSTAV")

plot_storm <- plot_storm %>% 
  mutate(
    ADVISNUM = as.double(ADVISNUM)
  )
# create buffers to match those of the data
#p <-

Al = min(plot_storm$ADVISNUM)
Au = max(plot_storm$ADVISNUM)

for(a in Al:Au){
  sub_df <- subset(plot_storm, ADVISNUM == a)%>%drop_na()
  
  p <- tm_shape(sub_df)+ tm_layout(frame = F, legend.show=F)+
    tm_fill(col = 'TAU', 
            palette = c(
              "0" = '#440154FF',
              "24" = "#414487FF",
              "48" = "#2A788EFF",
              "72" = "#22A884FF",
              "96" = "#7AD151FF",
              "120" = "#FDE725FF"),
            fill_alpha = 0.5)+
    tm_dots(size = .5)+
    tm_shape(us_counties) + tm_borders() 
  
  if(a < 10){
    fp <- file.path("Maps", paste0("0",a, ".png"))
  }else{
    fp <- file.path("Maps", paste0(a, ".png"))
  }
  
 tmap_save(p, fp, width=1920, height=1080, asp=0) 
}




imgs <- list.files("Maps", full.names = TRUE)
img_list <- lapply(imgs, image_read)

## join the images together
img_joined <- image_join(img_list)

## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 2)

## view animated image
img_animated

## save to disk
image_write(image = img_animated,
            path = "forecast.gif")




sub_df <- subset(plot_storm, ADVISNUM == 17)%>%drop_na()

tm_shape(sub_df)+ tm_layout(frame = F, legend.show=F)+
  tm_polygons(
    fill = "TAU",
    fill_alpha = 0.5,
    fill.scale = tm_scale_intervals(
      values =c(
        "0" = '#440154FF',
        "24" = "#414487FF",
      "48" = "#2A788EFF",
      "72" = "#22A884FF",
      "96" = "#7AD151FF",
      "120" = "#FDE725FF"),,
      label.style = "discrete"
    ),
    fill.legend = tm_legend(
      title = "Days until Landfall",
      labels = c("0", "1", "2", "3", "4", "5"),
      title.size = 1.5,
      text.size = 1.2,
      na.show = FALSE
    )
  )+
  tm_dots(size = .5)+
  tm_shape(us_counties) + tm_borders() 
