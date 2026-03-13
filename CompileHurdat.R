# Load in packages
library(pacman)

p_load(tidyverse, stringr, viridis, reshape2, jtools, sf, tmap,
       data.table, dtplyr, lubridate, plm, estimatr, fixest, magick)

# load in hurdat2

hurdat <- read_csv("HURDAT2.csv")

# clean up hurdat2 and save cleaned version ----------------------------------

# remove lines that are labels
hurdat <- subset(hurdat, !is.na(`Status of system`))

# overwrite data
write_csv(hurdat, "HURDAT2.csv", row.names = FALSE)

# subset for only landfalls
hurdat <- subset(hurdat, `Record Identifier` == "L")

# change system status to include major hurricane category
hurdat <- hurdat %>%
  mutate(
    `Status of system` = ifelse(
      `Maximum sustained wind` >= 96, "MH",
      `Status of system`
    )
  )

# remove last column as it is not consistent
hurdat <- hurdat[ ,-c(8)]

write.csv(hurdat, "HURDAT2_landfalls.csv", row.names = FALSE)

us_counties <- tigris::counties(state = c("01", "09", "10", "11", "12", "13",
                                          "22", "23", "24", "25", "28", "33",
                                          "34", "36", "37", "42", "44", "45",
                                          "48", "50", "51"))