library(tidyverse)
library(lubridate)
library(scales)

blpw.all1 <- blpw.all 
original.df <- blpw.all 
subset(blpw.all, band == 197052092)


# STEP 1: Remove rows with only one mass value per band number (removes birds that were not recaptured and birds that were recaptured but with only one observation present)
blpw.all <- blpw.all %>% 
  group_by(band) %>% 
  filter(n() >= 2)

# STEP 2: Tidy by removing some unnecessary columns
blpw.all <- blpw.all %>%
  select(location, band, mass, year, day, month)

# STEP 3: Combine all date information into one column named 'date'
blpw.all <- blpw.all %>%
  mutate(date = make_date(year, month, day))

# STEP 4: Create 'julian' day column 
blpw.all$julian <- yday(blpw.all$date)

# STEP 5: Create column of cumulative delta mass values
blpw.all <- blpw.all %>%
  group_by(band, year) %>%
  mutate(deltamass = c(0, diff(mass))) %>%
  mutate(cumdelta = cumsum(deltamass))

# STEP 6: Create graph mapping deltamass by date
ggplot(data = blpw.all, mapping = aes(x = (yday(date)), y = cumdelta, colour = band)) +
  geom_point() +
  geom_line() +
  theme(legend.position = "none") +
  facet_wrap(~ location) +
  ylab("Mass (in grams, relative to capture date)") +
  xlab("Time of Year") +
  scale_x_continuous(labels = function(x) format(as.Date(as.character(x), "%j"), "%b"))
  
#current codes I am working with
  scale_x_date(breaks = "1 month", labels = date_format("%b"))

  scale_x_date(labels = date_format('%b'))

  scale_x_date(breaks = date_breaks("months"), labels = date_format("%j"))
  
  scale_x_continuous(labels = function(x) format(as.Date(as.character(x), "%j"), "%b"))

  ?scale_x_continuous
#### Month labels are off in graph _ IS IT POSSIBLE TO GRAPH 'MONTH' COLUMN SEPERATELY ON X AXIS
  
  ggplot(data = blpw.all, mapping = aes(x = (julian), y = cumdelta, colour = band)) +
    geom_point() +
    geom_line() +
    theme(legend.position = "none") +
    facet_wrap(~ location) +
    ylab("Mass (in grams, relative to capture date)") +
    xlab("Time of Year") +
    scale_x_continuous(labels = function(x) format(as.Date(as.character(x), "%j"), "%b"))
  