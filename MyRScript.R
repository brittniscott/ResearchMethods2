library(tidyverse)
library(lubridate)

# only time of year matters, not what year

blpw.all1 <- blpw.all

subset(blpw.all1, band == 197052092)

### MAKE A DAY OF YEAR COLUMN AND RELABEL IT WITH MONTHS IN THE GRAPH

# STEP 1: Combine all date information into one column named 'date'
blpw.all <- original.df %>%
  mutate(date = make_date(month, day))

# STEP 2: Create 'julian' day column 
blpw.all$julian <- yday(blpw.all$date)

# STEP 3: Remove unnecessary columns
blpw.all <- blpw.all %>%
  select(location, band, mass, recap, year, day, month, date)

# STEP 4: Remove rows with only one mass value per band number (removes birds that were not recaptured and birds that were recaptured but with only one observation present)
blpw.all <- blpw.all %>% 
  group_by(band) %>% 
  filter(n() >= 2)

# STEP 5: Create column of delta mass values
blpw.all <- blpw.all %>%
  group_by(band) %>%
  mutate(deltamass = c(0, diff(mass)))

# STEP 6: Create graph mapping DeltaMass (grouped by band#) by date? Or is it using intervals of time??
ggplot(data = blpw.all, mapping = aes(x = julian, y = DeltaMass, colour = band)) +
  geom_point() +
  geom_line() +
  theme(legend.position = "none") +
  facet_wrap(~ location) +
  ylab("Mass (in grams, relative to capture date)") +
  xlab("Time of Year") +
  scale_x_continuous(labels = function(x) format(as.Date(as.character(x), "%j"), "%b"))
  

  