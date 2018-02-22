library(tidyverse)
library(lubridate)

## Nice work ... Phil.

blpw.all <- readRDS("blpw.all.RDS")

# STEP 1: Remove rows with only one mass value per band number (removes birds that were not recaptured and birds that were recaptured but with only one observation present)
blpw.all <- blpw.all %>% 
  group_by(band) %>% 
  filter(n() >= 2)

# STEP 2: Combine all date information into one column named 'date'
blpw.all <- blpw.all %>%
  mutate(date = make_date(year, month, day))

# STEP 3: Create column of cumulative delta mass values
blpw.all <- blpw.all %>%
  group_by(band, year) %>%
  mutate(deltamass = c(0, diff(mass))) %>%
  mutate(cumdelta = cumsum(deltamass))

# STEP 4: Tidy by removing some unnecessary columns
blpw.all <- blpw.all %>%
  select(location, band, year, date, cumdelta)

# STEP 5: Create graph mapping deltamass by date
ggplot(data = blpw.all, mapping = aes(x = (yday(date)), y = cumdelta, colour = band)) +
  geom_point() +
  geom_line() +
  theme(legend.position = "none") +
  facet_wrap(~ location) +
  ylab("Mass (in grams, relative to capture date)") +
  xlab("Time of Year") +
  scale_x_continuous(breaks = c(213, 227, 244, 258, 274, 288), 
                     labels = function(x) format(as.Date(as.character(x), "%j"), "%d-%b")) + 
  theme(axis.text.x=element_text(angle=90,hjust=1))

  