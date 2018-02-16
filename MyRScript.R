library(tidyverse)
library(lubridate)

### Need to create the desired variables : (Mass recap - Mass initial cap) & Times (converted to Months)
# only time of year matters, not what year
# need a column for delta mass; first observation, delta mass = 0; second observation, delta mass = mass2 - mass1

blpw.all1 <- blpw.all

subset(blpw.all1, band == 197052092)

# STEP 1: Combine all date information into one column named 'date'
blpw.all <- blpw.all %>%
  mutate(date = make_date(year, month, day))

# STEP 2: Remove unnecessary columns
blpw.all <- blpw.all %>%
  select(location, band, mass, recap, year, day, month, date)

# STEP 3: Remove rows with only one mass value per band number (removes birds that were not recaptured and birds that were recaptured but with only one observation present)
blpw.all <- blpw.all %>% 
  group_by(band) %>% 
  filter(n() >= 2)

# STEP 4: Create column of delta mass values
blpw.all <- blpw.all %>%
  group_by(band) %>%
  mutate(DeltaMass = c(0, diff(mass)))
