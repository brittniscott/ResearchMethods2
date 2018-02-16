library(tidyverse)
library(lubridate)

### Need to create the desired variables : (Mass recap - Mass initial cap) & Times (converted to Months)
# need to get rid of birds that were not recaptured
# only time of year matters, not what year
# only include data that has repeated tags??

blpw.all1 <- blpw.all

# STEP 1: Combine all date information into one column named 'date'
blpw.all <- blpw.all %>%
  mutate(date = make_date(year, month, day))

# STEP 2: Remove unnecessary columns
blpw.all <- blpw.all %>%
  select(location, band, mass, recap, year, day, month, date)

subset(blpw.all1, band == 197052092)
