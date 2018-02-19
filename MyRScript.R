library(tidyverse)
library(lubridate)

### Need to create the desired variables : (Mass recap - Mass initial cap) & Times (converted to Months)
# only time of year matters, not what year


blpw.all <- blpw.all1

subset(blpw.all1, band == 197052092)

# STEP 1: Combine all date information into one column named 'date'
blpw.all <- original.df %>%
  mutate(date = make_date(month, day))

# Create day.month column <- <- 


blpw.all1 %>%
  format(as.Date(date), "%m-%d")

# STEP 2: Remove unnecessary columns
blpw.all <- blpw.all %>%
  select(location, band, mass, recap, year, day, month, date)

# STEP 3: Remove rows with only one mass value per band number (removes birds that were not recaptured and birds that were recaptured but with only one observation present)
blpw.all <- blpw.all %>% 
  group_by(band) %>% 
  filter(n() >= 2)

# STEP 4: Create column of delta mass values
blpw.all <- blpw.all %>%
  group_by(band, year) %>%
  mutate(DeltaMass = c(0, diff(mass)))

# creat graph mapping DeltaMass (grouped by band#) by date? Or is it using intervals of time??
ggplot(data = blpw.all, mapping = aes(x = date, y = DeltaMass, colour = band)) +
  geom_point() +
  geom_line() +
  theme(legend.position = "none") +
  facet_wrap(~ location) +
  ylab("Mass (in grams, relaative to capture date)")

  