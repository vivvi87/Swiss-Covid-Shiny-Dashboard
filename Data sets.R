# Load Covid data for Switzerland from GitHub repository
data_swiss <- read_csv("https://raw.githubusercontent.com/openZH/covid_19/master/COVID19_Fallzahlen_CH_total_v2.csv")
# Load Canton population data from excell csv file made from Wikipedia data
canton_swiss <- read_csv("https://raw.githubusercontent.com/vivvi87/dataset-swiss-cantons/main/swiss_cantons.csv")
canton_swiss <- canton_swiss %>%
  select(-X4, -X5, -X6, -X7) %>%
  mutate(Canton_abbr = X3) %>%
  select(-X3)
# Load Switzerland spatial data (canton polygons)
switzerland <- ne_states(country = 'switzerland', returnclass = 'sf')
switzerland <- st_as_sf(switzerland)

# Join data frames
data_swiss = left_join(data_swiss, canton_swiss, by = c(abbreviation_canton_and_fl = "Canton_abbr"))


# Modify dataframe by adding more variables
#Add new cases/day
data_swiss <- data_swiss %>% group_by(abbreviation_canton_and_fl) %>%
  mutate(new_cases = ncumul_conf - lag(ncumul_conf, default = first(ncumul_conf), order_by = date))
#Add new deaths/day
data_swiss <- data_swiss %>% group_by(abbreviation_canton_and_fl) %>%
  mutate(new_deaths = ncumul_deceased - lag(ncumul_deceased, default = first(ncumul_deceased), order_by = date))
# Add new cases/day by pop (every 10000 people)
data_swiss <- data_swiss %>%
  mutate(pop_10thous = Pop/10000)

data_swiss <- data_swiss %>%
  mutate(new_cases_per_10thous = new_cases/pop_10thous)
# Add new deaths/day by pop
data_swiss <- data_swiss %>%
  mutate(new_deaths_per_10thous = new_deaths/pop_10thous)
# Add new cases 7 days rolling mean
data_swiss <- data_swiss %>%
  mutate(new_cases_smoothed = zoo::rollmean(new_cases, k = 7, fill = NA))
# Add new deaths 7 days rolling mean
data_swiss <- data_swiss %>%
  mutate(new_deaths_smoothed = zoo::rollmean(new_deaths, k = 7, fill = NA))

# Add total deaths by pop
data_swiss <- data_swiss%>%
  mutate(ncumul_deceased_per_10thous = ncumul_deceased/pop_10thous)
# Add total death/population
data_swiss <- data_swiss%>%
  mutate(ncumul_conf_per_10thous = ncumul_conf/pop_10thous)

# Merge with geo data
data_swiss_geo <- left_join(switzerland, data_swiss, by = c(postal = "abbreviation_canton_and_fl"))


# =============================================================================================
# Create new data frame with Switzerland totals
# Create dataframe for totals (Dashboard page 2)

data_swiss_noNA <- data_swiss %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))

# Calculate new cases
switzerland_new_cases <- data_swiss_noNA %>%
  group_by(date) %>%
  summarize(switzerland_new_cases = sum(new_cases, na.rm = TRUE))

# Calculate new cases 7 days rolling mean
switzerland_new_cases_smoothed <- data_swiss_noNA %>%
  group_by(date) %>%
  summarize(switzerland_new_cases_smoothed = sum(new_cases_smoothed, na.rm = TRUE)) %>%
  select(-date)

# Calculate new deaths
switzerland_new_deaths <- data_swiss_noNA %>%
  group_by(date) %>%
  summarize(switzerland_new_deaths = sum(new_deaths, na.rm = TRUE))%>%
  select(-date)

# Calculate new deaths rolling mean
switzerland_new_deaths_smoothed <- data_swiss_noNA %>%
  group_by(date) %>%
  summarize(switzerland_new_deaths_smoothed = sum(new_deaths_smoothed, na.rm = TRUE)) %>%
  select(-date)

# Put together in a data frame
data_total_swiss <- cbind(switzerland_new_cases, switzerland_new_cases_smoothed, switzerland_new_deaths, switzerland_new_deaths_smoothed)


# =========================================================================================
# Create new data frame for trend map (Page 3)
# Calculate total last 14 days
tot14days_last <- data_swiss %>%
  group_by(abbreviation_canton_and_fl) %>%
  filter(date <= max(date), date >= max(date)-14) %>%
  summarize(tot14days_last = sum(new_cases, na.rm = TRUE))

# Calculate total previous 14 days
tot14days_previous <- data_swiss %>%
  group_by(abbreviation_canton_and_fl) %>%
  filter(date <= max(date)-15, date >= max(date)-29) %>%
  summarize(tot14days_previous = sum(new_cases, na.rm = TRUE)) %>%
  select(-abbreviation_canton_and_fl)

# Put together in a data frame
trend <- cbind(tot14days_last, tot14days_previous)
# Add variation %
trend <- trend %>%
  mutate(change_percemt = round((tot14days_last-tot14days_previous)/tot14days_last*100, 0))

# Add geographic info for trend map
trend_swiss_geo <- left_join(switzerland, trend, by = c(postal = "abbreviation_canton_and_fl"))

# Add canton names to trend
trend <- left_join(canton_swiss, trend, by = c(Canton_abbr = "abbreviation_canton_and_fl"))



