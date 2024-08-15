
# List of required packages
required_packages <- c("tidyverse", "readxl", "sf")

# Function to check and install missing packages
install_missing_packages <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages, dependencies = TRUE)
}

# Install any missing packages
install_missing_packages(required_packages)

# Load the packages
lapply(required_packages, require, character.only = TRUE)


# Read Data ---------------------------------------------------------------

pm_per_station_original <- read_excel("pm_per_station_original.xlsx")
district_data_original <- read.csv("district_data_original.csv")
mashhad_shape <- st_read("mashhad_shape")

# Preprocessing pm_per_station_original -----------------------------------

# adapting col names
dates <- seq(42736, 44196, by = 1)
dates <- as.Date(dates, origin = "1899-12-30")

names(pm_per_station_original) <- 
  c("stationID", "Xcoor", "Ycoor", as.character(dates))

# long format
pm_per_station_original <- pm_per_station_original %>% pivot_longer(
  cols = starts_with("20"), # date columns
  names_to = "date",        
  values_to = "PM2.5")

# save final data
write.csv(pm_per_station_original, "pm_per_station.csv", row.names = FALSE)

# Preprocessing district_data_original ------------------------------------

# adapting column names
district_data_original <- district_data_original %>%
  rename(districtID = DistrictId, date = Date, percent_age65 = Percent_age.65,
         percent_unemployment = percent_unempolyment,
         CVD_below_age65 = CVD.agebellow65, CVD_above_age65 = CVD.ageup65)

# adapting class
district_data_original <- district_data_original %>%
  mutate(districtID = as.factor(districtID)) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
  mutate(percent_age65 = as.numeric(percent_age65)) %>%
  mutate(DOW = as.factor(DOW)) %>%
  mutate(holiday = as.factor(holiday)) %>%
  mutate(covid = as.factor(covid)) %>%
  mutate(screening = as.factor(screening)) %>%
  mutate(percent_illiterate = as.numeric(percent_illiterate)) %>%
  mutate(percent_unemployment = as.numeric(percent_unemployment))

# new variables
## population density
population_dens_df <- as.data.frame(mashhad_shape) %>%
  select(area, number_zon) %>% # area in m^2
  rename(districtID = number_zon) %>%
  mutate(area_km = area / (1000 * 1000)) %>% # area in km^2
  select(-area) %>%
  mutate(districtID = as.factor(districtID))

district_data_original <- 
  right_join(district_data_original, population_dens_df, by = "districtID") %>%
  mutate(population_density = population / area_km)

## year and month
district_data_original <- district_data_original %>%
  mutate(year = as.numeric(format(date, "%Y"))) %>%
  mutate(month = as.factor(format(date, "%m")))

# save final data
write.csv(district_data_original, "district_data.csv", row.names = FALSE)

