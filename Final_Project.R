# Libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(patchwork)

# All data is on New York State
evChargingStations <- read.csv("/cloud/project/ENVST325_Final_Project/Electric_Vehicle_Charging_Stations_Copy.csv")
evRegistrations <- read.csv("/cloud/project/ENVST325_Final_Project/ny_ev_registrations_Copy.csv")
nyGreenHouseGases <- read.csv("/cloud/project/ENVST325_Final_Project/Statewide_Greenhouse_Gas_Emissions__Beginning_1990_Copy.csv")

str(evChargingStations)
str(evRegistrations)
str(nyGreenHouseGases)

# Change data types
evChargingStations$ZIP <- as.factor(evChargingStations$ZIP)
evRegistrations$ZIP.Code <- as.factor(evRegistrations$ZIP.Code)
nyGreenHouseGases[, c(1:9, 11)] <- data.frame(lapply(nyGreenHouseGases[, c(1:9, 11)], 
                                                     as.factor))
evRegistrations$Registration.Valid.Date <- mdy(evRegistrations$Registration.Valid.Date)

# Add year column to EV Registrations dataframe
evRegistrations$Year <- year(evRegistrations$Registration.Valid.Date)

# Count number of EV registrations by ZIP Code
evRegistrations <- evRegistrations %>% add_count(ZIP.Code, 
                                                 name = "numRegistrationsByZip")
evRegistrations <- evRegistrations %>% add_count(Year, 
                                                 name = "numRegistrationsByYear")
evRegistrations <- unique(evRegistrations)

View(evChargingStations)
View(evRegistrations)
View(nyGreenHouseGases)

nyGreenHouseGases$Year <- ymd(nyGreenHouseGases$Year, truncated = 2L)
evRegistrations$Year <- ymd(evRegistrations$Year, truncated = 2L)

nyGreenHouseGasesSubset <- nyGreenHouseGases %>%
  select(Category, Year, Gas, MT.CO2e.AR5.20.yr, MT.CO2e.AR4.100.yr)

evRegistrationsSubset <- evRegistrations %>%
  select(Year, numRegistrationsByYear)
nyGreenHouseGasesSubset <- unique(nyGreenHouseGasesSubset)
evRegistrationsSubset <- unique(evRegistrationsSubset)
evRegistrationsSubset <- evRegistrationsSubset[order(evRegistrationsSubset$Year), ]
mergedDf <- full_join(nyGreenHouseGasesSubset, evRegistrationsSubset, by = "Year")

statsDf <- mergedDf %>%
  select(Year, MT.CO2e.AR5.20.yr, MT.CO2e.AR4.100.yr, numRegistrationsByYear)
statsDf <- unique(statsDf)

x <- statsDf %>%
  group_by(Year) %>%
  mutate(MTC02ByYear = sum(MT.CO2e.AR5.20.yr)) %>%
  select(Year, numRegistrationsByYear, MTC02ByYear)

x <- unique(x)




p1 <- ggplot(data = x, aes(x = Year, numRegistrationsByYear)) +
  geom_line() +
  theme_bw() +
  ylab("EV Registrations") +
  labs(title = "Number of EV Registrations in New York (2011 - 2022)")

p1

p2 <-ggplot(data = x, aes(x = Year, MTC02ByYear)) +
  geom_line() +
  theme_bw() + 
  ylab("Metric Tons of CO2") +
  labs(title = "Metric Tons of CO2 Emitted in New York Yearly (1990 - 2019)")

p2

p3 <- ggplot(data = evRegistrationsSubset, aes(x = Year, y = cumsum(numRegistrationsByYear))) +
  geom_line() +
  theme_bw() +
  labs(title = "Cumulative Number of EV Registrations in New York by Year (2011 - 2022)") +
  ylab("Cumulative EV Registrations")

p3


cor(x$numRegistrationsByYear, x$MTC02ByYear, use = "complete.obs")
