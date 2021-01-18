library(tidyverse)
library(readxl)
library(here)
library(countrycode)
library(magrittr)
library(WDI)

# Read in data
techcredit <- readxl::read_excel(here("data", "work887_data.xlsx"), sheet = 2, skip = 2)

# Basic cleaning
names(techcredit) %<>%
  tolower()  %>% 
  gsub("\\(.*", "", .) %>%
  str_trim() %>% 
  gsub(" ", "_", .)

techcredit %<>% mutate(iso3c = countrycode(country_name, "country.name", "iso3c"))

# GDP per capita (current USD)
gdppc <- WDI::WDI(country = "all", "NY.GDP.PCAP.CD", start = 2013, end = 2019)

# Mobile phone subscriptions per 100 people
phonesub <- WDI::WDI(country = "all", "NY.GDP.PCAP.CD", start = 2013, end = 2019)

# Lerner Index


# An index of regulatory stringency for the banking sector of economy i, as constructed by Barba Navaretti et al. (2017) fromWorld Bank data

# Density of the bank branch network per 100000 people
bankdensity <- WDI::WDI(country = "all", "FB.CBK.BRCH.P5", start = 2013, end = 2019)

# Growth in GDP
# Total credit 
# Real short term interest rate
# A dummy for whether a country had suffered a financial crisis since 2006, as defined by Laeven and Valencia (2018)
# Dummy for advanced economies

# Export
write_csv(techcredit, here("proc", "techcredit.csv"))

