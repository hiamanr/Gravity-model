### Gravity Model --------------------------------------------------------------
## Packages---------------------------------------------------------------------

#install.packages("tidyverse")
library(tidyverse)

#install.packages("dplyr")
library(dplyr)

#install.packages("ggplot2")
library(ggplot2)

#install.packages("ggrepel")
library(ggrepel)

#install.packages("lmtest")
library(lmtest)

## Setting work directory-------------------------------------------------------

file_directory <- dirname(rstudioapi::getSourceEditorContext()$path)%>%
  gsub("\\Analysis", "", .)

setwd(file_directory)

getwd()

## Setting folder path to database----------------------------------------------

folder_path <- "Database\\"

## Loading database file--------------------------------------------------------

database_file <- paste0(folder_path, "Gravity_V202211.rds")

## Creating database object-----------------------------------------------------

data <- readRDS(database_file)

## Treating main data base------------------------------------------------------

## Creating log variables-------------------------------------------------------

#Origin-------------------------------------------------------------------------

data <- data %>%
  mutate(lnflow_o = log(tradeflow_comtrade_o))

# Destination-------------------------------------------------------------------

data <- data %>%
  mutate(lnflow_d = log(tradeflow_comtrade_d))

# Distance (harmonic)-----------------------------------------------------------

data <- data %>%
  mutate(lndist = log(distw_harmonic))

# Creating log variable for GDP_o-----------------------------------------------

data <- data %>%
  mutate(lngdp_o = log(gdp_o))

# Creating log variable for GDP_d-----------------------------------------------

data <- data %>%
  mutate(lngdp_d = log(gdp_d))

## Creating dummy variables-----------------------------------------------------

# Commonwealth membership-------------------------------------------------------

# Creating list of member countries and respective year of accession------------

commonwealth_info <- data.frame(
  country_id = c("ATG", "AUS", "BHS", "BGD", "BRB", "BLZ", "BWA", "BRN", "CMR",
                 "CAN", "CYP", "DMA", "SWZ", "FJI", "GAB", "GMB", "GHA", "GRD",
                 "GUY", "IND", "JAM", "KEN", "KIR", "LSO", "MWI", "MYS", "MDV",
                 "MLT", "MUS", "MOZ", "NAM", "NRU", "NZL", "NGA", "PAK", "PNG", 
                 "RWA", "KNA", "LCA", "VCT", "WSM", "SYC", "SLE", "SGP", "SLB",
                 "ZAF", "LKA", "TZA", "TGO", "TON", "TTO", "TUV", "UGA", "GBR",
                 "VUT", "ZMB"),
  year_joined = c(1981, 1926, 1973, 1972, 1966, 1981, 1966, 1984, 1995, 1926, 
                  1961, 1978, 1968, 1970, 2022, 1965, 1957, 1974, 1966, 1947,
                  1962, 1963, 1979, 1966, 1964, 1957, 1982, 1964, 1968, 1995, 
                  1990, 1968, 1926, 1960, 1947, 1975, 2009, 1983, 1979, 1979,
                  1970, 1976, 1961, 1965, 1978, 1926, 1948, 1961, 2022, 1970,
                  1962, 1978, 1962, 1926, 1980, 1964)
)

# Merging with main dataframe---------------------------------------------------

data <- data %>%
  left_join(commonwealth_info, by = c("country_id_o" = "country_id")) %>%
  rename(year_joined_o = year_joined) %>%
  left_join(commonwealth_info, by = c("country_id_d" = "country_id")) %>%
  rename(year_joined_d = year_joined)

# Creating dummy variable for origin and destination----------------------------

data <- data %>%
  mutate(
    is_commonwealth_o = ifelse(!is.na(year_joined_o) & year >= year_joined_o, 1, 0),
    is_commonwealth_d = ifelse(!is.na(year_joined_d) & year >= year_joined_d, 1, 0)
  )

# Commonwealth membership-------------------------------------------------------

data <- data %>%
  mutate(both_commonwealth = ifelse(is_commonwealth_o == 1 & is_commonwealth_d == 1, 1, 0))

## First step-------------------------------------------------------------------
## Filtering data---------------------------------------------------------------

filtered_data <- data %>%
  filter(country_id_o == "GBR" | country_id_d == "GBR")

## Correlation (UK trade flows)-------------------------------------------------

# Setting year of reference for initial analysis--------------------------------

reference_year <- 2019

# Creating new dataframe, filtering for "GBR" as origin-------------------------

gbr_o <- filtered_data %>%
  filter(country_id_o == "GBR", year == reference_year)

# Plotting correlation graph----------------------------------------------------

ggplot(gbr_o, aes(x = lndist, y = lnflow_o)) +
  geom_point() +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add a linear regression line
  geom_text_repel(aes(label = country_id_d), size = 3) +  # Add labels using country_id_d (destination country)
  labs(title = paste("Tradeflow vs Distance (GBR as Origin, Year:", reference_year, ")"),
       x = "Log Distance",
       y = "Log Tradeflow") +
  theme_minimal()

# Negative relationship as per expected. 

# Indicating commonwealth membership--------------------------------------------





# Creating new dataframe, filtering for "GBR" as destination--------------------

# Filter for "GBR" as destination
gbr_d <- filtered_data %>%
  filter(country_id_d == "GBR", year == reference_year)

# Plot the graph
ggplot(gbr_d, aes(x = lndist, y = lnflow_d)) +
  geom_point() +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add a linear regression line
  geom_text_repel(aes(label = country_id_o), size = 3) +  # Add labels using country_id_o (origin country)
  labs(title = paste("Tradeflow vs Distance (GBR as Destination, Year:", reference_year, ")"),
       x = "Log Distance",
       y = "Log Tradeflow") +
  theme_minimal()

# Again, negative relationship as per expected. 

# Highlight Commonwealth membership---------------------------------------------





## Estimation-------------------------------------------------------------------

## Running model----------------------------------------------------------------


model <- lm(lnflow_o ~ lngdp_o + lngdp_d + lndist + both_commonwealth, 
                   data = data)

summary(model)

# Breusch-Pagan test------------------------------------------------------------


bptest(model)

# Indicates there is heteroskedasticity

# Plotting residuals------------------------------------------------------------

plot(model, which = 1)  # Residuals vs Fitted

# Controlling for heteroskedasticity--------------------------------------------

library(sandwich)
library(lmtest)

# Fit the model
model <- lm(lnflow_o ~ lngdp_o + lngdp_d + lndist + both_commonwealth, 
            data = data)

# Summary with robust standard errors
coeftest(model, vcov = vcovHC(model, type = "HC1"))

summary(model)
