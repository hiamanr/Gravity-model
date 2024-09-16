### Gravity Model --------------------------------------------------------------
## Packages---------------------------------------------------------------------

#install.packages("tidyverse")
library(tidyverse)

#install.packages("dplyr")
library(dplyr)

## Setting work directory-------------------------------------------------------
## Setting work directory-------------------------------------------------------

file_directory <- dirname(rstudioapi::getSourceEditorContext()$path)%>%
  gsub("\\Analysis", "", .)

setwd(file_directory)

getwd()

# Setting folder path to database-----------------------------------------------

folder_path <- "Database\\"


# Loading database file---------------------------------------------------------

database_file <- paste0(folder_path, "Gravity_V202211.rds")

# Creating database object------------------------------------------------------

data <- readRDS(database_file)


## Filtering data---------------------------------------------------------------
filtered_data <- Gravity_V202211 %>%
  filter(country_id_o == "GBR")


## Dropping irrelevant columns


## Transforming in log relevant variables


## Corrections

## Estimating regression


## Other

# Correlation distance and trade (all data)