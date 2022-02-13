## --------------------------------
##
## Script name: consent_to_SQL_server.R
##
## Purpose of script: migrate consent and demographics data to SQL server
##
## Author: Shanaa Modchalingam
##
## Date created: 2022-12-02
##
## Email: s.modcha@gmail.com
##
## --------------------------------
##
##
## --------------------------------

rm(list = ls())      # clean environment
library(data.table)
library(tidyverse)

# load data 

consent_df <- read_delim("data/consent_demographics/VR_exp_consent_12022022.csv", 
                         delim = ",") 

# filter out test cases and unfinished surveys
consent_df <- consent_df %>%
  filter(Status == "IP Address", Finished == "True") %>%
  select(-Status, -IPAddress, -StartDate, -EndDate,
         -LocationLatitude, -LocationLongitude, -DistributionChannel,
         -UserLanguage, -`Create New Field or Choose From Dropdown...`, 
         -`Duration (in seconds)`, -starts_with("Recipient"), 
         -ExternalReference)

fwrite(consent_df, file = "data/consent_demographics/VR_exp_consent_12022022_cleaned.csv")
