# Installing necessary packages

install.packages("tidyverse")
install.packages("readr")
install.packages("dplyr")
install.packages("stringr")

# Loading packages

library(tidyverse)
library(readr)
library(dplyr)
library(stringr)

# Compiling csv data from a hex report - Data excluded for confidentiality reasons

catalog_per_provider<- read.csv("catalog_by_country.csv") # Data for Catalog Size
june_to_june <- read.csv("popularity_2024_2025.csv") # Title Popularity for June 1, 2024 - June 1, 2025

# Adding lists for filtering by country/streaming list
country_list <- c("US","CA", "BR","DE","ES","FR","GB","IT", "MX") #List of countries the campaign is going out for
eu_list <- c("DE", "ES","FR","GB","IT") # List of countries included for the campaign but only in EU region
streaming_list <- read.csv("provider_list.csv") # List of providers that are deemed as SVOD / Subscription Video on Demand 


# Filtering by country list to get total catalog size per country for specific countries
size_per_country <- catalog_per_provider %>%  # Results in Total LGBT+ Catalog Size per Provider regardless of type of provider
  filter(COUNTRY_CODE %in% country_list) %>% 
  group_by(COUNTRY_CODE, PACKAGE_CLEAR_NAME) %>% 
  summarize(overallCount = n(), .groups="drop")

# Filtering by country list and streaming provider list THEN counting total titles per provider per country
streaming_size_per_country <- catalog_per_provider %>%# Results in Total LGBT+ Catalog size per streaming provider
  filter(COUNTRY_CODE %in% country_list) %>% 
  filter(PACKAGE_CLEAR_NAME %in% streaming_list$providers) %>% 
  group_by(COUNTRY_CODE, PACKAGE_CLEAR_NAME) %>% 
  summarize(streamingCount = n(), .groups="drop")

# Grouping by Country, Title Name and Title ID THEN Summing up total title popularity
total_top <- june_to_june %>% # Results in Total 1 day popularity per title accummulated over the period of June 1, 2024 - June 1, 2025
  group_by(COUNTRY, TITLE_NAME, TITLE_ID) %>% 
  summarize(total_popularity = sum(POPULAR_1_DAY), .groups="drop")

# Filters by Country and Streaming List / Deduplicates all titles by title ID / Counts total unique titles per country
total_size_per_country <- catalog_per_provider %>% # Results in Total Streaming Catalog Size per Country
  filter(COUNTRY_CODE %in% country_list) %>% 
  filter(PACKAGE_CLEAR_NAME %in% streaming_list$providers) %>% 
  distinct(TITLE_ID, COUNTRY_CODE) %>% 
  group_by(COUNTRY_CODE) %>% 
  summarize(total_catalog_size = n(), .groups="drop")

# Filters by Country / Deduplicates all titles by title ID / Counts total unique titles per country
all_size_per_country <- catalog_per_provider %>% # Results in All Catalog Size per Country
  filter(COUNTRY_CODE %in% country_list) %>% 
  distinct(TITLE_ID, COUNTRY_CODE) %>% 
  group_by(COUNTRY_CODE) %>% 
  summarize(total_catalog_size = n(), .groups="drop")

# Filters by Streaming List / Deduplicates by Title ID
total_streaming_catalog <- catalog_per_provider %>%  # Results in Streaming Catalog with all Unique Title IDs
  filter(PACKAGE_CLEAR_NAME %in% streaming_list$providers) %>% 
  distinct(TITLE_ID) 

# Filters by EU country list, deduplicates by Title ID and Provider Name, Filters by Streaming list THEN counts total unique titles per provider  
eu_catalog <- catalog_per_provider %>%  # Results in EU Streaming Catalog Size per Provider
  filter(COUNTRY_CODE %in% eu_list) %>%
  distinct(TITLE_ID, PACKAGE_CLEAR_NAME) %>% 
  filter(PACKAGE_CLEAR_NAME %in% streaming_list$providers) %>% 
  group_by(PACKAGE_CLEAR_NAME) %>% 
  summarize(streamingCount = n(), .groups="drop")

# Filters by EU list, Streaming list, deduplicates by Title ID, counts total titles in the countries.
eu_catalog_size <- catalog_per_provider %>% # Results in the Total number of LGBT titles in EU
  filter(COUNTRY_CODE %in% eu_list) %>%
  filter(PACKAGE_CLEAR_NAME %in% streaming_list$providers) %>% 
  distinct(TITLE_ID) %>% 
  summarize(streamingCount = n(), .groups="drop")

# Writes Data into CSVs for Uploading into GSheets for final review
# write.csv(size_per_country, "RESULTS/size_per_provider.csv")  
# write.csv(streaming_size_per_country, "RESULTS/streaming_size_per_provider.csv")  
# write.csv(total_top,"RESULTS/total_top.csv")
# write.csv(total_streaming_catalog,"RESULTS/total_streaming_catalog_count.csv")
# write.csv(eu_catalog,"RESULTS/eu_catalog_count.csv")
