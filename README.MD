# 🎬 LGBT+ Catalog Analysis by Country and Provider

This R project analyzes catalog size and content popularity from various streaming providers across selected countries, focusing on LGBT+ titles. It includes breakdowns by country, provider type, and region (EU vs Global), as well as cumulative popularity metrics over time.

---

## 📊 What This Project Does

- Calculates **total catalog size** per provider and per country
- Isolates **streaming-only (SVOD)** providers from the full dataset
- Aggregates **popularity metrics** for titles across a one-year window
- Highlights **regional insights**, especially for the EU market

---

## 🔒 Data Confidentiality Notice

**⚠️ No data is included in this repository.**

Due to confidentiality, the CSV data files used in this project are **not publicly shared**.

To replicate the results, you will need to provide your own data files that match the expected structure:

data/
├── catalog_by_country.csv # Catalog data by country and provider
├── popularity_2024_2025.csv # Title popularity over time
└── provider_list.csv # List of SVOD streaming providers

## 📦 Required R Packages

The following packages will be installed automatically when you run the script:

```r
tidyverse
readr
dplyr
stringr
