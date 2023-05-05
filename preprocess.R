# 
# packdir <- "/project/ARTIS/Package"
# setwd(packdir) # note: If running on zorro need to set directory to packdir before #devtools::install()
# 
# # Build and install artis package
# devtools::install()
# 
# library(tidyverse, lib.loc = "/home/rahulab/R/x86_64-pc-linux-gnu-library/3.6/")
# library(countrycode, lib.loc = "/home/rahulab/R/x86_64-pc-linux-gnu-library/3.6/")

# outdir <- "/project/ARTIS/ARTIS/shiny_app_resources"
outdir <- "artis_shiny_app"
# fp <- "/project/ARTIS/ARTIS/complete_artis/mid_custom_artis_ts.csv"
fp <- "../ARTIS_Data_Requests/data_requests/r_shiny_app/20230404_artis_shiny_ARTIS_snet.csv"
df <- read.csv(fp)

# isscaap <- read.csv(file.path(outdir, "sciname_isscaap_matches.csv"))
isscaap <- read.csv("/Volumes/jgephart/ARTIS/Outputs/clean_metadata/sciname_metadata.csv") %>%
  select(sciname, isscaap_group)

# Remove the HS6 codes from dataframe
threshold <- 10

df <- df %>%
  filter(!is.na(live_weight_t)) %>%
  filter(live_weight_t > threshold) %>%
  left_join(isscaap, by = "sciname") %>%
  # Everything except for HS product codes
  group_by(importer_iso3c, exporter_iso3c, source_country_iso3c, sciname, isscaap_group, habitat, method, dom_source, year, hs_version) %>%
  summarize(product_weight_t = sum(product_weight_t), live_weight_t = sum(live_weight_t)) %>%
  ungroup()

str(df)

# list of importers
importers <- df %>%
  select(importer_iso3c) %>%
  distinct() %>%
  mutate(importer_name=countrycode(importer_iso3c, origin="iso3c", destination="country.name")) %>%
  filter(!is.na(importer_name))

str(importers)

# list of exporters
exporters <- df %>%
  select(exporter_iso3c) %>%
  distinct() %>%
  mutate(exporter_name=countrycode(exporter_iso3c, origin="iso3c", destination="country.name")) %>%
  filter(!is.na(exporter_name))

str(exporters)

# list of source countries
source_countries <- df %>%
  select(source_country_iso3c) %>%
  distinct() %>%
  mutate(source_country_name=countrycode(source_country_iso3c, origin="iso3c", destination="country.name")) %>%
  filter(!is.na(source_country_name))

str(source_countries)

str(df)
# Join main dataframe with exporter and importer full names
df <- df %>%
  left_join(importers, by=c('importer_iso3c')) %>%
  left_join(exporters, by=c('exporter_iso3c')) %>%
  left_join(source_countries, by=c('source_country_iso3c')) %>%
  ungroup() %>%
  select(-c(importer_iso3c, exporter_iso3c, source_country_iso3c)) %>%
  mutate(isscaap_group = as.character(isscaap_group))
  
str(df)

# List of isscaap groups
isscaap_groups <- data.frame(isscaap_groups=as.character(unique(df$isscaap_group)))

# List of species
species <- data.frame(species=as.character(unique(df$sciname)))

# List of years
years <- data.frame(years=unique(df$year)) %>%
  arrange(desc(years))

# Dom Sources
dom_sources <- data.frame(dom_source=as.character(unique(df$dom_source)))

gc()

# Save current image
# save.image(file.path(outdir, 'data.RData'))
save.image(file.path(outdir, "data.RData"))

