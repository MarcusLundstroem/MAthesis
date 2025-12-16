#####################################################
#####The False Promise of Political Culture code#####
#####################################################

# Load libraries and data sources
library(tidyverse)
library(plm)
library(fixest)
library(marginaleffects)
library(sandwich)
library(car)
library(devtools)
library(countrycode)
library(ggthemes)
library(patchwork)
library(readr)
library(readxl)
library(modelsummary)
library(kableExtra)

Support_democracy_ajps_correct <- read_csv("C:/Users/Marcu/OneDrive/Skrivbord/Masteruppsats/Data/Claassen (dataverse)/Support_democracy_ajps_correct.csv")
NAVCO_1_3 <- read_excel("C:/Users/Marcu/OneDrive/Skrivbord/Masteruppsats/Data/NAVCO/dataverse_files/NAVCO 1.3 List.xlsx")
demscore_data <- readRDS("C:/Users/Marcu/OneDrive/Skrivbord/Masteruppsats/Data/demscore_data2/data.rds")
autocratization_dataset <- read_csv("C:/Users/Marcu/OneDrive/Skrivbord/Masteruppsats/Data/Autocratization data/autocratization_dataset.csv")
ert <- read_csv("C:/Users/Marcu/OneDrive/Skrivbord/Masteruppsats/Data/Autocratization data/ert.csv")

# Save the macro panel period (1980-2017) as objects + helper (fix_keys) to ease the joins and data manipulation
YEAR_MIN <- 1980L
YEAR_MAX <- 2017L

fix_keys <- \(df) df %>%
  mutate(
    ISO3 = as.character(ISO3),
    Year = as.integer(Year)) %>%
  filter(!is.na(ISO3), !is.na(Year), Year >= YEAR_MIN, Year <= YEAR_MAX)

####################################################################
###Restructuring NAVCO and preparing for country–year aggregation###
####################################################################

# NAVCO: ISO3 mapping with the countrycode() function
NAVCO <- NAVCO_1_3 %>%
  rename(Country = LOCATION) %>%
  mutate(ISO3 = countrycode(Country, origin = "country.name", destination = "iso3c"))

# Manual patches for unmatched names in the NAVCO data
manual_map <- c(
  "Northern Ireland" = "GBR",
  "Tibet" = "CHN",
  "Yemen Arab Republic" = "YEM",
  "Yemen People's Republic" = "YEM")

needs_patch <- is.na(NAVCO$ISO3) & NAVCO$Country %in% names(manual_map) #to identify which rows need manual fixing
NAVCO$ISO3[needs_patch] <- manual_map[NAVCO$Country[needs_patch]] # apply the manual patches to those rows

# Standard panel data expansion with the variables BYEAR–EYEAR to country–year rows
NAVCO_cy <- NAVCO %>%
  mutate(BYEAR = as.integer(BYEAR),
         EYEAR = as.integer(EYEAR)) %>%
  rowwise() %>%
  mutate(Year = list(seq(BYEAR, EYEAR))) %>%
  unnest(Year) %>%
  ungroup() %>%
  mutate(is_peak = as.integer(Year == EYEAR))

# Create the continuous + binary country-year measures of the variables
NAVCO_country_year <- NAVCO_cy %>%
  group_by(ISO3, Year) %>%
  summarise(
    num_campaigns = n_distinct(`NAVCOID`),
    num_nonviolent = sum(NONVIOL, na.rm = TRUE),
    num_violent = sum(VIOL, na.rm = TRUE),
    num_peak_campaigns = sum(is_peak, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(
    any_campaign = as.integer(num_campaigns > 0),
    any_nonviolent = as.integer(num_nonviolent > 0),
    any_violent = as.integer(num_violent > 0),
    any_peak_campaigns = as.integer(num_peak_campaigns > 0)) %>%
  fix_keys()

# Tidy workspace
remove(NAVCO_1_3, NAVCO, NAVCO_cy, needs_patch, manual_map)

#######################################
###ISO3 fixes for Claassen for joins###
#######################################

# Claassen: simple rename ensuring ISO3 is present
Support_democracy_ajps_correct <- Support_democracy_ajps_correct %>%
  rename(ISO3 = ISO_code)

cc_patches <- c(
  "Czech Republic" = "CZE",
  "Ivory Coast" = "CIV",
  "Cape Verde" = "CPV",
  "Macedonia" = "MKD",
  "Swaziland" = "SWZ",
  "Russia" = "RUS",
  "South Korea" = "KOR",
  "United States of America" = "USA",
  "United Kingdom" = "GBR",
  "Venezuela" = "VEN",
  "Bolivia" = "BOL",
  "Laos" = "LAO",
  "Syria" = "SYR")

Support_democracy_ajps_correct <- Support_democracy_ajps_correct %>%
  mutate(
    ISO3_from_name = countrycode(Country, origin = "country.name", destination = "iso3c"),
    ISO3_from_name = ifelse(is.na(ISO3_from_name) & Country %in% names(cc_patches), 
                            cc_patches[Country], ISO3_from_name), # if ISO3 is missing and the country name has a manual patch, use the patch value, otherwise keep the original ISO3
    ISO3 = coalesce(ISO3, ISO3_from_name), # if ISO3 already exists, use that, otherwise use ISO3_from_name value
    Year = as.integer(Year)) %>%
  select(-ISO3_from_name) %>%
  fix_keys()

############################################################################
###Demscore (V-Dem, QoG): standardize IDs, clean, and build backbone data###
############################################################################

# Helper function to convert any country indicator like (names, ISO2, COW code, ISO3) to ISO3 + suppress warnings to reduce cluttered console output
cc <- function(x, origin) suppressWarnings(countrycode(x, origin = origin, destination = "iso3c"))

demscore_std <- demscore_data %>%
  mutate(
    Year = as.integer(u_vdem_country_year_year),
    text_id = toupper(u_vdem_country_year_country_text_id),
    ISO3 = coalesce(
      cc(text_id, "iso3c"),
      cc(text_id, "cowc"),
      cc(u_vdem_country_year_cowcode, "cown"),
      cc(u_vdem_country_year_country, "country.name")),
    ISO3 = case_when(
      text_id == "DDR" ~ NA_character_,   # drop East Germany
      text_id == "YMD" ~ NA_character_,   # drop South Yemen
      text_id %in% c("XKX","PSG","SML","ZZB") ~ NA_character_, # drop other units
      TRUE ~ ISO3)) %>%
  select(ISO3, Year, everything())

# Convert special Demscore missing codes to NA
demscore_std <- demscore_std %>%
  mutate(across(where(is.numeric), ~ na_if(na_if(.x, -11111), -22222)))

# Keep tidy set, drop aux columns (but keep polyarchy CI for now)
all_codelow  <- grep("_codelow$",  names(demscore_std), value = TRUE)
all_codehigh <- grep("_codehigh$", names(demscore_std), value = TRUE)
keep_ci <- c("v2x_polyarchy_codelow", "v2x_polyarchy_codehigh")
drop_ci <- setdiff(c(all_codelow, all_codehigh), keep_ci)

demscore_clean <- demscore_std %>%
  rename(Country = u_vdem_country_year_country) %>%
  select(
    ISO3, Year, everything(),
    -all_of(drop_ci),
    -matches("(_osp$|_ord$|_sd$|_mean$|_nr$)"),
    -text_id,
    -u_vdem_country_year_country_text_id,
    -u_vdem_country_year_country_id,
    -u_vdem_country_year_cowcode,
    -u_vdem_country_year_year) %>%
  select(-Country) %>%
  fix_keys()

# Construct a backbone dataset from the Demscore data
backbone <- demscore_clean %>%
  distinct(ISO3, Year) %>%
  arrange(ISO3, Year)

######################
###Joining the data###
######################

# Select Claassen + NAVCO columns of interest
claassen_small <- Support_democracy_ajps_correct %>%
  select(
    ISO3, Year, Country, Cnt_code,
    Region_UN, Region_VD,
    Res_cp_WDI_di,
    Satis_trim, Satis_m1,
    ChgSup, SupDem_trim, SupDem_m1) %>%
  fix_keys()

navco_small <- NAVCO_country_year %>%
  select(ISO3, Year,
         num_campaigns, num_nonviolent, num_violent,
         any_campaign, any_nonviolent, any_violent) %>%
  fix_keys()

# Assemble panel with backbone data as the, you guessed it, backbone
merged_panel <- backbone %>%
  left_join(navco_small, by = c("ISO3","Year")) %>%
  left_join(claassen_small, by = c("ISO3","Year")) %>%
  left_join(demscore_clean, by = c("ISO3","Year")) %>%
  mutate(
    across(starts_with("num_"), ~replace_na(., 0L)), # ensuring that the created NAVCO measures with 0 aren't returned as NA, because 0 indicates 0 campaigns, not a missing value
    across(starts_with("any_"), ~replace_na(., 0L))) %>%
  arrange(ISO3, Year)

###############################
###Other data configurations###
###############################

# Balance within each ISO3 for YEAR_MIN:YEAR_MAX
Democraticsupport_Claassen_aggregated_w_controls <- merged_panel %>%
  group_by(ISO3) %>%
  tidyr::complete(Year = YEAR_MIN:YEAR_MAX) %>%
  ungroup() %>%
  arrange(ISO3, Year) %>%
  mutate(Year = as.integer(Year))  

# Create logs & lags (lags were ultimately not used in the analysis)
Democraticsupport_Claassen_aggregated_w_controls <-
  Democraticsupport_Claassen_aggregated_w_controls %>%
  group_by(ISO3) %>%
  arrange(ISO3, Year) %>%
  mutate(
    log_gdpcapcon2015 = ifelse(!is.na(wdi_gdpcapcon2015) & wdi_gdpcapcon2015 > 0,
                               log(wdi_gdpcapcon2015), NA_real_),
    log_pop = ifelse(!is.na(wdi_pop) & wdi_pop > 0,
                     log(wdi_pop), NA_real_),
    lag_v2cademmob = lag(v2cademmob, 1),
    lag_v2cagenmob = lag(v2cagenmob, 1),
    lag_any_campaign = lag(any_campaign, 1),
    lag_supdem = lag(SupDem_trim, 1)) %>%
  ungroup()

# Create pdata.frame 
panel_macro <- pdata.frame(
  Democraticsupport_Claassen_aggregated_w_controls %>%
    mutate(Year = as.integer(Year)),   
  index = c("ISO3","Year"), drop.index = FALSE, row.names = FALSE)

# Diagnostics 
pdim(panel_macro)
is.pbalanced(panel_macro)

############################
##Autocratization episodes##
############################

# (1) Pelke & Croissant (2021)
autocratization_dataset %>%
  filter(str_detect(country_name, regex("german|germany|federal republic|west germany|east germany|democratic republic", ignore_case = TRUE))) %>%
  distinct(country_name) %>%
  arrange(country_name)

autocratization_dataset %>%
  filter(str_detect(country_name, regex("yemen|arab republic|people", ignore_case = TRUE))) %>%
  distinct(country_name) %>%
  arrange(country_name)

autocratization_dataset_std <- autocratization_dataset %>%
  mutate(
    ISO3 = countrycode(country_name, "country.name", "iso3c"),
    Year = as.integer(year)) %>%
  mutate(
    ISO3 = case_when(
      country_name == "German Democratic Republic" ~ NA_character_,
      country_name == "South Yemen" ~ NA_character_,
      country_name == "Republic of Vietnam" ~ "VNM",
      country_name == "Zanzibar" ~ "TZA",
      country_name == "Kosovo" ~ NA_character_,
      country_name == "Somaliland" ~ NA_character_,
      TRUE ~ ISO3)) %>%
  arrange(ISO3, Year) %>%
  distinct(ISO3, Year, .keep_all = TRUE) %>%
  select(-country_name, -year, -...1) %>%
  fix_keys()

# Join onto macro panel
merged_macro_with_auto <- Democraticsupport_Claassen_aggregated_w_controls %>%
  mutate(ISO3 = as.character(ISO3), Year = as.integer(Year)) %>%
  left_join(autocratization_dataset_std, by = c("ISO3","Year")) %>%
  arrange(ISO3, Year)

# Make pdata.frame 
panel_macro_autocratization <- pdata.frame(
  merged_macro_with_auto %>% mutate(Year = as.integer(Year)),
  index = c("ISO3","Year"), drop.index = FALSE, row.names = FALSE)

# Duplicate check
as.data.frame(panel_macro_autocratization) %>%
  count(ISO3, Year) %>%
  filter(n > 1)

pdim(panel_macro_autocratization)
is.pbalanced(panel_macro_autocratization)

# (2) V-Dem ERT dataset
ert <- ert %>%
  mutate(
    ISO3 = country_text_id,
    Year = as.integer(year)) %>%
  select(ISO3, Year, aut_ep, aut_ep_id, aut_ep_start_year, aut_ep_end_year) %>%
  rename(
    ert_aut_ep = aut_ep,
    ert_aut_ep_id = aut_ep_id,
    ert_aut_ep_start_year = aut_ep_start_year,
    ert_aut_ep_end_year = aut_ep_end_year) %>%
  distinct(ISO3, Year, .keep_all = TRUE) %>%
  fix_keys()

# Join ERT: convert to normal df for the join, then back to pdata.frame
panel_macro_autocratization <- as.data.frame(panel_macro_autocratization) %>%
  mutate(
    ISO3 = as.character(ISO3),
    Year = as.integer(as.character(Year))) %>%
  left_join(ert, by = c("ISO3","Year")) %>%
  arrange(ISO3, Year) %>%
  pdata.frame(index = c("ISO3","Year"), drop.index = FALSE, row.names = FALSE)

#######################
##Autocratic contexts##
#######################

# Compute on plain df, then back to pdata.frame
panel_macro_autocratization <- as.data.frame(panel_macro_autocratization) %>%
  mutate(
    ISO3 = as.character(ISO3),            
    Year = as.integer(as.character(Year)), 
    edi_autocracy = 1 - v2x_polyarchy) %>%
  pdata.frame(index = c("ISO3","Year"), drop.index = FALSE, row.names = FALSE)

# Reverse BMR
panel_macro_autocratization <- as.data.frame(panel_macro_autocratization) %>%
  mutate(
    ISO3 = as.character(ISO3),
    Year = as.integer(as.character(Year)),
    e_boix_regime = ifelse(!is.na(e_boix_regime), 1 - e_boix_regime, NA)) %>%
  pdata.frame(index = c("ISO3", "Year"), drop.index = FALSE, row.names = FALSE)

# Create panel autocratic
panel_autocratic <- as.data.frame(panel_macro_autocratization) %>%
  mutate(
    ISO3 = as.character(ISO3),
    Year = as.integer(as.character(Year))) %>%
  filter(e_boix_regime == 1) %>%
  pdata.frame(index = c("ISO3", "Year"), drop.index = FALSE, row.names = FALSE)

###########################################################
###Emancipative values: Welzel, Inglehart & Kruse (2015)###
###########################################################

# See their supplementary material for data construction
load("C:/Users/Marcu/OneDrive/Skrivbord/Masteruppsats/Data/Emancipative values data/Pitfalls_in_the_Study_of_Democratization_dta.RData")
Pitfalls_in_the_Study_of_Democratization_dta <- x
remove(x)

# The authors run 5 imputations which generates 5 different country year values for the variables (page 10 in the supplementary materials).

# Take the mean of the imputations to create robust country-year estimates for emancipative values
Pitfalls_in_the_Study_of_Democratization_dta <- Pitfalls_in_the_Study_of_Democratization_dta %>%
  group_by(country, year) %>%
  summarise(
    Emancipative_values = mean(iv_ema, na.rm = TRUE),
    Emancipative_values_sd = sd(iv_ema, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop")

# Custom ISO3 mapping from misspelled country names
custom_iso3 <- c(
  "Australi"  = "AUS", "Azerbaij"  = "AZE", "Banglade"  = "BGD",
  "Germ-W."   = "DEU", "Indonesi"  = "IDN", "Lithuani"  = "LTU",
  "Macedoni"  = "MKD", "Monteneg"  = "MNE", "Netherld"  = "NLD",
  "New Zeal"  = "NZL", "Philippi"  = "PHL", "Singapor"  = "SGP",
  "South Ko"  = "KOR", "Trin-Tob"  = "TTO", "Venezuel"  = "VEN")

Pitfalls_in_the_Study_of_Democratization_dta <- Pitfalls_in_the_Study_of_Democratization_dta %>%
  mutate(
    Country = stringr::str_trim(country),
    ISO3 = countrycode(Country, "country.name", "iso3c", custom_match = custom_iso3)) %>%
  rename(Year = year) %>%
  select(Country, ISO3, Year, Emancipative_values, Emancipative_values_sd, n_obs) %>%
  filter(!is.na(ISO3)) %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year >= YEAR_MIN, Year <= YEAR_MAX) %>%
  select(-n_obs, -Country) %>%
  rename(
    Emancipative_valuesWIK2015 = Emancipative_values,
    Emancipative_valuesWIK2015_sd = Emancipative_values_sd)

remove(custom_iso3)

# Join WIK2015 data: convert to regular df, then back to pdata.frame
panel_macro_autocratization <- as.data.frame(panel_macro_autocratization) %>%
  mutate(
    ISO3 = as.character(ISO3),
    Year = as.integer(as.character(Year))) %>%
  left_join(Pitfalls_in_the_Study_of_Democratization_dta, by = c("ISO3", "Year")) %>%
  pdata.frame(index = c("ISO3", "Year"), drop.index = FALSE, row.names = FALSE)

# Add emancipative values to panel_autocratic (based on Boix Miller Rosato) and create autocratization subsets from Pelke and Croissant 2021
panel_autocratic <- as.data.frame(panel_macro_autocratization) %>%
  mutate(
    ISO3 = as.character(ISO3),
    Year = as.integer(as.character(Year))
  ) %>%
  filter(e_boix_regime == 1) %>%
  pdata.frame(index = c("ISO3", "Year"), drop.index = FALSE, row.names = FALSE)

panel_autocratization_01 <- as.data.frame(panel_macro_autocratization) %>%
  mutate(
    ISO3 = as.character(ISO3),
    Year = as.integer(as.character(Year))) %>%
  filter(auto_period01 == 1) %>%
  pdata.frame(index = c("ISO3", "Year"), drop.index = FALSE, row.names = FALSE)

panel_autocratization_005 <- as.data.frame(panel_macro_autocratization) %>%
  mutate(
    ISO3 = as.character(ISO3),
    Year = as.integer(as.character(Year))) %>%
  filter(auto_period005 == 1) %>%
  pdata.frame(index = c("ISO3", "Year"), drop.index = FALSE, row.names = FALSE)

panel_autocratization_01ci <- as.data.frame(panel_macro_autocratization) %>%
  mutate(
    ISO3 = as.character(ISO3),
    Year = as.integer(as.character(Year))) %>%
  filter(auto_period01ci == 1) %>%
  pdata.frame(index = c("ISO3", "Year"), drop.index = FALSE, row.names = FALSE)

# Diagnostics
pdim(panel_autocratization_01)
pdim(panel_autocratization_005)
pdim(panel_autocratization_01ci)

data.frame(panel_autocratization_01) %>%
  distinct(ISO3) %>%
  arrange(ISO3)

data.frame(panel_autocratization_005) %>%
  distinct(ISO3) %>%
  arrange(ISO3)

data.frame(panel_autocratization_01ci) %>%
  distinct(ISO3) %>%
  arrange(ISO3)

# Tidy workspace
remove(demscore_data, demscore_std, demscore_clean, drop_ci, keep_ci, fix_keys, cc, all_codehigh, all_codelow, cc_patches, YEAR_MAX, YEAR_MIN)
remove(navco_small, claassen_small, merged_panel, autocratization_dataset_std, ert, Support_democracy_ajps_correct, Pitfalls_in_the_Study_of_Democratization_dta, panel_macro, merged_macro_with_auto, NAVCO_country_year)
remove(autocratization_dataset, backbone, Democraticsupport_Claassen_aggregated_w_controls)

# Diagnostics of key variables for some countries, looks good!
panel_macro_autocratization %>%
  filter(ISO3 %in% c("USA", "DEU", "SWE", "YEM", "FRA")) %>%
  select(ISO3, Year, Emancipative_valuesWIK2015, SupDem_trim, v2cademmob, any_nonviolent) %>%
  arrange(ISO3, Year)

summary(panel_macro_autocratization$SupDem_trim) # similar summary statistics to Claassen on this variable apart from NA (natural given the longer time span of my panel, from 1980 instead of 1987)

# Are there any duplicate country-year rows in any dataset? No! All return 0 duplicates
panel_macro_autocratization %>%
  count(ISO3, Year) %>%
  filter(n > 1)

panel_autocratic %>%
  count(ISO3, Year) %>%
  filter(n > 1)

panel_autocratization_005 %>%
  count(ISO3, Year) %>%
  filter(n > 1)

panel_autocratization_01 %>%
  count(ISO3, Year) %>%
  filter(n > 1)

panel_autocratization_01ci %>%
  count(ISO3, Year) %>%
  filter(n > 1)

##############################
###Pelke & Croissant vs ERT###
##############################

# Testing the overlap in cases between Pelke and Croissant 2021 (the 01 measure) and ERT from V-Dem, count and percent
as.data.frame(panel_macro_autocratization) %>%
  mutate(pelke01 = auto_period01 == 1,
         ept_ert = ert_aut_ep == 1) %>%
  count(pelke01, ept_ert) %>%
  ungroup() %>%
  mutate(percent = round(n / sum(n) * 100, 2)) # 2 decimals

# And, where Pelke01 is true, how many ERT episodes are true?
as.data.frame(panel_macro_autocratization) %>%
  mutate(pelke01 = auto_period01 == 1,
         ept_ert = ert_aut_ep == 1) %>%
  count(pelke01, ept_ert) %>%
  group_by(pelke01) %>% 
  mutate(percent = n / sum(n) * 100)

######################################################################
###Removing unnecessary variables and remaining data investigations###
######################################################################
panel_macro_autocratization <- panel_macro_autocratization %>%
  select(-num_campaigns, -num_violent, -num_nonviolent, -Cnt_code, -Region_UN, -Region_VD, -Res_cp_WDI_di, -Country, -auto_period_polity, -auto_period_polity_id, -auto_period_polity005, -auto_period_polity_id005,
         -auto_period_fh, -auto_period_fh_id, -auto_period_fh005, -auto_period_fh_id005, -auto_decline_polity,
         -auto_decline_polity005, -auto_decline_fh, -auto_decline_fh005, -wdi_internet, -wdi_oilrent, 
         -v2x_polyarchy_codelow, -v2x_polyarchy_codehigh, -who_infmortt, -undp_hdi, -wdi_popden, -wdi_poprul,
         -wdi_pop, -lag_any_campaign, -lag_supdem, -lag_v2cademmob, -lag_v2cagenmob, -v2x_jucon, -v2xlg_legcon,
         -v2xeg_eqaccess, -v2x_regime, -v2psoppaut, -v2pscomprg, -v2mecenefi, -v2mecrit, -v2x_clpol, -v2smgovfilprc,
         -fe_etfra, -log_pop, -e_democracy_trans, -v2caassemb, -Satis_trim, -Satis_m1, -ChgSup, -SupDem_m1, -wdi_gdpcapgr, -v2x_libdem,
         -ert_aut_ep_start_year, -ert_aut_ep_end_year, -auto_decline, -auto_decline005, -auto_decline_libdem,
         -auto_decline005_libdem, -auto_decline01_libdem_ci, -auto_decline01ci, -auto_period005_libdem,
         -auto_period01_libdem, -auto_period01_libdem_ci, -auto_period_id_libdem, -auto_period_id005_libdem,
         -auto_period_id01_libdem_ci,-starts_with("v2eltype_"))

panel_autocratic <- panel_autocratic %>%
  select(-num_campaigns, -num_violent, -num_nonviolent, -Cnt_code, -Region_UN, -Region_VD, -Res_cp_WDI_di, -Country, -auto_period_polity, -auto_period_polity_id, -auto_period_polity005, -auto_period_polity_id005,
         -auto_period_fh, -auto_period_fh_id, -auto_period_fh005, -auto_period_fh_id005, -auto_decline_polity,
         -auto_decline_polity005, -auto_decline_fh, -auto_decline_fh005, -wdi_internet, -wdi_oilrent, 
         -v2x_polyarchy_codelow, -v2x_polyarchy_codehigh, -who_infmortt, -undp_hdi, -wdi_popden, -wdi_poprul,
         -wdi_pop, -lag_any_campaign, -lag_supdem, -lag_v2cademmob, -lag_v2cagenmob, -v2x_jucon, -v2xlg_legcon,
         -v2xeg_eqaccess, -v2x_regime, -v2psoppaut, -v2pscomprg, -v2mecenefi, -v2mecrit, -v2x_clpol, -v2smgovfilprc,
         -fe_etfra, -log_pop, -e_democracy_trans, -v2caassemb, -Satis_trim, -Satis_m1, -ChgSup, -SupDem_m1, -wdi_gdpcapgr, -v2x_libdem,
         -ert_aut_ep_start_year, -ert_aut_ep_end_year, -auto_decline, -auto_decline005, -auto_decline_libdem,
         -auto_decline005_libdem, -auto_decline01_libdem_ci, -auto_decline01ci, -auto_period005_libdem,
         -auto_period01_libdem, -auto_period01_libdem_ci, -auto_period_id_libdem, -auto_period_id005_libdem,
         -auto_period_id01_libdem_ci, -starts_with("v2eltype_"))

panel_autocratization_005 <- panel_autocratization_005 %>%
  select(-num_campaigns, -num_violent, -num_nonviolent, -Cnt_code, -Region_UN, -Region_VD, -Res_cp_WDI_di, -Country, -auto_period_polity, -auto_period_polity_id, -auto_period_polity005, -auto_period_polity_id005,
         -auto_period_fh, -auto_period_fh_id, -auto_period_fh005, -auto_period_fh_id005, -auto_decline_polity,
         -auto_decline_polity005, -auto_decline_fh, -auto_decline_fh005, -wdi_internet, -wdi_oilrent, 
         -v2x_polyarchy_codelow, -v2x_polyarchy_codehigh, -who_infmortt, -undp_hdi, -wdi_popden, -wdi_poprul,
         -wdi_pop, -lag_any_campaign, -lag_supdem, -lag_v2cademmob, -lag_v2cagenmob, -v2x_jucon, -v2xlg_legcon,
         -v2xeg_eqaccess, -v2x_regime, -v2psoppaut, -v2pscomprg, -v2mecenefi, -v2mecrit, -v2x_clpol, -v2smgovfilprc,
         -fe_etfra, -log_pop, -e_democracy_trans, -v2caassemb, -Satis_trim, -Satis_m1, -ChgSup, -SupDem_m1, -wdi_gdpcapgr, -v2x_libdem,
         -ert_aut_ep_start_year, -ert_aut_ep_end_year, -auto_decline, -auto_decline005, -auto_decline_libdem,
         -auto_decline005_libdem, -auto_decline01_libdem_ci, -auto_decline01ci, -auto_period005_libdem,
         -auto_period01_libdem, -auto_period01_libdem_ci, -auto_period_id_libdem, -auto_period_id005_libdem,
         -auto_period_id01_libdem_ci, -starts_with("v2eltype_"))

panel_autocratization_01 <- panel_autocratization_01 %>%
  select(-num_campaigns, -num_violent, -num_nonviolent, -Cnt_code, -Region_UN, -Region_VD, -Res_cp_WDI_di, -Country, -auto_period_polity, -auto_period_polity_id, -auto_period_polity005, -auto_period_polity_id005,
         -auto_period_fh, -auto_period_fh_id, -auto_period_fh005, -auto_period_fh_id005, -auto_decline_polity,
         -auto_decline_polity005, -auto_decline_fh, -auto_decline_fh005, -wdi_internet, -wdi_oilrent, 
         -v2x_polyarchy_codelow, -v2x_polyarchy_codehigh, -who_infmortt, -undp_hdi, -wdi_popden, -wdi_poprul,
         -wdi_pop, -lag_any_campaign, -lag_supdem, -lag_v2cademmob, -lag_v2cagenmob, -v2x_jucon, -v2xlg_legcon,
         -v2xeg_eqaccess, -v2x_regime, -v2psoppaut, -v2pscomprg, -v2mecenefi, -v2mecrit, -v2x_clpol, -v2smgovfilprc,
         -fe_etfra, -log_pop, -e_democracy_trans, -v2caassemb, -Satis_trim, -Satis_m1, -ChgSup, -SupDem_m1, -wdi_gdpcapgr, -v2x_libdem,
         -ert_aut_ep_start_year, -ert_aut_ep_end_year, -auto_decline, -auto_decline005, -auto_decline_libdem,
         -auto_decline005_libdem, -auto_decline01_libdem_ci, -auto_decline01ci, -auto_period005_libdem,
         -auto_period01_libdem, -auto_period01_libdem_ci, -auto_period_id_libdem, -auto_period_id005_libdem,
         -auto_period_id01_libdem_ci, -starts_with("v2eltype_"))

panel_autocratization_01ci <- panel_autocratization_01ci %>%
  select(-num_campaigns, -num_violent, -num_nonviolent, -Cnt_code, -Region_UN, -Region_VD, -Res_cp_WDI_di, -Country, -auto_period_polity, -auto_period_polity_id, -auto_period_polity005, -auto_period_polity_id005,
         -auto_period_fh, -auto_period_fh_id, -auto_period_fh005, -auto_period_fh_id005, -auto_decline_polity,
         -auto_decline_polity005, -auto_decline_fh, -auto_decline_fh005, -wdi_internet, -wdi_oilrent, 
         -v2x_polyarchy_codelow, -v2x_polyarchy_codehigh, -who_infmortt, -undp_hdi, -wdi_popden, -wdi_poprul,
         -wdi_pop, -lag_any_campaign, -lag_supdem, -lag_v2cademmob, -lag_v2cagenmob, -v2x_jucon, -v2xlg_legcon,
         -v2xeg_eqaccess, -v2x_regime, -v2psoppaut, -v2pscomprg, -v2mecenefi, -v2mecrit, -v2x_clpol, -v2smgovfilprc,
         -fe_etfra, -log_pop, -e_democracy_trans, -v2caassemb, -Satis_trim, -Satis_m1, -ChgSup, -SupDem_m1, -wdi_gdpcapgr, -v2x_libdem,
         -ert_aut_ep_start_year, -ert_aut_ep_end_year, -auto_decline, -auto_decline005, -auto_decline_libdem,
         -auto_decline005_libdem, -auto_decline01_libdem_ci, -auto_decline01ci, -auto_period005_libdem,
         -auto_period01_libdem, -auto_period01_libdem_ci, -auto_period_id_libdem, -auto_period_id005_libdem,
         -auto_period_id01_libdem_ci, -starts_with("v2eltype_"))

# Which country has the lowest and highest observed value of emancipative values in the data?
panel_macro_autocratization %>%
  slice_min(Emancipative_valuesWIK2015) %>%
  select(ISO3, Year, Emancipative_valuesWIK2015)

panel_macro_autocratization %>%
  slice_max(Emancipative_valuesWIK2015) %>%
  select(ISO3, Year, Emancipative_valuesWIK2015)

# Check min and max v2cademmob values too
panel_macro_autocratization %>%
  slice_min(v2cademmob) %>%
  select(ISO3, Year, v2cademmob)

panel_macro_autocratization %>%
  slice_max(v2cademmob) %>%
  select(ISO3, Year, v2cademmob)

# Ensure that the e_boix_regime variable truly is recoded
panel_macro_autocratization %>% 
  group_by(e_boix_regime) %>% 
  summarize(mean(edi_autocracy))

panel_macro_autocratization %>% 
  group_by(e_boix_regime) %>% 
  summarize(mean(v2x_polyarchy))

# Double check any remaining missing value special characters
missing_codes <- c(-66, -77, -88, -99, -5555, -6666, -7777, 
                   -8888, -9999, -11111, -22222, 6666, 7777, 8888, 9999)

any(sapply(panel_macro_autocratization, function(x) any(x %in% missing_codes, na.rm = TRUE)))

remove(missing_codes)

# Summary statistics
summary_statistics <- summary(panel_macro_autocratization[c("v2cademmob", "any_nonviolent", "SupDem_trim",
                                                            "Emancipative_valuesWIK2015", "log_gdpcapcon2015", 
                                                            "e_peaveduc", "wdi_popurb", "v2x_polyarchy", "Year",
                                                            "ISO3", "auto_period005", "auto_period01", "auto_period01ci", "e_boix_regime",
                                                            "edi_autocracy")])
summary_statistics

remove(summary_statistics)

#################################################################
###Distribution of values, independent and dependent variables###
#################################################################
p1 <- ggplot(as.data.frame(panel_macro_autocratization), 
             aes(x = as.numeric(SupDem_trim))) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 40, fill = "grey70", color = "white", alpha = 0.8) +
  geom_density(color = "steelblue", linewidth = 0.7) +
  labs(title = NULL, x = "Public support for democracy", y = "Density") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "plain", hjust = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())

p2 <- ggplot(as.data.frame(panel_macro_autocratization), 
             aes(x = as.numeric(Emancipative_valuesWIK2015))) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 40, fill = "grey70", color = "white", alpha = 0.8) +
  geom_density(color = "steelblue", linewidth = 0.7) +
  labs(title = NULL, x = "Emancipative values", y = "Density") +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "plain", hjust = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())

# Combine plots
p1 / p2 
# or
p1 | p2 

remove(p1, p2)

plot_nonviolent <- ggplot(as.data.frame(panel_macro_autocratization),
                          aes(x = factor(any_nonviolent))) +
  geom_bar(fill = "grey70", color = "white", alpha = 0.8) +
  labs(
    title = NULL,
    x = "Nonviolent campaign (0 = No, 1 = Yes)",
    y = "Number of observations") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "plain", hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank())

plot_mobilization <- ggplot(as.data.frame(panel_macro_autocratization),
                            aes(x = as.numeric(v2cademmob))) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 40, fill = "grey70", color = "white", alpha = 0.8) +
  geom_density(color = "steelblue", linewidth = 0.7, na.rm = TRUE) +
  labs(
    title = NULL,
    x = "Mobilization for democracy",
    y = "Density") +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "plain", hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank())

# Combine side by side
plot_nonviolent | plot_mobilization

remove(plot_nonviolent, plot_mobilization)

# Correlation between the two measures of the dependent variable
cor(panel_macro_autocratization$any_nonviolent, panel_macro_autocratization$v2cademmob, use = "pairwise.complete.obs")
