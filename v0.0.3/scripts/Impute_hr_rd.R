# ========================================================================================
# Project:  GRAPE
# Subject:  Script to impute hr rd data
# Author:   Michiel van Dijk
# Contact:  michiel.vandijk@wur.nl
# ========================================================================================

# ========================================================================================
# SETUP ----------------------------------------------------------------------------------
# ========================================================================================

# Load pacman for p_load
if(!require(pacman)) install.packages("pacman")
library(pacman)

# Load key packages
p_load(here, tidyverse, readxl, stringr, scales, glue)

# Load additional packages
p_load(Amelia, countrycode, imputeTS, yardstick, tsibble, fable, feasts, patchwork)

# R options
options(scipen = 999)
options(digits = 4)


# ========================================================================================
# SET VERSION AND DATABASE ---------------------------------------------------------------
# ========================================================================================

# Set version
source(here("set_version.R"))

# Set database folder
db_path <- "C:/Users/dijk158/OneDrive - Wageningen University & Research/data/AG_RD_DB/v0.0.3/grape_db"


# ========================================================================================
# SOURCE FUNCTIONS -----------------------------------------------------------------------
# ========================================================================================

source(here(glue("{db_version}/scripts/functions.R")))


# ========================================================================================
# LOAD DATA ------------------------------------------------------------------------------
# ========================================================================================

# Read R&D data
rd_pre_imp <- readRDS(file.path(db_path, glue("rd_pre_imp_{db_version}.rds")))
hr_pre_imp <- readRDS(file.path(db_path, glue("hr_pre_imp_{db_version}.rds")))

# Read macro db
macro_db <- read_excel(file.path(db_path, glue("macro_db_{macro_db_version}.xlsx")), sheet = "macro_db")

# Linking
li <- read.csv(here(glue("{db_version}/data/linking_list.csv")))
pr <- read.csv(here(glue("{db_version}/data/processing_list.csv")))


# ========================================================================================
# PREPARE DATABASE FOR IMPUTATION --------------------------------------------------------
# ========================================================================================

# Countries with at least one rd and hr observation
hr_rd_min_data <- full_join(
  rd_pre_imp |>
    dplyr::select(iso3c) |>
    mutate(rd = "available") |>
    unique(),
  hr_pre_imp |>
    dplyr::select(iso3c) |>
    mutate(hr = "available") |>
    unique()
) |>
  na.omit() |>
  distinct()

# Combine, rd, hr and select only countries for which rd-hr data is available
hr_rd_pre_imp <- bind_rows(hr_pre_imp |>
                             mutate(variable = "hr"),
                           rd_pre_imp |>
                             mutate(variable = "rd")
                           ) |>
  filter(iso3c %in% hr_rd_min_data$iso3c)

# Inspect for missing values
summary(hr_rd_pre_imp)
n_distinct(hr_rd_pre_imp$iso3c)


# ========================================================================================
# INTERPOLATION OF MISSING DATA WITHIN SERIES --------------------------------------------
# ========================================================================================

# Note that de default behavior of na_interpolation is to use carry forward and backward in case of endpoints.
# This can be changed by setting (na_interpolation(x, yleft = NA , yright = NA).
# We add 0.01 to log(value) as in rare cases hr is zero, and transform back.
interpolation_db <- hr_rd_pre_imp |>
  group_by(iso3c, variable) |>
  arrange(year) |>
  complete(year = c(min(year):max(year))) |>
  mutate(
    linking = ifelse(is.na(linking), li$linking[8], linking),
    source = ifelse(is.na(source), "Imputed", source),
    value = ifelse(value == 0, 0.01, value),
    value = exp(na_interpolation(log(value), yleft = NA , yright = NA)),
    value = ifelse(value <= 0.015, 0, value)) |>
  ungroup()
summary(interpolation_db)


# ========================================================================================
# BACKCAST AND FORCAST USING HR-RD -------------------------------------------------------
# ========================================================================================

# Backcasting using hr-rd
hr_rd_imp_db <- bind_rows(
  interpolation_db |>
    split(~iso3c, drop = TRUE) |>
    map_dfr(~fc_hr_rd(.x, so = "rd", ta = "hr")) |>
    mutate(source = "Imputed",
           processing = NA_character_),
  interpolation_db |>
    split(~iso3c, drop = TRUE) |>
    map_dfr(~fc_hr_rd(.x, so = "hr", ta = "rd")) |>
    mutate(source = "Imputed",
           processing = NA_character_)
  ) |>
  rename(value = forecast)


# ========================================================================================
# BACKCAST AND FORECAST USING NAIVE ------------------------------------------------------
# ========================================================================================

# We want NAIVE and ARI backcasting in case series are backcasted using hr-rd
# Determine for which countries hr or rd is backcasted
min_year <- interpolation_db |>
  group_by(iso3c) |>
  summarize(min_year_hr = min(year[variable == "hr"]),
            min_year_rd = min(year[variable == "rd"]),
            max_year_hr = max(year[variable == "hr"]),
            max_year_rd = max(year[variable == "rd"]),
            min_year = min(year),
            max_year = max(year),
            .groups = "drop") |>
  dplyr::select(iso3c, min_year)

naive_imp_db <-  interpolation_db |>
  left_join((min_year)) |>
  group_by(iso3c, variable) |>
  arrange(year) |>
  complete(year = c(min(year, min_year):max(ey))) |>
  ungroup() |>
  split(~iso3c + variable, drop = TRUE) |>
  map_dfr(fc_naive,  .id = "group") |>
  separate(group, sep = "\\.", into = c("iso3c", "variable")) |>
  mutate(linking = li$linking[13],
         source = "Imputed") |>
  dplyr::select(-.model) |>
  rename(value = forecast)


# ========================================================================================
# BACKCAST AND FORCAST USING AGGDP -------------------------------------------------------
# ========================================================================================

# Only apply this for countries with ag_output data
ag_gdp_iso3c <- macro_db |>
  filter(year > 1970) |>
  group_by(iso3c) |>
  filter(!all(is.na(ag_gdp_i))) |>
  pull(iso3c) |>
  unique()

# We removed several countries for which AGGDP is not considered reliable because of extreme fluctuations
# ROU was excluded because we backcast all the way back to the early 60s and ag_gdp is only available from
# 1970 onward
ag_gdp_exclude <- c("LBY", "COM", "GUY", "SUR", "POL", "ROU", "PLW")

aggdp_imp_db <-  interpolation_db |>
  filter(iso3c %in% ag_gdp_iso3c,
         !iso3c %in% ag_gdp_exclude) |>
  left_join((min_year)) |>
  group_by(iso3c, variable) |>
  arrange(year) |>
  complete(year = c(min(max(year, 1970), min_year):max(ey))) |>
  ungroup() |>
  split(~iso3c + variable, drop = TRUE) |>
  map_dfr(fc_ag_gdp,  .id = "group") |>
  separate(group, sep = "\\.", into = c("iso3c", "variable")) |>
  mutate(
    linking = li$linking[9],
         source = "Imputed") |>
  dplyr::select(-.model) |>
  rename(value = forecast) |>
  na.omit()
rm(ag_gdp_iso3c)


# ========================================================================================
# BACKCAST AND FORCAST USING AGOUTPUT ----------------------------------------------------
# ========================================================================================

# Only apply this for countries with ag_output data
ag_output_iso3c <- macro_db |>
  group_by(iso3c) |>
  filter(!all(is.na(ag_output_i))) |>
  pull(iso3c) |>
  unique()

agoutput_imp_db <-  interpolation_db |>
  filter(iso3c %in% ag_output_iso3c) |>
  left_join((min_year)) |>
  group_by(iso3c, variable) |>
  arrange(year) |>
  complete(year = c(unique(min_year):max(ey))) |>
  ungroup() |>
  split(~iso3c + variable, drop = TRUE) |>
  map_dfr(fc_ag_output,  .id = "group") |>
  separate(group, sep = "\\.", into = c("iso3c", "variable")) |>
  mutate(
    linking = li$linking[18],
    source = "Imputed") |>
  dplyr::select(-.model) |>
  rename(value = forecast) |>
  na.omit()
rm(ag_output_iso3c)


# ========================================================================================
# FORECAST USING DT AND SES --------------------------------------------------------------
# ========================================================================================

# Only apply this for countries with (a) available data = 20, not imputed
# and (b) max 7 missing
dt_ses_iso3c <-  interpolation_db |>
  group_by(iso3c, variable) |>
  filter(year > ey-30) |>
  complete(year = c(min(year):max(ey))) |>
  summarize(n_miss = sum(is.na(value)),
            n_available = sum(linking %in% c("none")),
            .groups = "drop") |>
  filter(n_miss > 0 & n_miss <= 6 & n_available >= 24)

dt_ses_imp_db <- dt_ses_iso3c |>
  dplyr::select(iso3c, variable) |>
  left_join(interpolation_db) |>
  group_by(iso3c, variable) |>
  filter(year > ey - 30) |>
  arrange(year) |>
  complete(year = c(min(year):max(ey))) |>
  ungroup() |>
  split(~iso3c + variable, drop = TRUE) |>
  map_dfr(fc_time_series,  .id = "group") |>
  separate(group, sep = "\\.", into = c("iso3c", "variable")) |>
  mutate(
    linking = ifelse(.model == "SES", li$linking[15], li$linking[14]),
    source = "Imputed") |>
  dplyr::select(-.model) |>
  rename(value = forecast)


# ========================================================================================
# BACKCAST AND FORCAST USING RD ----------------------------------------------------------
# ========================================================================================

# Combine rd imputations
imp_rd_db <- bind_rows(
  interpolation_db,
  naive_imp_db,
  hr_rd_imp_db,
  aggdp_imp_db,
  agoutput_imp_db,
  dt_ses_imp_db) |>
  arrange(iso3c, variable, year, linking) |>
  filter(variable == "rd")
summary(imp_rd_db)

# Calculate ensemble imputation
imp_rd_mean_db <- imp_rd_db |>
  group_by(iso3c, year, variable) |>
  summarize(value = mean(value, na.rm = TRUE),
            .groups = "drop")
summary(imp_rd_mean_db)

# Combine rd_ensemble with hr and backcast using rd
# In this way ensemble backcast for rd is used to backcast hr for consistency
hr_imp_db <- bind_rows(
  imp_rd_mean_db,
  interpolation_db |>
    filter(variable == "hr")
  ) |>
  dplyr::select(iso3c, year, variable, value, source, processing) |>
    split(~iso3c, drop = TRUE) |>
    map_dfr(~fc_hr_rd(.x, so = "rd", ta = "hr")) |>
    mutate(source = "Imputed",
           processing = NA_character_) |>
  rename(value = forecast)


# ========================================================================================
# COMBINE IMPUTATIONS --------------------------------------------------------------------
# ========================================================================================

imp_hr_db <- bind_rows(
  interpolation_db,
  naive_imp_db,
  hr_imp_db,
  aggdp_imp_db,
  agoutput_imp_db,
  dt_ses_imp_db) |>
  arrange(iso3c, variable, year, linking) |>
  filter(variable == "hr")
summary(imp_hr_db)


# ========================================================================================
# COMBINE HR AND RD ----------------------------------------------------------------------
# ========================================================================================

imp_db <- bind_rows(
  imp_hr_db,
  imp_rd_db)

imp_ensemble_db <- imp_db |>
  group_by(iso3c, year, variable) |>
  summarize(value = mean(value, na.rm = TRUE),
            source = unique(source),
            processing = unique(processing),
            linking = paste(linking, collapse = ", "),
            .groups = "drop")
summary(imp_ensemble_db)
table(imp_ensemble_db$linking)

# Update processing and set linking for ensemble imputation
imp_ensemble_db <- imp_ensemble_db |>
  mutate(
    linking = ifelse(linking %in% c("none", "ei", "splice_new", "splice_old", "i_naive"), linking,
                     li$linking[16]),
    processing = ifelse(is.na(processing), "none", processing)
  )
table(imp_ensemble_db$linking)


# ========================================================================================
# CHECK ----------------------------------------------------------------------------------
# ========================================================================================

# Check if start and end year are the same for all country-variable series
check_series <- imp_db |>
  group_by(iso3c) |>
  summarize(min_year_hr = min(year[variable == "hr"]),
            min_year_rd = min(year[variable == "rd"]),
            max_year_hr = max(year[variable == "hr"]),
            max_year_rd = max(year[variable == "rd"]),
            .groups = "drop") |>
  filter(min_year_hr != min_year_rd | max_year_hr != max_year_rd)
try(if(nrow(check_series) > 0) stop("Start and end of hr/rd series is not the same"))
n_distinct(imp_db$iso3c)


# ========================================================================================
# CLEAN UP -------------------------------------------------------------------------------
# ========================================================================================

rm(aggdp_imp_db, agoutput_imp_db, check_series, dt_ses_imp_db, dt_ses_iso3c, hr_imp_db, hr_pre_imp, hr_rd_min_data,
   hr_rd_imp_db, hr_rd_pre_imp, imp_db, imp_hr_db, imp_rd_db, imp_rd_mean_db, interpolation_db, li,
   macro_db, min_year, naive_imp_db, pr, rd_pre_imp)


