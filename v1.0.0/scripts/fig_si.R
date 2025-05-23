# ========================================================================================
# Project:  GRAPE
# Subject:  Script to evaluate forecasts
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
p_load(Amelia, parallel, countrycode, imputeTS, yardstick, tsibble, fable, feasts, furrr, texreg)

# R options
options(scipen = 999)
options(digits = 4)


# ========================================================================================
# SET VERSION AND DATABASE ---------------------------------------------------------------
# ========================================================================================

# Set version
source(here("set_version.R"))

# Set database folder
db_path <- glue("c:/Users/dijk158/OneDrive - Wageningen University & Research/data/AG_RD_DB/{db_version}/grape_db")


# ========================================================================================
# SOURCE FUNCTIONS -----------------------------------------------------------------------
# ========================================================================================

source(here(glue("{db_version}/scripts/functions.R")))


# ========================================================================================
# LOAD DATA ------------------------------------------------------------------------------
# ========================================================================================

# Read R&D data
rd_raw <- readRDS(file.path(db_path, glue("rd_pre_imp_{db_version}.rds")))
hr_raw <- readRDS(file.path(db_path, glue("hr_pre_imp_{db_version}.rds")))

# gbard
rd_gbard <- readRDS(here(glue("{db_version}/data/rd_oecd_eurostat_gbard.rds")))

# Read macro db
macro_db <- read_excel(file.path(db_path, glue("macro_db_{macro_db_version}.xlsx")), sheet = "macro_db")

# Linking
li <- read.csv(here(glue("{db_version}/data/linking_list.csv")))

# Linking
pr <- read.csv(here(glue("{db_version}/data/processing_list.csv")))


# ========================================================================================
# PREPARE DATABASE FOR IMPUTATION --------------------------------------------------------
# ========================================================================================

# Countries with at least one rd and hr observation
hr_rd_min_data <- full_join(
  rd_raw |>
    dplyr::select(iso3c) |>
    mutate(rd = "available") |>
    unique(),
  hr_raw |>
    dplyr::select(iso3c) |>
    mutate(hr = "available") |>
    unique()
) |>
  na.omit() |>
  distinct()

# Combine, rd, hr and macro_db, and select only countries for which rd-hr data is available
# and years with ag_gdp_i and ag_output_i
hr_rd_pre_imp <- full_join(hr_raw |>
                             dplyr::select(year, iso3c, hr = value),
                           rd_raw |>
                             dplyr::select(year, iso3c, rd = value)) |>
  filter(year >= 1970 & year <= ey, iso3c %in% hr_rd_min_data$iso3c) |>
  left_join(macro_db) |>
  filter(!is.na(ag_gdp_i) & !is.na(ag_output_i)) |>
  dplyr::select(iso3c, year, hr, rd)


# ========================================================================================
# CORRELATION BETWEEN HR AND RD DRIVERS --------------------------------------------------
# ========================================================================================

# hr-rd regression
df_reg_hr_rd <- hr_rd_pre_imp |>
  filter(!is.na(hr), !is.na(rd))
reg_hr_rd <- lm(hr ~ rd + factor(iso3c), data = df_reg_hr_rd)
n_hr_rd <- n_distinct(df_reg_hr_rd$iso3c)
summary(reg_hr_rd)

# hr-rd GBARD regression
df_reg_rd_gbard <- hr_rd_pre_imp |>
  left_join(rd_gbard) |>
  dplyr::rename(gbard = value) |>
  filter(!is.na(rd), !is.na(gbard))

df_reg_hr_gbard <- hr_rd_pre_imp |>
  left_join(rd_gbard) |>
  dplyr::rename(gbard = value) |>
  filter(!is.na(hr), !is.na(gbard))

reg_hr_gbard <- lm(hr ~ gbard + factor(iso3c), data = df_reg_hr_gbard)
reg_rd_gbard <- lm(rd ~ gbard + factor(iso3c), data = df_reg_rd_gbard)
n_hr_gbard <- n_distinct(df_reg_hr_gbard$iso3c)
n_rd_gbard <- n_distinct(df_reg_rd_gbard$iso3c)
summary(reg_hr_gbard)
summary(reg_rd_gbard)

# hr-rd ag_gdp regression
df_reg_rd_ag_gdp <- hr_rd_pre_imp |>
  left_join(macro_db) |>
  filter(!is.na(rd), !is.na(ag_gdp_i))

df_reg_hr_ag_gdp <- hr_rd_pre_imp |>
  left_join(macro_db) |>
  filter(!is.na(hr), !is.na(ag_gdp_i))

reg_hr_ag_gdp <- lm(hr ~ ag_gdp_i + factor(iso3c), data = df_reg_hr_ag_gdp)
reg_rd_ag_gdp <- lm(rd ~ ag_gdp_i + factor(iso3c), data = df_reg_rd_ag_gdp)
n_hr_ag_gdp <- n_distinct(df_reg_hr_ag_gdp$iso3c)
n_rd_ag_gdp <- n_distinct(df_reg_rd_ag_gdp$iso3c)
summary(reg_hr_ag_gdp)
summary(reg_rd_ag_gdp)

# hr-rd ag_output regression
df_reg_rd_ag_output <- hr_rd_pre_imp |>
  left_join(macro_db) |>
  filter(!is.na(rd), !is.na(ag_output_i))

df_reg_hr_ag_output <- hr_rd_pre_imp |>
  left_join(macro_db) |>
  filter(!is.na(hr), !is.na(ag_output_i))

reg_hr_ag_output <- lm(hr ~ ag_output_i + factor(iso3c), data = df_reg_hr_ag_output)
reg_rd_ag_output <- lm(rd ~ ag_output_i + factor(iso3c), data = df_reg_rd_ag_output)
n_hr_ag_output <- n_distinct(df_reg_hr_ag_output$iso3c)
n_rd_ag_output <- n_distinct(df_reg_rd_ag_output$iso3c)
summary(reg_hr_ag_output)
summary(reg_rd_ag_output)

# Regression table
screenreg(list(reg_hr_rd, reg_hr_gbard, reg_rd_gbard, reg_hr_ag_gdp, reg_rd_ag_gdp, reg_hr_ag_output, reg_rd_ag_output),
          custom.model.names = c("HR-RD", "HR-GBARD", "RD-GBARD", "HR-AGGDP", "RD-AGGDP", "HR-AGOUTPUT", "RD-AGOUTPUT"),
          custom.coef.map = list(
            "(Intercept)" = "Constant",
            "rd" = "RD",
            "hr" = "Researchers",
            "gbard" = "GBARD",
            "ag_gdp_i" = "AGGDP",
            "ag_output_i" = "AGOUTPUT"),
          custom.gof.rows = list("Country fixed effects" = c("Yes", "Yes", "yes", "yes", "yes", "yes", "yes"),
                                 "Number of countries" = c(n_hr_rd, n_hr_gbard, n_rd_gbard, n_hr_ag_gdp,
                                                           n_rd_ag_gdp, n_hr_ag_output, n_rd_ag_output)))


# ========================================================================================
# EVALUATION OF LONG-TERM FORECASTS ------------------------------------------------------
# ========================================================================================

# Identify countries with 30 year long time series for both hr and rd
# Note we need at least 4 times the forecast horizon for DT and SES
# So for this dataset max 6 year ahead, i.e. only short run forecast.
fc_long_sy <- 1987
fc_long_ey <- 2017
fc_long_range <- hr_rd_pre_imp |>
  pivot_longer(-c(iso3c, year), names_to = "variable", values_to = "value") |>
  group_by(iso3c, variable) |>
  filter(year %in% c(fc_long_sy:fc_long_ey)) |>
  filter(all(c(fc_long_sy:fc_long_ey) %in% year) & all(!is.na(value))) |>
  group_by(iso3c) |>
  ungroup() |>
  pivot_wider(names_from = variable, values_from = value) |>
  na.omit() |>
  pivot_longer(-c(iso3c, year), names_to = "variable", values_to = "value")
n_distinct(fc_long_range$iso3c)
unique(fc_long_range$iso3c)

# Impute using various length of NA values
fc_long_raw_db <- bind_rows(
  fc_long_range |>
    split(~iso3c, drop = TRUE) |>
    map_dfr(forecast_all, impute_length = 30) |>
    mutate(impute_length = 30),
  fc_long_range |>
    split(~iso3c, drop = TRUE) |>
    map_dfr(forecast_all, impute_length = 20) |>
    mutate(impute_length = 20),
  fc_long_range |>
    split(~iso3c, drop = TRUE) |>
    map_dfr(forecast_all, impute_length = 10) |>
    mutate(impute_length = 10),
  fc_long_range |>
    split(~iso3c, drop = TRUE) |>
    map_dfr(forecast_all, impute_length = 5) |>
    mutate(impute_length = 5),
  fc_long_range |>
    split(~iso3c, drop = TRUE) |>
    map_dfr(forecast_all, impute_length = 3) |>
    mutate(impute_length = 3),
  fc_long_range |>
    split(~iso3c, drop = TRUE) |>
    map_dfr(forecast_all, impute_length = 1) |>
    mutate(impute_length = 1)
  )

# Remove GBARD as we cannot use if for all countries and variable combinations due to lack of data.
# Remove SES and DT for impute_length > 5
fc_long_db <- fc_long_raw_db |>
  filter(.model != "GBARD") |>
  filter(!(.model %in% c("SES", "DT") & impute_length %in% c(20, 30)))
unique(fc_long_db$.model)

# Add mean
fc_long_db <- bind_rows(
  fc_long_db |>
  group_by(year, iso3c, variable, impute_length) |>
  summarize(forecast = mean(forecast, na.rm = TRUE),
            .groups = "drop") |>
  mutate(.model = "ENS"),
  fc_long_db
) |>
  left_join(fc_long_range)

# Accuracy statistics
fc_long_accuracy_db <- fc_long_db |>
  group_by(.model, iso3c, variable, impute_length) |>
  filter(!is.na(value), !is.na(forecast)) |>
  summarise(
    MAE = mae_vec(value, forecast, na_rm = TRUE),
    RMSE = rmse_vec(value, forecast, na_rm = TRUE),
    MAPE = mape_vec(value, forecast, na_rm = TRUE),
    MASE = ifelse(sum(!is.na(value)) <= 1, NA, mase_vec(value, forecast, na_rm = TRUE)),
    .groups = "drop")

okabe_ito_palette <- c(palette.colors(palette = "Okabe-Ito")[-1], "#9467bd" )
names(okabe_ito_palette) <- NULL

fig_fc_long <- fc_long_accuracy_db |>
  filter(impute_length != 1) |>
  mutate(variable = ifelse(variable == "hr", "Number of Researchers (HR)", "R&D expenditures (RD)")) |>
  mutate(.model = factor(.model,
                         levels = c("ENS", "AGGDP", "AGOUTPUT", "GBARD", "HR/RD", "NAIVE", "DT", "SES"))) |>
  ggplot(aes(x = .model, y = MASE, fill = .model)) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(outliers = FALSE) +
  labs(y = "Mean sbsolute scaled error (MASE)", x = NULL, color = NULL, fill = NULL) +
  scale_fill_manual(values = okabe_ito_palette) +
  theme_bw() +
  facet_grid(impute_length~variable) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    plot.margin = margin(t = 10, r = 10, b = 0, l = 10),
    axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5, hjust = 1)
  ) +
  coord_cartesian(ylim = c(0, 15)) +
  guides(fill = "none")


# ========================================================================================
# EVALUATION OF SHORT TERM FORECASTS -----------------------------------------------------
# ========================================================================================

# Identify countries with shorter series that also have EUROSTAT-OECD gbard data
fc_short_sy <- 2000
fc_short_ey <- 2020
fc_short_range <- hr_rd_pre_imp |>
  pivot_longer(-c(iso3c, year), names_to = "variable", values_to = "value") |>
  group_by(iso3c, variable) |>
  filter(year %in% c(fc_short_sy:fc_short_ey)) |>
  filter(all(c(fc_short_sy:fc_short_ey) %in% year) & all(!is.na(value))) |>
  group_by(iso3c) |>
  ungroup() |>
  pivot_wider(names_from = variable, values_from = value) |>
  na.omit() |>
  pivot_longer(-c(iso3c, year), names_to = "variable", values_to = "value")
n_distinct(fc_short_range$iso3c)
unique(fc_short_range$iso3c)

# Select series for which gbard data is available
fc_short_gbard <- rd_gbard |>
  group_by(iso3c) |>
  filter(year %in% c(fc_short_sy:fc_short_ey)) |>
  filter(all(c(fc_short_sy:fc_short_ey) %in% year)) |>
  ungroup()
n_distinct(fc_short_gbard$iso3c)
unique(fc_short_gbard$iso3c)

# Intersection gbard and short range
fc_short_iso3c <- intersect(fc_short_gbard$iso3c, fc_short_range$iso3c)
n_distinct(fc_short_iso3c)
unique(fc_short_iso3c)

fc_short_range <- fc_short_range |>
  filter(iso3c %in% fc_short_iso3c)

# Impute using various length of NA values
fc_short_raw_db <- bind_rows(
  fc_short_range |>
    split(~iso3c, drop = TRUE) |>
    map_dfr(forecast_all, impute_length = 19) |>
    mutate(impute_length = 20),
  fc_short_range |>
    split(~iso3c, drop = TRUE) |>
    map_dfr(forecast_all, impute_length = 10) |>
    mutate(impute_length = 10),
  fc_short_range |>
    split(~iso3c, drop = TRUE) |>
    map_dfr(forecast_all, impute_length = 5) |>
    mutate(impute_length = 5),
  fc_short_range |>
    split(~iso3c, drop = TRUE) |>
    map_dfr(forecast_all, impute_length = 3) |>
    mutate(impute_length = 3),
  fc_short_range |>
    split(~iso3c, drop = TRUE) |>
    map_dfr(forecast_all, impute_length = 1) |>
    mutate(impute_length = 1)
)

# Remove SES and DT for impute_length > 10
fc_short_db <- fc_short_raw_db |>
  filter(!(.model %in% c("SES", "DT") & impute_length %in% c(20)))
unique(fc_short_db$.model)

# Add mean
fc_short_db <- bind_rows(
  fc_short_db |>
    group_by(year, iso3c, variable, impute_length) |>
    summarize(forecast = mean(forecast, na.rm = TRUE),
              .groups = "drop") |>
    mutate(.model = "ENS"),
 fc_short_db |>
    filter(.model %in% c("HR/RD", "AGGDP", "AGOUTPUT", "NAIVE", "SES", "DT")) |>
    group_by(year, iso3c, variable, impute_length) |>
    summarize(forecast = mean(forecast, na.rm = TRUE),
              .groups = "drop") |>
    mutate(.model = "ENS-GB"),
  fc_short_db
) |>
  left_join(fc_short_range)

# Accuracy statistics
fc_short_accuracy_db <- fc_short_db |>
  group_by(.model, iso3c, variable, impute_length) |>
  filter(!is.na(value), !is.na(forecast)) |>
  summarise(
    MAE = mae_vec(value, forecast, na_rm = TRUE),
    RMSE = rmse_vec(value, forecast, na_rm = TRUE),
    MAPE = mape_vec(value, forecast, na_rm = TRUE),
    MASE = ifelse(sum(!is.na(value)) <= 1, NA, mase_vec(value, forecast, na_rm = TRUE)),
    .groups = "drop")

okabe_ito_palette <- palette.colors(palette = "Okabe-Ito")
#show_col(okabe_ito_palette)
names(okabe_ito_palette) <- NULL

fig_fc_short <- fc_short_accuracy_db |>
  filter(impute_length != 1) |>
  mutate(variable = ifelse(variable == "hr", "Number of Researchers (HR)", "R&D expenditures (RD)")) |>
  mutate(.model = factor(.model,
                         levels = c("ENS", "ENS-GB",
                                    "AGGDP", "AGOUTPUT", "GBARD", "HR/RD", "NAIVE", "DT", "SES"))) |>
  ggplot(aes(x = .model, y = MASE, fill = .model)) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(outliers = FALSE) +
  labs(y = "Mean absolute scaled error (MASE)", x = NULL, color = NULL, fill = NULL) +
  scale_fill_manual(values = okabe_ito_palette) +
  theme_bw() +
  facet_grid(impute_length~variable) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    plot.margin = margin(t = 10, r = 10, b = 0, l = 10),
    axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5, hjust = 1)
    ) +
  coord_cartesian(ylim = c(0, 15)) +
  guides(fill = "none")


# ========================================================================================
# EVALUATION OF INTERPOLATION ------------------------------------------------------------
# ========================================================================================

# Determine length of streak. We exclude all values that were pre-processed.
lri_length_streak <- hr_rd_pre_imp |>
  pivot_longer(-c(iso3c, year), names_to = "variable", values_to = "value") |>
  add_info(li$linking[9]) |>
  mutate(source = ifelse(source == "imputed", NA_character_, source),
         source = gsub("^ASTI.*", "ASTI", source),
         value = ifelse(processing != "none", NA_real_, value)) |>
  filter(year >= 1970) |>
  split( ~ iso3c + source + variable, drop = TRUE) |>
  map_df(~ tibble(
    iso3c = unique(.x$iso3c),
    source = unique(.x$source),
    variable = unique(.x$variable),
    longest_streak= longest_non_na_streak(.x$value))
  )

# Evaluate forecast approach. We selected series with at least 30 years of data.
# And ensure that ag gdp data is available
lri_longest_streak <- hr_rd_pre_imp |>
  pivot_longer(-c(iso3c, year), names_to = "variable", values_to = "value") |>
  add_info(li$linking[9]) |>
  mutate(source = ifelse(source == "imputed", NA_character_, source),
         source = gsub("^ASTI.*", "ASTI", source)) |>
  left_join(lri_length_streak) |>
  filter(longest_streak >= 32, iso3c %in% macro_db$iso3c)
n_distinct(dplyr::select(lri_longest_streak, c(iso3c, variable)))
table(lri_longest_streak$variable)
unique(lri_longest_streak$source)

# Randomly select NA values ten times and interpolate using different streak lengths.
set.seed(42)

# Create all combinations of streak_length and times
param_grid <- expand_grid(streak_length = c(3, 5, 10, 20, 30), times = 1:10)

# Run the function for each combination
plan(multisession, workers = 8)
lri_raw_db <- future_map2_dfr(param_grid$streak_length, param_grid$times,
                                     ~ interpolate_all(.x, .y),
                                     .progress = TRUE,
                                     .options = furrr_options(seed = TRUE))
plan(sequential)
unique(lri_raw_db$.model)

# We add the ensemble mean
lri_db <- bind_rows(
  lri_raw_db,
  lri_raw_db |>
    filter(.model %in% c("EI AGGDP", "EI", "EI AGOUTPUT", "AMELIA")) |>
    group_by(year, iso3c, variable, streak_length) |>
    summarize(interpolation = mean(interpolation, na.rm = TRUE),
              .groups = "drop") |>
    mutate(.model = "EI ENS"),
  lri_raw_db |>
    filter(.model %in% c("LI AGGDP", "LI", "LI AGOUTPUT", "AMELIA")) |>
    group_by(year, iso3c, variable, streak_length) |>
    summarize(interpolation = mean(interpolation, na.rm = TRUE),
              .groups = "drop") |>
    mutate(.model = "LI ENS")
)  |>
  left_join(lri_longest_streak |>
              dplyr::select(iso3c, year, value, variable),
            by = c("iso3c", "year", "variable")) |>
  filter(value != interpolation) # to filter out imputed variables from AMELIA


# Calculate accuracy statistics. We remove three small islands with 0 hr values.
lri_accuracy_db <- lri_db |>
  group_by(.model, iso3c, variable, streak_length) |>
  summarise(
    MAE = mae_vec(value, interpolation, na_rm = TRUE),
    RMSE = rmse_vec(value, interpolation, na_rm = TRUE),
    MAPE = mape_vec(value, interpolation, na_rm = TRUE),
    MASE = ifelse(sum(!is.na(value)) <= 1, NA, mase_vec(value, interpolation, na_rm = TRUE)),
    .groups = "drop")

okabe_ito_palette <- palette.colors(palette = "Okabe-Ito")
names(okabe_ito_palette) <- NULL

fig_lri <- lri_accuracy_db |>
  mutate(variable = ifelse(variable == "hr", "Number of Researchers (HR)", "R&D expenditures (RD)")) |>
  ggplot(aes(x = .model, y = MASE, fill = .model)) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(outliers = FALSE) +
  labs(y = "Mean absolute scaled error (MASE)", x = NULL, color = NULL, fill = NULL) +
  scale_fill_manual(values = okabe_ito_palette) +
  theme_bw() +
  facet_grid(streak_length~variable) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    plot.margin = margin(t = 10, r = 10, b = 0, l = 10),
    axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5, hjust = 1)
    ) +
  coord_cartesian(ylim = c(0, 6)) +
  guides(fill = "none")


