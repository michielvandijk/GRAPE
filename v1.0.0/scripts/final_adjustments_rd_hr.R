# ========================================================================================
# Project:  GRAPE
# Subject:  Script to prepare database
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
p_load(countrycode, patchwork, tictoc, openxlsx)

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
# LOAD DATA ------------------------------------------------------------------------------
# ========================================================================================

# Read imputed database
source(here(glue("{db_version}/scripts/impute_hr_rd.R")))

# Linking
li <- read.csv(here(glue("{db_version}/data/linking_list.csv")))

# Sources list
so <- read_csv(here(glue("{db_version}/data/sources_list.csv"))) |>
  select(ref_short, iso3c)

# ppp_by_db
ppp_by_db <- readRDS(file.path(db_path, glue("ppp_{by}_db_{macro_db_version}.rds")))

# xr_by_db
xr_by_db <- readRDS(file.path(db_path, glue("xr_{by}_db_{macro_db_version}.rds")))


# ========================================================================================
# FUNCTIONS ------------------------------------------------------------------------------
# ========================================================================================

plot_source_info  <- function(iso3c_sel, db){
  print(iso3c_sel)
  country <- countrycode(iso3c_sel, "iso3c", "country.name")
  title = glue("{country} ({iso3c_sel})")

  rd_df <- db |>
    filter(iso3c == iso3c_sel, variable == "rd")

  rd_p <- rd_df |>
    ggplot() +
    geom_point(aes(x = year, y = value, shape = source, color = source), size = 2) +
    geom_line(aes(x = year, y = value), color = "black") +
    geom_vline(xintercept = c(sy, ey), linetype = "dashed") +
    scale_shape_manual(values = seq(0,15)) +
    scale_x_continuous(
      limits = c(min(c(rd_df$year, sy)), 2025),
      breaks = seq(min(c(rd_df$year, sy)), 2025, 5)) +
    labs(title = "R&D expenditures", y = "USD 2017 PPP", x = NULL) +
    theme(
      legend.position = "bottom",        # Move legend to the bottom
      legend.direction = "horizontal"   # Arrange legend items horizontally
    ) +
    guides(color = guide_legend(ncol = 2), shape = guide_legend(ncol = 2))

  hr_df <- db |>
    filter(iso3c == iso3c_sel, variable == "hr")

  hr_p <- hr_df |>
    ggplot() +
    geom_point(aes(x = year, y = value, shape = source, color = source), size = 2) +
    geom_line(aes(x = year, y = value), color = "black") +
    geom_vline(xintercept = c(sy, ey), linetype = "dashed") +
    scale_shape_manual(values = seq(0,15)) +
    scale_x_continuous(
      limits = c(min(c(hr_df$year, sy)), 2025),
      breaks = seq(min(c(hr_df$year, sy)), 2025, 5)) +
    labs(title = "Number of researchers", y = "People", x = NULL) +
    theme(
      legend.position = "bottom",        # Move legend to the bottom
      legend.direction = "horizontal"   # Arrange legend items horizontally
    ) +
    guides(color = guide_legend(ncol = 2), shape = guide_legend(ncol = 2))

  p <- (rd_p | hr_p) +
    plot_annotation(
      title = title,
      theme = theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")))
  print(p)
}


# ========================================================================================
# COUNTRY ADJUSTMENTS --------------------------------------------------------------------
# ========================================================================================

hr_rd_db <- imp_ensemble_db

# SOLOMON ISLANDS (SLB) ------------------------------------------------------------------

# We impute up to 2001 (lowest point) and assume R&D spending stays constant onwards because of impact of conflict and
# destruction of research facilities.
# We set the lower and upper equal to the imputed value
plot_source_info ("SLB", hr_rd_db)

SLB <- bind_rows(
  hr_rd_db |>
    filter(iso3c == "SLB", variable == "rd") |>
    mutate(value = ifelse(year > 2001, NA_real_, value),
           value = na_locf(value),
           upper = ifelse(year > 2001, value, upper),
           lower = ifelse(year > 2001, value, lower),
           linking = ifelse(year > 2001, li$linking[17], linking)),
  hr_rd_db |>
    filter(iso3c == "SLB", variable == "hr") |>
    mutate(value = ifelse(year > 2001, NA_real_, value),
           value = na_locf(value),
           upper = ifelse(year > 2001, value, upper),
           lower = ifelse(year > 2001, value, lower),
           linking = ifelse(year > 2001, li$linking[17], linking))
)

plot_source_info ("SLB", SLB)

hr_rd_db <- bind_rows(
  hr_rd_db |>
    filter(iso3c != "SLB"),
  SLB
)
rm(SLB)


# MONTSERRAT (MSR) -----------------------------------------------------------------------

# Ag GDP data is missing for recent years, resulting in biased imputation.
# We apply NAIVE imputation instead
plot_source_info ("MSR", hr_rd_db)

MSR <- hr_rd_db |>
  filter(iso3c == "MSR") |>
  mutate(value = ifelse(source == "Imputed", NA_real_, value),
         upper = ifelse(source == "Imputed", NA_real_, value),
         lower = ifelse(source == "Imputed", NA_real_, value)) |>
  group_by(iso3c, variable) |>
  mutate(value = na_locf(value),
         upper = na_locf(upper),
         lower = na_locf(lower),
         linking = ifelse(source == "Imputed", li$linking[13], linking))

plot_source_info ("MSR", MSR)

hr_rd_db <- bind_rows(
  hr_rd_db |>
    filter(iso3c != "MSR"),
  MSR
)
rm(MSR)


# ========================================================================================
# CHECKS ---------------------------------------------------------------------------------
# ========================================================================================

# Check duplicates
check_duplicates <- hr_rd_db |>
  group_by(iso3c, variable, year) |>
  mutate(n = n()) |>
  filter(n > 1)
try(if(nrow(check_duplicates) > 0) stop("duplicates"))

# Check period coverage, i.e. if start and end year are the same for all country-variable series
check_series <- hr_rd_db |>
  group_by(iso3c) |>
  summarize(min_year_hr = min(year[variable == "hr"]),
            min_year_rd = min(year[variable == "rd"]),
            max_year_hr = max(year[variable == "hr"]),
            max_year_rd = max(year[variable == "rd"]),
            .groups = "drop") |>
  filter(min_year_hr != min_year_rd | max_year_hr != max_year_rd)
try(if(nrow(check_series) > 0) stop("Start and end of hr/rd series is not the same"))

# CHeck number of countries
n_distinct(hr_rd_db$iso3c)

# Check processing
table(hr_rd_db$processing)
n_distinct(hr_rd_db$processing)
try(if(any(is.na(hr_rd_db$processing))) stop("NA values"))

# Check linking
table(hr_rd_db$linking)
n_distinct(hr_rd_db$linking)
try(if(any(is.na(hr_rd_db$linking))) stop("NA values"))

# Check source
table(hr_rd_db$source)
n_distinct(hr_rd_db$source)
try(if(any(is.na(hr_rd_db$source))) stop("NA values"))


# ========================================================================================
# FINALIZE AND SAVE ----------------------------------------------------------------------
# ========================================================================================

grape_db <- hr_rd_db |>
  mutate(country = countrycode(iso3c, "iso3c", "country.name"),
         variable = toupper(variable),
         unit = ifelse(variable == "HR", "FTE", glue("million {by} PPP$"))) |>
  rename(pre_processing = processing) |>
  arrange(iso3c, variable, year) |>
  dplyr::select(country, iso3c, year, variable, unit, source, pre_processing, linking, value, lower, upper)

# Add documentation
var_grape_db <- data.frame(
  variable = names(grape_db),
  description = c(
    "Country name",
    "ISO three letter code",
    "Year of data",
    glue("Variable: HR - Number of public agricultural researchers (FTE), RD - Public agricultural R&D expenditures (million {by} PPP$)"),
    "Unit of variable",
    "Source of data, see GRAPE documentation for full references",
    "Code for data linking and imputation approaches, see GRAPE documentation for more information",
    "Code for quality control adjustments and treatment of missing data, see GRAPE documentation for more information",
    "Value",
    "Lower bound in case of ensemble imputation (i_ens)",
    "Upper bound in case of ensemble imputation (i_ens)")
)

# Set worksheet contents
ws_grape_db <- list("grape" = grape_db,
                    "var_grape" = var_grape_db)


# Create database folder
temp_path <- file.path(db_path, "database_test")
dir.create(temp_path, showWarnings = FALSE, recursive = TRUE)

# Save
write.xlsx(ws_grape_db, file.path(temp_path, glue("grape_{db_version}.xlsx")))


# ========================================================================================
# CREATE CONSTANT LCU AND US$ DB ---------------------------------------------------------
# ========================================================================================

# Missing xr and ppp
setdiff(unique(grape_db$iso3c), unique(xr_by_db$iso3c))
setdiff(unique(grape_db$iso3c), unique(ppp_by_db$iso3c))

# Prepare usd and con lcu
rd_db <- grape_db |>
  filter(variable == "RD") |>
  left_join(ppp_by_db |>
              dplyr::select(iso3c, usd_ppp)) |>
  left_join(xr_by_db |>
              dplyr::select(iso3c, usd_lcu_xr)) |>
  mutate(rd_con_lcu = value * usd_ppp,
         rd_con_usd = rd_con_lcu/usd_lcu_xr,
         lcu = countrycode(iso3c, "iso3c", "currency")) |>
  dplyr::select(country, iso3c, year, rd_ppp_usd = value, rd_con_usd, rd_con_lcu, lcu)

# Add documentation
var_rd_db <- data.frame(
  variable = names(rd_db),
  description = c(
    "Country name",
    "ISO three letter code",
    "Year of data",
    glue("Public agricultural R&D expenditures in million {by} PPP$, identical to main GRAPE data file"),
    glue("Public agricultural R&D expenditures in million {by} constant US$"),
    glue("Public agricultural R&D expenditures in million {by} constant local currency unit"),
    "local currency unit")
)

# Set worksheet contents
ws_rd_db <- list("rd" = rd_db,
                 "var_rd" = var_rd_db)

# Save
write.xlsx(ws_rd_db, file.path(temp_path, glue("rd_{db_version}.xlsx")))




