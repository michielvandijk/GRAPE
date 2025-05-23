# ========================================================================================
# Project:  GRAPE
# Subject:  Script to prepare figures for manuscript
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
p_load(here, tidyverse, readxl, scales, glue)

# Load additional packages
p_load(viridis, sf, patchwork, ggpubr, rnaturalearth, countrycode, ggrepel,
       ggthemes)

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

# grape_db
grape_db_raw <- read_excel(file.path(db_path, glue("grape_{db_version}.xlsx")), sheet = "grape")

# Read macro db
macro_db <- read_excel(file.path(db_path, glue("macro_db_{macro_db_version}.xlsx")), sheet = "macro_db")


# ========================================================================================
# PROCESS --------------------------------------------------------------------------------
# ========================================================================================

# usa deflator
usa_deflator <- macro_db |>
  dplyr::select(iso3c, year, deflator) |>
  filter(iso3c == "USA")

grape_db <- grape_db_raw |>
  filter(year >= sy, year <= ey) |>
  mutate(imputed = case_when(
    pre_processing %in% c("ei","i_aux") ~ "Imputed",
    linking %in% c("ei","i_ens", "i_man", "i_naive") ~ "Imputed",
    .default = source),
    category = case_when(
      imputed == "Imputed" ~ "Imputed",
      source %in% c("ASTI (2014)", "ASTI (2024a)", "ASTI (2024b)") ~ "ASTI (various)",
      source %in% c("Heisey & Fuglie (2018)") ~ "Heisey & Fuglie (2018)",
      source %in% c("EUROSTAT (2024) & OECD (2024)") ~ "EUROSTAT (2024) & OECD (2024)",
      source %in% c("Pardey & Roseboom (1989)", "Pardey et al. (1991)") ~ "Pardey et al. (1989 & 1991)",
       .default = "Various"),
    category = factor(category, levels = c("Imputed", "ASTI (various)", "EUROSTAT (2024) & OECD (2024)",
                                           "Pardey et al. (1989 & 1991)",
                                           "Heisey & Fuglie (2018)", "Various")))
col_vir <- viridis_pal()(6)
names(col_vir) <- c("Imputed", "ASTI (various)", "EUROSTAT (2024) & OECD (2024)",
                    "Pardey et al. (1989 & 1991)",
                    "Heisey & Fuglie (2018)", "Various")

table(grape_db$pre_processing)
table(grape_db$linking)
table(grape_db$imputed)
table(grape_db$source)
table(grape_db$category)


# ========================================================================================
# IMPUTED SHARE PER SOURCE ---------------------------------------------------------------
# ========================================================================================

# Set period
periods <- c(seq(1960, 2020, 10), 2022)
periods_df <- data.frame(length = diff(periods),
                         periods = c("1960-69", "1970-79", "1980-89", "1990-99", "2000-09", "2010-19", "2020-22"))

# Tables
tab_hr <- grape_db |>
  filter(variable == "HR") |>
  mutate(periods = cut(year, periods, include.lowest = T, right = F,
                       labels = c("1960-69", "1970-79", "1980-89", "1990-99", "2000-09", "2010-19", "2020-22"))) |>
  ungroup() |>
  left_join(periods_df) |>
  group_by(periods, variable) |>
  mutate(obs = n(),
         value_tot = sum(value, na.rm = TRUE)) |>
  ungroup() |>
  group_by(periods, variable, category) |>
  summarize(sh = n()/unique(obs),
            value_sh = sum(value, na.rm = T)/unique(value_tot),
            .groups = "drop") |>
  pivot_longer(-c(periods, variable, category), names_to = "type", values_to = "value") |>
  mutate(type = factor(type, levels = c("sh", "value_sh"),
                       labels = c("Share of observations", "Share of number of researchers")))

tab_rd <- grape_db |>
  filter(variable == "RD") |>
  mutate(periods = cut(year, periods, include.lowest = T, right = F,
                       labels = c("1960-69", "1970-79", "1980-89", "1990-99", "2000-09", "2010-19", "2020-22"))) |>
  ungroup() |>
  left_join(periods_df) |>
  group_by(periods, variable) |>
  mutate(obs = n(),
         value_tot = sum(value, na.rm = TRUE)) |>
  ungroup() |>
  group_by(periods, variable, category) |>
  summarize(sh = n()/unique(obs),
            value_sh = sum(value, na.rm = T)/unique(value_tot),
            .groups = "drop") |>
  pivot_longer(-c(periods, variable, category), names_to = "type", values_to = "value") |>
  mutate(type = factor(type, levels = c("sh", "value_sh"),
                       labels = c("Share of observations", "Share of R&D expenditures")))

# rd plots
fig_source_rd <- tab_rd |>
  filter(variable == "RD") |>
  ggplot(aes(fill = category, y = value, x = periods)) +
  geom_bar(stat="identity", position="fill") +
  scale_y_continuous(labels = percent_format(), expand = c(0,0)) +
  theme_classic() +
  labs(x = NULL, y = NULL, fill = NULL) +
  facet_wrap(~type) +
  scale_fill_manual(values = col_vir) +
  geom_text(aes(label=ifelse(value >= 0.03, paste0(sprintf("%.0f", value * 100),"%"),"")),
            position=position_stack(vjust=0.5), colour="white") +
  theme(strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(size = 15)) +
  labs_pubr()

# hr plot
fig_source_hr <- tab_hr |>
  filter(variable == "HR") |>
  ggplot(aes(fill = category, y = value, x = periods)) +
  geom_bar( stat="identity", position="fill") +
  scale_y_continuous(labels = percent_format(), expand = c(0,0)) +
  theme_classic() +
  labs(x = NULL, y = NULL, fill = NULL) +
  facet_wrap(~type) +
  scale_fill_manual(values = col_vir) +
  geom_text(aes(label=ifelse(value >= 0.03, paste0(sprintf("%.0f", value*100),"%"),"")),
            position=position_stack(vjust=0.5), colour="white") +
  theme(strip.background = element_rect(colour="white", fill="white"),
        strip.text.x = element_text(size = 15)) +
  guides(fill = "none") +
  labs_pubr()

# Combine
fig_source <- fig_source_rd + fig_source_hr +
  plot_layout(ncol = 1, guides = "collect") +
  plot_annotation(tag_levels = 'a')


# ========================================================================================
# MAP SHARE IMPUTED ----------------------------------------------------------------------
# ========================================================================================

# Calculate share imputed per country and variable, and add share imputed code
sh_imp <- grape_db |>
  group_by(iso3c, variable) |>
  summarize(n = n(),
            n_imp = sum(imputed == "Imputed", na.rm = TRUE),
            sh_imp = n_imp/n*100,
            min_year = min(year),
            imputed = cut(sh_imp, c(0, 25, 50, 100), include.lowest = T, right = T,
                          labels = c("green", "orange", "red")),
            .groups = "drop") |>
  mutate(region = countrycode(iso3c, "iso3c", "region"),
         continent = countrycode(iso3c, "iso3c", "continent"))

world_map <- ne_countries(returnclass = 'sf') |>
  dplyr::select(iso3c = adm0_a3, admin) |>
  mutate(country = countrycode(iso3c, "iso3c", "country.name")) |>
  filter(iso3c != "ATA")

# rd share imputation map
col <- c("red", "forestgreen", "orange")
names(col) <- c("red", "green", "orange")

rd_imputed_map_df <- world_map |>
  left_join(sh_imp |>
              filter(variable == "RD"))

fig_rd_imputed <- ggplot(rd_imputed_map_df) +
  geom_sf(aes(fill = imputed), colour = "black") +
  scale_fill_manual(values = col,
                    labels = c("<25% imputed", "25-50% imputed", ">50% imputed", "no data"),
                    na.value = "grey50") +
  theme_void() +
  labs(fill = NULL) +
  guides(fill = "none")


# hr share imputation map
hr_imputed_map_df <- world_map |>
  left_join(sh_imp |>
              filter(variable == "HR"))

fig_hr_imputed <- ggplot(hr_imputed_map_df) +
  geom_sf(aes(fill = imputed), colour = "black") +
  scale_fill_manual(values = col,
                    labels = c("<25% imputed", "25-50% imputed", ">50% imputed", "no data"),
                    na.value = "grey50") +
  theme_void() +
  labs(fill = NULL) +
  theme(legend.justification = c(0, 0), legend.position = c(0.1, 0.15))

# Combine
fig_imputed <- (fig_rd_imputed / fig_hr_imputed )  +
  plot_annotation(tag_levels = 'a')


# ========================================================================================
# VALIDATION -----------------------------------------------------------------------------
# ========================================================================================

# COMPARE WITH PARDEY ET AL 2016 ---------------------------------------------------------

# Convert from 2009 to 2017 USD
usd_conv_0917 <- usa_deflator$deflator[usa_deflator$year == 2017]/usa_deflator$deflator[usa_deflator$year == 2009]

# Load Pardey data and convert to USD 2017
pardey_db <- read_excel(here(glue("{db_version}/data/Pardey_et_al_2016.xlsx")), sheet = "agPERD") |>
  pivot_longer(-region, names_to = "year", values_to = "value") |>
  mutate(year = as.integer(year),
         value = value*1000 * usd_conv_0917,
         iso3c = countrycode(region, "country.name", "iso3c"),
         database = "Pardey") |>
  filter(!is.na(iso3c)) |>
  dplyr::select(-region)

# Combine
compare_pardey_db <- bind_rows(
  filter(grape_db, iso3c %in% pardey_db$iso3c,
         year %in% c(1980, 1990, 2000, 2005, 2010, 2011),
         variable == "RD") |>
    mutate(database = "GRAPE") |>
    ungroup() |>
    dplyr::select(iso3c, year, value, database),
  pardey_db) |>
  pivot_wider(names_from = database, values_from = value) |>
  mutate(region = countrycode(iso3c, "iso3c", "region"))

# Plot
cb_palette_29 <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999", "#A6761D",
  "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999",
  "#8DD3C7", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD",
  "#CCEBC5", "#FFED6F", "black"
)

# Show colors
#scales::show_col(cb_palette_20)

fig_val_pardey <- ggplot(data = compare_pardey_db, aes(x = log(Pardey), y = log(GRAPE), label = iso3c)) +
  geom_point(aes(colour = iso3c), alpha = 0.5, size = 3) +
  theme_bw() +
  geom_abline(slope = 1, intercept = 0) +
  coord_fixed() +
  labs(x = "Pardey et al. (2016) (log million 2017 PPP$)", y =  "GRAPE (log million 2017 PPP$)") +
  scale_y_continuous(limits = c(2, 9), labels = comma) +
  scale_x_continuous(limits = c(2, 9), labels = comma) +
  scale_color_manual(values = cb_palette_29) +
  geom_text_repel(data = filter(compare_pardey_db, year == 1990),
                  box.padding = 0.7,        # Add padding around text
                  point.padding = 0.5,      # Add padding around points
                  min.segment.length = 0,
                  seed = 10,
                  max.overlaps = Inf,
                  segment.color = "grey50", # Set connector line color
                  segment.size = 0.5) +        # Set connector line thickness)
  theme(legend.position = "right") +
  guides(color = "none") +
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01) +
  labs_pubr() +
  rremove("grid")
library(plotly)


# ========================================================================================
# COVERAGE -------------------------------------------------------------------------------
# ========================================================================================

# GDP and AG GDP coverage
countrycode(setdiff(grape_db$iso3c, macro_db$iso3c), "iso3c", "country.name")
countrycode(setdiff(macro_db$iso3c, grape_db$iso3c), "iso3c", "country.name")

macro_db |>
  filter(year == ey) |>
  mutate(coverage = ifelse(iso3c %in% grape_db$iso3c, 1, 0),
         gdp_ppp = gdp_cap_ppp * population) |>
  summarize(cov_gdp_ppp = sum(gdp_ppp * coverage, na.rm = TRUE)/sum(gdp_ppp, na.rm = TRUE) * 100,
            cov_ag_gdp_ppp = sum(ag_gdp_ppp * coverage, na.rm = TRUE)/sum(ag_gdp_ppp, na.rm = TRUE) * 100)

# Coverage over time
min_year <- grape_db_raw |>
  group_by(iso3c, variable) |>
  summarize(min_year = min(year, na.rm = TRUE),
            .groups = "drop") |>
  group_by(variable) |>
  count(min_year) |>
  ungroup()

fig_coverage <- grape_db_raw |>
  mutate(region = countrycode(iso3c, "iso3c", "region")) |>
  group_by(year, variable, region) |>
  summarize(n = n(),
            .groups = "drop") |>
  filter(variable == "RD") |>
  ggplot() +
  geom_area(aes(x = year, y = n, fill = region)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  scale_fill_colorblind() +
  labs(y = "Number of countries", x = NULL, fill = NULL) +
  facet_wrap(~region, scales = "free_y") +
  theme_bw() +
  theme(strip.background = element_rect(colour = "transparent", fill = "transparent")) +
  labs_pubr() +
  rremove("grid") +
  guides(fill = "none")


# ========================================================================================
# EXAMPLE --------------------------------------------------------------------------------
# ========================================================================================

example_db <- grape_db |>
  filter(iso3c == "KOR") |>
  arrange(variable, year) |>
  mutate(
    source = ifelse(source == "Imputed", linking, source),
    class_change = source != lag(source, default = first(source)),
    segment_id = cumsum(class_change),
    group_id = interaction(source, segment_id, drop = TRUE)
  )

cb_palette <- c(
  "#56B4E9", "#009E73",  "#0072B2", "#D55E00", "#999999", "#CC79A7", "#A73030", "black"
)

shapes <- c(1, 4, 8, 16, 17, 9, 19, 0, 3, 10, 12)
sources <- unique(example_db$source)

fixed_color <- "#E69F00"
names(fixed_color) <- "i_ens"
fixed_shape <- 15
names(fixed_shape) <- "i_ens"
other_sources <- setdiff(sources, "i_ens")
other_colors <- cb_palette[seq_along(other_sources)]
names(other_colors) <- other_sources
other_shapes <- setdiff(sources, "I_ens")
other_shapes <- shapes[seq_along(other_shapes)]
names(other_shapes) <- other_sources
color_map <- c(fixed_color, other_colors)
shape_map <- c(fixed_shape, other_shapes)

fig_example <- example_db |>
  mutate(variable2 = ifelse(variable == "RD", "R&D expenditures (million 2017 PPP$)", "Number of researchers (FTE)")) |>
  ggplot(aes(x = year, y = value)) +
  facet_grid(rows = vars(variable2), switch = "y", scales = "free") +
  geom_point(aes(color = source, shape = source), size = 2) +
  geom_ribbon(aes(ymin = lower,
                  ymax = upper,
                  fill = source,
                  group = group_id), alpha=0.25,
              show.legend = FALSE) +
  scale_shape_manual(values = shape_map) +
  scale_fill_manual(values = color_map) +
  scale_color_manual(values = color_map) +
  geom_line(aes(y = value), color = "black") +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(labels = comma, breaks = pretty_breaks()) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  ) +
  theme_bw() +
  theme(
    strip.placement = "outside",
    strip.background = element_blank()
  ) +
  labs(color = NULL, shape = NULL, fill = NULL, x = NULL, y = NULL) +
  labs_pubr() +
  rremove("grid")

