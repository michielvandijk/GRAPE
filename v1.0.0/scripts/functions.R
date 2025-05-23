# ========================================================================================
# FUNCTIONS ------------------------------------------------------------------------------
# ========================================================================================

# Function to forecast using time-series econometrics
fc_time_series <- function(df) {
  df_ts <- as_tsibble(df, index = year)
  model <- df_ts |>
    filter(!is.na(value)) |>
    model(
      SES = ETS(value ~ error("A") + trend("N") + season("N")),
      DT = ETS(value ~ error("A") + trend("Ad") + season("N"))
    )
  proj_data <- df_ts |>
    filter(is.na(value))
  fc <- forecast(model, new_data = proj_data) |>
    as_tibble() |>
    dplyr::select(.model, iso3c, year, forecast = .mean)
  return(fc)
}

# Function to forecast using NAIVE
fc_naive <- function(df) {
  fc <- df |>
    arrange(year) |>
    mutate(
      forecast = na_locf(value)) |>
    filter(is.na(value)) |>
    mutate(.model = "NAIVE") |>
    dplyr::select(.model, iso3c, year, forecast)
  return(fc)
}

# Function to forecast using ag gdp index
fc_ag_gdp <- function(df) {
  fc <- df |>
    left_join(macro_db, by = c("iso3c", "year")) |>
    mutate(var_ag_gdp = value / ag_gdp_i) |>
    arrange(year) |>
    mutate(
      var_ag_gdp = na_locf(var_ag_gdp)) |>
    filter(is.na(value)) |>
    mutate(forecast = ag_gdp_i * var_ag_gdp) |>
    mutate(.model = "AGGDP") |>
    dplyr::select(.model, iso3c, year, forecast)
  return(fc)
}

# Function to forecast using ag output
fc_ag_output <- function(df) {
  fc <- df |>
    left_join(macro_db, by = c("iso3c", "year")) |>
    mutate(var_ag_output = value / ag_output_i) |>
    arrange(year) |>
    mutate(
      var_ag_output = na_locf(var_ag_output)) |>
    filter(is.na(value)) |>
    mutate(forecast = ag_output_i * var_ag_output) |>
    mutate(.model = "AGOUTPUT") |>
    dplyr::select(.model, iso3c, year, forecast)
  return(fc)
}

# Function to forecast using hr/rd
fc_hr_rd <- function(df, so, ta) {
  if(nrow(df[df$variable == ta,]) == 0) {
    t <- df |>
      filter(variable == ta)
    return(t)
  } else {
    t <- df |>
      filter(variable == ta) |>
      arrange(year) |>
      filter(!is.na(value))
  }
  if(nrow(df[df$variable == so,]) == 0) {
    s <- df |>
      filter(variable == so)
  } else {
    s <- df |>
      filter(variable == so) |>
      arrange(year) |>
      filter(!is.na(value))
  }
  overlap <- intersect(s$year, t$year)
  difference <- setdiff(s$year, t$year)
  bc <- NULL
  fc <- NULL

  if(length(overlap) > 0 & length(difference) > 0) {
    if(length(s$year[s$year < min(overlap)]) > 0 & s$value[s$year == min(overlap)] != 0) {
      min_ol <- min(overlap)
      bc <- s |>
        mutate(index = value/value[year == min_ol]) |>
        filter(year %in% year[year < min_ol]) |>
        mutate(value = t$value[t$year == min_ol] * index,
               value = ifelse(value < 0, 0, value),
               variable = ta) |>
        dplyr::select(-index) |>
        mutate(linking = case_when(
          variable == "hr" ~ li$linking[11],
          variable == "rd" ~ li$linking[10],
          .default = NA_character_)) |>
        dplyr::rename(forecast = value)
    }
    if(length(s$year[s$year > max(overlap)]) > 0 & s$value[s$year == max(overlap)] != 0) {
      max_ol <- max(overlap)
      fc <- s |>
        mutate(index = value/value[year == max_ol]) |>
        filter(year %in% year[year > max_ol]) |>
        mutate(value = t$value[t$year == max_ol] * index,
               variable = ta) |>
        dplyr::select(-index) |>
        mutate(linking = case_when(
          variable == "hr" ~ li$linking[11],
          variable == "rd" ~ li$linking[10],
          .default = NA_character_)) |>
        dplyr::rename(forecast = value)
    }
  }
  return(bind_rows(bc, fc))
}

# Function to forecast using gbard
fc_gbard <- function(df) {
  fc <- df |>
    left_join(rd_gbard |>
                dplyr::select(year, iso3c, gbard = value)) |>
    mutate(var_gbard = value / gbard) |>
    arrange(year)
  if(all(is.na(fc$var_gbard))) {
    return(data.frame())
  } else {
    fc <- fc |>
      mutate(var_gbard = na_locf(var_gbard)) |>
      filter(is.na(value)) |>
      mutate(forecast = gbard * var_gbard) |>
      mutate(.model = "GBARD") |>
      dplyr::select(.model, iso3c, year, forecast)
    if(nrow(fc) == 0){
      return(data.frame())
    } else {
      return(fc)
    }
  }
}


# Function to interpolate using ag gdp index
int_li_ag_gdp <- function(df) {
  int <- df |>
    left_join(macro_db, by = c("iso3c", "year")) |>
    mutate(var_ag_gdp = value / ag_gdp_i) |>
    arrange(year) |>
    mutate(
      var_ag_gdp = na_interpolation(var_ag_gdp, yleft = NA , yright = NA),
      interpolation = ag_gdp_i * var_ag_gdp,
      .model = "LI AGGDP" ) |>
    filter(is.na(value)) |>
    dplyr::select(.model, source, variable, iso3c, year, interpolation)
  return(int)
}

int_ei_ag_gdp <- function(df) {
  int <- df |>
    left_join(macro_db, by = c("iso3c", "year")) |>
    mutate(var_ag_gdp = value / ag_gdp_i) |>
    arrange(year) |>
    mutate(
      var_ag_gdp = exp(na_interpolation(log(var_ag_gdp), yleft = NA , yright = NA)),
      interpolation = ag_gdp_i * var_ag_gdp,
      .model = "EI AGGDP" ) |>
    filter(is.na(value)) |>
    dplyr::select(.model, source, variable, iso3c, year, interpolation)
  return(int)
}

# Function to interpolate using ag output index
int_li_ag_output <- function(df) {
  int <- df |>
    left_join(macro_db, by = c("iso3c", "year")) |>
    mutate(var_ag_output = value / ag_output_i) |>
    arrange(year) |>
    mutate(
      var_ag_output = na_interpolation(var_ag_output, yleft = NA , yright = NA),
      interpolation = ag_output_i * var_ag_output,
      .model = "LI AGOUTPUT" ) |>
    filter(is.na(value)) |>
    dplyr::select(.model, source, variable, iso3c, year, interpolation)
  return(int)
}

int_ei_ag_output <- function(df) {
  int <- df |>
    left_join(macro_db, by = c("iso3c", "year")) |>
    mutate(var_ag_output = value / ag_output_i) |>
    arrange(year) |>
    mutate(
      var_ag_output = exp(na_interpolation(log(var_ag_output), yleft = NA , yright = NA)),
      interpolation = ag_output_i * var_ag_output,
      .model = "EI AGOUTPUT" ) |>
    filter(is.na(value)) |>
    dplyr::select(.model, source, variable, iso3c, year, interpolation)
  return(int)
}

# Function to interpolate using growth rate
int_li <- function(df) {
  int <- df |>
    arrange(year) |>
    mutate(
      interpolation = na_interpolation(value, yleft = NA , yright = NA),
      .model = "LI" ) |>
    filter(is.na(value)) |>
    dplyr::select(.model, source, variable, iso3c, year, interpolation)
  return(int)
}

int_ei <- function(df) {
  int <- df |>
    arrange(year) |>
    mutate(
      interpolation = exp(na_interpolation(log(value), yleft = NA , yright = NA)),
      .model = "EI" ) |>
    filter(is.na(value)) |>
    dplyr::select(.model, source, variable, iso3c, year, interpolation)
  return(int)
}

# Function to interpolate using amelia
int_amelia <- function(df, streak_length){

  process_amelia <- function(amelia_db){
    amelia_out <- map_dfr(amelia_db[["imputations"]], bind_rows, .id = "imp_nr") |>
      dplyr::select(iso3c, year, value) |>
      group_by(year, iso3c) |>
      summarise(value = mean(value, na.rm = TRUE),
                .groups = "drop")
    return(amelia_out)
  }

  run_amelia <- function(df){
    detectCores(all.tests = FALSE, logical = TRUE)
    numCores = detectCores() - 2

    amelia_db <- df |>
      dplyr::select(-variable)
    amelia_db <- amelia(amelia_db, m = 5, ts = "year", cs = "iso3c", p2s=2,
                        intercs = TRUE,
                        lags = c("value"),
                        leads = c("value"),
                        polytime = 3,
                        empri = 0.01*nrow(amelia_db),
                        logs = c("value", "ag_gdp_ppp", "gdp_cap_ppp", "population"),
                        parallel = "snow",  ncpus = numCores, cl = makePSOCKcluster(numCores))
    plan(sequential)
    amelia_db <- process_amelia(amelia_db) |>
      mutate(.model = "AMELIA") |>
      dplyr::rename(interpolation = value)
    return(amelia_db)
  }

  db <- df |>
    left_join(macro_db) |>
    dplyr::select(year, iso3c, variable, value, ag_gdp_ppp, gdp_cap_ppp, ag_output_i, ag_gdp_share, population, rural_pop_share) |>
    split(~variable) |>
    map(~run_amelia(.x)) |>
    bind_rows(.id = "variable")
  return(db)
}

# Add source data and imputation method to imputation output
add_info <- function(imp_input, method){
  source_rd <- rd_raw |>
    dplyr::select(iso3c, year, source, processing, linking)

  source_hr <- hr_raw |>
    dplyr::select(iso3c, year, source, processing, linking)

  imp_output <- bind_rows(
    imp_input |>
      filter(variable == "rd") |>
      left_join(source_rd) |>
      arrange(iso3c, year) |>
      mutate(
        linking = ifelse(is.na(linking) & !is.na(value), method, linking)),
    imp_input |>
      filter(variable == "hr") |>
      left_join(source_hr) |>
      arrange(iso3c, year) |>
      mutate(
        linking = ifelse(is.na(linking) & !is.na(value), method, linking))
  ) |>
    mutate(processing = ifelse(is.na(processing), pr$processing[1], processing),
           source = ifelse(is.na(source), "imputed", source))
  return(imp_output)
}

# Function to run all forecast approaches
forecast_all <- function(df_raw, impute_length){
  df <- df_raw |>
    group_by(variable) |>
    arrange(year) |>
    mutate(value = ifelse(year %in% c(max(year) - (impute_length-1)): max(year), NA_real_, value)) |>
    ungroup()

  ts_db <-  df |>
    split(~iso3c + variable) |>
    map_dfr(fc_time_series, .id = "group") |>
    separate(group, sep = "\\.", into = c("iso3c", "variable"))

  naive_db <-  df |>
    split(~iso3c + variable) |>
    map_dfr(fc_naive, .id = "group") |>
    separate(group, sep = "\\.", into = c("iso3c", "variable"))

  ag_gdp_db <- df |>
    split(~iso3c + variable) |>
    map_dfr(fc_ag_gdp, .id = "group") |>
    separate(group, sep = "\\.", into = c("iso3c", "variable"))

  ag_output_db <- df |>
    split(~iso3c + variable) |>
    map_dfr(fc_ag_output, .id = "group") |>
    separate(group, sep = "\\.", into = c("iso3c", "variable"))

  gbard_db <- df |>
    split(~iso3c + variable) |>
    map_dfr(fc_gbard, .id = "group") |>
    separate(group, sep = "\\.", into = c("iso3c", "variable"))

  hr_rd_db <- bind_rows(
    df_raw |>
      mutate(value = ifelse(variable == "rd" & year %in% c(max(year) - (impute_length-1)): max(year), NA_real_, value)) |>
      fc_hr_rd(so = "hr", ta = "rd"),
    df_raw |>
      mutate(value = ifelse(variable == "hr" & year %in% c(max(year) - (impute_length-1)): max(year), NA_real_, value)) |>
      fc_hr_rd(so = "rd", ta = "hr")) |>
    mutate(.model = "HR/RD")

  db <- bind_rows(
    ts_db,
    naive_db,
    ag_gdp_db,
    ag_output_db,
    gbard_db,
    hr_rd_db
  )
  return(db)
}

# General function to compute the longest streak of consecutive non-NA values
longest_non_na_streak <- function(vec) {
  if (all(is.na(vec))) {
    return(0)  # If all values are NA, return 0
  }

  rle_result <- rle(!is.na(vec))  # Run-Length Encoding of non-NA values
  max(rle_result$lengths[rle_result$values], na.rm = TRUE)  # Extract max streak
}

# Function to set streak to NA, ensuring start and end value to interpolate
random_na_streak <- function(df, streak_length = 5) {
  n <- nrow(df)  # Number of rows

  if (n < streak_length + 2) return(df)  # Skip if streak is longer than data or if we can't have non-NA values before and after streak

  # Choose a random start position ensuring the streak fits and has non-NA boundaries
  start_pos <- sample(2:(n - streak_length - 1), 1)  # Ensure there are non-NA values before and after the streak
  end_pos <- start_pos + streak_length - 1

  # Set the selected range to NA (excluding the boundaries)
  df$value[start_pos:end_pos] <- NA
  return(df)
}

# Function to run all imputation approaches
interpolate_all <- function(streak_length, times){
  combine_int <- function(df){
    int <- bind_rows(
      int_ei_ag_gdp(df),
      int_li_ag_gdp(df),
      int_ei_ag_output(df),
      int_li_ag_output(df),
      int_ei(df),
      int_li(df)
    )
    return(int)
  }

  input <- lri_longest_streak |>
    group_by(iso3c, source, variable) |>
    group_split() |>
    map(~ random_na_streak(.x, streak_length))

  int_db <- bind_rows(
    input |>
      map_df(~ combine_int(.x)),
    bind_rows(input) |>
      int_amelia(streak_length)) |>
    mutate(iteration = times,
           streak_length = streak_length)

  return(int_db)
}
