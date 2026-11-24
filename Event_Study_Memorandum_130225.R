


# Data sources:
# Bloomberg
# Risk-free rate: Board of Governors of the Federal Reserve System (US), 
#Market Yield on U.S. Treasury Securities at 3-Month Constant Maturity, 
#Quoted on an Investment Basis [DGS3MO], retrieved from FRED, 
#Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/DGS3MO, August 10, 2025. 


# Disclaimer: In the preparation of the R code, ChatGPT was used as a supportive tool for code generation and debugging.



# Load relevant libraries
library(xts)
library(quantmod)
library(tidyverse) 
library(dplyr)
library(tseries)
library(ggplot2)
library(car)
library(corrplot)
library(tidyr)
library(lubridate)
library(purrr)
library(tibble)





# ==============================================
# Read data
# ==============================================
# Data for the country analysis
CHN <- read.csv2("SHCOMP.csv",header=TRUE)
DEU <- read.csv2("DAX.csv",header=TRUE)
CAN <- read.csv2("SPTSX.csv",header=TRUE)
MEX <- read.csv2("MEXBOL.csv",header=TRUE)
VNM <- read.csv2("VNINDEX.csv",header=TRUE)

# Market index (MSCI World)
market_index_df = read.csv2("MXWO.csv",header=TRUE)

# Risk-free rate
rf <- read.csv("DGS3MO.csv",header=TRUE, stringsAsFactors = FALSE)

# Data for the industry analysis (Bloomberg World Index)
world <- read.csv2("World_Index_members_pxlast.csv",header=TRUE)

# Mapping table for the industry analysis
mapping_raw <- read.csv2("World_Index_member_data.csv",header=TRUE)

# Constituent weights of each country's index
CHN_weights <- read.csv2("SHCOMP_weights.csv",header=TRUE)
DEU_weights <- read.csv2("DAX_weights.csv",header=TRUE)
CAN_weights <- read.csv2("SPTSX_weights.csv",header=TRUE)
MEX_weights <- read.csv2("MEXBOL_weights.csv",header=TRUE)
VNM_weights <- read.csv2("VNINDEX_weights.csv",header=TRUE)



# ==============================================
# Data preparation
# ==============================================

#------------------------------------------------Country data
# Vector with the dataframe names as strings
dfs_country <- c("CHN","DEU","CAN", "MEX", "VNM")

for (df_name in dfs_country) {
  # Retrieve dataframe
  data_raw <- get(df_name)
  
  # Convert date
  Dates <- as.Date(data_raw$Dates, format="%d.%m.%Y")
  
  # New column "date" in first position, remove old first column
  data_raw <- cbind(Dates, data_raw[,-1])
  
  # Remaining columns: replace ',' with '.' and convert to numeric
  data_raw[,-1] <- lapply(data_raw[,-1], function(x) as.numeric(gsub(",", ".", x)))
  
  # Sort dataframe by date
  data_raw <- data_raw[order(data_raw$Dates), ]
  
  # Compute logarithmic returns
  data_raw <- data_raw %>%
    mutate(across(-Dates, ~ c(NA, diff(log(.)) * 100)))
  
  # Delete rows that contain only NA values in the return columns
  rendite_cols <- setdiff(names(data_raw), "Dates")
  data_raw <- data_raw[rowSums(!is.na(data_raw[rendite_cols])) > 0, ]
  
  # Delete columns that are completely empty
  data_raw <- data_raw[, colSums(!is.na(data_raw)) > 0]
  
  # Delete columns that have no values in 2025
  year2025_rows <- format(data_raw$Dates, "%Y") == "2025"
  rendite_cols <- setdiff(names(data_raw), "Dates")
  cols_with_2025_values <- sapply(data_raw[rendite_cols], function(x) any(!is.na(x[year2025_rows])))
  data_raw <- data_raw[, c("Dates", names(cols_with_2025_values)[cols_with_2025_values])]
  
  
  # Write dataframe back under its original name
  assign(df_name, data_raw)
}

# Adjust the weight dataframes per index so they can later be joined via the index name
for (df_name in dfs_country) {
  df_weights <- paste0(df_name, "_weights")
  if (exists(df_weights)) {
    df <- get(df_weights)
    df <- df %>% mutate(Index = make.names(Index))
    assign(df_weights, df, envir = .GlobalEnv)
  } else {
    warning(paste("Attention", df_weights, "does not exist"))
  }
}



#------------------------------------------------Market index
market_index_df$Dates <- as.Date(market_index_df$Dates, format = "%d.%m.%Y")
market_index_df <- market_index_df[order(market_index_df$Dates),]

# Compute logarithmic return
market_index_df <- market_index_df %>%
  mutate(across(-Dates, ~ c(NA, diff(log(.)) * 100)))


#------------------------------------------------Risk-free rate
rf$Dates <- as.Date(rf$observation_date, format = "%Y-%m-%d") # Date format
rf$observation_date <- NULL # Delete redundant column
names(rf)[names(rf) == "DGS3MO"] <- "rf" # Rename column

# Compute logarithmic return
rf <- rf %>%
  mutate(across(-Dates, ~ c(NA, diff(log(.)) * 100)))


#------------------------------------------------Data for the industry analysis
# Convert date
Dates <- as.Date(world$Dates, format="%d.%m.%Y")

# New column "date" in first position, remove old first column
world <- cbind(Dates, world[,-1])

# Sort dataframe by date 
world <- world[order(world$Dates), ]

# Format all columns except date as numeric
world <- world %>%
  mutate(across(!Dates, as.numeric))

# Compute logarithmic returns
world <- world %>%
  mutate(across(-Dates, ~ c(NA, diff(log(.)) * 100)))

# Delete rows that contain only NA values in the return columns
rendite_cols <- setdiff(names(world), "Dates")
world <- world[rowSums(!is.na(world[rendite_cols])) > 0, ]

# Delete columns that are completely empty
world <- world[, colSums(!is.na(world)) > 0]

# Delete columns that have no values in 2025
year2025_rows <- format(world$Dates, "%Y") == "2025"
rendite_cols <- setdiff(names(world), "Dates")
cols_with_2025_values <- sapply(world[rendite_cols], function(x) any(!is.na(x[year2025_rows])))
world <- world[, c("Dates", names(cols_with_2025_values)[cols_with_2025_values])]

# Filter relevant columns and rename them
mapping_gics_sector <- mapping_raw %>%
  select(id.., gics_sector.., gics_sector_name.., gics_industry.., gics_industry_name.., cntry_of_domicile..) %>%
  rename(
    index = id..,
    gics_sector = gics_sector..,
    gics_sector_name = gics_sector_name..,
    gics_industry = gics_industry..,
    gics_industry_name = gics_industry_name..,
    country_domicile = cntry_of_domicile..
  )


# Adjust column names for the mapping:
mapping_gics_sector <- mapping_gics_sector %>%
  mutate(
    index_clean = make.names(index)   # produces the same syntax as R column names
  )


# Create a separate dataframe in the environment for each sector

# Sectors to be analyzed
selected_sectors <- c(15, 20, 25, 45)

# Empty vector for names
dfs_sector <- c()

for (sector in selected_sectors) {
  
  # Get the indices for this sector from the mapping
  indices <- mapping_gics_sector %>%
    filter(gics_sector == sector) %>%
    pull(index_clean)
  
  # Create dataframe: date + relevant indices
  df_sector <- world %>%
    select(Dates, all_of(indices))
  
  # Name of the dataframe
  df_name <- paste0("sector_", sector)
  
  # Store dataframe in the environment
  assign(df_name, df_sector, envir = .GlobalEnv)
  
  # Collect names in the vector
  dfs_sector <- c(dfs_sector, df_name)
  
}


# Create a separate dataframe in the environment for each industry
dfs_industry <- c()

# Industries to be analyzed
selected_industries <- c(151010, 151040, 201010, 201040, 251010, 251020, 452030, 453010)

for (industry in selected_industries) {
  
  # Get the indices for this sector from the mapping
  indices <- mapping_gics_sector %>%
    filter(gics_industry == industry) %>%
    pull(index_clean)
  
  # Create dataframe: date + relevant indices
  df_industry <- world %>%
    select(Dates, all_of(indices))
  
  # Name of the dataframe
  df_name <- paste0("industry_", industry)
  
  # Store dataframe in the environment
  assign(df_name, df_industry, envir = .GlobalEnv)
  
  # Collect names in the vector
  dfs_industry <- c(dfs_industry, df_name)
  
}




# ==============================================
# Definition of the event day
# ==============================================
event_dates <- c(
  "CHN" = as.Date("2025-02-14"),
  "DEU" = as.Date("2025-02-14"),
  "CAN" = as.Date("2025-02-14"),
  "VNM" = as.Date("2025-02-14"),
  "MEX" = as.Date("2025-02-13")
)



# ==============================================
# Define event window & estimation window
# ==============================================
days_prior_event = 1
days_post_event = 3
estimation_length <- 250  # Number of trading days in the estimation period

# Define event window
event_windows <- list(
  c(-1, 0),
  c(0, 1),
  c(0, 3),
  c(-1, 3)
)


# ==============================================
# Define analysis levels
# ==============================================
analyses <- list(
  country  = dfs_country,
  sector   = dfs_sector,
  industry = dfs_industry
)



# ==============================================
# Accounting for the different time zones of the countries
# ==============================================

# Derive relevant countries from the companies' country of domicile 

results_countries <- list()

for (scope in c("sector", "industry")) {
  dfs_current <- analyses[[scope]]
  if (is.null(names(dfs_current))) names(dfs_current) <- dfs_current  # Naming
  
  results_countries[[scope]] <- lapply(dfs_current, function(df_name) {
    df <- get(df_name)  # <--- the dataframe is retrieved from the environment here
    
    # Columns (index names) except date
    cols_idx <- setdiff(names(df), c("Dates"))
    
    # Mapping to countries
    matched <- mapping_gics_sector %>%
      filter(index_clean %in% cols_idx)
    
    unique(matched$country_domicile)
  })
}

# Compact overview
countries_per_scope <- lapply(results_countries, function(x) sort(unique(unlist(x))))

# Merge all countries from sector + industry and remove duplicates
all_countries <- unique(c(countries_per_scope$sector, countries_per_scope$industry))

# ISO code → time zone (capital/financial center)
tz_mapping <- tibble(
  iso2c = all_countries,
  tz = c(
    "Asia/Dubai",         # AE
    "Australia/Sydney",   # AU
    "Europe/Brussels",    # BE
    "Atlantic/Bermuda",   # BM
    "America/Sao_Paulo",  # BR
    "America/Toronto",    # CA
    "Europe/Zurich",      # CH
    "America/Santiago",   # CL
    "Asia/Shanghai",      # CN
    "Europe/Berlin",      # DE
    "Europe/Copenhagen",  # DK
    "Europe/Madrid",      # ES
    "Europe/Helsinki",    # FI
    "Europe/Paris",       # FR
    "Europe/London",      # GB
    "Europe/Athens",      # GR
    "Asia/Hong_Kong",     # HK
    "Asia/Jakarta",       # ID
    "Europe/Dublin",      # IE
    "Asia/Jerusalem",     # IL
    "Asia/Kolkata",       # IN
    "Europe/Rome",        # IT
    "Asia/Tokyo",         # JP
    "Asia/Seoul",         # KR
    "Europe/Luxembourg",  # LU
    "Asia/Macau",         # MO
    "America/Mexico_City",# MX
    "Asia/Kuala_Lumpur",  # MY
    "Europe/Amsterdam",   # NL
    "Europe/Oslo",        # NO
    "Pacific/Auckland",   # NZ
    "America/Lima",       # PE
    "Asia/Manila",        # PH
    "Europe/Warsaw",      # PL
    "Asia/Qatar",         # QA
    "Asia/Riyadh",        # SA
    "Europe/Stockholm",   # SE
    "Asia/Singapore",     # SG
    "Asia/Bangkok",       # TH
    "Europe/Istanbul",    # TR
    "Asia/Taipei",        # TW
    "America/New_York",   # US
    "America/Montevideo", # UY
    "Africa/Johannesburg" # ZA
  )
)



# Event time in New York
event_time_us <- force_tz(as.POSIXct("2025-02-13 16:00:00"), tzone = "America/New_York")

# Prepare list
local_times <- list()

# Loop over countries + time zones
for(i in seq_len(nrow(tz_mapping))){
  iso <- tz_mapping$iso2c[i]
  tz  <- tz_mapping$tz[i]
  local_times[[iso]] <- with_tz(event_time_us, tz)
}

# For all industry dataframes
indices_industry <- unique(unlist(
  lapply(dfs_industry, function(df_name) {
    df <- get(df_name)
    setdiff(names(df), "Dates")  # all columns except Dates
  })
))

# For all sector dataframes
indices_sector <- unique(unlist(
  lapply(dfs_sector, function(df_name) {
    df <- get(df_name)
    setdiff(names(df), "Dates")  # all columns except Dates
  })
))

# All combined
indices <- unique(c(indices_sector, indices_industry))

# Identify the event date per index
index_event_days <- mapping_gics_sector %>%
  filter(index_clean %in% indices) %>%
  rowwise() %>%
  mutate(
    # Local time: convert reference time to ISO time zone
    local_time = force_tz(as.POSIXct("2025-02-13 16:00:00"), tzone = "America/New_York"),
    
    # Determine event day (taking the 4 p.m. market close into account)
    event_day_date = case_when(
      as.Date(local_time) > as.Date("2025-02-13") | hour(local_time) >= 16 ~ as.Date("2025-02-14"),
      as.Date(local_time) < as.Date("2025-02-13") | hour(local_time) < 16 ~ as.Date("2025-02-12"),
      TRUE ~ as.Date("2025-02-13")
    )
  ) %>%
  ungroup() %>%
  select(index_clean, event_day_date)




# ==============================================
# Create dataframes for event window and estimation window 
# ==============================================
for (scope in names(analyses)) {
  
  dfs_current <- analyses[[scope]]
  
  for (df_name in dfs_current) {
    data_raw <- get(df_name)
    
    if(scope == "country" && df_name %in% names(event_dates)){
      # -------------------------------
      # Country: fixed event date
      # -------------------------------
      current_event_date <- event_dates[[df_name]]
      
      event_idx <- which(data_raw$Dates == current_event_date)
      if(length(event_idx)==0) next
      
      window_idx <- (event_idx - days_prior_event):(event_idx + days_post_event)
      est_idx    <- (event_idx - days_prior_event - estimation_length):(event_idx - days_prior_event - 1)
      
      # Event window
      event_window <- data_raw[window_idx, ] %>%
        arrange(Dates) %>%
        mutate(event_day = row_number() - which(Dates == current_event_date)) %>%
        select(event_day, everything())
      
      # Estimation window
      est_window <- data_raw[est_idx, ] %>%
        arrange(Dates) %>%
        mutate(event_day = seq(-(days_prior_event + estimation_length),
                               -(days_prior_event + 1))) %>%
        select(event_day, everything())
      
      
    } else {
      # -------------------------------
      # Sector / Industry: event day per index from index_event_days
      # -------------------------------
      data_long <- data_raw %>%
        pivot_longer(cols = -Dates, names_to = "index_clean", values_to = "Close") %>%
        left_join(index_event_days, by = "index_clean")
      
      event_windows_prep <- list()
      est_windows_prep   <- list()
      
      for(idx in unique(data_long$index_clean)){
        idx_data <- data_long %>% filter(index_clean == idx)
        event_date <- as.Date(unique(idx_data$event_day_date))
        
        event_idx <- which(idx_data$Dates == event_date)
        
        if(length(event_idx)==0) next
        
        window_idx <- (event_idx - days_prior_event):(event_idx + days_post_event)
        est_idx    <- (event_idx - days_prior_event - estimation_length):(event_idx - days_prior_event - 1)
      
        # Event window
        event_windows_prep[[idx]] <- idx_data[window_idx, ] %>%
          arrange(Dates) %>%
          mutate(event_day = row_number() - which(Dates == event_date)) %>%
          select(event_day, Dates, index_clean, Close)
        
        # Estimation window
        est_windows_prep[[idx]] <- idx_data[est_idx, ] %>%
          arrange(Dates) %>%
          mutate(event_day = seq(-(days_prior_event + estimation_length),
                                 -(days_prior_event + 1))) %>%
          select(event_day, Dates, index_clean, Close)
      }
      
      # convert back to wide format
      event_window <- bind_rows(event_windows_prep) %>% pivot_wider(names_from = index_clean, values_from = Close)
      est_window   <- bind_rows(est_windows_prep)   %>% pivot_wider(names_from = index_clean, values_from = Close)
    }
    
    # -------------------------------
    # Save
    # -------------------------------
    assign(paste0(df_name, "_event"), event_window)
    assign(paste0(df_name, "_est"),   est_window)
  }
}







# ==============================================
# Estimation of returns using the CAPM
# ==============================================

#------------------------------------------------
# Trading partners
#------------------------------------------------
capm_ar_countries   <- list()
capm_aar_countries  <- list()
capm_caar_countries <- list()
min_obs <- 100 # Indices with fewer than 100 observations are not considered



# ----------------------------------------------------------
# Create functions
# ----------------------------------------------------------
# CAPM estimation
fit_capm <- function(df, idx, min_obs) {
  dat <- df %>%
    dplyr::select(Dates, all_of(idx), MXWO, rf) %>%
    dplyr::mutate(
      Ri_ex = !!rlang::sym(idx) - dplyr::coalesce(rf, 0),
      Rm_ex = MXWO - dplyr::coalesce(rf, 0)
    ) %>%
    tidyr::drop_na()
  
  if (nrow(dat) < min_obs) return(NULL)
  
  mod <- lm(Ri_ex ~ Rm_ex, data = dat)
  sm  <- summary(mod)
  
  if (is.na(sm$sigma) || sm$sigma == 0 || sm$df[2] <= 0) return(NULL)
  
  list(
    alpha = coef(mod)[1],
    beta  = coef(mod)[2],
    sigma = sm$sigma,
    df    = mod$df.residual
  )
}



# Compute AR
calc_ar <- function(df, idx, beta) {
  df %>%
    dplyr::select(Dates, event_day, all_of(idx), MXWO, rf) %>%
    dplyr::mutate(
      Ri = !!rlang::sym(idx),
      Rm_ex = MXWO - dplyr::coalesce(rf, 0),
      exp_ret = dplyr::coalesce(rf, 0) + beta * Rm_ex,
      AR = Ri - exp_ret
    )
}


# Weights of the individual constituents per index are needed to compute the weighted average of the ARs
get_weights <- function(country, idxs) {
  weights_df_name <- paste0(country, "_weights")
  if (!exists(weights_df_name)) {
    warning(paste("No weights for", country, "- average applied."))
    return(setNames(rep(1 / length(idxs), length(idxs)), idxs))
  }
  
  weights_df <- get(weights_df_name)
  weights <- weights_df %>%
    filter(Index %in% idxs) %>%
    select(Index, Weight) %>%
    complete(Index = idxs, fill = list(Weight = 0)) %>%
    mutate(Weight = Weight / sum(Weight, na.rm = TRUE))
  
  setNames(weights$Weight, weights$Index)
}

# ----------------------------------------------------------
# Main loop over all countries
# ----------------------------------------------------------
for (df_name in analyses$country) {
  
  # Prepare data
  est_df <- get(paste0(df_name, "_est")) %>%
    left_join(market_index_df, by = "Dates") %>%
    left_join(rf, by = "Dates")
  
  ev_df <- get(paste0(df_name, "_event")) %>%
    left_join(market_index_df, by = "Dates") %>%
    left_join(rf, by = "Dates")
  
  index_cols <- setdiff(names(est_df), c("Dates", "MXWO", "rf", "event_day"))
  weights <- get_weights(df_name, index_cols)
  
  ar_list <- list()
  est_ar_list <- list()
  
  # ------------------------------------------------------
  # 1) CAPM estimation & ARs per index
  # ------------------------------------------------------
  for (idx in index_cols) {
    capm <- fit_capm(est_df, idx, min_obs)
    if (is.null(capm)) next
    
    ev_ar <- calc_ar(ev_df, idx, capm$beta) %>%
      mutate(Index = idx,
             Weight = weights[idx],
             t_value = AR / capm$sigma,
             p_value = 2 * pt(abs(t_value), df = capm$df, lower.tail = FALSE),
             significant = p_value < 0.05)
    
    ar_list[[idx]] <- ev_ar %>%
      select(Dates, event_day, Index, AR, Weight, t_value, p_value, significant)
    
    est_ar <- calc_ar(est_df, idx, capm$beta)
    est_ar_list[[idx]] <- est_ar$AR * weights[idx]
  }
  
  # ------------------------------------------------------
  # 2) Save results
  # ------------------------------------------------------
  if (length(ar_list) == 0) {
    warning(paste("No ARs for", df_name))
    next
  }
  
  capm_ar_countries[[df_name]] <- bind_rows(ar_list)
  
  # ------------------------------------------------------
  # 3) AAR
  # ------------------------------------------------------
  aar_event <- rowSums(do.call(cbind, est_ar_list), na.rm = TRUE)
  S_AAR <- sd(aar_event, na.rm = TRUE)
  
  aar_df <- capm_ar_countries[[df_name]] %>%
    group_by(Dates, event_day) %>%
    summarise(AAR = sum(AR * Weight, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      t_value = AAR / S_AAR,
      p_value = 2 * pt(abs(t_value), df = length(aar_event)-1, lower.tail = FALSE),
      significant = p_value < 0.05
    )
  
  capm_aar_countries[[df_name]] <- aar_df
 
  # ------------------------------------------------------
  # 4) CAAR
  # ------------------------------------------------------
  S_hat_sq <- var(aar_event, na.rm = TRUE)
  
  car_list <- lapply(event_windows, function(win) {
    t_start <- win[1]; t_end <- win[2]; L <- t_end - t_start + 1
    CAAR <- sum(aar_df$AAR[aar_df$event_day >= t_start & aar_df$event_day <= t_end], na.rm = TRUE)
    S_CAAR <- sqrt(L * S_hat_sq)
    t_val <- CAAR / S_CAAR
    p_val <- 2 * pt(abs(t_val), df = length(na.omit(aar_event))-1, lower.tail = FALSE)
    
    data.frame(
      window = paste0("[", t_start, ",", t_end, "]"),
      Country = df_name,
      CAAR = CAAR,
      S_CAAR = S_CAAR,
      t_value = t_val,
      p_value = p_val,
      significant = p_val < 0.05
    )
  })
  
  capm_caar_countries[[df_name]] <- bind_rows(car_list)
}




#------------------------------------------------
# Sectors and industries
#------------------------------------------------
# Results container
capm_ar_gics   <- list()
capm_aar_gics <- list()
capm_caar_gics <- list()


# Main loop ---------------------------------------------------------

for (scope in names(analyses)) {
  
  if (scope == "country") next  # skip
  
  dfs_current <- analyses[[scope]] 
  
  for (df_name in dfs_current) {

  est_df <- get(paste0(df_name, "_est")) %>%
    left_join(market_index_df, by = "Dates") %>%
    left_join(rf, by = "Dates")
  
  ev_df <- get(paste0(df_name, "_event")) %>%
    left_join(market_index_df, by = "Dates") %>%
    left_join(rf, by = "Dates")
  
  index_cols <- setdiff(names(est_df), c("Dates","MXWO","rf","event_day"))
  
  ar_list  <- list()
  aar_list <- list()
  
  
  for (idx in index_cols) {
    capm <- fit_capm(est_df, idx, min_obs)
    if (is.null(capm)) next
    
    
    # Event ARs
    ev_ar <- calc_ar(ev_df, idx, capm$beta)
    ev_ar <- ev_ar %>%
      mutate(Index = idx,
             t_value = AR / capm$sigma,
             p_value = 2 * pt(abs(t_value), df = capm$df, lower.tail = FALSE),
             significant = p_value < 0.05)
    ar_list[[idx]] <- ev_ar %>%
      select(Dates, event_day, Index, AR, t_value, p_value, significant)
    
    
    # Estimation ARs (for variance estimation)
    est_ar <- calc_ar(est_df, idx, capm$beta)
    aar_list[[idx]] <- est_ar$AR
  }
  
  if (length(ar_list) == 0) next
  
  # 1) AR sector results
  capm_ar_gics[[df_name]] <- bind_rows(ar_list)
  
  # 2) AAR
  aar_event <- rowMeans(do.call(cbind, aar_list), na.rm = TRUE)
  S_AAR     <- sd(aar_event, na.rm = TRUE) # the standard deviation of the abnormal returns from the estimation period is used as the measure of dispersion
  aar_df <- capm_ar_gics[[df_name]] %>%
    group_by(Dates, event_day) %>%
    summarise(AAR = mean(AR, na.rm = TRUE), .groups = "drop") %>%
    mutate(t_value = AAR / S_AAR,
           p_value = 2 * pt(abs(t_value), df = length(aar_event)-1, lower.tail = FALSE),
           significant = p_value < 0.05)
  capm_aar_gics[[df_name]] <- aar_df
  
  # 3) CAAR
  S_hat_sq <- var(aar_event, na.rm = TRUE)
  car_list <- lapply(event_windows, function(win) {
    t_start <- win[1]; t_end <- win[2]; L <- t_end - t_start + 1
    CAAR <- sum(aar_df$AAR[aar_df$event_day >= t_start & aar_df$event_day <= t_end], na.rm = TRUE)
    S_CAAR <- sqrt(L * S_hat_sq)
    t_val <- CAAR / S_CAAR
    p_val <- 2 * pt(abs(t_val), df = length(na.omit(aar_event))-1, lower.tail = FALSE)
    data.frame(window = paste0("[",t_start,",",t_end,"]"),
               Country = df_name,
               CAAR = CAAR,
               S_CAAR = S_CAAR,
               t_value = t_val,
               p_value = p_val,
               significant = p_val < 0.05)
  })
  capm_caar_gics[[df_name]] <- bind_rows(car_list)
}

}





# ==============================================
# Estimation of returns using the market model -> robustness check
# ==============================================

#------------------------------------------------
# Trading partners
#------------------------------------------------
# Results container
mm_ar_countries   <- list()
mm_aar_countries  <- list()
mm_caar_countries <- list()


# ----------------------------------------------------------
# Helper functions
# ----------------------------------------------------------
# Market model estimation
fit_mm <- function(df, idx, min_obs) {
  dat <- df %>%
    select(Dates, all_of(idx), MXWO) %>%
    rename(Ri = all_of(idx)) %>%
    drop_na()
  
  if (nrow(dat) < min_obs) return(NULL)
  mod <- lm(Ri ~ MXWO, data = dat)
  sm  <- summary(mod)
  
  if (is.na(sm$sigma) || sm$sigma == 0 || sm$df[2] <= 0) return(NULL)
  
  list(alpha = coef(mod)[1],
       beta = coef(mod)[2],
       sigma = sm$sigma,
       df = mod$df.residual)
}

# Compute AR (market model)
calc_ar_mm <- function(df, idx, alpha, beta) {
  df %>%
    select(Dates, event_day, all_of(idx), MXWO) %>%
    rename(Ri = all_of(idx)) %>%
    mutate(exp_ret = alpha + beta * MXWO,
           AR = Ri - exp_ret)
}




#------------------------------------------------
# Main loop over countries
#------------------------------------------------
for (df_name in analyses$country) {
  
  # Prepare data
  est_df <- get(paste0(df_name, "_est")) %>%
    left_join(market_index_df, by = "Dates")
  
  ev_df <- get(paste0(df_name, "_event")) %>%
    left_join(market_index_df, by = "Dates")
  
  index_cols <- setdiff(names(est_df), c("Dates", "MXWO", "event_day"))
  weights <- get_weights(df_name, index_cols)
  
  ar_list <- list()
  est_ar_list <- list()
  
  # ------------------------------------------------------
  # 1) Market model estimation & ARs per index
  # ------------------------------------------------------
  for (idx in index_cols) {
    mm <- fit_mm(est_df, idx, min_obs)
    if (is.null(mm)) next
    
    # Event ARs
    ev_ar <- calc_ar_mm(ev_df, idx, mm$alpha, mm$beta) %>%
      mutate(
        Index = idx,
        Weight = weights[idx],
        t_value = AR / mm$sigma,
        p_value = 2 * pt(abs(t_value), df = mm$df, lower.tail = FALSE),
        significant = p_value < 0.05
      )
    
    ar_list[[idx]] <- ev_ar %>%
      select(Dates, event_day, Index, AR, Weight, t_value, p_value, significant)
    
    # Estimation ARs (for variance estimation)
    est_ar <- calc_ar_mm(est_df, idx, mm$alpha, mm$beta)
    est_ar_list[[idx]] <- est_ar$AR * weights[idx]
  }
  
  # ------------------------------------------------------
  # 2) Save results
  # ------------------------------------------------------
  if (length(ar_list) == 0) {
    warning(paste("No ARs for", df_name))
    next
  }
  
  mm_ar_countries[[df_name]] <- bind_rows(ar_list)
  
  # ------------------------------------------------------
  # 3) AAR (weighted average)
  # ------------------------------------------------------
  aar_event <- rowSums(do.call(cbind, est_ar_list), na.rm = TRUE)
  S_AAR <- sd(aar_event, na.rm = TRUE)
  
  aar_df <- mm_ar_countries[[df_name]] %>%
    group_by(Dates, event_day) %>%
    summarise(
      AAR = sum(AR * Weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      t_value = AAR / S_AAR,
      p_value = 2 * pt(abs(t_value), df = length(aar_event)-1, lower.tail = FALSE),
      significant = p_value < 0.05
    )
  
  mm_aar_countries[[df_name]] <- aar_df
  
  # ------------------------------------------------------
  # 4) CAAR (cumulative weighted AAR)
  # ------------------------------------------------------
  S_hat_sq <- var(aar_event, na.rm = TRUE)
  
  car_list <- lapply(event_windows, function(win) {
    t_start <- win[1]; t_end <- win[2]; L <- t_end - t_start + 1
    CAAR <- sum(aar_df$AAR[aar_df$event_day >= t_start & aar_df$event_day <= t_end], na.rm = TRUE)
    S_CAAR <- sqrt(L * S_hat_sq)
    t_val <- CAAR / S_CAAR
    p_val <- 2 * pt(abs(t_val), df = length(na.omit(aar_event))-1, lower.tail = FALSE)
    
    data.frame(
      window = paste0("[", t_start, ",", t_end, "]"),
      Country = df_name,
      CAAR = CAAR,
      S_CAAR = S_CAAR,
      t_value = t_val,
      p_value = p_val,
      significant = p_val < 0.05
    )
  })
  
  mm_caar_countries[[df_name]] <- bind_rows(car_list)
}





#------------------------------------------------
# Sectors and industries
#------------------------------------------------
# Results container for market model
mm_ar_gics   <- list()
mm_aar_gics  <- list()
mm_caar_gics <- list()


# Main loop ---------------------------------------------------------
for (scope in names(analyses)) {
  
  if (scope == "country") next  # skip
  
  dfs_current <- analyses[[scope]] 
  
  for (df_name in dfs_current) {
    
    
    est_df <- get(paste0(df_name, "_est")) %>%
      left_join(market_index_df, by = "Dates")
    
    ev_df <- get(paste0(df_name, "_event")) %>%
      left_join(market_index_df, by = "Dates")
    
    index_cols <- setdiff(names(est_df), c("Dates","MXWO","event_day"))
    
    ar_list  <- list()
    aar_list <- list()
    
    for (idx in index_cols) {
      mm <- fit_mm(est_df, idx, min_obs)
      if (is.null(mm)) next
      
      # Event ARs
      ev_ar <- calc_ar_mm(ev_df, idx, mm$alpha, mm$beta)
      ev_ar <- ev_ar %>%
        mutate(Index = idx,
               t_value = AR / mm$sigma,
               p_value = 2 * pt(abs(t_value), df = mm$df, lower.tail = FALSE),
               significant = p_value < 0.05)
      
      ar_list[[idx]] <- ev_ar %>%
        select(Dates, event_day, Index, AR, t_value, p_value, significant)
      
      # Estimation ARs
      est_ar <- calc_ar_mm(est_df, idx, mm$alpha, mm$beta)
      aar_list[[idx]] <- est_ar$AR
    }
    
    if (length(ar_list) == 0) next
    
    # 1) AR sector results
    mm_ar_gics[[df_name]] <- bind_rows(ar_list)
    
    # 2) AAR
    aar_event <- rowMeans(do.call(cbind, aar_list), na.rm = TRUE)
    S_AAR     <- sd(aar_event, na.rm = TRUE)
    aar_df <- mm_ar_gics[[df_name]] %>%
      group_by(Dates, event_day) %>%
      summarise(AAR = mean(AR, na.rm = TRUE), .groups = "drop") %>%
      mutate(t_value = AAR / S_AAR,
             p_value = 2 * pt(abs(t_value), df = length(aar_event)-1, lower.tail = FALSE),
             significant = p_value < 0.05)
    mm_aar_gics[[df_name]] <- aar_df
    
    # 3) CAAR
    S_hat_sq <- var(aar_event, na.rm = TRUE)
    car_list <- lapply(event_windows, function(win) {
      t_start <- win[1]; t_end <- win[2]; L <- t_end - t_start + 1
      CAAR <- sum(aar_df$AAR[aar_df$event_day >= t_start & aar_df$event_day <= t_end], na.rm = TRUE)
      S_CAAR <- sqrt(L * S_hat_sq)
      t_val <- CAAR / S_CAAR
      p_val <- 2 * pt(abs(t_val), df = length(na.omit(aar_event))-1, lower.tail = FALSE)
      data.frame(window = paste0("[",t_start,",",t_end,"]"),
                 Country = df_name,
                 CAAR = CAAR,
                 S_CAAR = S_CAAR,
                 t_value = t_val,
                 p_value = p_val,
                 significant = p_val < 0.05)
    })
    mm_caar_gics[[df_name]] <- bind_rows(car_list)
  }
  
}







# ==============================================
# Non-parametric test procedures
# ==============================================

# Abnormal returns in the estimation period and in the event window are required


# ----------------------------------------------------------
# Countries
# ----------------------------------------------------------
capm_ar_est_countries  <- list()  # AR in the estimation period 
capm_aar_est_countries  <- list()


for (scope in names(analyses)) {
  
  if (scope != "country") next  # Countries only (no sector / industry)
  
  dfs_current <- analyses[[scope]]
  
  for (df_name in dfs_current) {
    
    # Load datasets
    est_df <- get(paste0(df_name, "_est")) %>%
      left_join(market_index_df, by = "Dates") %>%
      left_join(rf, by = "Dates")
    
    index_cols <- setdiff(names(est_df), c("Dates", "MXWO", "rf", "event_day"))
    
    # Retrieve weights
    weights <- get_weights(df_name, index_cols)
    
    ar_est_list <- list()
    
    # ------------------------------------------------------
    # 1) CAPM estimation per index
    # ------------------------------------------------------
    for (idx in index_cols) {
      capm <- fit_capm(est_df, idx, min_obs)
      if (is.null(capm)) next
      
      # AR in the estimation period
      est_ar <- calc_ar(est_df, idx, capm$beta) %>%
        mutate(Index = idx,
               Weight = weights[idx],
               t_value = AR / capm$sigma,
               p_value = 2 * pt(abs(t_value), df = capm$df, lower.tail = FALSE),
               significant = p_value < 0.05)
      
      ar_est_list[[idx]] <- est_ar %>%
        select(Dates, event_day, Index, AR, Weight, t_value, p_value, significant)
    }
    
    if (length(ar_est_list) == 0) {
      warning(paste("No ARs in estimation period for", df_name))
      next
    }
    
    # ------------------------------------------------------
    # 2) Save ARs in the estimation period
    # ------------------------------------------------------
    capm_ar_est_countries[[df_name]] <- bind_rows(ar_est_list)
    
    # ------------------------------------------------------
    # 3) Compute weighted AARs
    # ------------------------------------------------------
    AAR_est_df <- capm_ar_est_countries[[df_name]] %>%
      group_by(Dates) %>%
      summarise(AAR_est = sum(AR * Weight, na.rm = TRUE),
                .groups = "drop")
    
    capm_aar_est_countries[[df_name]] <- AAR_est_df
  }
}


# ----------------------------------------------------------
# Sectors and industries
# ----------------------------------------------------------
capm_ar_est_gics  <- list()  # AR in the estimation period 
capm_aar_est_gics  <- list()


# Main loop ---------------------------------------------------------

for (scope in names(analyses)) {
  
  if (scope == "country") next  # skip
  
  dfs_current <- analyses[[scope]] 
  
  for (df_name in dfs_current) {
    
    est_df <- get(paste0(df_name, "_est")) %>%
      left_join(market_index_df, by = "Dates") %>%
      left_join(rf, by = "Dates")
    
    index_cols <- setdiff(names(est_df), c("Dates","MXWO","rf","event_day"))
    
    ar_est_list   <- list()  # AR in the estimation period
    
    for (idx in index_cols) {
      capm <- fit_capm(est_df, idx, min_obs)
      if (is.null(capm)) next
      
      # 2) AR in the estimation period 
      est_ar <- calc_ar(est_df, idx, capm$beta)
      est_ar <- est_ar %>%
        mutate(Index = idx,
               t_value = AR / capm$sigma,
               p_value = 2 * pt(abs(t_value), df = capm$df, lower.tail = FALSE),
               significant = p_value < 0.05)
      
      ar_est_list[[idx]] <- est_ar %>%
        select(Dates, event_day, Index, AR, t_value, p_value, significant)
      
      # 3) AAR computation
      aar_list[[idx]] <- est_ar$AR
    }
    
    if (length(ar_list) == 0) next
    
    # 2) Save AR in the estimation period
    capm_ar_est_gics[[df_name]] <- bind_rows(ar_est_list)
    
    # save as dataframe with date
    AAR_est_df <- capm_ar_est_gics[[df_name]] %>%
      group_by(Dates) %>%
      summarise(AAR_est = mean(AR, na.rm = TRUE), .groups = "drop")
    
    # Save result
    capm_aar_est_gics[[df_name]] <- AAR_est_df
    
  }
}




# ----------------------------------------------------------
# Market model: compute AR in the estimation period
# ----------------------------------------------------------
mm_ar_est_countries  <- list()   # ARs in the estimation period
mm_aar_est_countries <- list()   # weighted AARs in the estimation period

for (scope in names(analyses)) {
  
  if (scope != "country") next  # Consider countries only
  
  dfs_current <- analyses[[scope]]
  
  for (df_name in dfs_current) {
    
    # ------------------------------------------------------
    # 1) Load and prepare data
    # ------------------------------------------------------
    est_df <- get(paste0(df_name, "_est")) %>%
      left_join(market_index_df, by = "Dates")
    
    index_cols <- setdiff(names(est_df), c("Dates", "MXWO", "event_day"))
    
    # Retrieve weights
    weights <- get_weights(df_name, index_cols)
    
    ar_est_list <- list()
    
    # ------------------------------------------------------
    # 2) Market model estimation per index
    # ------------------------------------------------------
    for (idx in index_cols) {
      mm <- fit_mm(est_df, idx, min_obs)
      if (is.null(mm)) next
      
      # ARs in the estimation period
      est_ar <- calc_ar_mm(est_df, idx, mm$alpha, mm$beta) %>%
        mutate(Index = idx,
               Weight = weights[idx],
               t_value = AR / mm$sigma,
               p_value = 2 * pt(abs(t_value), df = mm$df, lower.tail = FALSE),
               significant = p_value < 0.05)
      
      ar_est_list[[idx]] <- est_ar %>%
        select(Dates, event_day, Index, AR, Weight, t_value, p_value, significant)
    }
    
    # ------------------------------------------------------
    # 3) Save results
    # ------------------------------------------------------
    if (length(ar_est_list) == 0) {
      warning(paste("No AR in estimation period for", df_name))
      next
    }
    
    mm_ar_est_countries[[df_name]] <- bind_rows(ar_est_list)
    
    # ------------------------------------------------------
    # 4) Weighted AARs in the estimation period
    # ------------------------------------------------------
    AAR_est_df <- mm_ar_est_countries[[df_name]] %>%
      group_by(Dates) %>%
      summarise(AAR_est = sum(AR * Weight, na.rm = TRUE),
                .groups = "drop")
    
    mm_aar_est_countries[[df_name]] <- AAR_est_df
  }
}






# ----------------------------------------------------------
# Sectors and industries (estimation period) – market model
# ----------------------------------------------------------
mm_ar_est_gics  <- list()  # AR in the estimation period 
mm_aar_est_gics <- list()  # AAR in the estimation period


# Main loop ---------------------------------------------------------
for (scope in names(analyses)) {
  
  if (scope == "country") next  # Skip countries
  
  dfs_current <- analyses[[scope]] 
  
  for (df_name in dfs_current) {
    
    # ------------------------------------------------------
    # 1) Load and prepare data
    # ------------------------------------------------------
    est_df <- get(paste0(df_name, "_est")) %>%
      left_join(market_index_df, by = "Dates")
    
    index_cols <- setdiff(names(est_df), c("Dates", "MXWO", "event_day"))
    
    ar_est_list <- list()
    
    # ------------------------------------------------------
    # 2) Market model estimation per index
    # ------------------------------------------------------
    for (idx in index_cols) {
      mm <- fit_mm(est_df, idx, min_obs)
      if (is.null(mm)) next
      
      # AR in the estimation period
      est_ar <- calc_ar_mm(est_df, idx, mm$alpha, mm$beta)
      est_ar <- est_ar %>%
        mutate(Index = idx,
               t_value = AR / mm$sigma,
               p_value = 2 * pt(abs(t_value), df = mm$df, lower.tail = FALSE),
               significant = p_value < 0.05)
      
      ar_est_list[[idx]] <- est_ar %>%
        select(Dates, event_day, Index, AR, t_value, p_value, significant)
    }
    
    # ------------------------------------------------------
    # 3) Save results
    # ------------------------------------------------------
    if (length(ar_est_list) == 0) {
      warning(paste("No AR in estimation period for", df_name))
      next
    }
    
    mm_ar_est_gics[[df_name]] <- bind_rows(ar_est_list)
    
    # ------------------------------------------------------
    # 4) Compute AAR
    # ------------------------------------------------------
    AAR_est_df <- mm_ar_est_gics[[df_name]] %>%
      group_by(Dates) %>%
      summarise(AAR_est = mean(AR, na.rm = TRUE), .groups = "drop")
    
    mm_aar_est_gics[[df_name]] <- AAR_est_df
  }
}









#------------------------------------------------
# Generalized sign test
#------------------------------------------------

# Models and lists
lists_est <- list(
  capm_countries = capm_ar_est_countries,
  capm_gics = capm_ar_est_gics,
  mm_gics   = mm_ar_est_gics,
  mm_countries = mm_ar_est_countries
)

lists_event <- list(
  capm_countries = capm_ar_countries,
  capm_gics = capm_ar_gics,
  mm_gics   = mm_ar_gics,
  mm_countries = mm_ar_countries
)


generalized_sign_test <- function(event_list, est_list) {
  
  results_all <- lapply(names(event_list), function(country) {
    df_event <- event_list[[country]]
    df_est   <- est_list[[country]]
    
    # Median(AR) per firm
    med_df <- df_est %>%
      group_by(Index) %>%
      summarise(median_AR = median(AR, na.rm = TRUE), .groups = "drop")
    
    # Entire period (estimation + event data)
    df_all <- bind_rows(df_est, df_event) %>%
      left_join(med_df, by = "Index") %>%
      mutate(G = sign(AR - median_AR))
    
    # T_t over the entire period
    Tt_all <- df_all %>%
      group_by(event_day) %>%
      summarise(
        N_t = sum(!is.na(G)),
        sum_G = sum(G, na.rm = TRUE),
        Tt = ifelse(N_t > 0, sum_G / sqrt(N_t), NA_real_),
        .groups = "drop"
      )
    
    # S(G) over the entire period
    S_G <- sqrt(mean(Tt_all$Tt^2, na.rm = TRUE))
    
    # Extract event days
    day_detail <- df_all %>%
      filter(event_day %in% unique(df_event$event_day)) %>%
      group_by(event_day) %>%
      summarise(
        N_t       = sum(!is.na(G)),
        count_pos = sum(G ==  1, na.rm = TRUE),
        count_neg = sum(G == -1, na.rm = TRUE),
        sum_G     = sum(G, na.rm = TRUE),
        Tt        = ifelse(N_t > 0, sum_G / sqrt(N_t), NA_real_),
        .groups   = "drop"
      ) %>%
      mutate(
        Zt        = ifelse(!is.na(Tt), Tt / S_G, NA_real_),
        p_value   = ifelse(!is.na(Zt), 2 * (1 - pnorm(abs(Zt))), NA_real_),
        significant_0.1  = p_value < 0.1,
        significant_0.05 = p_value < 0.05,
        significant_0.01 = p_value < 0.01,
        Country    = country
      ) %>%
      select(Country, event_day, N_t, count_pos, count_neg, sum_G, Tt, Zt, p_value,
             significant_0.1, significant_0.05, significant_0.01)
    
    return(day_detail)
  })
  
  bind_rows(results_all)
}


sign_cowan_results_capm_countries <- generalized_sign_test(
  event_list = capm_ar_countries,
  est_list   = capm_ar_est_countries)

sign_cowan_results_capm_gics <- generalized_sign_test(
  event_list = capm_ar_gics,
  est_list   = capm_ar_est_gics)

sign_cowan_results_mm_countries <- generalized_sign_test(
  event_list = mm_ar_countries,
  est_list   = mm_ar_est_countries)

sign_cowan_results_mm_gics <- generalized_sign_test(
  event_list = mm_ar_gics,
  est_list   = mm_ar_est_gics)




#------------------------------------------------
#  Sign test for CAR 
#------------------------------------------------

generalized_sign_test_CAR <- function(event_list, event_windows) {
  
  results_all <- lapply(names(event_list), function(country) {
    
    df_event <- event_list[[country]]
    
    # Loop over event windows
    window_results <- lapply(seq_along(event_windows), function(i) {
      
      window_range <- event_windows[[i]]
      window_name <- paste(window_range[1], window_range[2], sep = ", ")
      
      # Select event days within the window
      df_event_window <- df_event %>%
        filter(event_day >= window_range[1] & event_day <= window_range[2])
      
      # CAR per firm
      df_car <- df_event_window %>%
        group_by(Index) %>%
        summarise(CAR = sum(AR, na.rm = TRUE), .groups = "drop")
      
      # Number of positive CARs
      N <- nrow(df_car)
      N_pos <- sum(df_car$CAR > 0, na.rm = TRUE)
      
      # Test statistic θ2
      theta2 <- ifelse(N > 0, (N_pos / N - 0.5) / sqrt(0.5 / N), NA_real_)
      
      # p-value
      p_value <- ifelse(!is.na(theta2), 2 * (1 - pnorm(abs(theta2))), NA_real_)
      
      # Results table
      df_result <- tibble(
        Country      = country,
        Event_Window = window_name,
        N_t          = N,
        count_pos    = N_pos,
        count_neg    = N - N_pos,
        theta2       = theta2,
        p_value      = p_value,
        significant_0.1  = !is.na(p_value) & p_value < 0.1,
        significant_0.05 = !is.na(p_value) & p_value < 0.05,
        significant_0.01 = !is.na(p_value) & p_value < 0.01
      )
      
      return(df_result)
    })
    
    bind_rows(window_results)
  })
  
  bind_rows(results_all)
}


# Application
sign_results_CAR_capm_countries <- generalized_sign_test_CAR(
  event_list    = capm_ar_countries,
  event_windows = event_windows )

sign_results_CAR_capm_gics <- generalized_sign_test_CAR(
  event_list    = capm_ar_gics,
  event_windows = event_windows)


sign_results_CAR_mm_countries <- generalized_sign_test_CAR(
  event_list    = mm_ar_countries,
  event_windows = event_windows)

sign_results_CAR_mm_gics <- generalized_sign_test_CAR(
  event_list    = mm_ar_gics,
  event_windows = event_windows)









#------------------------------------------------
# Corrado rank test for individual event days
#------------------------------------------------
calc_corrado_all_days <- function(ar_est_df, ar_evt_df) {
  
  # Combine the ARs into a wide matrix (firms in columns)
  ar_est <- ar_est_df %>%
    select(event_day, Index, AR) %>%
    pivot_wider(names_from = Index, values_from = AR)
  
  ar_evt <- ar_evt_df %>%
    select(event_day, Index, AR) %>%
    pivot_wider(names_from = Index, values_from = AR)
  
  # Combine
  ar_combined <- rbind(ar_est, ar_evt)
  
  # Delete empty columns
  ar_combined <- rbind(ar_est, ar_evt) %>%
    arrange(event_day) %>%
    filter(rowSums(is.na(across(-event_day))) < ncol(.) - 1) 
  
  # Ranking per firm over the entire period
  ranks_mat <- apply(ar_combined[, -1], 2, rank, ties.method = "average", na.last = "keep")
  
  # Base quantities
  T_total <- nrow(ranks_mat)
  N <- ncol(ranks_mat)
  mean_rank <- (T_total + 1) / 2
  
  # Daily means across firms
  K_t_bar <- rowMeans(ranks_mat, na.rm = TRUE)
  
  # Standard deviation
  S_K <- sqrt(sum((K_t_bar - mean_rank)^2, na.rm = TRUE) / T_total)
  
  # For each event day: compute test statistic
  event_days <- ar_evt$event_day
  results <- data.frame(
    event_day = event_days,
    T_corrado = NA_real_,
    p_value = NA_real_
  )
  
  for (i in seq_along(event_days)) {
    t_row <- which(ar_combined$event_day == event_days[i])
    
    if (length(t_row) == 1) {
      K_i_t <- as.numeric(ranks_mat[t_row, ])
      T_t <- (1 / N) * sum(K_i_t - mean_rank, na.rm = TRUE)
      T_corrado <- T_t / S_K
      p_val <- 2 * (1 - pnorm(abs(T_corrado)))
      
      results$T_corrado[i] <- T_corrado
      results$p_value[i] <- p_val
    }
  }
  
  results$significant_0.1 <- results$p_value < 0.1
  results$significant_0.05 <- results$p_value < 0.05
  results$significant_0.01 <- results$p_value < 0.01
  
  return(results)
}

# Application to the country results (CAPM)
all_results <- list()

for (country in names(capm_ar_countries)) {
  ar_est_df <- capm_ar_est_countries[[country]]
  ar_evt_df <- capm_ar_countries[[country]]
  
  country_res <- calc_corrado_all_days(ar_est_df, ar_evt_df)
  country_res$Country <- country
  
  all_results[[country]] <- country_res
}

corrado_ar_capm_countries <- do.call(rbind, all_results)
rownames(corrado_ar_capm_countries) <- NULL


# Application to the sector and industry results (CAPM)
all_results <- list()

for (country in names(capm_ar_gics)) {
  ar_est_df <- capm_ar_est_gics[[country]]
  ar_evt_df <- capm_ar_gics[[country]]
  
  country_res <- calc_corrado_all_days(ar_est_df, ar_evt_df)
  country_res$Country <- country
  
  all_results[[country]] <- country_res
}

corrado_ar_capm_gics <- do.call(rbind, all_results)
rownames(corrado_ar_capm_gics) <- NULL


# Application to the country results (MM)
all_results <- list()

for (country in names(mm_ar_countries)) {
  ar_est_df <- mm_ar_est_countries[[country]]
  ar_evt_df <- mm_ar_countries[[country]]
  
  country_res <- calc_corrado_all_days(ar_est_df, ar_evt_df)
  country_res$Country <- country
  
  all_results[[country]] <- country_res
}

corrado_ar_mm_countries <- do.call(rbind, all_results)
rownames(corrado_ar_mm_countries) <- NULL


# Application to the sector and industry results (MM)
all_results <- list()

for (country in names(mm_ar_gics)) {
  ar_est_df <- mm_ar_est_gics[[country]]
  ar_evt_df <- mm_ar_gics[[country]]
  
  country_res <- calc_corrado_all_days(ar_est_df, ar_evt_df)
  country_res$Country <- country
  
  all_results[[country]] <- country_res
}

corrado_ar_mm_gics <- do.call(rbind, all_results)
rownames(corrado_ar_mm_gics) <- NULL





#------------------------------------------------
# Corrado rank test for cumulative abnormal returns per event window
#------------------------------------------------

# Helper function: compute Corrado Z_R for a single country & event window
calc_corrado_zr <- function(ar_est_df, ar_evt_df, tested_days) {
  
  # Pivot: ARs into a matrix per firm
  ar_est <- ar_est_df %>%
    select(event_day, Index, AR) %>%
    pivot_wider(names_from = Index, values_from = AR)
  
  ar_evt <- ar_evt_df %>%
    select(event_day, Index, AR) %>%
    pivot_wider(names_from = Index, values_from = AR)
  
  # Combine estimation + event
  ar_combined <- rbind(ar_est, ar_evt)
  
  # Delete empty columns
  ar_combined <- rbind(ar_est, ar_evt) %>%
    arrange(event_day) %>%
    filter(rowSums(is.na(across(-event_day))) < ncol(.) - 1) 
  
  # Remove rows with NAs
  ar_combined_clean <-ar_combined 
  
  T_total <- nrow(ar_combined_clean)
  nfirms <- ncol(ar_combined_clean) - 1
  d <- length(tested_days)
  
  # Rank column-wise: each firm across all days
  ranks_mat <- apply(ar_combined_clean[, -1], 2, rank, ties.method = "average", na.last = "keep")
  
  # Average rank per day (K̅_t)
  K_t_bar <- rowMeans(ranks_mat, na.rm = TRUE)
  
  # Expected mean
  mean_rank <- (T_total + 1) / 2
  
  # Identify event days
  idx_event_rows <- which(ar_combined_clean$event_day %in% tested_days)
  ranks_event <- ranks_mat[idx_event_rows, , drop = FALSE]
  
  # K̅_D: average across event days and firms
  K_D_bar <- mean(as.vector(ranks_event), na.rm = TRUE)
  
  # Denominator
  denom <- sqrt(sum((K_t_bar - mean_rank)^2, na.rm=TRUE) / T_total)
  
  # Corrado Z_R & p-value
  Z_R <- sqrt(d) * (K_D_bar - mean_rank) / denom
  p_value <- 2 * (1 - pnorm(abs(Z_R)))
  
  # Return with intermediate results
  return(list(
    Z_R = Z_R,
    p_value = p_value,
    d = d,
    T = T_total,
    mean_rank = mean_rank,
    K_t_bar = K_t_bar,
    K_D_bar = K_D_bar,
    denom = denom
  ))
}



# Define event windows (relative days)
event_windows <- list(`-1:0` = -1:0, `0:1` = 0:1, `0:3` = 0:3,  `-1:3` = -1:3)

# Application to the country results (CAPM)
all_results <- list()

for (country in names(capm_ar_countries)) {
  ar_est_df <- capm_ar_est_countries[[country]]
  ar_evt_df <- capm_ar_countries[[country]]
  
  country_res <- list()
  
  for (win_name in names(event_windows)) {
    tested_days <- event_windows[[win_name]]
    
    out <- calc_corrado_zr(ar_est_df, ar_evt_df, tested_days)
    
    country_res[[win_name]] <- data.frame(
      Country = country,
      Window = win_name,
      d = out$d,
      T = out$T,
      Z_R = out$Z_R,
      p_value = out$p_value,
      K_D_bar = out$K_D_bar,
      mean_rank = out$mean_rank,
      denom = out$denom,
      row.names = NULL
    )
  }
  
  all_results[[country]] <- do.call(rbind, country_res)
}

corrado_car_capm_countries <- do.call(rbind, all_results)

corrado_car_capm_countries$significant_0.01 <- corrado_car_capm_countries$p_value < 0.01
corrado_car_capm_countries$significant_0.05 <- corrado_car_capm_countries$p_value < 0.05
corrado_car_capm_countries$significant_0.1 <- corrado_car_capm_countries$p_value < 0.1
rownames(corrado_car_capm_countries) <- NULL


# Application to the sector and industry results (CAPM)
all_results <- list()

for (country in names(capm_ar_gics)) {
  ar_est_df <- capm_ar_est_gics[[country]]
  ar_evt_df <- capm_ar_gics[[country]]
  
  country_res <- list()
  
  for (win_name in names(event_windows)) {
    tested_days <- event_windows[[win_name]]
    
    out <- calc_corrado_zr(ar_est_df, ar_evt_df, tested_days)
    
    country_res[[win_name]] <- data.frame(
      Country = country,
      Window = win_name,
      d = out$d,
      T = out$T,
      Z_R = out$Z_R,
      p_value = out$p_value,
      K_D_bar = out$K_D_bar,
      mean_rank = out$mean_rank,
      denom = out$denom,
      row.names = NULL
    )
  }
  
  all_results[[country]] <- do.call(rbind, country_res)
}

corrado_car_capm_gics <- do.call(rbind, all_results)

corrado_car_capm_gics$significant_0.01 <- corrado_car_capm_gics$p_value < 0.01
corrado_car_capm_gics$significant_0.05 <- corrado_car_capm_gics$p_value < 0.05
corrado_car_capm_gics$significant_0.1 <- corrado_car_capm_gics$p_value < 0.1
rownames(corrado_car_capm_gics) <- NULL


# Application to the country results (MM)
all_results <- list()

for (country in names(mm_ar_countries)) {
  ar_est_df <- mm_ar_est_countries[[country]]
  ar_evt_df <- mm_ar_countries[[country]]
  
  country_res <- list()
  
  for (win_name in names(event_windows)) {
    tested_days <- event_windows[[win_name]]
    
    out <- calc_corrado_zr(ar_est_df, ar_evt_df, tested_days)
    
    country_res[[win_name]] <- data.frame(
      Country = country,
      Window = win_name,
      d = out$d,
      T = out$T,
      Z_R = out$Z_R,
      p_value = out$p_value,
      K_D_bar = out$K_D_bar,
      mean_rank = out$mean_rank,
      denom = out$denom,
      row.names = NULL
    )
  }
  
  all_results[[country]] <- do.call(rbind, country_res)
}

corrado_car_mm_countries <- do.call(rbind, all_results)

corrado_car_mm_countries$significant_0.01 <- corrado_car_mm_countries$p_value < 0.01
corrado_car_mm_countries$significant_0.05 <- corrado_car_mm_countries$p_value < 0.05
corrado_car_mm_countries$significant_0.1 <- corrado_car_mm_countries$p_value < 0.1
rownames(corrado_car_mm_countries) <- NULL


# Application to the sector and industry results (MM)
all_results <- list()

for (country in names(mm_ar_gics)) {
  ar_est_df <- mm_ar_est_gics[[country]]
  ar_evt_df <- mm_ar_gics[[country]]
  
  country_res <- list()
  
  for (win_name in names(event_windows)) {
    tested_days <- event_windows[[win_name]]
    
    out <- calc_corrado_zr(ar_est_df, ar_evt_df, tested_days)
    
    country_res[[win_name]] <- data.frame(
      Country = country,
      Window = win_name,
      d = out$d,
      T = out$T,
      Z_R = out$Z_R,
      p_value = out$p_value,
      K_D_bar = out$K_D_bar,
      mean_rank = out$mean_rank,
      denom = out$denom,
      row.names = NULL
    )
  }
  
  all_results[[country]] <- do.call(rbind, country_res)
}

corrado_car_mm_gics <- do.call(rbind, all_results)

corrado_car_mm_gics$significant_0.01 <- corrado_car_mm_gics$p_value < 0.01
corrado_car_mm_gics$significant_0.05 <- corrado_car_mm_gics$p_value < 0.05
corrado_car_mm_gics$significant_0.1 <- corrado_car_mm_gics$p_value < 0.1
rownames(corrado_car_mm_gics) <- NULL







# ==============================================
# Graphical visualization of the results
# ==============================================


#------------------------------------------------
# Figure: Weighted abnormal returns and cumulative abnormal returns per trading partner based on the CAPM in the event window (February 13, 2025) 
#------------------------------------------------
capm_aar_countries_df <- map_dfr(
  names(capm_aar_countries),
  ~ capm_aar_countries[[.x]] %>%
    mutate(country = .x)
)

mm_aar_countries_df <- map_dfr(
  names(mm_aar_countries),
  ~ mm_aar_countries[[.x]] %>%
    mutate(country = .x)
)

capm_aar_countries_df <- capm_aar_countries_df %>%
  mutate(Model = "CAPM")

mm_aar_countries_df <- mm_aar_countries_df %>%
  mutate(Model = "MM")

combined_ar <- bind_rows(capm_aar_countries_df, mm_aar_countries_df)

combined_car <- combined_ar %>%
  arrange(country, Model, event_day) %>%
  group_by(country, Model) %>%
  mutate(CAR = cumsum(AAR)) %>%
  ungroup()

combined_car <- combined_car %>%
  mutate(
    country_name = case_when(
      country == "CHN" ~ "China",
      country == "DEU" ~ "Germany",
      country == "CAN" ~ "Canada",
      country == "MEX" ~ "Mexico",
      country == "VNM" ~ "Vietnam",
      TRUE ~ as.character(country)  # fallback for other countries
    ),
    country_name = factor(
      country_name,
      levels = c("China", "Germany", "Canada", "Mexico", "Vietnam")
    )
  )

# Select CAPM only
capm_only <- combined_car %>%
  filter(Model == "CAPM")

# Create plot
ggplot(capm_only, aes(x = event_day)) +
  # AR as bars
  geom_col(aes(y = AAR), fill = "grey50", width = 0.7) +
  # CAR as line
  geom_line(aes(y = CAR), size = 1.2, color = "grey30") +
  geom_vline(xintercept = 0, color = "grey", linetype = "dashed") +
  facet_wrap(~country_name, ncol = 3, scales = "fixed") +
  scale_x_continuous(breaks = unique(capm_only$event_day)) +
  labs(
    title = "Abnormal Return and Cumulative Abnormal Return in Event Window",
    subtitle = "Bars = Weighted Abnormal Return, line = Cumulative Weighted Abnormal Return",
    x = "Days relative to Event",
    y = "Abnormal Return in percent"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    text = element_text(family = "Arial"),
    panel.grid.minor = element_blank()
  )






#------------------------------------------------
# Figure: Average abnormal returns and cumulative abnormal returns per sector based on the CAPM in the event window (February 13, 2025)
#------------------------------------------------
# --- 1. Filter: sector dataframes only
sector_names <- names(capm_aar_gics)[str_detect(names(capm_aar_gics), "sector")]

# --- 2. AAR dataframe for sectors
aar_df <- map_dfr(sector_names, function(name) {
  capm_aar_gics[[name]] %>%
    arrange(event_day) %>%
    mutate(sector = name)
})

# --- 3. Compute CAAR (cumulative AAR)
caar_df <- map_dfr(sector_names, function(name) {
  capm_aar_gics[[name]] %>%
    arrange(event_day) %>%
    mutate(CAAR = cumsum(AAR), sector = name)
})

# --- 4. Spell out sector names (mapping)
mapping_gics_unique <- mapping_gics_sector %>%
  select(gics_sector, gics_sector_name) %>%
  distinct()

aar_df <- aar_df %>%
  mutate(gics_sector = as.integer(str_extract(sector, "\\d+"))) %>%
  left_join(mapping_gics_unique, by = "gics_sector")

caar_df <- caar_df %>%
  mutate(gics_sector = as.integer(str_extract(sector, "\\d+"))) %>%
  left_join(mapping_gics_unique, by = "gics_sector")

# --- 5. Merge AAR and CAAR
plot_df <- left_join(aar_df, caar_df, by = c("gics_sector", "event_day", "gics_sector_name", "sector"))

y_min <- floor(min(c(plot_df$AAR.x, plot_df$CAAR)) / 2.0) * 2.0
y_max <- ceiling(max(c(plot_df$AAR.x, plot_df$CAAR)) / 2.0) * 2.0

# --- 6. Create plot
ggplot(plot_df, aes(x = event_day)) +
  # AAR as bars
  geom_col(aes(y = AAR.x), fill = "grey50", width = 0.7) +
  # CAAR as line
  geom_line(aes(y = CAAR), size = 1.2, color = "grey30") +
  geom_vline(xintercept = 0, color = "grey", linetype = "dashed") +
  facet_wrap(~gics_sector_name, ncol = 2, scales = "fixed") +
  scale_x_continuous(breaks = unique(plot_df$event_day))  +
  scale_y_continuous(
    breaks = seq(y_min, y_max, by = 2.0),
    limits = c(y_min, y_max)
  ) +
  labs(
    title = "Abnormal Return and Cumulative Abnormal Return in Event Window",
    subtitle = "Bars = Average Abnormal Return, line = Cumulative Average Abnormal Return",
    x = "Days relative to Event",
    y = "Abnormal Return in percent"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    text = element_text(family = "Arial"),
    panel.grid.minor = element_blank()
  )





#------------------------------------------------
# Further figures
#------------------------------------------------

#------------------------------------------------
# Sector Materials
#------------------------------------------------

##### 1. Select relevant data
dfs_industry15 <- grep("^industry_15", names(capm_aar_gics), value = TRUE)

##### 2. Compute CAAR data
capm_caar_gics_plot <- map(capm_aar_gics[dfs_industry15], function(df) {
  df %>%
    arrange(event_day) %>%
    mutate(CAAR = cumsum(AAR))
})

##### 3. Merge both lists
aar_df <- map_dfr(dfs_industry15, function(name) {
  capm_aar_gics[[name]] %>%
    mutate(industry = name)
})

caar_df <- map_dfr(dfs_industry15, function(name) {
  capm_caar_gics_plot[[name]] %>%
    mutate(industry = name)
})

##### 4. Merge
plot_df_industry15 <- left_join(aar_df, caar_df, by = c("industry", "event_day"))

##### 5. Assign GICS names
plot_df_industry15 <- plot_df_industry15 %>%
  mutate(gics_industry = as.integer(str_extract(industry, "\\d+"))) %>%
  left_join(mapping_gics_sector %>%
              select(gics_industry, gics_industry_name) %>%
              distinct(),
            by = "gics_industry")

##### 6. Dynamic y-axis scaling
y_min <- floor(min(c(plot_df_industry15$AAR.x, plot_df_industry15$CAAR)) / 2.5) * 2.5
y_max <- ceiling(max(c(plot_df_industry15$AAR.x, plot_df_industry15$CAAR)) / 2.5) * 2.5

##### 7. Define symbols 
industries <- unique(plot_df_industry15$gics_industry_name)
n_industries <- length(industries)
shapes <- rep(c(16, 17, 15, 18, 19, 20), length.out = n_industries)
names(shapes) <- industries

##### 8. Plot
ggplot(plot_df_industry15, aes(x = event_day)) +
  # AAR bars
  geom_col(aes(y = AAR.x, fill = gics_industry_name), position = "dodge", alpha = 1) +
  # CAAR lines (in the foreground)
  geom_line(aes(y = CAAR, color = gics_industry_name, group = gics_industry_name), size = 1.1) +
  # CAAR points with symbols
  geom_point(aes(y = CAAR, color = gics_industry_name, shape = gics_industry_name), size = 3) +
  # Zero line
  geom_hline(yintercept = 0, color = "gray40", linewidth = 0.5) +
  
  # Grayscale
  scale_fill_grey(start = 0.2, end = 0.6) +
  scale_color_grey(start = 0.1, end = 0.5) +
  scale_shape_manual(values = shapes) +
  
  # X-axis: all event days, no minor ticks
  scale_x_continuous(
    breaks = min(plot_df_industry15$event_day):max(plot_df_industry15$event_day),
    minor_breaks = NULL
  ) +
  
  # Dynamic y-axis with 2.5% steps
  scale_y_continuous(
    breaks = seq(y_min, y_max, by = 2.5),
    limits = c(y_min, y_max)
  ) +
  
  # Labels
  labs(
    title = "Abnormal Return and Cumulative Abnormal Return in Event Window",
    subtitle = "Sector Materials",
    x = "Days relative to Event",
    y = "Abnormal Return in percent",
    fill = "Industry",
    color = "Industry",
    shape = "Industry"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "Arial"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank()
  )







#------------------------------------------------
# Sector Industrials
#------------------------------------------------
##### 1. Select relevant data
dfs_industry20 <- grep("^industry_20", names(capm_aar_gics), value = TRUE)

##### 2. Compute CAAR data
capm_caar_gics_plot <- map(capm_aar_gics[dfs_industry20], function(df) {
  df %>%
    arrange(event_day) %>%
    mutate(CAAR = cumsum(AAR))
})

##### 3. Merge both lists
aar_df <- map_dfr(dfs_industry20, function(name) {
  capm_aar_gics[[name]] %>%
    mutate(industry = name)
})

caar_df <- map_dfr(dfs_industry20, function(name) {
  capm_caar_gics_plot[[name]] %>%
    mutate(industry = name)
})

##### 4. Merge
plot_df_industry20 <- left_join(aar_df, caar_df, by = c("industry", "event_day"))

##### 5. Assign GICS names
plot_df_industry20 <- plot_df_industry20 %>%
  mutate(gics_industry = as.integer(str_extract(industry, "\\d+"))) %>%
  left_join(mapping_gics_sector %>%
              select(gics_industry, gics_industry_name) %>%
              distinct(),
            by = "gics_industry")

##### 6. Dynamic y-axis scaling
y_min <- floor(min(c(plot_df_industry20$AAR.x, plot_df_industry20$CAAR)) / 2.5) * 2.5
y_max <- ceiling(max(c(plot_df_industry20$AAR.x, plot_df_industry20$CAAR)) / 2.5) * 2.5

##### 7. Define symbols
industries <- unique(plot_df_industry20$gics_industry_name)
n_industries <- length(industries)
shapes <- rep(c(16, 17, 15, 18, 19, 20), length.out = n_industries)
names(shapes) <- industries

##### 8. Plot
ggplot(plot_df_industry20, aes(x = event_day)) +
  # AAR bars
  geom_col(aes(y = AAR.x, fill = gics_industry_name), position = "dodge", alpha = 1) +
  # CAAR lines (in the foreground)
  geom_line(aes(y = CAAR, color = gics_industry_name, group = gics_industry_name), size = 1.1) +
  # CAAR points with symbols
  geom_point(aes(y = CAAR, color = gics_industry_name, shape = gics_industry_name), size = 3) +
  # Zero line
  geom_hline(yintercept = 0, color = "gray40", linewidth = 0.5) +
  
  # Grayscale
  scale_fill_grey(start = 0.2, end = 0.6) +
  scale_color_grey(start = 0.1, end = 0.5) +
  scale_shape_manual(values = shapes) +
  
  # X-axis: all event days, no minor ticks
  scale_x_continuous(
    breaks = min(plot_df_industry20$event_day):max(plot_df_industry20$event_day),
    minor_breaks = NULL
  ) +
  
  # Dynamic y-axis with 2.5% steps
  scale_y_continuous(
    breaks = seq(y_min, y_max, by = 2.5),
    limits = c(y_min, y_max)
  ) +
  
  # Labels
  labs(
    title = "Abnormal Return and Cumulative Abnormal Return in Event Window",
    subtitle = "Sector Industrials",
    x = "Days relative to Event",
    y = "Abnormal Return in percent",
    fill = "Industry",
    color = "Industry",
    shape = "Industry"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "Arial"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank()
  )





#------------------------------------------------
# Sector Consumer Discretionary
#------------------------------------------------
##### 1. Select relevant data
dfs_industry25 <- grep("^industry_25", names(capm_aar_gics), value = TRUE)

##### 2. Compute CAAR data
capm_caar_gics_plot <- map(capm_aar_gics[dfs_industry25], function(df) {
  df %>%
    arrange(event_day) %>%
    mutate(CAAR = cumsum(AAR))
})

##### 3. Merge both lists
aar_df <- map_dfr(dfs_industry25, function(name) {
  capm_aar_gics[[name]] %>%
    mutate(industry = name)
})

caar_df <- map_dfr(dfs_industry25, function(name) {
  capm_caar_gics_plot[[name]] %>%
    mutate(industry = name)
})

##### 4. Merge 
plot_df_industry25 <- left_join(aar_df, caar_df, by = c("industry", "event_day"))

##### 5. Assign GICS names
plot_df_industry25 <- plot_df_industry25 %>%
  mutate(gics_industry = as.integer(str_extract(industry, "\\d+"))) %>%
  left_join(mapping_gics_sector %>%
              select(gics_industry, gics_industry_name) %>%
              distinct(),
            by = "gics_industry")

##### 6. Dynamic y-axis scaling
y_min <- floor(min(c(plot_df_industry25$AAR.x, plot_df_industry25$CAAR)) / 2.5) * 2.5
y_max <- ceiling(max(c(plot_df_industry25$AAR.x, plot_df_industry25$CAAR)) / 2.5) * 2.5

##### 7. Define symbols
industries <- unique(plot_df_industry25$gics_industry_name)
n_industries <- length(industries)
shapes <- rep(c(16, 17, 15, 18, 19, 20), length.out = n_industries)
names(shapes) <- industries

##### 8. Plot
ggplot(plot_df_industry25, aes(x = event_day)) +
  # AAR bars
  geom_col(aes(y = AAR.x, fill = gics_industry_name), position = "dodge", alpha = 1) +
  # CAAR lines (in the foreground)
  geom_line(aes(y = CAAR, color = gics_industry_name, group = gics_industry_name), size = 1.1) +
  # CAAR points with symbols
  geom_point(aes(y = CAAR, color = gics_industry_name, shape = gics_industry_name), size = 3) +
  # Zero line
  geom_hline(yintercept = 0, color = "gray40", linewidth = 0.5) +
  
  # Grayscale
  scale_fill_grey(start = 0.2, end = 0.6) +
  scale_color_grey(start = 0.1, end = 0.5) +
  scale_shape_manual(values = shapes) +
  
  # X-axis: all event days, no minor ticks
  scale_x_continuous(
    breaks = min(plot_df_industry25$event_day):max(plot_df_industry25$event_day),
    minor_breaks = NULL
  ) +
  
  # Dynamic y-axis with 2.5% steps
  scale_y_continuous(
    breaks = seq(y_min, y_max, by = 2.5),
    limits = c(y_min, y_max)
  ) +
  
  # Labels
  labs(
    title = "Abnormal Return and Cumulative Abnormal Return in Event Window",
    subtitle = "Sector Consumer Discretionary",
    x = "Days relative to event",
    y = "Abnormal Return in percent",
    fill = "Industry",
    color = "Industry",
    shape = "Industry"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "Arial"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank()
  )





#------------------------------------------------
# Sector Information Technology
#------------------------------------------------
##### 1. Select relevant data
dfs_industry45 <- grep("^industry_45", names(capm_aar_gics), value = TRUE)

##### 2. Compute CAAR data
capm_caar_gics_plot <- map(capm_aar_gics[dfs_industry45], function(df) {
  df %>%
    arrange(event_day) %>%
    mutate(CAAR = cumsum(AAR))
})

##### 3. Merge both lists
aar_df <- map_dfr(dfs_industry45, function(name) {
  capm_aar_gics[[name]] %>%
    mutate(industry = name)
})

caar_df <- map_dfr(dfs_industry45, function(name) {
  capm_caar_gics_plot[[name]] %>%
    mutate(industry = name)
})

##### 4. Merge
plot_df_industry45 <- left_join(aar_df, caar_df, by = c("industry", "event_day"))

##### 5. Assign GICS names
plot_df_industry45 <- plot_df_industry45 %>%
  mutate(gics_industry = as.integer(str_extract(industry, "\\d+"))) %>%
  left_join(mapping_gics_sector %>%
              select(gics_industry, gics_industry_name) %>%
              distinct(),
            by = "gics_industry")

##### 6. Dynamic y-axis scaling
y_min <- floor(min(c(plot_df_industry45$AAR.x, plot_df_industry45$CAAR)) / 2.5) * 2.5
y_max <- ceiling(max(c(plot_df_industry45$AAR.x, plot_df_industry45$CAAR)) / 2.5) * 2.5

##### 7. Define symbols
industries <- unique(plot_df_industry45$gics_industry_name)
n_industries <- length(industries)
shapes <- rep(c(16, 17, 15, 18, 19, 20), length.out = n_industries)
names(shapes) <- industries

##### 8. Plot
ggplot(plot_df_industry45, aes(x = event_day)) +
  # AAR bars
  geom_col(aes(y = AAR.x, fill = gics_industry_name), position = "dodge", alpha = 1) +
  # CAAR lines (in the foreground)
  geom_line(aes(y = CAAR, color = gics_industry_name, group = gics_industry_name), size = 1.1) +
  # CAAR points with symbols
  geom_point(aes(y = CAAR, color = gics_industry_name, shape = gics_industry_name), size = 3) +
  # Zero line
  geom_hline(yintercept = 0, color = "gray40", linewidth = 0.5) +
  
  # Grayscale
  scale_fill_grey(start = 0.2, end = 0.6) +
  scale_color_grey(start = 0.1, end = 0.5) +
  scale_shape_manual(values = shapes) +
  
  # X-axis: all event days, no minor ticks
  scale_x_continuous(
    breaks = min(plot_df_industry45$event_day):max(plot_df_industry45$event_day),
    minor_breaks = NULL
  ) +
  
  # Dynamic y-axis with 2.5% steps
  scale_y_continuous(
    breaks = seq(y_min, y_max, by = 2.5),
    limits = c(y_min, y_max)
  ) +
  
  # Labels
  labs(
    title = "Abnormal Return and Cumulative Abnormal Return in Event Window",
    subtitle = "Sector Information Technology",
    x = "Days relative to event",
    y = "Abnormal Return in percent",
    fill = "Industry",
    color = "Industry",
    shape = "Industry"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "Arial"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank()
  )





#------------------------------------------------
# Comparison of the CAPM and MM results -> countries
#------------------------------------------------
ggplot(combined_car, aes(x = event_day, y = CAR, linetype = Model)) +
  geom_line(size = 1.2, color = "grey40") +
  geom_vline(xintercept = 0, color = "grey", linetype = "dashed") +
  facet_wrap(~country_name, ncol = 3, scales = "fixed") +
  scale_x_continuous(breaks = unique(combined_car$event_day)) +
  labs(
    title = "Cumulative Weighted Abnormal Returns in Event Window – CAPM vs. MM",
    # subtitle = "Comparison of CAAR in Event Window",
    x = "Days relative to event",
    y = "Abnormal Return in percent",
    linetype = "Model"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    text = element_text(family = "Arial"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )





#------------------------------------------------
# Comparison of the CAPM and MM results -> sectors
#------------------------------------------------
# --- 1. Create AAR dataframes for CAPM and MM
capm_aar_gics_df <- map_dfr(
  names(capm_aar_gics),
  ~ capm_aar_gics[[.x]] %>%
    mutate(sector = .x)
) %>%
  mutate(Model = "CAPM")

mm_aar_gics_df <- map_dfr(
  names(mm_aar_gics),
  ~ mm_aar_gics[[.x]] %>%
    mutate(sector = .x)
) %>%
  mutate(Model = "MM")

combined_ar <- bind_rows(capm_aar_gics_df, mm_aar_gics_df)

# --- 2. Compute cumulative CAAR
combined_car <- combined_ar %>%
  arrange(sector, Model, event_day) %>%
  group_by(sector, Model) %>%
  mutate(CAAR = cumsum(AAR)) %>%
  ungroup()

# --- 3. Spell out sector names
mapping_gics_unique <- mapping_gics_sector %>%
  select(gics_sector, gics_sector_name) %>%
  distinct()

combined_car <- combined_car %>%
  mutate(
    gics_sector = as.integer(str_extract(sector, "\\d+"))
  ) %>%
  left_join(mapping_gics_unique, by = "gics_sector")

combined_car <- combined_car %>%
  filter(str_detect(sector, "^sector"))


# --- 5. Create plot
ggplot(combined_car, aes(x = event_day, y = CAAR, linetype = Model)) +
  geom_line(size = 1.2, color = "grey40") +
  geom_vline(xintercept = 0, color = "grey", linetype = "dashed") +
  facet_wrap(~gics_sector_name, ncol = 2, scales = "fixed") +  # 2 columns
  scale_x_continuous(breaks = unique(combined_car$event_day)) +
  labs(
    title = "CAAR in Event Window – CAPM vs. MM",
    x = "Days relative to event",
    y = "Abnormal Return in percent",
    linetype = "Model"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    text = element_text(family = "Arial"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )



