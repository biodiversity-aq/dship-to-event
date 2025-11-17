library(dplyr)      
library(readr)      
library(stringr)   
library(lubridate)  
library(glue)       
library(here)       
library(obistools) 
library(janitor)

# ---- Read raw TSV -----------------------------------------------------------
url  <- Sys.getenv("O2A_EXPORT")

# Read as Latin-1 (so special chars don’t break) 
lines_latin1 <- read_lines(url, locale = locale(encoding = "windows-1252"))

# Replace the *date–TAB–time* with a single space (or use "T" if you prefer)
# Works for YYYY/MM/DD or YYYY-MM-DD
fixed_lines <- str_replace_all(
  lines_latin1,
  "(?<=\\d{4}[/-]\\d{2}[/-]\\d{2})\\t(?=\\d{2}:\\d{2}:\\d{2})",
  "T"
)

# Parse the cleaned TSV, translate comma to point for decimals
df <- read_tsv(I(fixed_lines), col_types = cols(.default = col_guess()), locale = locale(decimal_mark = ",")) %>%
  clean_names()

# ---- Write out station information ------------------------------------------
# Note: station.txt will be written at the end from the 'events' dataframe to avoid duplicate processing

# ---- Transform to Darwin Core Event ----------------------------------------
# 0) Minimal columns we need early on ----------------------------------------
df_actions <- df %>%
  filter(science_activity_number > 0) %>%
  mutate(
    event_time = ymd_hms(event_time, tz = "UTC"),
    action = str_to_lower(action),
    is_profile_start = action == "profile start",
    is_profile_end   = action == "profile end",
    is_station_start = action == "station start",
    is_station_end   = action == "station end"
  ) %>%
  select(
    device_operation,
    sensor_id,
    device_operation_label,
    device_operation_subdevices,
    device_operation_devicetypes,
    device,
    event_time,
    latitude_deg,
    longitude_deg,
    depth_m,
    is_profile_start,
    is_profile_end,
    is_station_start,
    is_station_end
  ) %>%
  filter(is_profile_start | is_profile_end | is_station_start | is_station_end) %>%
  arrange(device_operation, event_time)

# 1) Decide which pair to use per operation (profile preferred) --------------
df_pairs <- df_actions %>%
  group_by(device_operation) %>%
  summarise(
    use_profile = any(is_profile_start) & any(is_profile_end),
    
    idx_start = if (use_profile) which(is_profile_start)[1] else which(is_station_start)[1],
    idx_end   = if (use_profile) which(is_profile_end)[1]   else which(is_station_end)[1],
    
    time_start = event_time[idx_start],
    time_end   = event_time[idx_end],
    
    # coords from chosen rows
    lat_start  = format(round(latitude_deg[idx_start], 4), nsmall = 4),
    lon_start  = format(round(longitude_deg[idx_start], 4), nsmall = 4),
    lat_end    = format(round(latitude_deg[idx_end], 4), nsmall = 4),
    lon_end    = format(round(longitude_deg[idx_end], 4), nsmall = 4),
    
    # depths from chosen rows
    depth_start_m = depth_m[idx_start],
    depth_end_m   = depth_m[idx_end],
    
    # carry-through metadata from the chosen rows
    sensor_id = coalesce(sensor_id[idx_start], sensor_id[idx_end]),
    device_operation_label      = coalesce(device_operation_label[idx_start],      device_operation_label[idx_end]),
    device_operation_subdevices = coalesce(device_operation_subdevices[idx_start], device_operation_subdevices[idx_end]),
    device_operation_devicetypes = coalesce(device_operation_devicetypes[idx_start], device_operation_devicetypes[idx_end]),
    device = coalesce(device[idx_start], device[idx_end]),
    .groups = "drop"
  ) %>%
  # ensure complete pairs
  filter(!is.na(time_start), !is.na(time_end)) %>%
  mutate(
    eventID   = device_operation,
    eventType = if_else(use_profile, "profile", "station")
  )

# 2) Event timing (ISO interval) and eventDate --------------------------------
df_time <- df_pairs %>%
  # ensure POSIXct in UTC
  mutate(
    start_timestamp = with_tz(time_start, "UTC"),
    end_timestamp   = with_tz(time_end, "UTC"),
    time_start = strftime(start_timestamp, "%H:%M:%S"),
    time_end   = strftime(end_timestamp,   "%H:%M:%S"),
    same_day   = as_date(start_timestamp) == as_date(end_timestamp),
    date_start = format(as_date(start_timestamp), "%Y-%m-%d"),
    date_end = format(as_date(end_timestamp), "%Y-%m-%d"),
    eventDate = if_else(
      same_day,
      # same day → date only
      date_start,
      # spans days → full datetime range with Z
      paste0(
        strftime(start_timestamp, "%Y-%m-%dT%H:%M:%SZ"),
        "/",
        strftime(end_timestamp, "%Y-%m-%dT%H:%M:%SZ")
      )
    ),
    eventTime = if_else(
      same_day,
      # same day → time range only
      # TODO: consider adding "Z" to times?
      paste0(
        time_start, "/", time_end
      ),
      # spans days → blank
      NA_character_
    )
  ) %>%
  select(eventID, eventType, date_start, date_end, time_start, time_end, eventDate, eventTime)

# 3) Geometry: footprintWKT, centroid, uncertainty ----------------------------
df_geometry_inputs <- df_pairs %>%
  mutate(
    same_point = !is.na(lat_start) & !is.na(lon_start) &
      !is.na(lat_end)   & !is.na(lon_end)   &
      lat_start == lat_end & lon_start == lon_end,
    footprintWKT = case_when(
      isTRUE(same_point) ~ NA_character_,
      is.na(lat_start) | is.na(lon_start) | is.na(lat_end) | is.na(lon_end) ~ NA_character_,
      TRUE ~ glue("LINESTRING({lon_start} {lat_start}, {lon_end} {lat_end})")  # lon lat order
    )
  ) %>%
  select(
    eventID,
    lat_start, lon_start, lat_end, lon_end,
    footprintWKT,
    same_point
  )

# Use obistools::calculate_centroid() where we have a LINESTRING; else handle point/NA
# Optimized: process in groups rather than row-by-row for better performance
df_geometry <- df_geometry_inputs %>%
  mutate(
    # Pre-calculate which rows need centroid calculation
    needs_centroid = !is.na(footprintWKT),
    # Initialize result columns
    decimalLongitude = NA_real_,
    decimalLatitude = NA_real_,
    coordinateUncertaintyInMeters = NA_real_
  )

# Process rows with same_point (fastest case)
same_point_rows <- which(df_geometry$same_point & is.na(df_geometry$footprintWKT))
if (length(same_point_rows) > 0) {
  df_geometry$decimalLongitude[same_point_rows] <- as.numeric(df_geometry$lon_start[same_point_rows])
  df_geometry$decimalLatitude[same_point_rows] <- as.numeric(df_geometry$lat_start[same_point_rows])
  df_geometry$coordinateUncertaintyInMeters[same_point_rows] <- 0
}

# Process rows that need centroid calculation
centroid_rows <- which(df_geometry$needs_centroid)
if (length(centroid_rows) > 0) {
  for (i in centroid_rows) {
    out <- try(calculate_centroid(df_geometry$footprintWKT[i]), silent = TRUE)
    if (!inherits(out, "try-error") && nrow(out) > 0) {
      df_geometry$decimalLongitude[i] <- out$decimalLongitude[1]
      df_geometry$decimalLatitude[i] <- out$decimalLatitude[1]
      df_geometry$coordinateUncertaintyInMeters[i] <- out$coordinateUncertaintyInMeters[1]
    }
  }
}

# Format coordinates
df_geometry <- df_geometry %>%
  mutate(
    decimalLatitude  = format(round(decimalLatitude, 4), nsmall = 4),  # 4 decimal places
    decimalLongitude = format(round(decimalLongitude, 4), nsmall = 4),
    coordinateUncertaintyInMeters = round(coordinateUncertaintyInMeters, 0)  # integers
  ) %>%
  select(-needs_centroid)

# 4) Depths only ---------------------------------------------------------------
df_depths <- df_pairs %>%
  transmute(
    eventID,
    depth_start_m,
    depth_end_m
  )

# 5) Metadata to keep ----------------------------------------------------------
df_meta <- df_pairs %>%
  transmute(
    eventID,
    sensor_id,
    device_operation_label,
    device_operation_subdevices,
    device_operation_devicetypes,
    device
  )

# 6) Final join (only when you’re ready) --------------------------------------
events <- df_time %>%
  left_join(df_geometry, by = "eventID") %>%
  left_join(df_depths,   by = "eventID") %>%
  left_join(df_meta,     by = "eventID") %>%
  select(eventID, eventType, device, device_operation_label, device_operation_subdevices, device_operation_devicetypes, sensor_id,
         date_start, date_end, time_start, time_end, lat_start, lon_start, lat_end, lon_end, depth_start_m, depth_end_m, eventDate, eventTime,
         decimalLatitude, decimalLongitude, coordinateUncertaintyInMeters, footprintWKT) 

station <- events %>% select(
  eventID, eventType, device, device_operation_label, device_operation_subdevices, device_operation_devicetypes, 
  date_start, date_end, time_start, time_end, lat_start, lon_start, lat_end, lon_end, depth_start_m, depth_end_m)

# ---- Write out --------------------------------------------------------------
write_tsv(station, here::here("data", "processed", "station.txt"), na = "")
out_path <- here::here("data", "processed", "event.txt")
readr::write_tsv(events, out_path, na = "")
message("Wrote Darwin Core Event to ", out_path)

