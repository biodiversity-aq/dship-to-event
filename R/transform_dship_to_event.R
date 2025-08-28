library(tidyverse)
library(here)
library(readr)
library(stringr)
library(janitor)
library(lubridate)
library(glue)
library(obistools)


# ---- Read raw TSV -----------------------------------------------------------
url  <- "https://o2a-data.de/static/events/PS149.tsv"

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
# station information for stations with science_activity_number > 0
df_station <- df %>%
  filter(science_activity_number > 0) %>%
  select(device_operation,
         device,
         action,
         action_comment,
         device_operation_label,
         event_time,
         latitude_deg,
         longitude_deg,
         depth_m,
         sensor_id,
         speed_kn,
         wind_dir,
         wind_velocity,
         device_operation_subdevices,
         device_operation_devicetypes,
         device_comment,
         event_comment)

write_tsv(df_station, here::here("data", "processed", "station.txt"), na = "")

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
    
    start_time = event_time[idx_start],
    end_time   = event_time[idx_end],
    
    # coords from chosen rows
    lat_start  = format(round(latitude_deg[idx_start], 4), nsmall = 4),
    lon_start  = format(round(longitude_deg[idx_start], 4), nsmall = 4),
    lat_end    = format(round(latitude_deg[idx_end], 4), nsmall = 4),
    lon_end    = format(round(longitude_deg[idx_end], 4), nsmall = 4),
    
    # depths from chosen rows
    start_depth_m = depth_m[idx_start],
    end_depth_m   = depth_m[idx_end],
    
    # carry-through metadata from the chosen rows
    sensor_id = coalesce(sensor_id[idx_start], sensor_id[idx_end]),
    device_operation_label      = coalesce(device_operation_label[idx_start],      device_operation_label[idx_end]),
    device_operation_subdevices = coalesce(device_operation_subdevices[idx_start], device_operation_subdevices[idx_end]),
    device_operation_devicetypes = coalesce(device_operation_devicetypes[idx_start], device_operation_devicetypes[idx_end]),
    device = coalesce(device[idx_start], device[idx_end]),
    .groups = "drop"
  ) %>%
  # ensure complete pairs
  filter(!is.na(start_time), !is.na(end_time)) %>%
  mutate(
    eventID   = device_operation,
    eventType = if_else(use_profile, "profile", "station")
  )

# 2) Event timing (ISO interval) and eventDate --------------------------------
df_time <- df_pairs %>%
  # ensure POSIXct in UTC
  mutate(
    start_timestamp = with_tz(start_time, "UTC"),
    end_timestamp   = with_tz(end_time, "UTC"),
    start_time = strftime(start_timestamp, "%H:%M:%S"),
    end_time   = strftime(end_timestamp,   "%H:%M:%S"),
    same_day   = as_date(start_timestamp) == as_date(end_timestamp),
    start_date = format(as_date(start_timestamp), "%Y-%m-%d"),
    end_date = format(as_date(end_timestamp), "%Y-%m-%d"),
    eventDate = if_else(
      same_day,
      # same day → date only
      start_date,
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
        start_time, "/", end_time
      ),
      # spans days → blank
      NA_character_
    )
  ) %>%
  select(eventID, eventType, start_date, end_date, start_time, end_time, eventDate, eventTime)

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
df_geometry <- df_geometry_inputs %>%
  rowwise() %>%
  mutate(
    centroid_list = list({
      if (!is.na(footprintWKT)) {
        out <- try(calculate_centroid(footprintWKT), silent = TRUE)
        if (inherits(out, "try-error")) {
          tibble(decimalLongitude = NA_real_, decimalLatitude = NA_real_, coordinateUncertaintyInMeters = NA_real_)
        } else {
          out
        }
      } else if (isTRUE(same_point)) {
        tibble(decimalLongitude = lon_start,
               decimalLatitude  = lat_start,
               coordinateUncertaintyInMeters = 0)
      } else {
        tibble(decimalLongitude = NA_real_, decimalLatitude = NA_real_, coordinateUncertaintyInMeters = NA_real_)
      }
    })
  ) %>%
  ungroup() %>%
  tidyr::unnest(centroid_list) %>%
  mutate(
    decimalLatitude  = format(round(decimalLatitude, 4), nsmall = 4),  # 4 decimal places
    decimalLongitude = format(round(decimalLongitude, 4), nsmall = 4),
    coordinateUncertaintyInMeters = round(coordinateUncertaintyInMeters, 0)  # integers
  ) # %>%
  # select(
  #   eventID,
  #   decimalLatitude,
  #   decimalLongitude,
  #   coordinateUncertaintyInMeters,
  #   footprintWKT
  # )

# 4) Depths only ---------------------------------------------------------------
df_depths <- df_pairs %>%
  transmute(
    eventID,
    start_depth_m,
    end_depth_m
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
         start_date, end_date, start_time, end_time, start_depth_m, end_depth_m, eventDate, eventTime,
         decimalLatitude, decimalLongitude, coordinateUncertaintyInMeters, footprintWKT) 

station <- events %>% select(
  eventID, eventType, device, device_operation_label, device_operation_subdevices, device_operation_devicetypes, 
  start_date, end_date, start_time, end_time, start_depth_m, end_depth_m)

# ---- Write out --------------------------------------------------------------
write_tsv(station, here::here("data", "processed", "station.txt"), na = "")
out_path <- here::here("data", "processed", "event.txt")
readr::write_tsv(events, out_path, na = "")
message("Wrote Darwin Core Event to ", out_path)
