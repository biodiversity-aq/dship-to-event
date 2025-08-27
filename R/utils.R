library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(janitor)
library(purrr)


# Safe numeric conversion
num <- function(x) suppressWarnings(as.numeric(x))

# Parse eventDate from one or two columns; returns ISO 8601 range or single date
make_event_date <- function(date_col = NULL, time_col = NULL, end_date_col = NULL, end_time_col = NULL) {
  # Helper to paste date+time if both present
  dt <- function(d, t) {
    if (!is.null(d) && !is.null(t) && !all(is.na(t))) paste0(d, "T", t) else d
  }
  
  start <- dt(date_col, time_col)
  end   <- dt(end_date_col, end_time_col)
  
  # If both start and end exist and differ, format as interval
  if (!is.null(end) && !all(is.na(end)) && !identical(start, end)) {
    out <- ifelse(!is.na(start) & !is.na(end), paste0(start, "/", end), start)
  } else {
    out <- start
  }
  out
}

# Coalesce columns by regex priority; returns first matching column present
coalesce_by_pattern <- function(df, patterns) {
  cols <- names(df)
  for (p in patterns) {
    idx <- which(str_detect(cols, regex(p, ignore_case = TRUE)))
    if (length(idx) > 0) return(df[[idx[1]]])
  }
  return(rep(NA_character_, nrow(df)))
}

# Generate a stable eventID when none is present
make_event_id <- function(df, prefix = "PS149") {
  if ("eventID" %in% names(df)) return(as.character(df$eventID))
  paste0(prefix, ":", seq_len(nrow(df)))
}

# Normalise depth/elevation to Darwin Core numeric columns
norm_depth <- function(x) {
  x <- str_replace_all(x, ",", ".")
  num(x)
}

# Clean and trim character
ch <- function(x) {
  x <- as.character(x)
  x <- str_squish(x)
  na_if(x, "")
}