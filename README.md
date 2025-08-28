# DSHIP to Darwin Core Event

This repository provides an R workflow to transform [DSHIP](https://spaces.awi.de/spaces/EFPW/pages/324149323/DSHIP-System) station/event metadata exports (TSV format) into a **Darwin Core Event** table, following the [Darwin Core Event Core (2024-02-19)](https://rs.gbif.org/core/dwc_event_2024-02-19.xml) standard.

The workflow reads the raw `.tsv` file, cleans and normalizes the fields, and outputs two processed tables:

-   **`station.txt`** — simplified station information (DSHIP actions).
-   **`event.txt`** — Darwin Core Event table, including event timing, geometry, depth, and metadata.

------------------------------------------------------------------------

## Workflow Overview

1.  **Read & Clean Input**
    -   Input: A tsv file from o2a export.
    -   Reads with correct encoding (`windows-1252`) to preserve special characters.
    -   Fixes date/time separation and converts comma decimals to points.
2.  **Extract Station Information**
    -   Filters rows with `science_activity_number > 0`.
    -   Selects key columns describing device operations, actions, coordinates, depth, and comments.
    -   Writes to `data/processed/station.txt`.
3.  **Transform to Darwin Core Event**
    -   Identifies valid start/end pairs (preferring *profile* over *station* actions).
    -   Derives:
        -   `eventID` (from `device_operation`)
        -   `eventType` (`profile` or `station`)
        -   `eventDate` and `eventTime` in ISO 8601 (date vs datetime intervals).
        -   Geometry: `footprintWKT`, centroid (`decimalLatitude`, `decimalLongitude`), and `coordinateUncertaintyInMeters`.
        -   Depth range (`start_depth_m`, `end_depth_m`).
        -   Metadata (`sensor_id`, `device_operation_label`, etc.).
    -   Writes to `data/processed/event.txt`.

------------------------------------------------------------------------

## Repository Structure

```         
├── R/
│   └── dship_to_event.R      # Main script
├── data/
│   └── processed/            # Output tables (station.txt, event.txt)
├── renv/                     # renv library folder
├── renv.lock                 # Reproducible package versions
└── README.md                 # Documentation (this file)
```

## Getting Started

This project uses renv to manage R packages for reproducibility.

1.  Clone the repository
2.  Open R / RStudio in the project directory.
3.  Restore the package environment:

``` r
renv::restore()
```

4.  Run the script:

``` r
source("R/dship_to_event.R")
```

## Notes & Assumptions

```         
•   If both profile and station actions exist, the workflow uses profile start/end.
•   Coordinates are rounded to 4 decimal places.
•   coordinateUncertaintyInMeters is calculated using obistools::calculate_centroid() (or set to 0 for identical start/end points).
•   eventDate handling:
•   Same-day events → YYYY-MM-DD with separate eventTime.
•   Multi-day events → full datetime interval in eventDate, blank eventTime.
```
