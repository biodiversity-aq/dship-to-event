# DSHIP to Darwin Core Event

This repository provides an R workflow to transform [DSHIP](https://spaces.awi.de/spaces/EFPW/pages/324149323/DSHIP-System) station/event metadata exports (TSV format) into a tables containing the [Darwin Core Event Core (2024-02-19)](https://rs.gbif.org/core/dwc_event_2024-02-19.xml) standard.


## Repository Structure

```         
├── .github/
│   └── workflows/            # GitHub Actions workflows
├── R/
│   └── transform_dship_to_event.R  # Main transformation script
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
source("R/transform_dship_to_event.R")
```

## Automated Processing

This repository includes a GitHub Action that automatically runs the transformation script every 20 minutes. The workflow:

1. Sets up the R environment with the correct version (4.5.1)
2. Restores the renv package environment (cached for faster execution)
3. Runs the transformation script using the `O2A_EXPORT` environment variable
4. Commits and pushes any changes to the processed data files

### Performance Optimization

The workflow has been optimized for fast execution:
- **R packages caching**: Both the global renv cache (`~/.local/share/renv`) and project-specific library (`renv/library`) are cached
- **Cache invalidation**: The cache is automatically refreshed when `renv.lock` changes
- **Vectorized operations**: The R script uses optimized vectorized operations instead of row-by-row processing for geometry calculations
- **Reduced redundancy**: Eliminated duplicate data processing and file writes
- **System dependencies**: Optimized apt-get installation with minimal output and no upgrades
- **Concurrency control**: Prevents multiple workflow runs from overlapping
- **Result**: Subsequent runs are significantly faster, saving GitHub Actions minutes

### Configuration

The GitHub Action requires the `O2A_EXPORT` variable to be set in the repository variables. This should contain the URL to the DSHIP data export.

To configure this:
1. Go to your repository Settings → Secrets and variables → Actions
2. Add a new repository variable named `O2A_EXPORT` with the appropriate URL value

### Manual Execution

You can also manually trigger the workflow from the Actions tab in GitHub, or run the script locally as described above.

## Notes & Assumptions

      
- If both profile and station actions exist, the workflow uses profile start/end.
- Coordinates are rounded to 4 decimal places.
- coordinateUncertaintyInMeters is calculated using obistools::calculate_centroid() (or set to 0 for identical start/end points).
- eventDate handling:
  - Same-day events → YYYY-MM-DD with separate eventTime.
  - Multi-day events → full datetime interval in eventDate, blank eventTime.

