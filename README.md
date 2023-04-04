
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pacta.supervisor.analysis

<!-- badges: start -->
<!-- badges: end -->

This repository is meant to host all scripts and R functions used to
generate all output used in PACTA 4 Banks projects run by bank
supervisors or similar institutions.

All third party data must be input by the user and is not part of this
repository.

File paths for input and output files are set up in the `.env` file as
explained below.

Scripts will be found at root level.

## Installation

You can install the development version of pacta.supervisor.analysis
from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("RMI-PACTA/pacta.supervisor.analysis")
```

## dotenv

A file called `.env` must be created in the root of this directory. The
file must have the following variables:

``` bash
# I/O directories
DIR_SCENARIO="PATH/TO/SCENARIO/FOLDER"
DIR_ABCD="PATH/TO/ABCD/FOLDER"
DIR_RAW="PATH/TO/RAW_LOANBOOK/FOLDER"
DIR_MATCHED="PATH/TO/MATCHED_LOANBOOK/FOLDER"
DIR_OUTPUT="PATH/TO/OUTPUT/FOLDER"

# input file names
FILENAME_SCENARIO_TMS="scenario_tms.csv"
FILENAME_SCENARIO_SDA="scenario_sda.csv"
FILENAME_REGIONS_GECO_2022="region_geco2022.csv"
FILENAME_REGIONS_WEO_2022="region_weo2022.csv"
FILENAME_ABCD="abcd_data.csv"

# project parameters
PARAM_SCENARIO_SOURCE="weo_2022"
PARAM_SCENARIO_SELECT="nze_2050"
PARAM_REGION_SELECT="global"
# normally the start year should correspond with year of the publication of the
# scenario in use
PARAM_START_YEAR=2022
# regions must be available for the selected scenario
PARAM_BENCHMARK_REGIONS="global,european union"
```

These configurations set up required directories (prefixed `DIR_`),
input files (prefixed `FILENAME_`) and project parameters (prefixed
`PARAM_`) which will all be used in the work flow. Please ensure to set
directories and file names that exist in your work environment and that
the paramters are consistent with the input files available to you.

## Running the Analysis

Once you have set up the .env file correctly, you can simply run the
`workflow_supervisor_analysis.R` entirely to:

- set up project configurations
- load required input files
- prepare raw loan book data for batch processing in supervisor analysis
- run a simplified version of the matching (NOTE: in a real project, you
  will have to follow the matching guidance as provided in the general
  PACTA for Banks user guide. There is currently no way you can around
  some manual matching or at least manual validation for obtaining
  reasonable PACTA for Banks results!)
- create a matched data set for calculations of benchmarks (no manual
  matching required)
- batch run PACTA for Banks TMS and SDA calculations for all groups
- generate all standard PACTA for Banks plots and ouput files for a
  given combination of `region` and `sector`
- prepare unweighted PACTA for Banks results at the company level as a
  preparatory step for calculating the alignment metrics.
- calculate alignment metrics both at the company level and the group
  level
- tweak the plot code to output supervisor-focused plots based on the
  alignment metrics, including:
  - Sankey plot of aligned/unaligned companies the groups are exposed to
  - Timeline plot that shows the forward-looking trend of the aligment
    metric over time (net, buildout and phaseout)
  - Scatter plot that allows for peer comparison of alignment metric
    across companies or groups (automotive and power sectors only)
