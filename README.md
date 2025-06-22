# exploreARTIS

The `exploreARTIS` R package provides functions for filtering and visualizing trade and consumption data from the ARTIS (Aquatic Resource Trade In Species) database. This package is designed to facilitate and streamline investigation of the ARTIS database. Most functions are wrappers for `ggplot2::ggplot()` and can accept additional layers to further customize the figures, with the exception of `exploreARTIS::plot_sankey()` which is based on `ggsankey`. 

## Installation

### Setup
**Mac Users** Run the following commands in terminal:
```bash
brew install pkg-config
brew install gdal
```
Once installed run the following command in the R console:
```r
install.packages("sf", configure.args = "--with-proj-lib=/usr/local/lib/")
```

**Windows Users** Please make sure you have Rtools installed first. Follow the instructions [here](https://cran.r-project.org/bin/windows/Rtools/). Then run the following command in the R console:
```r
install.packages("sf")
```

### exploreARTIS Package installation
You can install this package with the devtools package. The first time you do it you will have to run 
```r 
install.packages("devtools")
library(devtools)
```

Then, you can run 
```r 
devtools::install_github("Seafood-Globalization-Lab/exploreARTIS@v1.0.0", dependencies = TRUE)
# @v1.0.0 indicates the released version of the package. 
```

After you install the exploreARTIS package, you can just load it with `library(exploreARTIS)`. You will also need to reinstall the package whenever there are updates to the package code.

## ARTIS data structure

ARTIS data consists of the following variables:

- `exporter_iso3c` (string): Exporter Country ISO 3 code
- `importer_iso3c` (string): Importer Country ISO 3 code
- `source_country_iso3c` (string): Producer Country ISO 3 code
- `dom_source` (string): Domestic Export / Foreign Export / Error Export
- `hs6` (string): 6-digit HS commodity code
- `sciname` (string): Species or Species group
- `environment` (string): Marine / Freshwater
- `method` (string): Capture / Aquaculture / Unknown
- `product_weight_t` (double): Product weight (tonnes)
- `live_weight_t` (double): Live weight (tonnes)
- `hs_version` (string): version of HS codes
- `year` (double): Year

## Filtering ARTIS Data

### Bulk ARTIS data
If you have downloaded bulk ARTIS data, it is generally split into separate csv files for each HS version and year combination. This is because the combined file is large and slow to load, sometimes causing users' R sessions to crash. If you would like to combine files into a single data frame, you will need to pick which HS version-year combinations you would like to include. Then, you can decide if you would like to filter down any of the variabales (e.g., keep select exporters, species, etc.). Once you have made these decisions, you can use the example script `scripts/filter_bulk_artis.R` as a starting place to loop through the desired ARTIS files and filter based on your specified criteria. **Note** that this is not a function, but rather an example script with comments to facilitate customization for your own project. 

### ARTIS data frames for visualization
Once you have the ARTIS data frame you are using for your analysis, you may still want to filter it for specific visualizations. For example, you may be working with all trade for a given country but want to generate a plot for just one species. You can of course use any base R or tidyverse functions to filter the data, but we also provide a function in this package to filter any of the ARTIS variables: `filter_artis()`. The filtered data frame can then be passed to any visualization function. 

```r
# loading library
library(exploreARTIS)

# Filter ARTIS data to Chilean exports of Atlantic salmon in 2016-2020
filter_artis(mini_artis,
              year = 2016:2020,
              exporter = "CHL",
              species = "salmo salar")
```

## Visualization Examples

Here are examples of all types of plots that can be created with this package. `mini_artis` is dataframe with a subset of ARTIS data that is included in the `exploreARTIS` package. 

### Bar charts

`plot_bar()` creates ranked bar plots, with bar categories indicated by the `bar_group` argument. The number of bars to display can be controlled with `top_n`. argument. 

```r
# loading library
library(exploreARTIS)

# Bar chart visualizing seafood trade volumes by exporter
plot_bar(mini_artis,
          bar_group = "exporter_iso3c")
```
<p align="center">
  <img src="images/all_trade_export_bar.png" alt="drawing" width="50%"/>
</p>

Bar charts can optionally be filled by an ARTIS variable. 

```r
# loading library
library(exploreARTIS)

# Bar chart visualizing seafood trade volumes by exporter and filling by export source
plot_bar(mini_artis,
        bar_group = "exporter_iso3c",
        fill_type = "dom_source")
```
<p align="center">
  <img src="images/all_trade_export_dom_source_bar.png" alt="drawing" width="50%"/>
</p>

### Line and area plots

`plot_ts()` creates time series line or area plots for any specified `artis_var`. The `plot.type` argument allows options of "line" (default) or "stacked" to change plot views. To keep the number of colors reasonable, the `prop_flow_cutoff` argument groups lines falling below the cut-off into "other" and this can be adjusted to show more or fewer lines/fills. 

```r
# loading library
library(exploreARTIS)

plot_ts(mini_artis,
        artis_var = "exporter_iso3c")
```

<p align="center">
  <img src="images/line_all_trade.png" alt="drawing" width="50%"/>
</p>

A stacked line graph of all export partners in the ARTIS dataset
```r
# loading library
library(exploreARTIS)

plot_ts(mini_artis,
        artis_var = "exporter_iso3c",
        plot.type = "stacked")
```

<p align="center">
  <img src="images/line_stacked_all_trade.png" alt="drawing" width="50%"/>
</p>

### Sankey plots

`plot_sankey()` creates a sankey plot to display flows among nodes across columns. This function is flexible in allowing the user to specify which data columns should be used to produce the colunns of the sankey plot. This function includes the argument `prop_flow_cutoff` to control how many groups are included in "other" (which can help keep the larger flows readable). It also includes an argumen to drop the group "other" entirely if preferred. 

```r
# loading library
library(exploreARTIS)

# Sankey plot of all seafood trade
plot_sankey(mini_artis,
            cols = c("sciname", "exporter_iso3c", "importer_iso3c"))

```

<p align="center">
  <img src="images/sankey_all_trade.png" alt="drawing" width="50%"/>
</p>

### Chord diagrams

`plot_chord()` creates a chord diagram for visualizing flows among countries/regions. 

```r
# loading library
library(exploreARTIS)

# Chord diagram of all seafood trade
plot_chord(mini_artis,
          region_colors = region7_palette)
```
![Chord Diagram - all seafood trade](images/all_trade_chord.png)

Individual countries can be pulled out to highlight their trade by specifying the country/countries' iso3c code(s) in the `focal_country` argument. 

```r
# loading library
library(exploreARTIS)

# Chord diagram of all seafood trade with Vietnam highlighted
plot_chord(mini_artis,
          focal_country = "VNM",
          region_colors = region7_palette)
```
![Chord Diagram - all seafood trade](images/all_trade_chord_vnm.png)

### Chloropleth and flow arrow maps 

`plot_map()` creates maps that are optionally colored by `country_fill` and optionally include flow arrows colored by flow volume with `flow_arrows`. The number of arrows can be specified with `n_flows`. 

```r
# loading library
library(exploreARTIS)

# Map of top seafood exports and flows
plot_map(mini_artis,
        country_fill = "importer_iso3c",
        flow_arrows = TRUE,
        arrow_label = "Trade (live t)",
        fill_label = "Import (live t)")
```

<p align="center">
  <img src="images/all_map_flows.png" alt="drawing" width="75%"/>
</p>

Individual country's trade flows can be isolated by filtering the importer or exporter column before passing it to the plot function. 

```r
# loading library
library(exploreARTIS)

# Map of seafood exports from Chile
mini_artis %>% filter(exporter_iso3c == "CHL") %>%
                plot_map(country_fill = "importer_iso3c",
                          flow_arrows = TRUE,
                          arrow_label = "Trade (live t)",
                          fill_label = "Import (live t)")
```

<p align="center">
  <img src="images/chl_map_flows.png" alt="drawing" width="75%"/>
</p>

### Facetting

Both `plot_ts()` and `r plot_bar()` allow facetting by an ARTIS variable with the `facet_variable` argument. If a facet variable is specified then `facet_values` must also be defined, either as a number (the number of facets to create) or a vector (the specific facets to create). 

```r
# loading libraries
library(exploreARTIS)

# Area plot of top importers facetted by method
plot_ts(mini_artis,
        artis_var = "importer_iso3c",
        plot.type = "stacked",
        facet_variable = "method",
        facet_values = c("capture", "aquaculture"))
```

<p align="center">
  <img src="images/area_plot_facetted.png" alt="drawing" width="50%"/>
</p>

```r
# loading libraries
library(exploreARTIS)

# Bar plot of top importers facetted by method
plot_bar(mini_artis,
        bar_group = "importer_iso3c",
        facet_variable = "method",
        facet_values = c("capture", "aquaculture"))
```
<p align="center">
  <img src="images/bar_plot_facetted.png" alt="drawing" width="50%"/>
</p>


## Beta Version: Building a DuckDB Database from KNB Resource Map

As a powerful addition to the `exploreARTIS` toolkit, we provide a new experimental function: `process_knb_to_duckdb()`. This function is designed to streamline the **extraction, transformation, and storage** of ARTIS and related data from [KNB (Knowledge Network for Biocomplexity)](https://knb.ecoinformatics.org/) into a fast and lightweight **[DuckDB](https://duckdb.org/)** database for analysis.

### What is DuckDB?

[**DuckDB**](https://duckdb.org/) is an **in-process SQL OLAP database management system** — think of it as SQLite for analytics. It is designed for speed, portability, and ease of use, especially with columnar data and Parquet/CSV formats. DuckDB is ideal for local, analytical queries and works seamlessly with R, Python, and other modern data tools.

**Why DuckDB?**

* **No server needed** — it runs in-process.
* **Fast analytics** — optimized for vectorized execution and columnar storage.
* **Native support for Parquet/CSV** — perfect for working with ARTIS and KNB data.
* **Portable** — single-file `.duckdb` database makes sharing easy.

> **Note:** DuckDB integration is currently **experimental** for this package. We welcome feedback and contributions!

---

### Function: `process_knb_to_duckdb()`

This function pulls a **KNB resource map**, downloads the data files (CSV/Parquet), extracts them, and builds a **DuckDB database**. It optionally filters ARTIS trade and consumption data by HS version and year, and generates a `metadata` table to describe all ingested tables.

#### Usage

```r
process_knb_to_duckdb(
  resource_map_id = "resource_map_urn:uuid:...",     # required
  duckdb_name = "ARTIS_SAU.duckdb",                  # optional, default name
  directory = "data_knb",                            # local folder to save files
  timeout_seconds = 600,                             # optional timeout for downloads
  wait_minutes = 1,                                  # optional pause between download and ingest
  artis_custom_timeseries = TRUE,                    # whether to filter ARTIS tables
  hs_version_filter = c("HS_2012"),                  # optional HS versions to keep
  year_filter = 2015:2020                            # optional year range
)
```

---

### Parameters

| Argument                  | Type               | Description                                                                                        |
| ------------------------- | ------------------ | -------------------------------------------------------------------------------------------------- |
| `resource_map_id`         | `character`        | **REQUIRED.** The KNB resource map ID (e.g., `"resource_map_urn:uuid:..."`) to download data from. |
| `duckdb_name`             | `character`        | Optional. The name of the DuckDB file to create. Default is `"ARTIS_SAU.duckdb"`.                  |
| `directory`               | `character`        | Directory where all files will be downloaded and unzipped. Must be writeable.                      |
| `timeout_seconds`         | `numeric`          | Timeout in seconds for download requests. Default is `600`.                                        |
| `wait_minutes`            | `numeric`          | Optional pause time (in minutes) after download, before ingestion begins. Default is `1`.          |
| `artis_custom_timeseries` | `logical`          | If `TRUE`, will apply filtering to ARTIS tables (`trade`, `consumption`) by HS version and year.   |
| `hs_version_filter`       | `character vector` | HS version(s) to retain (e.g., `"HS_2012"`). Used only if filtering is enabled.                    |
| `year_filter`             | `numeric vector`   | Year(s) to retain (e.g., `2015:2020`). Used only if filtering is enabled.                          |

---

### Outputs

* A **`.duckdb`** database file is saved to disk at the path specified by `duckdb_name`.
* Includes the following tables if present in the KNB dataset:

  * `trade` – ARTIS trade flows
  * `consumption` – ARTIS consumption estimates
  * `baci` – BACI bilateral trade
  * `code_max_resolved` – Taxonomic resolution table
  * `countries` – Country reference
  * `products` – Product reference
  * `sciname` – Species names
  * `metadata` – Table names and descriptions for reference

---

### Example Workflow

```r
# Load exploreARTIS if needed
library(exploreARTIS)

# Run the DuckDB builder
process_knb_to_duckdb(
  resource_map_id = "resource_map_urn:uuid:xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx",
  directory = "data_knb",
  artis_custom_timeseries = TRUE,
  hs_version_filter = c("HS_2012"),
  year_filter = 2015:2020
)
```

---

### Notes

* If running this on a new machine, ensure the following R packages are installed:

  * `dataone`, `datapack`, `duckdb`, `arrow`, `utils`, `tools`
* The script automatically handles zipped files and skips files larger than 50MB from API download (downloads them via direct HTTP).
* Use [DuckDB CLI](https://duckdb.org/docs/installation/) or the `duckdb` R package to explore the generated database.






