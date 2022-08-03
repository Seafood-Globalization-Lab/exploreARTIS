# exploreARTIS

Visualize and summarize ARTIS data. ARTIS data consists of:
- exporter_iso3c: (string) Exporter Country ISO 3 code
- importer_iso3c: (string) Importer Country ISO 3 code
- source_country_iso3c: (string) Producer Country ISO 3 code
- dom_source: (string) Domestic Export / Foreign Export / Error Export
- hs6: (string) 6-digit HS commodity code
- sciname: Species or Species group
- environment: Marine / Freshwater
- method: Capture / Aquaculture / Unknown
- product_weight_t: (double) Product weight (tonnes)
- live_weight_t: (double) Live weight (tonnes)
- hs_version: (string) version of HS codes
- year: (double) Year
- snet_est: (string) Maximum / Midpoint / Minimum estimate for domestic export and foreign export calculation

You can install this package with the devtools package. The first time you do it you will have to run install.package("devtools"). After that, you will only need to run library(devtools). Then, you can run devtools::install_github("Seafood-Globalization-Lab/exploreARTIS").

After you install the exploreARTIS package, you can just load it with library(exploreARTIS). You will also need to reinstall the package whenever there are updates to the package code.

