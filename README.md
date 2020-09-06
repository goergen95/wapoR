<!-- badges: start -->
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

wapoR - Acces to the FAO GISManager API from R
----------------------------------------------

This package includes functionalities to search for datasets in
different collections of the FAO GISManager API, query the metadata of
included products and finally download GTiff files of the desired
spatio-temporal extent to your local machine. This package is at an
experimental state and currently only supports the download of raster
files.

**Disclaimer**
This package is in no way associated with the WaPOR portal or the FAO.

To install the package run:

```r
remotes::install_github("goergen95/wapoR")
```

Once installed, you can load the library and start looking at the
available collections.

```r
library(wapoR)
collections = wapor_collections()
collections
```

    ##             code 
    ## 1       AQUAMAPS
    ## 2           ASIS
    ## 3        C2ATLAS
    ## 4         CHIRPS
    ## 5           DLMF
    ## 6        FAOSTAT
    ## 7      GAEZ_2015
    ## 8            GLW
    ## 9           NASA
    ## 10 NATURAL_EARTH
    ## 11          NMME
    ## 12          RDMS
    ## 13        RICCAR
    ## 14      RICCAR_2
    ## 15           RVF
    ## 16         WAPOR
    ## 17       WAPOR_2
    ## 18          WPOP
    ##                                                               caption
    ## 1                    Global spatial database on water and agriculture
    ## 2                                     Agriculture Stress Index System
    ## 3                                                Climate Change ATLAS
    ## 4  Climate Hazard group InfraRed Precipitation with Stations (CHIRPS)
    ## 5                            Desert Locust Monitoring and Forecasting
    ## 6                                  FAO Corporate Statistical Database
    ## 7                                 Global Agro-Ecological Zones (2015)
    ## 8                                      Gridded Livestock of the World
    ## 9                National Aeronautics and Space Administration (NASA)
    ## 10                                                      Natural Earth
    ## 11                         North American Multi-Model Ensemble (NMME)
    ## 12                                 Regional Drought Monitoring System
    ## 13                     Regional Arab Climate Change Assessment Report
    ## 14                     Regional Arab Climate Change Assessment Report
    ## 15                                                  Rift Valley Fever
    ## 16                  FAO Water Productivity Open-access portal (WaPOR)
    ## 17                  FAO Water Productivity Open-access portal (WaPOR)
    ## 18                                                   WorldPop project

In its current state, the package mainly supports products from the
WAPOR collections. For some other collection it is also possible to
query and download raster datasets. However, for some of the collections
errors will be returned if queried. Currently, you might run into errors
for these collections: 
- C2ATLAS 
-  DLMF 
-  NASA 
-  NATURAL\_EARTH

To query the available products within the collections we can run
\`wapor\_products(“<collection_name>”)\`.

```r
products = wapor_products("WAPOR_2")
length(products)
str(products[1:3], max.level = 2)
```
    ## [1] 196
    
    ## List of 3
    ##  $ L1_GBWP_A:List of 2
    ##   ..$ product:'data.frame':  1 obs. of  3 variables:
    ##   ..$ meta   :'data.frame':  1 obs. of  12 variables:
    ##  $ L1_NBWP_A:List of 2
    ##   ..$ product:'data.frame':  1 obs. of  3 variables:
    ##   ..$ meta   :'data.frame':  1 obs. of  12 variables:
    ##  $ L1_AETI_A:List of 2
    ##   ..$ product:'data.frame':  1 obs. of  3 variables:
    ##   ..$ meta   :'data.frame':  1 obs. of  12 variables:


In the WAPOR\_2 collection we have a total number of 196. As you can see
from the structured output, each element in the list object represents
one product, e.g. L1\_GBWP\_A. In the lower level you find tow distinct
objects, one called product the other one called meta. In the product
object you will get some general information about the specific product.

```r
products[[1]]$product
```

<table>
<thead>
<tr>
<th style="text-align:left;">
code
</th>
<th style="text-align:left;">
caption
</th>
<th style="text-align:left;">
description
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
L1\_GBWP\_A
</td>
<td style="text-align:left;">
Gross Biomass Water Productivity
</td>
<td style="text-align:left;">
The annual Gross Biomass Water Productivity expresses the quantity of
output (total biomass production) in relation to the total volume of
water consumed in the year (actual evapotranspiration). By relating
biomass production to total evapotranspiration (sum of soil evaporation,
canopy transpiration and interception), this indicator provides insights
on the impact of vegetation development on consumptive water use and
thus on water balance in a given domain. When the focus is on monitoring
performance of irrigated agriculture in relation to water consumption,
it is more appropriate to use transpiration alone as a denominator, as a
measure of water beneficially consumed by the plant. This latter
indicator, for which we use the term &quot;net water productivity&quot;,
provides useful information on how effectively vegetation (and
particularly crops) uses water to develop its biomass (and thus yield).
</td>
</tr>
</tbody>
</table>

In the meta object you will find more specific information such as the
spatio-temporal extent of the dataset as well as some information on the
methodology and data types.

```r
str(products[[1]]$meta)
```

    ## 'data.frame':    1 obs. of  12 variables:
    ##  $ format                : chr "Raster Dataset"
    ##  $ unit                  : chr "kg/m³ is the ratio of kg of dry matter per cubic meter of water transpired by vegetation in one hectare"
    ##  $ dataType              : chr "Int32 (32bit Integer)"
    ##  $ conversionFactor      : chr "the pixel value in the downloaded data must be multiplied by 0.001"
    ##  $ noDataValue           : int -9999
    ##  $ spatialResolution     : chr "250m (0.00223 degree)"
    ##  $ spatialExtent         : chr "Africa and Near East"
    ##  $ spatialReferenceSystem: chr "EPSG:4326 - WGS84 - Geographic Coordinate System (lat/long)"
    ##  $ temporalResolution    : chr "from January 2009 to present"
    ##  $ temporalExtent        : chr "Annual"
    ##  $ nearRealTime          : chr "New dekadal data layers are released approximately 5 days after the end of a dekad. A higher quality version of"| __truncated__
    ##  $ methodology           : chr "The calculation of gross biomass water productivity (GBWP) is as follows: GBWP = TBP/ETIa Where TBP is annual T"| __truncated__


With these information combined, we can start downloading some data.
Let’s say we are interested in the Gross Biomass Water Productivity
(GPWP) during the first season of the year 2015 in Uganda. Let’s prepare
a call to download this data.

```r
library(sf)
library(rnaturalearth)

ugn = ne_countries(country = "Uganda", returnclass = "sf")

collection = "WAPOR_2"
product = "L2_GBWP_S" # product code
begin = as.Date("2015-01-01") # begin date is inclusive
end = as.Date("2016-01-01") # end date is exclusive
dimensions = list(SEASON = "S1") # GBWP only has dimension SEASON - S1 and S2
key = "<your_personal_API_Key" # can be obtained in the profile section of the WAPOR website 
                               # at https://wapor.apps.fao.org

# run the query command - see ?wapor_queryRaster() for more information
wapor_queryRaster(collection = collection,
                 product = product,
                 dimensions = dimensions,
                 aoi = ugn,
                 begin = begin,
                 end = end, 
                 APIkey = APIkey, 
                 outdir = ".", 
                 cutline = FALSE, 
                 tiled = TRUE, 
                 compressed = TRUE, 
                 overviews = TRUE)
```