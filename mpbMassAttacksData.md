---
title: "mpbRedTopMassAttacks"
author:
  - "Alex M. Chubaty"
  - "Barry J. Cooke"
  - "Eliot J. B. McIntire"
date: "June 25, 2021"
output:
  html_document:
    keep_md: yes
    toc: true
    toc_depth: 3
    toc_float: true
bibliography:
  - bibliography.bib
editor_options:
  chunk_output_type: console
---



# Overview

This module imports the raw MPB red-top attack data from AB over several years (2001-2020), following the methodology of @Cooke:2017fem.
MPB attack data for AB (derived from spatial points and polygons) were reprojected to use a Lambert conformal conic projection and rasterized using a common resolution of 1 km and merged into a single raster layer for each year.
Number of attacked trees per pixel is estimated using an average stem density of 1125 stems/ha [per @Whitehead:2004rz].
Where possible, all data downloads and preprocessing were scripted for reproducibility from raw, original sources.

Because of the long processing time and high memory requirements of the GIS operations performed during data preprocessing, with use a combination of caching approaches – implemented in the `reproducible` R package [@McIntire:2021] – to speed up subsequent loading and use of these data.
All preprocessed data are made available in a private Google Drive folder, and subsequently downloaded from there when used by a `SpaDES` module.

# Usage


```r
library(SpaDES.core)

setPaths(modulePath = file.path(".."))
getPaths() # shows where the 4 relevant paths are

times <- list(start = 0, end = 10)

parameters <- list(
  #.progress = list(type = "text", interval = 1), # for a progress bar
  ## If there are further modules, each can have its own set of parameters:
  #module1 = list(param1 = value1, param2 = value2),
  #module2 = list(param1 = value1, param2 = value2)
)
modules <- list("mpbMassAttacksData")
objects <- list()
inputs <- list()
outputs <- list()

mySim <- simInit(times = times, params = parameters, modules = modules,
                 objects = objects)

mySimOut <- spades(mySim)
```

# Parameters

Provide a summary of user-visible parameters.


```
## defineParameter: 'startYear' is not of specified type 'numeric'.
```

```
## defineParameter: 'endYear' is not of specified type 'numeric'.
```



|paramName        |paramClass |default    |min |max |paramDesc                                                                                                             |
|:----------------|:----------|:----------|:---|:---|:---------------------------------------------------------------------------------------------------------------------|
|startYear        |numeric    |start(sim) |NA  |NA  |The start year for the `sim$massAttacksData` stack; this is needed as a parameter so that Cache can detect the change |
|endYear          |numeric    |end(sim)   |NA  |NA  |The end year for the sim$massAttacksData stack; this is needed as a parameter so that Cache can detect the change     |
|.maxMemory       |numeric    |1e+09      |NA  |NA  |Used to set the 'maxmemory' raster option. See '?rasterOptions'.                                                      |
|.plotInitialTime |numeric    |NA         |NA  |NA  |This describes the simulation time at which the first plot event should occur                                         |
|.plotInterval    |numeric    |1          |NA  |NA  |This describes the interval between plot events                                                                       |
|.saveInitialTime |numeric    |NA         |NA  |NA  |This describes the simulation time at which the first save event should occur                                         |
|.saveInterval    |numeric    |NA         |NA  |NA  |This describes the interval between save events                                                                       |
|.tempdir         |character  |           |NA  |NA  |Temporary (scratch) directory to use for transient files (e.g., GIS intermediates).                                   |
|.useCache        |logical    |FALSE      |NA  |NA  |Should this entire module be run with caching activated?                                                              |

# Events

Describe what happens for each event type.

## Plotting

Write what is plotted.

## Saving

Write what is saved.

# Data dependencies

## Input data

How to obtain input data, and a description of the data required by the module.
If `sourceURL` is specified, `downloadData("mpbMassAttacksData", "..")` may be sufficient.


```
## defineParameter: 'startYear' is not of specified type 'numeric'.
```

```
## defineParameter: 'endYear' is not of specified type 'numeric'.
```



|objectName           |objectClass     |desc                                                                                                             |sourceURL                                                                                                                                                                                   |
|:--------------------|:---------------|:----------------------------------------------------------------------------------------------------------------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|massAttacksStackFile |RasterStack     |temporary pre-build raster stack of mpb attacks                                                                  |https://drive.google.com/file/d/18xd6Bu8tAecb_Lm3icLJfJ7XqL4m3wf2                                                                                                                           |
|rasterToMatch        |RasterLayer     |Template raster to which all maps will be cropped and reprojected. If not supplied, will default to standAgeMap. |NA                                                                                                                                                                                          |
|standAgeMap          |RasterLayer     |stand age map in study area, default is Canada national stand age map                                            |http://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/canada-forests-attributes_attributs-forests-canada/2001-attributes_attributs-2001/NFI_MODIS250m_2001_kNN_Structure_Stand_Age_v1.tif |
|studyArea            |SpatialPolygons |The study area.                                                                                                  |NA                                                                                                                                                                                          |

## Output data

Description of the module outputs.


```
## defineParameter: 'startYear' is not of specified type 'numeric'.
```

```
## defineParameter: 'endYear' is not of specified type 'numeric'.
```



|objectName       |objectClass |desc                                                       |
|:----------------|:-----------|:----------------------------------------------------------|
|massAttacksStack |RasterStack |Historical MPB attack maps (number of red attacked trees). |

# Links to other modules

Mountain Pine Beetle Red Top Growth Model: Short-run Potential for Establishment, Eruption, and Spread.

- `mpbPine`
- `mpbRedTopSpread`

# References
