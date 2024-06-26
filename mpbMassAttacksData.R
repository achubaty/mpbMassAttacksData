defineModule(sim, list(
  name = "mpbMassAttacksData",
  description = "Mountain Pine Beetle Red Attack Data (AB)",
  keywords = c("mountain pine beetle, outbreak dynamics, eruptive potential, spread, climate change, twitch response"),
  authors = c(
    person(c("Alex", "M"), "Chubaty", email = "achubaty@for-cast.ca", role = c("aut", "cre")),
    person(c("Barry", "J"), "Cooke", email = "barry.cooke@canada.ca", role = c("aut")),
    person(c("Eliot", "J B"), "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut"))
  ),
  childModules = character(),
  version = numeric_version("0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list(),
  documentation = list(),
  reqdPkgs = list("achubaty/amc@development", "jimhester/archive", "data.table",
                  "PredictiveEcology/LandR@development",
                  "PredictiveEcology/reproducible@gdb_archiveNA (>= 1.2.6.9019)",
                  "magrittr", "quickPlot", "PredictiveEcology/mpbutils (>= 0.1.2)",
                  "raster", "RColorBrewer", "sf", "sp", "spatialEco"),
  parameters = rbind(
    defineParameter("startYear", "numeric", start(sim), NA, NA,
                    "The start year for the `sim$massAttacksData` stack; this is needed as a parameter
                    so that Cache can detect the change"),
    defineParameter("endYear", "numeric", end(sim), NA, NA,
                    "The end year for the sim$massAttacksData stack; this is needed as a parameter
                    so that Cache can detect the change"),
    defineParameter("stemsPerHaAvg", "integer", 1125, NA, NA,
                    desc = "The average number of pine stems per ha in the study area. ",
                    "Taken from Whitehead & Russo (2005), Cooke & Carroll (2017)"),
    defineParameter(".maxMemory", "numeric", 1e+9, NA, NA,
                    "Used to set the 'maxmemory' raster option. See '?rasterOptions'."),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA,
                    "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", 1, NA, NA,
                    "This describes the interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the interval between save events"),
    defineParameter(".tempdir", "character", NULL, NA, NA,
                    "Temporary (scratch) directory to use for transient files (e.g., GIS intermediates)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should this entire module be run with caching activated?")
  ),
  inputObjects = bindrows(
    expectsInput("massAttacksStackFile", "RasterStack",
                 desc = "temporary pre-build raster stack of mpb attacks", ## TODO: incorporate creation of this into the module
                 #sourceURL = "https://drive.google.com/file/d/1b5W835MPttLsVknVEg1CR_IrC_Nyz6La"), ## BC+AB
                 sourceURL = "https://drive.google.com/file/d/18xd6Bu8tAecb_Lm3icLJfJ7XqL4m3wf2"), ## AB only
    expectsInput("rasterToMatch", "RasterLayer",
                 desc = paste("Template raster to which all maps will be cropped and reprojected.",
                              "If not supplied, will default to standAgeMap."),
                 sourceURL = NA),
    expectsInput("standAgeMap", "RasterLayer",
                 desc = "stand age map in study area, default is Canada national stand age map",
                 sourceURL = paste0("http://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/",
                                    "canada-forests-attributes_attributs-forests-canada/",
                                    "2001-attributes_attributs-2001/",
                                    "NFI_MODIS250m_2001_kNN_Structure_Stand_Age_v1.tif")),
    expectsInput("studyArea", "SpatialPolygons",
                 desc = "The study area.",
                 sourceURL = NA)
  ),
  outputObjects = bindrows(
    # createsOutput("currentAttacks", "RasterLayer",
    #               desc = "Current year MPB attack maps (number of red attacked trees)."),
    createsOutput("massAttacksStack", "RasterStack",
                  desc = "Historical MPB attack maps (number of red attacked trees).")
    # createsOutput("massAttacksDT", "data.table",
    #               desc = "same data (though presence only) as currentAttacks, but in data.table format. Colnames: ID for pixelID and ATKTREES for number of attacked trees")
  )
))

## event types
#   - type `init` is required for initiliazation

doEvent.mpbMassAttacksData <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(eventType,
         "init" = {
           ### check for more detailed object dependencies:
           ### (use `checkObject` or similar)

           # do stuff for this event
           sim <- Init(sim)

           # schedule future event(s)
           sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "mpbMassAttacksData", "plot", .last() - 1)
           sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "mpbMassAttacksData", "save", .last())
         },
         "plot" = {
           # Plot(sim$currentAttacks)
           # Plot(sim$studyArea, addTo = "sim$currentAttacks", gp = gpar(col = "black", fill = 0),
           #      title = "")
           # Plot(sim$studyAreaLarge, addTo = "sim$currentAttacks", gp = gpar(col = "black", fill = 0),
           #      title = "")

           sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "mpbMassAttacksData", "plot", .last() - 1)
         },
         warning(paste("Undefined event type: '", events(sim)[1, "eventType", with = FALSE],
                       "' in module '", events(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  cPath <- cachePath(sim)
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  if (getOption("LandR.verbose", TRUE) > 0)
    message(currentModule(sim), ": using dataPath '", dPath, "'.")

  #mod$prj <- paste("+proj=aea +lat_1=47.5 +lat_2=54.5 +lat_0=0 +lon_0=-113",
  #                 "+x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
  mod$prj <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                   "+x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

  ## load study area
  if (!suppliedElsewhere("studyArea")) {
    sim$studyArea <- mpbStudyArea(ecoregions = c(112, 120, 122, 124, 126), mod$prj,
                                  cPath, dPath)
  }

  ## raster to match
  if (!suppliedElsewhere("rasterToMatch", sim)) {
    sim$rasterToMatch <- Cache(
      LandR::prepInputsLCC,
      year = 2005, ## TODO: use 2010
      destinationPath = dPath,
      studyArea = sf::as_Spatial(sim$studyArea)
    )
  }
  ## stand age map
  if (!suppliedElsewhere("standAgeMap", sim)) {
    sim$standAgeMap <- LandR::prepInputsStandAgeMap(
      startTime = 2010,
      ageUrl = na.omit(extractURL("standAgeMap")),
      destinationPath = dPath,
      studyArea = sim$studyArea,
      rasterToMatch = sim$rasterToMatch,
      userTags = c("stable", currentModule(sim)) ## TODO: does this need rasterToMatch? it IS rtm!
    )
    sim$standAgeMap[] <- asInteger(sim$standAgeMap[])
  }

  return(invisible(sim))
}

## event functions

Init <- function(sim) {
  ## TODO: incorporate code from MPB_maps.R to create the raster layers

  datasets <- c(
    pre2019 = "https://drive.google.com/file/d/11YoAxkHzWNsNGkNtoe5ph5BdT5xXUoGg/view?usp=sharing",  # 1970s to 2018
    Y2019 = "https://drive.google.com/file/d/1vhSLJf03KTi0Oeec_pSiwK7EQZYd_PlF/view?usp=sharing", # 2019
    Y2020 = "https://drive.google.com/file/d/1S5Lw5g5ACcTyhf8kwR7sqwPzGOCjfpwB/view?usp=sharing" # 2020
  )
  sim$massAttacksStack <- Cache(prepInputsMPB_ABdata, url = datasets,
                                startYear = Par$startYear,
                                endYear = Par$endYear,
                                rasterToMatch = sim$rasterToMatch,
                                maskWithRTM = TRUE,
                                destinationPath = inputPath(sim),
                                stemsPerHaAvg = P(sim)$stemsPerHaAvg,
                                disaggregateFactor = 10)

  annualAbundances <- lapply(sim$massAttacksStack, function(x) round(sum(x[], na.rm = TRUE), 0))
  sim$massAttacksStack <- terra::rast(sim$massAttacksStack)

  # setColors(sim$massAttacksStack) <- rep(list(brewer.pal(9, "YlOrRd")), nlayers(sim$massAttacksStack))

  return(invisible(sim))
}

loadRasterStackTruncateYears <- function(fname, startTime) {
  from <- gsub("^.*_([[:digit:]]{4,4})-([[:digit:]]{4,4}).*", "\\1-\\2", fname)
  years <- as.numeric(strsplit(from, split = "-")[[1]])
  years <- seq(years[1], years[2])
  layerNames <- paste0("X", years)
  allMaps <- raster::stack(fname) %>% setNames(layerNames)
  toDrop <- seq(which(grepl(startTime - 1, layerNames)))
  message("Removing years ", paste(years[toDrop], collapse = ", "),
          " as these occur prior to start(sim)")
  allMaps <- dropLayer(allMaps, toDrop) ## drop layers that won't be used
  allMaps
}

prepInputsMPB_ABdata <- function(urls, rasterToMatch, startYear, endYear,
                                 disaggregateFactor = 10, destinationPath, stemsPerHaAvg,
                                 maskWithRTM = TRUE, ...) {

  outOuter <- Map(url = urls, nam = names(urls), function(url, nam)  {
    # fileInfo <- prepInputs(url = url, fun = NULL, ..., archive = NA)

    dirForExtract <- file.path(destinationPath, "MPB_data_AB", nam)
    a <- prepInputs(url = url, fun = "sf::st_read",
                    destinationPath = dirForExtract,
                    targetFile = "MPB_AERIAL_SURVEY.gdb"
    ) |> Cache()

    if (FALSE) { # this is "non-prepInputs" original way
      fileInfo <- preProcess(url = url, fun = "sf::st_read", destinationPath = destinationPath, ..., archive = NA)
      message("extracting to ", dirForExtract)

      out <- try(archive::archive_extract(fileInfo$targetFilePath, dir = dirForExtract), silent = TRUE)
      if (is(out, "try-error")) {
        SevenZip <- Sys.which("7z")
        if (nchar(SevenZip) == 0)
          stop("archive::archive_extract was unable to deal with this archive; 7z is also not installed; please install for your system")
        system(paste0(Sys.which("7z"), " x ", fileInfo$targetFilePath, " -aos -o", dirForExtract))
      }
    }
    gdbName <- dir(dirForExtract, pattern = ".gdb$", full.names = TRUE)

    origDir <- setwd(dirForExtract)
    on.exit(setwd(origDir))
    layerNames <- sf::st_layers(gdbName)
    yrsAvail <- substr(layerNames$name, start = nchar("ab_0ufohn") + 1, stop = 13)
    yrsAvail <- gsub(".$", "", yrsAvail)
    layerNames$year <- yrsAvail

    # Years are 2 digit -- so numbers above 70 are in 20th C; below 30 are 21st C.
    yrs <- ifelse(as.numeric(yrsAvail) > 50, paste0(19, yrsAvail), paste0(20, yrsAvail))
    yrs <- as.integer(yrs)
    layerNames$yearNum <- yrs
    out <- list()
    if (startYear <= max(yrs) && endYear >= min(yrs)) {
      yearsToDo <- max(startYear, min(yrs)):min(max(yrs), endYear)
      lays <- as.data.table(sapply(layerNames, function(x) x))
      rtmTemplate <- terra::rast(rasterToMatch)
      rtmNAs <- which(is.na(rasterToMatch[]))
      rtmCRS <- st_crs(rasterToMatch)
      dig <- CacheDigest(list(rtmTemplate, disaggregateFactor)) |> suppressWarnings()
      allFilesInGDB <- asPath(dir(gdbName, recursive = TRUE, full.names = TRUE))
      digGDB <- CacheDigest(allFilesInGDB)
      if (disaggregateFactor > 1)
        rasterToMatch <- terra::rast(terra::disagg(rtmTemplate, fact = disaggregateFactor))

      for (ytd in yearsToDo) {
        yrsIn <- lays[yearNum == ytd]$name
        names(yrsIn) <- yrsIn
        if (NROW(yrsIn)) {

          pointsAndPolys <- lapply(yrsIn, rasterToMatch = rasterToMatch,
                                   function(y, rasterToMatch) {
                                     message(crayon::green(y))
                                     co <- capture.output({
                                       mpbMap <- sf::st_read(gdbName, layer = y)
                                     })
                                     mpbMap <- projectTo(mpbMap, rtmCRS)
                                     mpbMap <- fixErrorsIn(mpbMap)
                                     if (NROW(mpbMap)) {
                                       if (all(sf::st_is(mpbMap, "POINT"))) {
                                         mpbRaster <- rtmTemplate
                                         # mpbMapSp <- sf::as_Spatial(mpbMap)
                                         pixels <- cellFromXY(rtmTemplate, st_coordinates(mpbMap))
                                         whNA <- is.na(pixels)
                                         colnam <- grep("num_trees", names(mpbMap), ignore.case = TRUE, value = TRUE)
                                         # mpbMapSpOnRTM <- mpbMapSp[!whNA,]
                                         mpbMapOnRTM <- mpbMap[!whNA,]
                                         pixelsOnRTM <- cellFromXY(rtmTemplate, st_coordinates(mpbMapOnRTM))
                                         # pixels <- which(pixels)[!whNA]
                                         dt1 <- data.table(num_trees = mpbMapOnRTM[[colnam]], pixel = pixelsOnRTM)
                                         dt1 <- dt1[, list(num_trees = sum(num_trees)), by = "pixel" ]
                                         message(crayon::green(paste0("  There are total ", sum(dt1$num_trees),
                                                                      " attacked trees on map due to pixels")))
                                         mpbRaster[dt1$pixel] <- dt1$num_trees
                                       } else {
                                         mpbMap <- st_cast(mpbMap, "MULTIPOLYGON")
                                         mpbMap <- cropTo(mpbMap, rasterToMatch)
                                         mpbMap$area_new <- sf::st_area(mpbMap)
                                         if (is.null(mpbMap$POLY_PERC)) {
                                           if (!all(unique(mpbMap$SEVERITY %in% 1:3))) browser()
                                           mpbMap$POLY_PERC <- ifelse(mpbMap$SEVERITY == 1, 0.05, ifelse(mpbMap$SEVERITY == 2, 0.25, ifelse(mpbMap$SEVERITY ==  3, 0.5, 0)))
                                         }
                                         totAbund <- round(sum(abundance(mpbMap$area_new/1e4, mpbMap$POLY_PERC, avgDensity = stemsPerHaAvg)), 0)
                                         message(crayon::green(paste0("  There are total ", totAbund,
                                                                      " attacked trees on map due to polygons")))
                                         # mpbRaster2 <- fasterize::fasterize(mpbMap, rtmTemplate, field = "POLY_PERC")

                                         # Leave this as a `raster` as it is a temporary object anyway... fasterize is still faster than terra::rasterize
                                         mpbRaster <- fasterize::fasterize(mpbMap, raster::raster(rasterToMatch), field = "POLY_PERC")
                                         whNoNA <- which(!is.na(mpbRaster[]))
                                         mpbRaster[whNoNA] <- abundance(areaPerUnit = (prod(res(mpbRaster)))/1e4, percentPerUnit = mpbRaster[whNoNA],
                                                                        avgDensity = stemsPerHaAvg)
                                         rasterizationDiff <- abs(sum(mpbRaster[][whNoNA], na.rm = T) - as.numeric(totAbund)) / as.numeric(totAbund)
                                         if (!anyNA(rasterizationDiff)) {
                                           mess <- paste0("  Rasterization % deviation of total number of trees attacked: ", round(rasterizationDiff*100, 3))
                                           if (rasterizationDiff > 0.001) {
                                             message(crayon::red(mess))
                                             warning(mess)
                                           } else {
                                             message(crayon::green(mess))
                                           }
                                         }

                                         if (disaggregateFactor > 1) {
                                           # data.table way
                                           mpbRasterDT <- terra::rast(aggregateRasByDT(mpbRaster, rtmTemplate, fn = sum))

                                           # Raster package way is WAY TOO SLOW
                                           # mpbRasterSmall <- raster::aggregate(mpbRaster, fact = disaggregateFactor, fun = sum)
                                           mpbRaster <- mpbRasterDT
                                         }
                                       }
                                     } else {
                                       mpbRaster <- NULL
                                     }
                                     mpbRaster
                                   }) |>
            Cache(omitArgs = "rasterToMatch", .cacheExtra = c(dig, digGDB),
                  userTags = "pointsAndPolys")

          pointsAndPolys <- pointsAndPolys[!sapply(pointsAndPolys, is.null)]
          if (length(pointsAndPolys) > 1) {
            pointsAndPolys <- terra::rast(pointsAndPolys)
            pointsAndPolys <- sum(pointsAndPolys, na.rm = TRUE)#, sum, na.rm = TRUE)
          } else {
            pointsAndPolys <- pointsAndPolys[[1]]
          }
          pointsAndPolys[pointsAndPolys[] == 0] <- NA
          if (isTRUE(maskWithRTM))
            pointsAndPolys[rtmNAs] <- NA
          out[[paste0("X",ytd)]] <- pointsAndPolys
        }
      }
    }
    out
  })
  Reduce(modifyList, outOuter)
}

abundance <- function(areaPerUnit, percentPerUnit, avgDensity) {
  areaPerUnit * avgDensity * percentPerUnit / 100
}

aggregateRasByDT <- function(ras, newRas, fn = sum) {
  whNonNA <- which(!is.na(ras[]))
  rc2 <- rowColFromCell(ras, whNonNA)
  if (!all(((res(newRas)/res(ras)) %% 1) == 0))
    stop("The resolutions of the original raster and new raster are not integer multiples")
  disaggregateFactor <- unique(res(newRas)/res(ras))
  dt <- data.table(vals = ras[][whNonNA], ceiling(rc2 / disaggregateFactor))
  dt2 <- dt[, list(vals = fn(vals)), by = c("row", "col")]
  pixes <- cellFromRowCol(newRas, row = dt2$row, col = dt2$col)
  newRasOut <- raster(newRas)
  newRasOut[pixes] <- dt2$vals
  newRasOut
}
