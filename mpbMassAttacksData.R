defineModule(sim, list(
  name = "mpbMassAttacksData",
  description = "Mountain Pine Beetle Red Top Growth Model: Short-run Potential for Establishment, Eruption, and Spread",
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
  reqdPkgs = list("achubaty/amc@development", "data.table",
                  "PredictiveEcology/LandR@development",
                  "magrittr", "quickPlot",
                  "raster", "RColorBrewer", "reproducible", "sf", "sp", "spatialEco"),
  parameters = rbind(
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
    expectsInput("massAttacksMapFile", "RasterLayer",
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
    createsOutput("currentAttacks", "RasterLayer",
                  desc = "Current year MPB attack maps (number of red attacked trees).",
                  sourceURL = NA),
    createsOutput("massAttacksMap", "RasterStack",
                  desc = "Historical MPB attack maps (number of red attacked trees).",
                  sourceURL = NA),
    createsOutput("massAttacksDT", "data.table",
                  desc = "same data (though presence only) as currentAttacks, but in data.table format. Colnames: ID for pixelID and ATKTREES for number of attacked trees",
                  sourceURL = NA)
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
      Plot(sim$currentAttacks)
      Plot(sim$studyArea, addTo = "sim$currentAttacks", gp = gpar(col = "black", fill = 0),
           title = "")
      Plot(sim$studyAreaLarge, addTo = "sim$currentAttacks", gp = gpar(col = "black", fill = 0),
           title = "")

      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "mpbMassAttacksData", "plot", .last() - 1)
    },
    warning(paste("Undefined event type: '", events(sim)[1, "eventType", with = FALSE],
                  "' in module '", events(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  if (getOption("LandR.verbose", TRUE) > 0)
    message(currentModule(sim), ": using dataPath '", dPath, "'.")

  #mod$prj <- paste("+proj=aea +lat_1=47.5 +lat_2=54.5 +lat_0=0 +lon_0=-113",
  #                 "+x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
  mod$prj <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                   "+x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

  ## load study area
  if (!suppliedElsewhere("studyArea")) {
    sim$studyArea <- amc::loadStudyArea(dataPath(sim), "studyArea.kml", mod$prj)
  }

  ## raster to match
  if (!suppliedElsewhere("rasterToMatch", sim)) {
    sim$rasterToMatch <- Cache(
      LandR::prepInputsLCC,
      year = 2005,
      destinationPath = dPath,
      studyArea = sim$studyArea
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

  sim$massAttacksMap <- Cache(prepInputs,
                              url = extractURL("massAttacksMapFile"),
                              destinationPath = asPath(dataPath(sim)),
                              # Custom function for loading to R, with 1 custom arg + function itself
                              fun = quote(
                                loadRasterStackTruncateYears(targetFilePath, startTime = startTime)
                              ),
                              loadRasterStackTruncateYears = loadRasterStackTruncateYears,
                              startTime = start(sim),
                              # For postProcessing
                              rasterToMatch = sim$rasterToMatch,
                              filename2 = NULL,
                              method = "bilinear",
                              datatype = "FLT4S",
                              overwrite = TRUE,
                              userTags = c("stable", currentModule(sim)))
  setColors(sim$massAttacksMap) <- rep(list(brewer.pal(9, "YlOrRd")), nlayers(massAttacksMap))

  sim$currentAttacks <- sim$massAttacksMap[[paste0("X", start(sim))]]
  setColors(sim$currentAttacks) <- list(brewer.pal(9, "YlOrRd"))

  ## data.table of MPB attacks in study area (NUMTREES is number of attacked trees)
  sim$massAttacksDT <- data.table(ID = 1L:ncell(sim$currentAttacks),
                                  ATKTREES = sim$currentAttacks[]) %>%
    setkey(., "ID")
  sim$massAttacksDT <- sim$massAttacksDT[ATKTREES > 0]

  return(invisible(sim))
}

loadRasterStackTruncateYears <- function(fname, startTime) {
  from <- gsub("^.*_([[:digit:]]{4,4})-([[:digit:]]{4,4}).*", "\\1-\\2", fname)
  years <- as.numeric(strsplit(from, split = "-")[[1]])
  years <- seq(years[1], years[2])
  layerNames <- paste0("X", years)
  allMaps <- raster::stack(fname) %>% setNames(layerNames)
  toDrop <- seq(which(grepl(startTime-1, layerNames)))
  message("Removing years ", paste(years[toDrop], collapse = ", "),
          " as these occur prior to start(sim)")
  allMaps <- dropLayer(allMaps, toDrop) ## drop layers that won't be used
  allMaps
}
