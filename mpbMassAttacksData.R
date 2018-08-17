
# Everything in this file gets sourced during simInit, and all functions and objects
#  are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "mpbMassAttacksData",
  description = "Mountain Pine Beetle Red Top Growth Model: Short-run Potential for Establishment, Eruption, and Spread",
  keywords = c("mountain pine beetle, outbreak dynamics, eruptive potential, spread, climate change, twitch response"),
  authors = c(
    person(c("Alex", "M"), "Chubaty", email = "alexander.chubaty@canada.ca", role = c("aut", "cre")),
    person(c("Barry", "J"), "Cooke", email = "barry.cooke@canada.ca", role = c("aut")),
    person(c("Eliot", "J B"), "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut"))
  ),
  childModules = character(),
  version = numeric_version("0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list(),
  reqdPkgs = list("amc", "data.table", "quickPlot", "raster", "RColorBrewer", "reproducible"),
  parameters = rbind(
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", 1, NA, NA, "This describes the interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the interval between save events"),
    defineParameter(".useCache", "numeric", FALSE, NA, NA, "Should this entire module be run with caching activated?")
  ),
  inputObjects = bind_rows(
    expectsInput("studyArea", "SpatialPolygons", "The study area to which all maps will be cropped and reprojected.", sourceURL = NA)
  ),
  outputObjects = bind_rows(
    createsOutput("massAttacksMap", "RasterStack", "Historical MPB attack maps (number of red attacked trees).")
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
      sim <- sim$mpbMassAttacksDataInit(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime,
                           "mpbMassAttacksData", "plot", .last() - 1)
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime,
                           "mpbMassAttacksData", "save", .last())
    },
    "plot" = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      Plot(sim$massAttacksMap)

      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval,
                           "mpbMassAttacksData", "plot", .last() - 1)

      # ! ----- STOP EDITING ----- ! #
    },
    warning(paste("Undefined event type: '", events(sim)[1, "eventType", with = FALSE],
                  "' in module '", events(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  if (!('studyArea' %in% sim$.userSuppliedObjNames)) {
    f <- file.path(modulePath(sim), "mpbMassAttacksData", "data", "studyArea.kml")
    prj <- paste("+proj=aea +lat_1=47.5 +lat_2=54.5 +lat_0=0 +lon_0=-113",
                 "+x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
    sim$studyArea <- readOGR(f, "studyArea.kml") %>%
      sp::spTransform(., prj)
  }

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initilization
mpbMassAttacksDataInit <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #

  ## MPB data for 2008 onward (NOTE: missing 1999 and 2000)
  ## TODO: incorporate code from MPB_maps.R to create the raster layers

  # load each of the annual rasters and stack them
  layerNames <- paste0("X", c(1998, 2001:2016))
  files <- file.path(modulePath(sim), "mpbMassAttacksData", "data",
                     "mpb_bcab_boreal_1998-2016.tif")
  allMaps <- stack(files) %>% set_names(layerNames)

  ## crop and reproject for the study area
  sim$massAttacksMap <- Cache(amc::cropReproj, allMaps, sim$studyArea, layerNames)
  setColors(sim$massAttacksMap) <- rep(list(brewer.pal(9, "YlOrRd")), nlayers(sim$massAttacksMap))

  # TODO: use fasterize (requires use of sf)
  rstStudyArea <- Cache(rasterize, sim$studyArea, sim$massAttacksMap)

  ## data.table of MPB attacks in study area (NUMTREES is number of attacked trees)
  sim$massAttacksDT <- data.table(ID = 1L:ncell(sim$massAttacksMap),
                                  NUMTREES = sim$massAttacksMap[[paste0("X", start(sim))]][])
  setkey(sim$massAttacksDT, "ID")
  sim$massAttacksDT <- sim$massAttacksDT[NUMTREES > 0]

  # join with pine data.table
  sim$massAttacksDT <- sim$massAttacksDT[sim$pineDT, nomatch = 0]

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
