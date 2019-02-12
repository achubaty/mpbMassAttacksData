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
  reqdPkgs = list("amc", "data.table", "quickPlot", "magrittr", "raster", "RColorBrewer", "reproducible"),
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
    defineParameter(".tempdir", "character", tempdir(), NA, NA,
                    "Temporary (scratch) directory to use for transient files (e.g., GIS intermediates)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should this entire module be run with caching activated?")
  ),
  inputObjects = bind_rows(
    expectsInput("massAttacksMapFile", "RasterLayer",
                 desc = "temporary pre-build raster stack of mpb attacks", ## TODO: incororate creation of this into the module
                 #sourceURL = "https://drive.google.com/file/d/1b5W835MPttLsVknVEg1CR_IrC_Nyz6La/view?usp=sharing"), ## BC+AB
                 sourceurl = "https://drive.google.com/file/d/1i4wRPjGDpaBOL6gs7FB9bQ9qqCTUyybw/view?usp=sharing"), ## AB only
    expectsInput("pineDT", "data.table",
                 desc = "Proportion cover etc. by species (lodgepole and jack pine).",
                 sourceURL = NA),
    expectsInput("rasterToMatch", "RasterLayer",
                 desc = "if not supplied, will default to standAgeMap", # TODO: description needed
                 sourceURL = NA),
    expectsInput("standAgeMap", "RasterLayer",
                 desc = "stand age map in study area, default is Canada national stand age map",
                 sourceURL = "http://tree.pfc.forestry.ca/kNN-StructureStandVolume.tar"),
    expectsInput("studyArea", "SpatialPolygons",
                 desc = "The study area to which all maps will be cropped and reprojected.",
                 sourceURL = NA)
  ),
  outputObjects = bind_rows(
    createsOutput("massAttacksMap", "RasterStack",
                  desc = "Historical MPB attack maps (number of red attacked trees).",
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
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  if (getOption("LandR.verbose", TRUE) > 0)
    message(currentModule(sim), ": using dataPath '", dPath, "'.")

  ## load study area
  if (!suppliedElsewhere("studyArea")) {
    prj <- paste("+proj=aea +lat_1=47.5 +lat_2=54.5 +lat_0=0 +lon_0=-113",
                 "+x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
    sim$studyArea <- amc::loadStudyArea(dataPath(sim), "studyArea.kml", prj)
  }

  ## stand age map
  if (!suppliedElsewhere("standAgeMap", sim)) {
    standAgeMapFilename <- file.path(dPath, "NFI_MODIS250m_kNN_Structure_Stand_Age_v0.tif")
    sim$standAgeMap <- Cache(prepInputs,
                             targetFile = basename(standAgeMapFilename),
                             archive = asPath(c("kNN-StructureStandVolume.tar",
                                                "NFI_MODIS250m_kNN_Structure_Stand_Age_v0.zip")),
                             destinationPath = dPath,
                             url = na.omit(extractURL("standAgeMap")),
                             fun = "raster::raster",
                             studyArea = sim$studyArea,
                             #rasterToMatch = sim$rasterToMatch,
                             method = "bilinear",
                             datatype = "INT2U",
                             filename2 = paste0(tools::file_path_sans_ext(basename(standAgeMapFilename)), "_cropped"),
                             overwrite = TRUE,
                             userTags = c("stable", currentModule(sim)))
    sim$standAgeMap[] <- asInteger(sim$standAgeMap[])
  }

  ## raster to match
  if (!suppliedElsewhere("rasterToMatch", sim)) {
    sim$rasterToMatch <- sim$standAgeMap
  }

  return(invisible(sim))
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initilization
Init <- function(sim) {
  ## MPB data for 2008 onward (NOTE: missing 1999 and 2000)
  ## TODO: incorporate code from MPB_maps.R to create the raster layers

  # load each of the annual rasters and stack them
  layerNames <- paste0("X", c(1998, 2001:2016))
  fname <- file.path(dataPath(sim), "mpb_bcab_boreal_1998-2016.tif")

  ## TODO: prepInputs can't handle a stack...at all...it returns a brick of all NA values
  # sim$massAttacksMap <- Cache(prepInputs,
  #                             targetFile = basename(fname),
  #                             archive = NULL,
  #                             destinationPath = asPath(dataPath(sim)),
  #                             url = extractURL("massAttacksMapFile"),
  #                             fun = "raster::stack",
  #                             studyArea = sim$studyArea,
  #                             rasterToMatch = sim$rasterToMatch,
  #                             method = "bilinear",
  #                             datatype = "FLT4S",
  #                             filename2 = NULL,
  #                             overwrite = TRUE,
  #                             userTags = c("stable", currentModule(sim))) %>%
  #   stack() %>%
  #   set_names(layerNames)

  ## workaround broken prepInputs by downloading only; use Alex's code to load etc.
  fileInfo <- Cache(preProcess,
                    targetFile = basename(fname),
                    archive = NULL,
                    destinationPath = asPath(dataPath(sim)),
                    url = extractURL("massAttacksMapFile"),
                    fun = "raster::stack",
                    method = "bilinear",
                    datatype = "FLT4S",
                    filename2 = NULL,
                    overwrite = TRUE,
                    userTags = c("stable", currentModule(sim)))
browser()
  allMaps <- stack(fname) %>% set_names(layerNames)
  sim$massAttacksMap <- Cache(amc::cropReproj, allMaps, sim$studyArea, layerNames)
  setColors(sim$massAttacksMap) <- rep(list(brewer.pal(9, "YlOrRd")), nlayers(sim$massAttacksMap))

  # TODO: use fasterize (requires use of sf)
  rstStudyArea <- Cache(rasterize, sim$studyArea, sim$massAttacksMap[[15]])

  ## data.table of MPB attacks in study area (NUMTREES is number of attacked trees)
  sim$massAttacksDT <- data.table(ID = 1L:ncell(sim$massAttacksMap),
                                  NUMTREES = sim$massAttacksMap[[paste0("X", start(sim))]][])
  setkey(sim$massAttacksDT, "ID")
  sim$massAttacksDT <- sim$massAttacksDT[NUMTREES > 0]
browser()
  # join with pine data.table
  sim$massAttacksDT <- sim$massAttacksDT[sim$pineDT, nomatch = 0]

  return(invisible(sim))
}
