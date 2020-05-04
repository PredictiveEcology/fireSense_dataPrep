## Everything in this file gets sourced during `simInit()`,
## and all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used without `sim$` as they are namespaced to the module,
## just like functions in R packages.
## If exact location is required, functions will be: `sim$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "fireSense_dataPrep",
  description = paste0("Data preparation module for fireSense. ",
                       "Currently only the dataPrep for the spread prediction is working. This module needs", 
                       " to happen before any fireSense modules"),
  keywords = c("fireSense", "spread", "data munging"),
  authors = structure(list(list(given = "Tati", family = "Micheletti", role = c("aut", "cre"), 
                                email = "tati.micheletti@gmail.com", comment = NULL)), 
                      class = "person"),
  childModules = character(0),
  version = list(SpaDES.core = "1.0.0.9004", fireSense_dataPrep = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "fireSense_dataPrep.Rmd")),
  reqdPkgs = list(),
  parameters = rbind(
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    paste("Should this entire module be run with caching activated?",
                          "This is generally intended for data-type modules, where stochasticity",
                          "and time are not relevant"))
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "cohortData", objectClass = "data.table", 
                 desc = paste0("Table that defines the cohorts by pixelGroup"), 
                 sourceURL = NA), 
    expectsInput(objectName = "pixelGroupMap", objectClass = "RasterLayer", 
                 desc = paste0("RasterLayer that defines the pixelGroups for cohortData table"), 
                 sourceURL = NA), 
    expectsInput(objectName = "MDC06", objectClass = "RasterLayer", 
                 desc = paste0("Contains MDC06 for the current year.",
                               "Currently, for the NWT this is being created by the fireSense_NWT_DataPrep module",
                               "Should, however, potentially be created here at some point (checking if other ",
                               "modules -- to be created -- such as fireSense_ignitionDataPrep or ",
                               "fireSense_escapeDataPrep prepared it already)"),
                 sourceURL = NA)
    
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "dataFireSense_SpreadPredict", 
                  objectClass = "RasterStack", 
                  desc = paste0("One or more RasterLayers or RasterStacks in which to look for ",
                                "variables present in the model formula."))
  )
))

## event types
#   - type `init` is required for initialization

doEvent.fireSense_dataPrep = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)
      
      # do stuff for this event
      sim <- Init(sim)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "fireSense_dataPrep", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "fireSense_dataPrep", "save")
    },
    plot = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      
      #plotFun(sim) # uncomment this, replace with object to plot
      # schedule future event(s)
      
      # e.g.,
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "fireSense_dataPrep", "plot")
      
      # ! ----- STOP EDITING ----- ! #
    },
    save = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      
      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function
      
      # schedule future event(s)
      
      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "fireSense_dataPrep", "save")
      
      # ! ----- STOP EDITING ----- ! #
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #
  
  # ! ----- STOP EDITING ----- ! #
  
  return(invisible(sim))
}

### template for save events
Save <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
plotFun <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  #Plot(sim$object)
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
Event1 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event1Test1 <- " this is test for event 1. " # for dummy unit test
  # sim$event1Test2 <- 999 # for dummy unit test
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event2
Event2 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  # sim$event2Test1 <- " this is test for event 2. " # for dummy unit test
  # sim$event2Test2 <- 777  # for dummy unit test
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create a named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.
  # Note: the module developer can check if an object is 'suppliedElsewhere' to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call, or another module will supply or has supplied it. e.g.,
  # if (!suppliedElsewhere('defaultColor', sim)) {
  #   sim$map <- Cache(prepInputs, extractURL('map')) # download, extract, load file from url in sourceURL
  # }
  
  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  
  # ! ----- EDIT BELOW ----- ! #
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### add additional events as needed by copy/pasting from above
