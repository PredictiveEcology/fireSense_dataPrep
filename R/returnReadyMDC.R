returnReadyMDC <- function(pathInputs,
                           rasterToMatch,
                           climateModel,
                           RCP,
                           ensemble,
                           fileResolution,
                           model,
                           tag,
                           y){
  message(red(paste0("Skipping MDC prep. Trying to shortcut file deletion problem for year ", y)))
  fileName <- file.path(pathInputs, paste0(paste(climateModel, RCP, ensemble,
                                                 fileResolution, model,
                                                 tag, y, sep = "_"),
                                           ".grd"))
  if (!file.exists(fileName))
    stop(paste0("To use this function, please place your climate layers (.gri and .grd) in the following folder: ",
                pathInputs, ". Alternatively, pass the parameter skipMDCprep = FALSE"))
  MDC <- raster::stack(fileName)
  # Check alignement with RTM
  tryCatch({
    r <- raster::stack(MDC, rasterToMatch)
  }, error = function(e){
    stop(paste0("Your MDC files do not align with rasterToMatch. Please provide matching files in ",
                pathInputs, " or run try running the module with the parameter skipMDCprep = FALSE"))
  })
  message(green(paste0(fileName, " exists. Returning the raster stack")))
  names(MDC) <- paste0("year", y)
  return(MDC)
}