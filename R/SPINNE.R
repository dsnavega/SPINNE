#' SPINNE
#' 
#' Run SPINNE GUI
#' 
#' @export
#' 
SPINNE <- function() {
  
  spinne_dir <- system.file("SPINNE", package = "SPINNE", mustWork = T)
  spinne_app <- shiny::shinyAppDir(appDir = spinne_dir)
  shiny::runApp(
    appDir = spinne_app,
    quiet = TRUE
  )
  
}

