#' @title metRscreen
#' @description The metRscreen shiny app allows you to screen papers via their abstracts and titles and allows for highlighting of keywords in multiple colours.
#' @return A dataframe of decisioned papers
#' @param reject.list list of rejection reasons to be added to metRscreen, can be left empty
#' @param metRDS a save state file path to resume screening from a previous metRscreen screen
#' @export

metRscreen <- function(reject.list = NULL, metRDS = NULL) {
  # if data.str if missing, assign an empty data.frame
  if(missing(reject.list)) reject.list <- NULL
  if(missing(metRDS)) metRDS <- NULL

  # pass data.str into shiny environment
  shiny_env <- 1
  envir = as.environment(shiny_env)
  assign("reject.list", reject.list, envir = envir)
  assign("metRDS", metRDS, envir = envir)

  appDir <- system.file("metRscreen", package = "metRscreen")
  shiny::runApp(appDir, display.mode = "normal")
}
