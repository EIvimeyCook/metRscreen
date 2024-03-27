#' @title metRscreen
#' @description The metRscreen shiny app allows you to screen papers via their abstracts and titles and allows for highlighting of keywords in multiple colours. 
#' @return A dataframe of decisioned papers
#' @export

metaScreen <- function() {
  appDir <- system.file("metRscreen", package = "metRscreen")
  shiny::runApp(appDir, display.mode = "normal")
}