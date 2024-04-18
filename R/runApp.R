#' @title Run the metRscreen paper screening app
#' @description The metRscreen shiny app allows you to screen papers via their abstracts and titles and allows for highlighting of keywords in multiple colours.
#' @return A dataframe of decisioned papers
#' @param screen.file path to the csv file containing references you wish to screen.
#' @param reject.vec vector of rejection reasons to be added to metRscreen, can be left empty
#' @param collab.names vector of names to identify screeners to be added to metRscreen, can be left empty
#' @export

metRscreen <- function(screen.file, reject.vec = NULL, collab.names = NULL) {
  # if data.str if missing, assign an empty data.frame
  if (missing(screen.file)) cat("\nError: Please provide a .csv file to screen\n")
  if (missing(reject.vec)) reject.vec <- NULL
  if (missing(collab.names)) collab.names <- NULL

  if (length(list.files(path = dirname(screen.file), pattern = "\\.rds$")) > 0) {
    screen.history <- list.files(path = dirname(screen.file), pattern = "\\.rds$", full.names = TRUE)
    cat("\nPrevious screening history found\n")
  } else {
    screen.history <- NULL
    cat("\nNo screening history found\n")
  }
  
  if(length(collab.names) < 1){
    cat("\nCollaborative mode, authors identified:", paste(collab.names), "\n")
  } else if(length(collab.names) ==1){
    cat("\n Screener identified, author:", paste(collab.names), "\n")
  }
    

  # pass data.str into shiny environment
  shiny_env <- 1
  envir <- as.environment(shiny_env)
  assign("screen.file", screen.file, envir = envir)
  assign("reject.vec", reject.vec, envir = envir)
  assign("collab.names", collab.names, envir = envir)
  assign("screen.history", screen.history, envir = envir)

  appDir <- system.file("metRscreen", package = "metRscreen")
  shiny::runApp(appDir, display.mode = "normal")
}
