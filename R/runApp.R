#' @title Run the metRscreen paper screening app
#' @description The metRscreen shiny app allows you to screen papers via their abstracts and titles and allows for highlighting of keywords in multiple colours.
#' @return A dataframe of decisioned papers
#' @param screen.file path to the csv file containing references you wish to screen.
#' @param reject.list list of rejection reasons to be added to metRscreen, can be left empty
#' @param collab.names vector of names to identify screeners to be added to metRscreen, can be left empty
#' @param keywords takes a list of green, red, purple, orange, or blue keywords to add to the highlight word command.
#' @export

metRscreen <- function(screen.file, reject.list = NULL, collab.names = NULL,
                       keywords = list(green = NULL, red = NULL, purple = NULL, 
                                       orange = NULL, blue = NULL)) {
  if (missing(screen.file)) cat("\nError: Please provide a .csv file to screen\n")
  if (missing(reject.list)) reject.list <- NULL
  if (missing(collab.names)) collab.names <- NULL
  
  # Convert .ris to .csv in place before anything else
  if (grepl("\\.ris$", screen.file, ignore.case = TRUE)) {
    
    tag_map <- c(
      TI = "Title", T1 = "Title",
      AU = "Author",
      PY = "Publication.Year", Y1 = "Publication.Year",
      JO = "Publication.Title", JF = "Publication.Title",
      T2 = "Publication.Title", JA = "Publication.Title",
      AB = "Abstract",
      KW = "Manual.Tags"
    )
    multi_fields <- c("Author", "Manual.Tags")
    required_cols <- c("Title", "Author", "Publication.Year",
                       "Publication.Title", "Abstract", "Manual.Tags")
    
    lines <- readLines(screen.file, encoding = "UTF-8", warn = FALSE)
    records <- list()
    current <- list()
    
    for (line in lines) {
      if (grepl("^ER\\s*-", line)) {
        for (field in multi_fields) {
          if (!is.null(current[[field]])) {
            current[[field]] <- paste(current[[field]], collapse = "; ")
          }
        }
        records <- append(records, list(lapply(current, `[[`, 1)))
        current <- list()
        next
      }
      if (grepl("^[A-Z][A-Z0-9]\\s+-\\s", line)) {
        tag   <- trimws(sub("^([A-Z][A-Z0-9])\\s+-.*", "\\1", line))
        value <- trimws(sub("^[A-Z][A-Z0-9]\\s+-\\s+", "", line))
        if (tag %in% names(tag_map)) {
          col <- tag_map[[tag]]
          if (col %in% multi_fields) {
            current[[col]] <- c(current[[col]], value)
          } else if (is.null(current[[col]])) {
            current[[col]] <- value
          }
        }
      }
    }
    
    df <- dplyr::bind_rows(records) |>
      tibble::add_column(!!!purrr::set_names(
        rep(list(NA_character_), sum(!required_cols %in% names(dplyr::bind_rows(records)))),
        required_cols[!required_cols %in% names(dplyr::bind_rows(records))]
      )) |>
      dplyr::select(dplyr::all_of(required_cols))
    
    screen.file <- sub("\\.ris$", ".csv", screen.file, ignore.case = TRUE)
    readr::write_csv(df, screen.file)
    cat(sprintf("\nConverted %d records to .csv: %s\n", nrow(df), screen.file))
  }
  
  if (file.exists(screen.file)) {
    if (length(list.files(path = dirname(screen.file), pattern = "\\.rds$")) > 0) {
      screen.history <- list.files(path = dirname(screen.file), pattern = "\\.rds$", full.names = TRUE)
      cat("\nPrevious screening history found\n")
    } else {
      screen.history <- NULL
      cat("\nNo screening history found\n")
    }
    shiny_env <- 1
    envir <- as.environment(shiny_env)
    assign("screen.file",    screen.file,    envir = envir)
    assign("reject.list",    reject.list,    envir = envir)
    assign("collab.names",   collab.names,   envir = envir)
    assign("screen.history", screen.history, envir = envir)
    assign("keywords",       keywords,       envir = envir)
    appDir <- system.file("metRscreen", package = "metRscreen")
    shiny::runApp(appDir, display.mode = "normal")
  } else {
    cat("\nError: no file detected. Please select a valid file to screen\n")
  }
}
