#' @title Run the metRscreen paper screening app
#' @description The metRscreen shiny app allows you to screen papers via their abstracts and titles and allows for highlighting of keywords in multiple colours.
#' @return A dataframe of decisioned papers
#' @param screen.file path to the csv (or .ris) file containing references you wish to screen. Can be
#'   absolute (`"~/Desktop/refs.csv"`), relative to the working directory (`"data/refs.csv"`), or built
#'   with `here::here()`.
#' @param reject.list list of rejection reasons to be added to metRscreen, can be left empty
#' @param collab.names vector of screener names to switch on collaborative mode, can be left empty.
#'   Each screener gets their own files (`<screen.file>_<name>_Screened.csv` and
#'   `<screen.file>_<name>_history.rds`), you switch screener in the app, and other
#'   screeners' decisions can be shown or hidden. A combined
#'   `<screen.file>_Collab_Summary.csv` flags agreements and conflicts. Names are
#'   remembered, so later sessions can leave this empty or add new screeners.
#' @param collab.split how papers are shared out in collaborative mode. `"all"` (the default): every
#'   screener screens every paper. `2`: double screening, where every paper is screened by exactly two
#'   screeners, with papers spread evenly and at random across the team (useful for three or more
#'   screeners). The split is saved as `<screen.file>_collab_assignment.csv` and reused in later sessions,
#'   so it never reshuffles part-way through; each screener only sees the papers assigned to them.
#' @param keywords takes a list of green, red, purple, orange, or blue keywords to add to the highlight word command.
#' @export

metRscreen <- function(screen.file, reject.list = NULL, collab.names = NULL, collab.split = "all",
                       keywords = list(green = NULL, red = NULL, purple = NULL, 
                                       orange = NULL, blue = NULL)) {
  if (missing(screen.file)) stop("Please provide a .csv or .ris file to screen", call. = FALSE)
  # make the path absolute: the app runs with its own folder as the working directory, so a relative
  # path (e.g. "data/refs.csv") has to be resolved against the folder metRscreen() was called from
  screen.file <- path.expand(screen.file)
  if (!grepl("^(/|\\\\|[A-Za-z]:)", screen.file)) screen.file <- file.path(getwd(), screen.file)
  screen.file <- normalizePath(screen.file, winslash = "/", mustWork = FALSE)
  if (missing(reject.list)) reject.list <- NULL
  if (missing(collab.names)) collab.names <- NULL
  split_given <- !missing(collab.split)
  check_collab_split(collab.split)   # before anything is saved
  
  # Convert .ris to .csv in place before anything else. If the .csv already exists it is used as it is,
  # so papers can't change under a screening session that has already started (delete the .csv to
  # import the .ris again).
  ris_csv <- sub("\\.ris$", ".csv", screen.file, ignore.case = TRUE)
  if (grepl("\\.ris$", screen.file, ignore.case = TRUE) && file.exists(ris_csv)) {
    cat("\nUsing", basename(ris_csv), "converted earlier (delete it to import", basename(screen.file), "again)\n")
    screen.file <- ris_csv
  }
  if (grepl("\\.ris$", screen.file, ignore.case = TRUE)) {
    
    tag_map <- c(
      TI = "Title", T1 = "Title",
      AU = "Author", A1 = "Author",
      PY = "Publication.Year", Y1 = "Publication.Year",
      JO = "Publication.Title", JF = "Publication.Title",
      T2 = "Publication.Title", JA = "Publication.Title",
      AB = "Abstract", N2 = "Abstract",
      KW = "Manual.Tags"
    )
    multi_fields <- c("Author", "Manual.Tags")
    required_cols <- c("Title", "Author", "Publication.Year",
                       "Publication.Title", "Abstract", "Manual.Tags")
    
    lines <- readLines(screen.file, encoding = "UTF-8", warn = FALSE)
    records <- list()
    current <- list()
    last_col <- NULL   # field the previous tagged line went into
    
    for (line in lines) {
      if (grepl("^ER\\s*-", line)) {
        current <- current[vapply(current, function(v) any(nzchar(v)), logical(1))]   # drop empty fields
        for (field in multi_fields) {
          if (!is.null(current[[field]])) {
            current[[field]] <- paste(current[[field]], collapse = "; ")
          }
        }
        # dates such as "2020///" or "2020/05/01": keep the year
        if (!is.null(current[["Publication.Year"]]) && grepl("^\\s*[0-9]{4}", current[["Publication.Year"]])) {
          current[["Publication.Year"]] <- sub("^\\s*([0-9]{4}).*$", "\\1", current[["Publication.Year"]])
        }
        records <- append(records, list(lapply(current, `[[`, 1)))
        current <- list()
        last_col <- NULL
        next
      }
      # a tag line: two characters, spaces, "-", then a space or the end of the line (empty tags
      # such as "N1  -" have no trailing space in some exports)
      if (grepl("^[A-Z][A-Z0-9]\\s+-(\\s|$)", line)) {
        tag   <- trimws(sub("^([A-Z][A-Z0-9])\\s+-.*", "\\1", line))
        value <- trimws(sub("^[A-Z][A-Z0-9]\\s+-\\s*", "", line))
        last_col <- NULL
        if (tag %in% names(tag_map)) {
          col <- tag_map[[tag]]
          if (col %in% multi_fields) {
            if (nzchar(value)) current[[col]] <- c(current[[col]], value)   # empty tags add nothing
            last_col <- col
          } else if (is.null(current[[col]]) || !nzchar(current[[col]])) {
            current[[col]] <- value
            last_col <- col
          }
        }
      } else if (!is.null(last_col) && nzchar(trimws(line))) {
        # a line without a tag continues the previous field (some exports wrap long
        # abstracts over several lines; previously only the first line was kept)
        k <- length(current[[last_col]])
        if (k == 0) {
          current[[last_col]] <- trimws(line)
        } else {
          current[[last_col]][k] <- trimws(paste(current[[last_col]][k], trimws(line)))
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
    # only this file's own history (other .rds files in the folder, e.g. each
    # collaborator's history, must not be picked up here)
    history.file <- paste0(screen.file, "_history.rds")
    if (file.exists(history.file)) {
      screen.history <- history.file
      cat("\nPrevious screening history found\n")
    } else {
      screen.history <- NULL
      cat("\nNo screening history found\n")
    }

    # collaborative mode: remember the screeners for this file, adding any new ones
    collab.file <- paste0(screen.file, "_collaborators.rds")
    old.collab <- NULL
    if (file.exists(collab.file)) {
      old.collab <- readRDS(collab.file)
    } else if (!is.null(screen.history)) {
      # screeners named in an older shared (pre-collaborative-mode) session
      old.collab <- readRDS(screen.history)$collab.names
    }
    collab.names <- unique(trimws(as.character(c(old.collab, collab.names))))
    collab.names <- mark_utf8(collab.names[!is.na(collab.names) & collab.names != ""])
    # each screener's name is used in their file names, so names must stay distinct
    safe.names <- safe_user(collab.names)
    if (any(safe.names == "") || anyDuplicated(tolower(safe.names))) {
      stop("Screener names must contain letters or numbers (A-Z, 0-9; accents are dropped) and be distinct once ",
           "spaces, punctuation, accents and upper/lower case are ignored (e.g. 'Joel Pick', 'Joel-Pick' and ",
           "'joel pick' would share files).",
           if (length(old.collab)) paste0(" Names saved for this project (in ", basename(collab.file), "): ",
                                           paste(old.collab, collapse = ", "), "."),
           call. = FALSE)
    }
    # screeners' files under their current names first, so the split checks find them
    if (length(collab.names) > 0) migrate_user_files(screen.file, collab.names, previous = old.collab)
    # double screening: which two screeners screen each paper (NULL = everyone screens everything).
    # Before the screeners are saved, so a refused split leaves the project as it was.
    collab.assignment <- collab_assignment(screen.file, collab.names, collab.split, split_given)
    if (length(collab.names) > 0) {
      saveRDS(collab.names, collab.file)
      cat("\nCollaborative mode with screeners:", paste(collab.names, collapse = ", "), "\n")
      # reject reasons are shared by the project, so screeners who start the app without them get them too
      reject.file <- paste0(screen.file, "_reject_list.rds")
      if (!is.null(reject.list)) {
        saveRDS(reject.list, reject.file)
      } else if (file.exists(reject.file)) {
        reject.list <- tryCatch(readRDS(reject.file), error = function(e) NULL)
      } else {
        # projects started before the reasons were saved for the project: take them from a screener's session
        for (f in c(vapply(collab.names, function(u) user_paths(screen.file, u)$history, ""), history.file)) {
          if (!is.null(reject.list)) break
          if (file.exists(f)) reject.list <- tryCatch(readRDS(f)$reject.list, error = function(e) NULL)
        }
        if (!is.null(reject.list)) saveRDS(reject.list, reject.file)
      }
    }
    shiny_env <- 1
    envir <- as.environment(shiny_env)
    assign("collab.assignment", collab.assignment, envir = envir)
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
