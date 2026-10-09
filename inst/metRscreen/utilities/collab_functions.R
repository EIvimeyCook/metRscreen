#' Collaborative-mode helpers for metRscreen
#'
#' In collaborative mode (collab.names supplied) every screener gets their own
#' files next to the screening file, so screening is independent:
#'   <screen.file>_<name>_Screened.csv   decisions for that screener
#'   <screen.file>_<name>_history.rds    that screener's resumable session
#'   <screen.file>_collaborators.rds     the list of screeners for the project
#'   <screen.file>_Collab_Summary.csv    every screener's decision side by side

#' @title safe_user / user_paths
#' @description A screener's name as used in file names, and their file paths. Shared with metRscreen()
#'   (R/collab_assign.R) so the app and the package always agree on the file names.
safe_user <- metRscreen:::safe_user
user_paths <- metRscreen:::user_paths
same_titles <- metRscreen:::same_titles

#' @title collab_file
#' @description Path of the file storing the screeners for a project
#' @param screen.file path to the file being screened
collab_file <- function(screen.file) paste0(screen.file, "_collaborators.rds")

#' @title blank_screen
#' @description Unscreened template built from the screening file
#' @param screen.file path to the file being screened
blank_screen <- function(screen.file) {
  cbind(
    utils::read.csv(screen.file),
    Screen = "To be screened",
    Reason = "No reason given",
    Comment = "No comments given",
    Screen.Name = "No screener name given"
  )
}

#' @title first_unscreened
#' @description Row of the first paper still to be screened (or the last paper)
#' @param data screening data frame
#' @param rows rows this screener screens (all rows unless papers are split between screeners)
first_unscreened <- function(data, rows = seq_len(nrow(data))) {
  todo <- rows[data$Screen[rows] == "To be screened"]
  if (length(todo) > 0) todo[1] else if (length(rows)) rows[length(rows)] else nrow(data)
}

#' @title assigned_rows
#' @description Rows (papers) a screener screens. With no split every screener screens every paper;
#'   with a split, the rows where the screener is one of the two assigned screeners.
#' @param assignment NULL (everyone screens everything) or the data frame from the assignment file
#' @param user screener name (NULL = none chosen yet)
#' @param n number of papers
assigned_rows <- function(assignment, user, n) {
  if (is.null(assignment) || is.null(user)) return(seq_len(n))
  sort(assignment$row[assignment$Screener1 == user | assignment$Screener2 == user])
}

#' @title assigned_screeners
#' @description Screeners assigned to one paper (all screeners when there is no split)
assigned_screeners <- function(assignment, users, row) {
  if (is.null(assignment)) return(users)
  a <- assignment[assignment$row == row, , drop = FALSE]
  if (!nrow(a)) return(character(0))
  c(a$Screener1[1], a$Screener2[1])
}

#' @title load_user_state
#' @description Loads a screener's saved session. If they have none yet it starts
#'   a fresh one, carrying over any decisions they made in an older shared
#'   (non-collaborative) metRscreen session.
#' @param screen.file path to the file being screened
#' @param user screener name
#' @return list with settings (as saved by metRscreen) and a status message, or
#'   NULL settings with an error message if the saved file does not match
load_user_state <- function(screen.file, user, rows = NULL) {
  paths <- user_paths(screen.file, user)
  current <- utils::read.csv(screen.file)

  if (file.exists(paths$history)) {
    s <- tryCatch(readRDS(paths$history), error = function(e) NULL)
    if (!is.null(s)) {
      if (!same_titles(s$new.data$Title, current$Title)) {
        return(list(settings = NULL, message = paste0(
          "The saved screening file for ", user,
          " does not match the papers being screened - please revert to the previous version"
        )))
      }
      return(list(settings = s, message = paste("Resuming screening for", user)))
    }
    # unreadable (e.g. still syncing): use their decisions file below, if there is one
    if (!file.exists(paths$screened)) {
      return(list(settings = NULL, message = paste0(
        "The saved screening file for ", user, " (", basename(paths$history), ") could not be read - ",
        "if it is still syncing, wait and try again, otherwise restore it from a backup"
      )))
    }
  }

  # new screener - start from a blank template
  s <- list(new.data = blank_screen(screen.file), counter = 1)
  msg <- paste("Starting a new screening file for", user)

  # their decisions file without the session file (e.g. only the .csv was synced or committed):
  # start from it rather than overwriting it with a blank one
  if (file.exists(paths$screened)) {
    d <- read_user_decisions(screen.file, user)
    if (!is.null(d) && nrow(d) == nrow(current) && same_titles(d$Title, current$Title) &&
      all(c("Screen", "Reason", "Comment", "Screen.Name") %in% names(d))) {
      s$new.data <- d
      s$counter <- first_unscreened(d, if (is.null(rows)) seq_len(nrow(d)) else rows)
      return(list(settings = s, message = paste("Resuming screening for", user, "from", basename(paths$screened))))
    }
    return(list(settings = NULL, message = paste0(
      basename(paths$screened), " does not match the papers being screened - please revert to the previous version"
    )))
  }

  # carry over decisions this person made in a shared, pre-collaborative session
  legacy <- paste0(screen.file, "_history.rds")
  if (file.exists(legacy)) {
    old <- tryCatch(readRDS(legacy), error = function(e) NULL)
    old_dat <- old$new.data
    if (!is.null(old_dat) && "Screen.Name" %in% names(old_dat) &&
      same_titles(old_dat$Title, current$Title)) {
      # only their own papers (double screening)
      mine <- which(old_dat$Screen.Name == user & old_dat$Screen != "To be screened")
      if (!is.null(rows)) mine <- intersect(mine, rows)
      if (length(mine) > 0) {
        cols <- c("Screen", "Reason", "Comment", "Screen.Name")
        s$new.data[mine, cols] <- old_dat[mine, cols]
        msg <- paste0(msg, " (", length(mine), " earlier decision(s) carried over)")
      }
      for (nm in c("inputs", "search1", "search2", "search3", "search4", "search5", "reject.list")) {
        s[[nm]] <- old[[nm]]
      }
    }
  }
  s$counter <- first_unscreened(s$new.data, if (is.null(rows)) seq_len(nrow(s$new.data)) else rows)
  list(settings = s, message = msg)
}

#' @title unnamed_decisions
#' @description Rows decided in an earlier single-screener session, where no screener name was recorded
#' @param screen.file path to the file being screened
#' @return list(rows, data) or NULL if there are none (or they have already been claimed)
unnamed_decisions <- function(screen.file) {
  legacy <- paste0(screen.file, "_history.rds")
  if (!file.exists(legacy) || file.exists(paste0(screen.file, "_unnamed_claimed.rds"))) return(NULL)
  old_dat <- tryCatch(readRDS(legacy)$new.data, error = function(e) NULL)
  current <- utils::read.csv(screen.file)
  if (is.null(old_dat) || !same_titles(old_dat$Title, current$Title)) return(NULL)
  who <- if ("Screen.Name" %in% names(old_dat)) as.character(old_dat$Screen.Name) else rep(NA, nrow(old_dat))
  rows <- which(old_dat$Screen != "To be screened" & (is.na(who) | who %in% c("", "No screener name given")))
  if (!length(rows)) return(NULL)
  list(rows = rows, data = old_dat)
}

#' @title save_user_state
#' @description Writes a screener's decisions (.csv) and session (.rds)
#' @param screen.file path to the file being screened
#' @param user screener name
#' @param settings list of settings (reactiveValuesToList(settings.store))
#' @param rows rows decided in this session. Every other row is taken from the screener's saved file,
#'   so decisions they made elsewhere in the meantime (another computer or window sharing the folder)
#'   are never overwritten with this session's older copy.
#' @return the screening data as written (with any decisions made elsewhere merged in)
save_user_state <- function(screen.file, user, settings, rows = integer(0)) {
  paths <- user_paths(screen.file, user)
  disk <- if (file.exists(paths$history)) tryCatch(readRDS(paths$history)$new.data, error = function(e) NULL)
  # session file unreadable (e.g. mid-sync): merge with the decisions file instead
  if (is.null(disk)) disk <- read_user_decisions(screen.file, user)
  if (!is.null(disk) && nrow(disk) == nrow(settings$new.data) &&
    same_titles(disk$Title, settings$new.data$Title)) {
    cols <- intersect(c("Screen", "Reason", "Comment", "Screen.Name"), names(disk))
    disk[rows, cols] <- settings$new.data[rows, cols]
    settings$new.data <- disk
  }
  utils::write.csv(settings$new.data, file = paths$screened, row.names = FALSE)
  saveRDS(settings, file = paths$history)
  invisible(settings$new.data)
}

#' @title read_user_decisions
#' @description Reads another screener's decisions, or NULL if they have not started
#' @param screen.file path to the file being screened
#' @param user screener name
read_user_decisions <- function(screen.file, user) {
  path <- user_paths(screen.file, user)$screened
  if (!file.exists(path)) return(NULL)
  tryCatch(utils::read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
}

#' @title agreement_status
#' @description Agreement between screeners for each paper
#' @param screens data frame/matrix with one column of Screen decisions per screener
agreement_status <- function(screens) {
  screens <- as.matrix(screens)
  apply(screens, 1, function(x) {
    x <- x[is.na(x) | x != "Not assigned"]      # only the screeners assigned to the paper count
    if (!length(x) || any(is.na(x) | x == "To be screened")) {
      "Incomplete"
    } else if (length(unique(x)) == 1) {
      "Agree"
    } else {
      "Conflict"
    }
  })
}

#' @title write_collab_summary
#' @description Writes every screener's decision side by side with an agreement column
#' @param screen.file path to the file being screened
#' @param users screener names
#' @param assignment NULL (everyone screens everything) or the split of papers between screeners
write_collab_summary <- function(screen.file, users, assignment = NULL, current = NULL) {
  base <- utils::read.csv(screen.file)
  keep <- intersect(c("Title", "Author", "Publication.Year", "Publication.Title"), names(base))
  out <- base[, keep, drop = FALSE]
  if (!is.null(assignment)) {
    out$Assigned.To <- paste(assignment$Screener1, assignment$Screener2, sep = " & ")[match(seq_len(nrow(base)), assignment$row)]
  }
  screen_cols <- character(0)

  for (u in users) {
    # the active screener's decisions come from the app when given (their file may not be saved yet)
    d <- if (!is.null(current) && identical(current$user, u)) as.data.frame(current$data) else read_user_decisions(screen.file, u)
    ok <- !is.null(d) && nrow(d) == nrow(base) && same_titles(d$Title, base$Title)
    mine <- seq_len(nrow(base)) %in% assigned_rows(assignment, u, nrow(base))
    for (col in c("Screen", "Reason", "Comment")) {
      v <- if (ok) d[[col]] else rep(if (col == "Screen") "To be screened" else NA, nrow(base))
      v[!mine] <- if (col == "Screen") "Not assigned" else NA
      out[[paste0(u, ".", col)]] <- v
    }
    screen_cols <- c(screen_cols, paste0(u, ".Screen"))
  }

  out$Agreement <- agreement_status(out[, screen_cols, drop = FALSE])
  path <- paste0(screen.file, "_Collab_Summary.csv")
  utils::write.csv(out, file = path, row.names = FALSE)
  invisible(out)
}
