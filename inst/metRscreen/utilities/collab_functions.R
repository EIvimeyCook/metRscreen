#' Collaborative-mode helpers for metRscreen
#'
#' In collaborative mode (collab.names supplied) every screener gets their own
#' files next to the screening file, so screening is independent:
#'   <screen.file>_<name>_Screened.csv   decisions for that screener
#'   <screen.file>_<name>_history.rds    that screener's resumable session
#'   <screen.file>_collaborators.rds     the list of screeners for the project
#'   <screen.file>_Collab_Summary.csv    every screener's decision side by side

#' @title safe_user
#' @description Turns a screener name into something safe to use in a file name
#' @param user screener name
safe_user <- function(user) {
  gsub("^-+|-+$", "", gsub("[^A-Za-z0-9]+", "-", user))
}

#' @title user_paths
#' @description File paths for one screener
#' @param screen.file path to the file being screened
#' @param user screener name
user_paths <- function(screen.file, user) {
  stem <- paste0(screen.file, "_", safe_user(user))
  list(
    screened = paste0(stem, "_Screened.csv"),
    history = paste0(stem, "_history.rds")
  )
}

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
    s <- readRDS(paths$history)
    if (!isTRUE(all.equal(s$new.data$Title, current$Title))) {
      return(list(settings = NULL, message = paste0(
        "The saved screening file for ", user,
        " does not match the papers being screened - please revert to the previous version"
      )))
    }
    return(list(settings = s, message = paste("Resuming screening for", user)))
  }

  # new screener - start from a blank template
  s <- list(new.data = blank_screen(screen.file), counter = 1)
  msg <- paste("Starting a new screening file for", user)

  # carry over decisions this person made in a shared, pre-collaborative session
  legacy <- paste0(screen.file, "_history.rds")
  if (file.exists(legacy)) {
    old <- readRDS(legacy)
    old_dat <- old$new.data
    if (!is.null(old_dat) && "Screen.Name" %in% names(old_dat) &&
      isTRUE(all.equal(old_dat$Title, current$Title))) {
      mine <- which(old_dat$Screen.Name == user & old_dat$Screen != "To be screened")
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

#' @title save_user_state
#' @description Writes a screener's decisions (.csv) and session (.rds)
#' @param screen.file path to the file being screened
#' @param user screener name
#' @param settings list of settings (reactiveValuesToList(settings.store))
save_user_state <- function(screen.file, user, settings) {
  paths <- user_paths(screen.file, user)
  utils::write.csv(settings$new.data, file = paths$screened, row.names = FALSE)
  saveRDS(settings, file = paths$history)
  invisible(paths)
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
write_collab_summary <- function(screen.file, users, assignment = NULL) {
  base <- utils::read.csv(screen.file)
  keep <- intersect(c("Title", "Author", "Publication.Year", "Publication.Title"), names(base))
  out <- base[, keep, drop = FALSE]
  if (!is.null(assignment)) {
    out$Assigned.To <- paste(assignment$Screener1, assignment$Screener2, sep = " & ")[match(seq_len(nrow(base)), assignment$row)]
  }
  screen_cols <- character(0)

  for (u in users) {
    d <- read_user_decisions(screen.file, u)
    ok <- !is.null(d) && nrow(d) == nrow(base) && isTRUE(all.equal(d$Title, base$Title))
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
