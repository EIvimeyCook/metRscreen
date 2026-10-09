# Splitting papers between screeners in collaborative mode (internal helpers used by metRscreen()).
#
# collab.split = "all": every screener screens every paper (default).
# collab.split = 2:     every paper is screened by exactly two screeners (double screening), with the papers
#                       spread evenly across the team and each pair of screeners sharing a similar number.
# The split is made once, saved as <screen.file>_collab_assignment.csv and reused in every later session.

# Screener names in file names ----------------------------------------------------------------------
# Each screener's files are <screen.file>_<safe name>_Screened.csv / _history.rds. Accented Latin letters
# are spelled without the accent (fixed table, so every computer gives the same file name), then anything
# other than A-Z and 0-9 becomes "-".
translit_from <- "\u00c0\u00c1\u00c2\u00c3\u00c4\u00c5\u00c7\u00c8\u00c9\u00ca\u00cb\u00cc\u00cd\u00ce\u00cf\u00d0\u00d1\u00d2\u00d3\u00d4\u00d5\u00d6\u00d8\u00d9\u00da\u00db\u00dc\u00dd\u00e0\u00e1\u00e2\u00e3\u00e4\u00e5\u00e7\u00e8\u00e9\u00ea\u00eb\u00ec\u00ed\u00ee\u00ef\u00f0\u00f1\u00f2\u00f3\u00f4\u00f5\u00f6\u00f8\u00f9\u00fa\u00fb\u00fc\u00fd\u00ff\u0100\u0101\u0102\u0103\u0104\u0105\u0106\u0107\u010c\u010d\u010e\u010f\u0110\u0111\u0112\u0113\u0116\u0117\u0118\u0119\u011a\u011b\u011e\u011f\u0122\u0123\u012a\u012b\u012e\u012f\u0130\u0131\u0136\u0137\u0139\u013a\u013b\u013c\u013d\u013e\u0141\u0142\u0143\u0144\u0145\u0146\u0147\u0148\u014c\u014d\u0150\u0151\u0154\u0155\u0158\u0159\u015a\u015b\u015e\u015f\u0160\u0161\u0162\u0163\u0164\u0165\u016a\u016b\u016e\u016f\u0170\u0171\u0172\u0173\u0178\u0179\u017a\u017b\u017c\u017d\u017e"
translit_to <- "AAAAAACEEEEIIIIDNOOOOOOUUUUYaaaaaaceeeeiiiidnoooooouuuuyyAaAaAaCcCcDdDdEeEeEeEeGgGgIiIiIiKkLlLlLlLlNnNnNnOoOoRrRrSsSsSsTtTtUuUuUuUuYZzZzZz"

translit_codes <- c(utf8ToInt(translit_from), utf8ToInt("\u00df\u00c6\u00e6\u0152\u0153\u00de\u00fe"))
translit_out <- c(strsplit(translit_to, "")[[1]], "ss", "AE", "ae", "OE", "oe", "Th", "th")

# Works on the characters' code points rather than with regular expressions, so the result is the same
# whatever R's locale (otherwise a session not in UTF-8 would look for different files).
safe_user <- function(user, translit = TRUE) {
  vapply(as.character(user), function(u) {
    if (is.na(u)) return("")
    # text already in UTF-8 is used as it is (enc2utf8() would mangle it in a non-UTF-8 locale)
    if (!(Encoding(u) %in% c("unknown", "UTF-8") && validUTF8(u))) u <- enc2utf8(u)
    cp <- utf8ToInt(u)
    # accents typed as separate combining marks ("e" + U+0308) are dropped, so they match "\u00eb" -> "e"
    if (translit) cp <- cp[cp < 0x300 | cp > 0x36F]
    if (anyNA(cp)) return("")
    out <- rep("-", length(cp))
    keep <- (cp >= 48 & cp <= 57) | (cp >= 65 & cp <= 90) | (cp >= 97 & cp <= 122)   # 0-9 A-Z a-z
    out[keep] <- intToUtf8(cp[keep], multiple = TRUE)
    if (translit) {
      m <- match(cp, translit_codes)
      out[!is.na(m)] <- translit_out[m[!is.na(m)]]
    }
    gsub("^-+|-+$", "", gsub("-+", "-", paste(out, collapse = "")))
  }, "", USE.NAMES = FALSE)
}

# Mark names that are valid UTF-8 as such: in a session that isn't in UTF-8 they are otherwise "unknown",
# which some functions (e.g. sort(method = "radix")) refuse
mark_utf8 <- function(x) {
  x <- as.character(x)
  i <- !is.na(x) & Encoding(x) == "unknown" & validUTF8(x)
  if (any(i)) Encoding(x)[i] <- "UTF-8"
  x
}

# Do two sets of titles match? Compared as UTF-8, so a file saved in one locale matches the references read
# in another (otherwise accented titles look different and screening can't carry on)
same_titles <- function(a, b) isTRUE(all.equal(mark_utf8(a), mark_utf8(b)))

# File names used before accents were spelled out ("Zo\u00eb" was "Zo")
old_safe_user <- function(user) safe_user(user, translit = FALSE)

user_paths <- function(screen.file, user) {
  stem <- paste0(screen.file, "_", safe_user(user))
  list(screened = paste0(stem, "_Screened.csv"), history = paste0(stem, "_history.rds"))
}

# Rename a screener's files from the old naming to the new one, if they have no files under the new
# name yet and the old files are theirs: they hold their decisions, or hold no decisions and the old name
# isn't that of another screener who was already in the project (and so could own them). So "Jos" can
# never be given "Jos\u00e9"'s files, or the reverse.
migrate_user_files <- function(screen.file, users, previous = users) {
  for (u in users) {
    old_stem <- old_safe_user(u)
    old <- paste0(screen.file, "_", old_stem, c("_Screened.csv", "_history.rds"))
    new <- unlist(user_paths(screen.file, u))
    if (identical(old, unname(new)) || any(file.exists(new)) || !file.exists(old[1])) next
    d <- tryCatch(utils::read.csv(old[1], stringsAsFactors = FALSE), error = function(e) NULL)
    if (is.null(d)) next
    # the session file keeps the names' encoding, so also look there (the .csv's can differ by locale)
    h <- if (file.exists(old[2])) tryCatch(readRDS(old[2])$new.data, error = function(e) NULL)
    theirs <- isTRUE(any(mark_utf8(d$Screen.Name) == u)) || isTRUE(any(h$Screen.Name == u))
    unused <- !isTRUE(any(d$Screen != "To be screened")) && !old_stem %in% safe_user(setdiff(previous, u))
    if (!theirs && !unused) next
    ex <- file.exists(old)
    if (all(file.rename(old[ex], new[ex]))) cat("\nRenamed ", u, "'s screening files to ", basename(new[1]), "\n", sep = "")
  }
}

# collab.split must be "all" or 2
check_collab_split <- function(split) {
  if (length(split) != 1 || !(identical(split, "all") || isTRUE(suppressWarnings(as.numeric(split)) == 2))) {
    stop("collab.split must be \"all\" (everyone screens every paper) or 2 (each paper screened by two people).", call. = FALSE)
  }
}

# Balanced random assignment of each paper to two distinct screeners.
make_pair_assignment <- function(n, users, seed = 1) {
  if (length(users) < 2) stop("Double screening needs at least two screeners.", call. = FALSE)
  # use a fixed seed without changing the user's random-number stream
  old_seed <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) get(".Random.seed", envir = globalenv()) else NULL
  on.exit({
    if (is.null(old_seed)) {
      if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
    } else assign(".Random.seed", old_seed, envir = globalenv())
  }, add = TRUE)
  set.seed(seed)

  pairs <- t(utils::combn(users, 2))
  n_pairs <- nrow(pairs)
  papers <- sample.int(n)                     # papers in random order
  full <- (n %/% n_pairs) * n_pairs           # papers that fill whole rounds of every pair
  pair_of <- integer(n)
  if (full > 0) pair_of[seq_len(full)] <- as.vector(replicate(n %/% n_pairs, sample.int(n_pairs)))
  # the remaining papers (fewer than one per pair) each go to two of the screeners with the fewest papers
  # so far (so papers per screener differ by at most one), avoiding using any pair twice; random tie-breaks,
  # retried a few times if a pair ends up used twice
  rem <- seq_len(n - full) + full
  for (attempt in seq_len(100)) {
    load <- stats::setNames(numeric(length(users)), users)
    used <- numeric(n_pairs)
    for (j in rem) {
      l2 <- sort(load)[2]
      lp <- cbind(load[pairs[, 1]], load[pairs[, 2]])
      ok <- which(pmin(lp[, 1], lp[, 2]) == min(load) & pmax(lp[, 1], lp[, 2]) <= l2)
      ok <- ok[used[ok] == min(used[ok])]
      k <- if (length(ok) > 1) sample(ok, 1) else ok
      used[k] <- used[k] + 1
      pair_of[j] <- k
      load[pairs[k, ]] <- load[pairs[k, ]] + 1
    }
    if (all(used <= 1)) break
  }
  # which of the two is listed first carries no meaning; randomise it
  flip <- stats::runif(n) < 0.5
  s1 <- ifelse(flip, pairs[pair_of, 2], pairs[pair_of, 1])
  s2 <- ifelse(flip, pairs[pair_of, 1], pairs[pair_of, 2])
  out <- data.frame(row = papers, Screener1 = s1, Screener2 = s2, stringsAsFactors = FALSE)
  out[order(out$row), , drop = FALSE]
}

# Has anyone in `users` made a decision yet?  (their own _Screened.csv files, plus decisions from an earlier
# single-screener session, which are carried over to collaborators)
screening_started <- function(screen.file, users) {
  decided <- function(d) !is.null(d) && any(d$Screen != "To be screened", na.rm = TRUE)
  who <- users[vapply(users, function(u) {
    f <- user_paths(screen.file, u)$screened
    file.exists(f) && decided(tryCatch(utils::read.csv(f, stringsAsFactors = FALSE), error = function(e) NULL))
  }, logical(1))]
  legacy <- paste0(screen.file, "_history.rds")
  if (file.exists(legacy) && decided(tryCatch(readRDS(legacy)$new.data, error = function(e) NULL))) {
    who <- c(who, "an earlier single-screener session")
  }
  who
}

# The saved split, or NULL if it can't be read or isn't a valid split of these papers between these screeners
read_split <- function(path, refs, users) {
  a <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8"), error = function(e) NULL)
  if (is.null(a) || !all(c("row", "Title", "Screener1", "Screener2") %in% names(a)) || nrow(a) != nrow(refs) ||
      !setequal(a$row, seq_len(nrow(refs))) ||
      !same_titles(a$Title[order(a$row)], refs$Title)) {
    return(NULL)
  }
  # every paper needs two different screeners who are part of the project, otherwise agreement
  # would be judged from a single screener
  bad <- is.na(a$Screener1) | is.na(a$Screener2) | a$Screener1 == "" | a$Screener2 == "" |
    a$Screener1 == a$Screener2 | !a$Screener1 %in% users | !a$Screener2 %in% users
  if (any(bad)) return(NULL)
  a
}

# The split to use for this session: NULL (everyone screens everything) or the assignment data frame.
collab_assignment <- function(screen.file, users, split = "all", split_given = TRUE) {
  path <- paste0(screen.file, "_collab_assignment.csv")
  check_collab_split(split)

  # The project remembers the last collab.split given, so leaving it out continues in the same mode (older
  # projects without this setting: double screening if they have a split file).
  # A mode of 2 carries the attribute split_made once papers have been split, so a split file that has
  # gone missing (e.g. not synced or pulled yet) is never mistaken for a project without one.
  mode_file <- paste0(screen.file, "_collab_split.rds")
  saved <- if (file.exists(mode_file)) {
    tryCatch(readRDS(mode_file), error = function(e) {
      # don't guess the project's mode: guessing wrong could reshuffle or merge everyone's papers
      if (!split_given) {
        stop(basename(mode_file), " could not be read (is it still syncing?). Wait and try again, or give ",
             "collab.split (\"all\" or 2) to say how papers are shared out and replace it.", call. = FALSE)
      }
      cat("\n", basename(mode_file), " could not be read and will be replaced\n", sep = "")
      NULL
    })
  }
  split_made <- isTRUE(attr(saved, "split_made"))
  # the mode to save, written by done() only once every check has passed, so a refused call never
  # changes the project
  pending <- NULL
  done <- function(x) {
    if (!is.null(pending)) saveRDS(pending, mode_file)
    x
  }
  if (split_given) {
    mode <- if (identical(split, "all")) "all" else 2
    pending <- if (split_made) structure(mode, split_made = TRUE) else mode
  } else {
    mode <- if (!is.null(saved)) as.vector(saved) else if (file.exists(path)) 2 else "all"
  }

  # A saved split for a different set of screeners (e.g. someone added), or one that can't be read or isn't
  # valid, is discarded if no one has screened yet, so the papers are shared out again among the current
  # screeners when a split is used. Once screening has started it is kept (and checked below).
  refs <- utils::read.csv(screen.file)
  # (with everyone screening every paper the split isn't used, so it is left as it is)
  if (file.exists(path) && !identical(mode, "all")) {
    old <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8"), error = function(e) NULL)
    in_split <- unique(c(old$Screener1, old$Screener2))
    in_split <- in_split[!is.na(in_split) & in_split != ""]
    changed <- !setequal(in_split, users) || is.null(read_split(path, refs, users))
    if (changed && !length(screening_started(screen.file, union(users, in_split)))) {
      if (!file.remove(path)) {
        stop("The saved split (", basename(path), ") needs to be made again, but couldn't be deleted ",
             "(is it open in another program?). Close it, or delete it yourself, and start metRscreen again.",
             call. = FALSE)
      }
      cat("\nThe screeners have changed or the saved split isn't valid, and no one has screened yet, so the",
          "saved split has been discarded\n")
      # no split now (a new one is made below if needed), so a missing split file is no longer an error
      split_made <- FALSE
      pending <- mode
    }
  }

  if (length(users) < 2) return(done(NULL))

  if (identical(mode, "all")) {
    if (file.exists(path)) {
      cat("\nEvery screener screens every paper (the saved split in", basename(path), "is kept but not used;",
          "collab.split = 2 uses it again)\n")
    }
    return(done(NULL))
  }

  if (length(users) == 2) {
    cat("\nWith two screeners, double screening means both screen every paper\n")
    return(done(NULL))
  }

  if (file.exists(path)) {
    a <- read_split(path, refs, users)
    if (is.null(a)) {
      stop("The saved split (", basename(path), ") can't be read, doesn't match the papers in ",
           basename(screen.file), ", or doesn't give every paper two different screeners from this project. ",
           "Screening has started, so it can't be made again: restore the original split file (and reference ",
           "file) from a backup or GitHub.", call. = FALSE)
    }
    in_split <- unique(c(a$Screener1, a$Screener2))
    missing_users <- setdiff(users, in_split)
    if (length(missing_users)) {
      cat("\nNot in the saved split, so no papers to screen:", paste(missing_users, collapse = ", "),
          "\n(screening has started, so the split can't be remade)\n")
    }
    cat("\nDouble screening: using the saved split in", basename(path), "\n")
    # projects split before split_made was recorded
    if (!split_made) pending <- structure(2, split_made = TRUE)
    a <- a[order(a$row), , drop = FALSE]
    rownames(a) <- NULL
    return(done(a))
  }

  # a new split would reshuffle papers - never do that once anyone has started screening
  started <- screening_started(screen.file, users)
  if (length(started) && split_made) {
    stop("This project splits papers between screeners, but ", basename(path), " is missing. If it hasn't ",
         "synced or been pulled from GitHub yet, wait for it; otherwise restore it from a backup. ",
         "(To have everyone screen every paper instead, use collab.split = \"all\".)", call. = FALSE)
  }
  if (length(started) && !split_given) {
    # carrying on from an earlier session: don't block the project, carry on with everyone screening everything
    cat("\nScreening has already started (", paste(started, collapse = ", "), "), so the papers can't be split between ",
        "screeners now: every screener screens every paper.\n", sep = "")
    saveRDS("all", mode_file)
    return(NULL)
  }
  if (length(started)) {
    stop("Screening has already started (", paste(started, collapse = ", "), "), so the papers can't be split ",
         "between screeners now (it would reassign papers people have already screened). Carry on with ",
         "collab.split = \"all\" (everyone screens every paper), or, if ", basename(path), " was deleted by ",
         "mistake, restore it from a backup or GitHub.", call. = FALSE)
  }
  # sorted, so the split doesn't depend on the order the names were given in
  a <- make_pair_assignment(nrow(refs), sort(mark_utf8(users), method = "radix"))   # radix: same order on every computer
  a$Title <- mark_utf8(refs$Title[a$row])   # so readr writes accented titles as they are
  a <- a[, c("row", "Title", "Screener1", "Screener2")]
  rownames(a) <- NULL
  # always UTF-8 (write.csv would spell accented names as "<U+00EB>" in a session not in UTF-8)
  readr::write_csv(a, path)
  pending <- structure(2, split_made = TRUE)
  per <- table(factor(c(a$Screener1, a$Screener2), levels = users))
  cat("\nDouble screening: each paper assigned to two screeners, saved in", basename(path), "\n")
  cat(paste0("  ", names(per), ": ", as.integer(per), " papers", collapse = "\n"), "\n")
  done(a)
}
