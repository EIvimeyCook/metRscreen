# Splitting papers between screeners in collaborative mode (internal helpers used by metRscreen()).
#
# collab.split = "all": every screener screens every paper (default).
# collab.split = 2:     every paper is screened by exactly two screeners (double screening), with the papers
#                       spread evenly across the team and each pair of screeners sharing a similar number.
# The split is made once, saved as <screen.file>_collab_assignment.csv and reused in every later session.

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

# Has anyone in `users` made a decision yet?  (their own _Screened.csv files)
screening_started <- function(screen.file, users) {
  users[vapply(users, function(u) {
    f <- paste0(screen.file, "_", gsub("^-+|-+$", "", gsub("[^A-Za-z0-9]+", "-", u)), "_Screened.csv")
    file.exists(f) && any(utils::read.csv(f, stringsAsFactors = FALSE)$Screen != "To be screened", na.rm = TRUE)
  }, logical(1))]
}

# The split to use for this session: NULL (everyone screens everything) or the assignment data frame.
collab_assignment <- function(screen.file, users, split = "all", split_given = TRUE) {
  path <- paste0(screen.file, "_collab_assignment.csv")
  if (length(split) != 1 || !(identical(split, "all") || isTRUE(suppressWarnings(as.numeric(split)) == 2))) {
    stop("collab.split must be \"all\" (everyone screens every paper) or 2 (each paper screened by two people).", call. = FALSE)
  }

  # The project remembers the last collab.split given, so leaving it out continues in the same mode (older
  # projects without this setting: double screening if they have a split file).
  mode_file <- paste0(screen.file, "_collab_split.rds")
  if (split_given) {
    mode <- if (identical(split, "all")) "all" else 2
    saveRDS(mode, mode_file)
  } else {
    mode <- if (file.exists(mode_file)) readRDS(mode_file) else if (file.exists(path)) 2 else "all"
  }

  # A saved split for a different set of screeners (e.g. someone added) is discarded if no one
  # has screened yet, so the papers are shared out again among the current screeners when a split is used.
  # Once screening has started it is kept (and checked below).
  if (file.exists(path)) {
    old <- utils::read.csv(path, stringsAsFactors = FALSE)
    in_split <- unique(c(old$Screener1, old$Screener2))
    in_split <- in_split[!is.na(in_split) & in_split != ""]
    if (!setequal(in_split, users) && !length(screening_started(screen.file, union(users, in_split)))) {
      if (!file.remove(path)) {
        stop("The screeners have changed, but ", basename(path), " couldn't be deleted to share the papers out ",
             "again (is it open in another program?). Close it, or delete it yourself, and start metRscreen again.",
             call. = FALSE)
      }
      cat("\nThe screeners have changed and no one has screened yet, so the saved split has been discarded\n")
    }
  }

  if (length(users) < 2) return(NULL)

  if (identical(mode, "all")) {
    if (file.exists(path)) {
      cat("\nEvery screener screens every paper (the saved split in", basename(path), "is kept but not used;",
          "collab.split = 2 uses it again)\n")
    }
    return(NULL)
  }

  if (length(users) == 2) {
    cat("\nWith two screeners, double screening means both screen every paper\n")
    return(NULL)
  }

  refs <- utils::read.csv(screen.file)
  if (file.exists(path)) {
    a <- utils::read.csv(path, stringsAsFactors = FALSE)
    if (!all(c("row", "Title", "Screener1", "Screener2") %in% names(a)) || nrow(a) != nrow(refs) ||
        !isTRUE(all.equal(as.character(a$Title[order(a$row)]), as.character(refs$Title)))) {
      stop("The saved split (", basename(path), ") does not match the papers in ", basename(screen.file),
           ". Restore the original reference file, or delete the split file to make a new one.", call. = FALSE)
    }
    in_split <- unique(c(a$Screener1, a$Screener2))
    # every paper needs two different screeners who are part of the project, otherwise agreement
    # would be judged from a single screener
    bad <- is.na(a$Screener1) | is.na(a$Screener2) | a$Screener1 == "" | a$Screener2 == "" |
      a$Screener1 == a$Screener2 | !a$Screener1 %in% users | !a$Screener2 %in% users
    if (any(bad)) {
      stop("The saved split (", basename(path), ") is not valid for ", sum(bad), " paper(s): each paper needs two ",
           "different screeners from the project's screeners. Restore the original split file (e.g. from a backup ",
           "or GitHub).", call. = FALSE)
    }
    missing_users <- setdiff(users, in_split)
    if (length(missing_users)) {
      cat("\nNot in the saved split, so no papers to screen:", paste(missing_users, collapse = ", "),
          "\n(screening has started, so the split can't be remade)\n")
    }
    cat("\nDouble screening: using the saved split in", basename(path), "\n")
    a <- a[order(a$row), , drop = FALSE]
    rownames(a) <- NULL
    return(a)
  }

  # a new split would reshuffle papers - never do that once anyone has started screening
  started <- screening_started(screen.file, users)
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
  a <- make_pair_assignment(nrow(refs), sort(users, method = "radix"))   # radix: same order on every computer
  a$Title <- refs$Title[a$row]
  a <- a[, c("row", "Title", "Screener1", "Screener2")]
  rownames(a) <- NULL
  utils::write.csv(a, path, row.names = FALSE)
  per <- table(factor(c(a$Screener1, a$Screener2), levels = users))
  cat("\nDouble screening: each paper assigned to two screeners, saved in", basename(path), "\n")
  cat(paste0("  ", names(per), ": ", as.integer(per), " papers", collapse = "\n"), "\n")
  a
}
