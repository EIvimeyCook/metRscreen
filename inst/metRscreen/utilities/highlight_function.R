#' @title highlight_text
#' @description Highlights keywords in text, one colour per keyword box.
#'   Each box can hold several comma-separated terms. Matching ignores case and
#'   only matches whole words. Two wildcards are supported:
#'   `*` matches zero or more letters or digits (`parent*` matches parent,
#'   parents, parental) and `?` matches exactly one (`wom?n` matches woman and
#'   women). Every other character is matched literally. When matches overlap,
#'   the earlier keyword box wins.
#' @param text text for searching
#' @param search list of search strings, one per colour

highlight_text <- function(text, search) {
  colours <- c("#52D017", "#DC143C", "#AF33FF", "#FFB133", "#334DFF")

  patterns <- list()
  for (j in seq_along(search)) {
    for (term in split_terms(search[[j]])) {
      pattern <- term_to_regex(term)
      if (!is.null(pattern)) {
        patterns[[length(patterns) + 1]] <- list(regex = pattern, colour = colours[j])
      }
    }
  }

  text <- vapply(as.character(text), highlight_one, character(1),
    patterns = patterns, USE.NAMES = FALSE
  )
  paste0("<div>", text, "</div>")
}

# Split one keyword box into its terms: commas, with or without spaces.
split_terms <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) {
    return(character(0))
  }
  terms <- trimws(strsplit(as.character(x[1]), ",", fixed = TRUE)[[1]])
  terms[nzchar(terms)]
}

# Turn a term into a whole-word, case-insensitive PCRE pattern. Returns NULL
# for a term made only of wildcards, which would otherwise match everywhere.
term_to_regex <- function(term) {
  chars <- strsplit(enc2utf8(term), "")[[1]]
  if (all(chars %in% c("*", "?"))) {
    return(NULL)
  }
  word_char <- "[\\p{L}\\p{N}]"
  body <- vapply(chars, function(ch) {
    if (ch == "*") {
      paste0(word_char, "*")
    } else if (ch == "?") {
      word_char
    } else if (grepl("[][{}()+*^$|\\\\?.]", ch)) {
      paste0("\\", ch)
    } else {
      ch
    }
  }, character(1))
  paste0("(*UCP)(?i)(?<!\\w)(?:", paste(body, collapse = ""), ")(?!\\w)")
}

# Find every match on the original text, keep the first claim on any stretch
# of characters, then add the spans in one pass so later keywords never match
# inside HTML that earlier keywords added.
highlight_one <- function(text, patterns) {
  if (is.na(text) || length(patterns) == 0) {
    return(text)
  }
  text <- enc2utf8(text)

  starts <- integer(0)
  ends <- integer(0)
  cols <- character(0)
  for (p in patterns) {
    m <- gregexpr(p$regex, text, perl = TRUE)[[1]]
    if (m[1] == -1) next
    lens <- attr(m, "match.length")
    for (k in seq_along(m)) {
      if (lens[k] <= 0) next
      s <- m[k]
      e <- m[k] + lens[k] - 1
      if (any(s <= ends & e >= starts)) next
      starts <- c(starts, s)
      ends <- c(ends, e)
      cols <- c(cols, p$colour)
    }
  }
  if (length(starts) == 0) {
    return(text)
  }

  ord <- order(starts)
  out <- character(0)
  pos <- 1
  for (k in ord) {
    out <- c(
      out,
      substr(text, pos, starts[k] - 1),
      "<span style=\"color:", cols[k], ";\"><b>",
      substr(text, starts[k], ends[k]),
      "</b></span>"
    )
    pos <- ends[k] + 1
  }
  out <- c(out, substr(text, pos, nchar(text)))
  paste(out, collapse = "")
}
