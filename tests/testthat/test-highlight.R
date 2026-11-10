source(
  system.file("metRscreen", "utilities", "highlight_function.R", package = "metRscreen"),
  local = TRUE
)

# Which words got highlighted, in order.
hits <- function(text, search) {
  html <- highlight_text(text, search)
  m <- regmatches(html, gregexpr("<b>[^<]*</b>", html))[[1]]
  gsub("</?b>", "", m)
}

test_that("* matches zero or more letters", {
  expect_equal(
    hits("parent parental parenting parents", "parent*"),
    c("parent", "parental", "parenting", "parents")
  )
  expect_equal(hits("mice mouse", "m*e"), c("mice", "mouse"))
  expect_equal(hits("COVID19 covid", "covid*"), c("COVID19", "covid"))
})

test_that("* does not cross hyphens or spaces", {
  expect_equal(hits("self-report selfreport", "self*"), c("self", "selfreport"))
})

test_that("? matches exactly one letter", {
  expect_equal(hits("woman women womens", "wom?n"), c("woman", "women"))
  expect_equal(hits("colour color", "colo?r"), "colour")
})

test_that("adjacent matches are all highlighted", {
  expect_equal(hits("parents parents parents", "parent*"), rep("parents", 3))
})

test_that("terms can be separated with or without spaces", {
  expect_equal(hits("mother, father", "mother,father"), c("mother", "father"))
  expect_equal(hits("mother, father", "mother , father"), c("mother", "father"))
  expect_equal(hits("mother", "mother,,"), "mother")
})

test_that("keywords never match inside added HTML", {
  expect_equal(
    highlight_text("the style", "the, style"),
    paste0(
      "<div><span style=\"color:#52D017;\"><b>the</b></span> ",
      "<span style=\"color:#52D017;\"><b>style</b></span></div>"
    )
  )
  expect_equal(hits("a red span", list("red", "span", "color", "b")), c("red", "span"))
})

test_that("regex characters are matched literally", {
  expect_equal(hits("C++ is fun (x) y", "C++, (x)"), c("C++", "(x)"))
  expect_equal(hits("a.c abc", "a.c"), "a.c")
})

test_that("accented letters match", {
  expect_equal(hits("café cafés CAFÉ naïve", "caf*, na?ve"), c("café", "cafés", "CAFÉ", "naïve"))
})

test_that("matching ignores case and needs whole words", {
  expect_equal(hits("PARENTS grandparents", "parent*"), "PARENTS")
})

test_that("the first keyword box wins when matches overlap", {
  html <- highlight_text("parental", list("parent*", "parental"))
  expect_match(html, "#52D017")
  expect_false(grepl("#DC143C", html))
})

test_that("wildcard-only and empty terms are ignored", {
  expect_equal(highlight_text("anything", "*, ??"), "<div>anything</div>")
  expect_equal(highlight_text("plain", list(NULL, "", NA)), "<div>plain</div>")
})

test_that("multi-word keywords still work", {
  expect_equal(
    hits("Mental health in a meta-analysis", "mental health, meta-analys*"),
    c("Mental health", "meta-analysis")
  )
})
