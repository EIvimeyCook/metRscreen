#' @keywords internal
"_PACKAGE"

# The app itself lives in inst/metRscreen and calls these packages with pkg::fun.
# R CMD check only looks at the R/ folder, so one function from each package is
# imported here to record that the app needs them.
#' @importFrom bslib page_fluid
#' @importFrom shinyalert shinyalert
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyWidgets actionBttn
#' @importFrom tippy tippy_this
NULL
