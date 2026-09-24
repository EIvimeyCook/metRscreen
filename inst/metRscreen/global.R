#load internal functions
source("./utilities/highlight_function.R", local = TRUE)
source("./utilities/collab_functions.R", local = TRUE)

# blank keyword box when no keyword is supplied (base R version of `%||%`,
# which only exists in base R from 4.4.0)
or_blank <- function(x) if (is.null(x)) "" else x
