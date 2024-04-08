# UI##########
ui <- function() {
  bslib::page_fluid(
    shinyjs::useShinyjs(),
    title = "metRscreen",
    shiny::actionButton(
      inputId = "citeme",
      style = "color: white; background-color: white; border-color: white; box-shadow: 0px 0px 0px 0px white;",
      label = shiny::tags$img(src = "logo/metRscreen.png", height = "78px", width = "70px")
    ),
    tippy::tippy_this(
      "citeme",
      "Click me!"
    ),
    shinyWidgets::actionBttn(
      inputId = "help",
      label = "Help",
      style = "fill",
      color = "success"
    ),
    bslib::layout_sidebar(
      sidebar =
        bslib::sidebar(
          width = 400,
          position = "left",
          open = "open",
          bslib::card(
            shinyFiles::shinyFilesButton("ref", "Import a .csv",
              title = NULL,
              multiple = FALSE,
              filetype = ".csv"
            ),
            shinyjs::hidden(
              shiny::htmlOutput("progress")
            )
          ),
          bslib::card(
            shinyWidgets::checkboxGroupButtons(
              inputId = "show_fields",
              label = character(0),
              choices = c("Title", "Author", "Year", "Journal"),
              status = "primary",
              selected = character(0),
              justified = "TRUE",
              checkIcon = list(
                yes = shiny::icon("square-check"),
                no = shiny::icon("square")
              ),
              size = "sm",
              direction = "horizontal"
            ),
            shinyWidgets::textInputIcon("search1", "Green Keyword:", placeholder = NULL),
            shinyWidgets::textInputIcon("search2", "Red Keyword:", placeholder = NULL),
            shinyWidgets::textInputIcon("search3", "Purple Keyword:", placeholder = NULL),
            shinyWidgets::textInputIcon("search4", "Orange Keyword:", placeholder = NULL),
            shinyWidgets::textInputIcon("search5", "Blue Keyword:", placeholder = NULL),
          )
        ),
      bslib::card(
        max_height = 500,
        full_screen = TRUE,
        shinyjs::hidden(
          shiny::htmlOutput("title")
        ),
        shinyjs::hidden(
          shiny::htmlOutput("author")
        ),
        shinyjs::hidden(
          shiny::htmlOutput("year")
        ),
        shinyjs::hidden(
          shiny::htmlOutput("journal")
        ),
        shiny::htmlOutput("abstract"),
        shiny::htmlOutput("keyword")
      ),
      shiny::splitLayout(
        shinyWidgets::actionBttn(
          inputId = "Reject",
          label = "Reject",
          style = "fill",
          color = "warning",
          block = T,
          size = "sm"
        ),
        shinyWidgets::actionBttn(
          inputId = "NoDecision",
          label = "No Decision",
          style = "fill",
          color = "primary",
          block = T,
          size = "sm"
        ),
        shinyWidgets::actionBttn(
          inputId = "Accept",
          label = "Accept",
          style = "fill",
          color = "success",
          block = T,
          size = "sm"
        )
      ),
      shinyjs::hidden(
        shinyWidgets::prettyRadioButtons(
        inputId = "reject.reason",
        label = NULL,
        choices = "",
      ))
    )
  )
}
