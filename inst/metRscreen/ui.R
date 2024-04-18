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
            shinyjs::hidden(
              shiny::tagList(
                shiny::div(
                  id = "previous.decisions",
                  shiny::tags$h5(shiny::HTML("<u><b>Previous screen decisions</b></u>")),
                  shiny::htmlOutput("hist.screen"),
                  shiny::htmlOutput("hist.reason"),
                  shiny::htmlOutput("screen.comment"),
                  shiny::htmlOutput("name.screener")
                )
              )
            ),
            shinyjs::hidden(
              shiny::htmlOutput("progress")
            )
          ),
          bslib::card(
            shinyjs::hidden(
              shiny::radioButtons(
                inputId = "choose.collab",
                label = shiny::tags$strong("Who is screening?"),
                choices = c(""),
                inline = TRUE
              )
            ),
            shinyWidgets::checkboxGroupButtons(
              inputId = "show.fields",
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
          ),
          bslib::card(
            shiny::splitLayout(
              shinyWidgets::actionBttn(
                inputId = "Previous",
                label = "Previous Study",
                style = "fill",
                color = "default",
                block = T,
                size = "sm"
              ),
              shinyWidgets::actionBttn(
                inputId = "Next",
                label = "Next Study",
                style = "fill",
                color = "default",
                block = T,
                size = "sm"
              )
            )
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
      shinyWidgets::actionGroupButtons(
        inputIds = c("Reject", "NoDecision", "Accept"),
        labels = list("Reject", "No Decision", "Accept"),
        status = c("danger", "primary", "success"),
        size = "normal"
      ),
      shinyjs::hidden(
        shinyWidgets::prettyRadioButtons(
          inputId = "reject.reason",
          label = NULL,
          choices = "",
        )
      ),
      shinyjs::hidden(
        shinyWidgets::textInputIcon("comments",
          placeholder = "Screening comments",
          label = NULL
        )
      )
    )
  )
}
