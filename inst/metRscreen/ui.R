# UI##########
ui <- function() {
  bslib::page_fluid(
    shinyjs::useShinyjs(),
    shiny::tags$script(shiny::HTML("
  $(document).on('keydown', function(e) {
    var tag = document.activeElement.tagName.toLowerCase();
    if (tag === 'input' || tag === 'textarea') return;
    if (e.key === 'y') $('#Accept').click();
    if (e.key === 'm') $('#NoDecision').click();
    if (e.key === 'n') $('#Reject').click();
  });
")),
    shiny::tags$script(shiny::HTML("
  $(document).on('shiny:inputchanged', function(e) {
    if (e.name === 'font_size') {
      $('#title, #author, #year, #journal, #abstract, #keyword').css('font-size', e.value + 'px');
    }
  });
")),

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
            shiny::sliderInput(
              inputId = "font_size",
              label = "Font size",
              min = 10,
              max = 28,
              value = 14,
              step = 1,
              post = "px",
              ticks = FALSE
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
      shiny::tags$style(shiny::HTML("
  #decision_buttons .btn-group { width: 100%; }
  #decision_buttons .btn-group .btn { flex: 1; }
")),
      shiny::fluidRow(
        shiny::column(
          width = 9,
          bslib::card(
            max_height = 500,
            full_screen = TRUE,
            style = "overflow-y: auto;",
            shinyjs::hidden(shiny::htmlOutput("title")),
            shinyjs::hidden(shiny::htmlOutput("author")),
            shinyjs::hidden(shiny::htmlOutput("year")),
            shinyjs::hidden(shiny::htmlOutput("journal")),
            shiny::htmlOutput("abstract"),
            shiny::htmlOutput("keyword")
          ),
          shiny::div(
            id = "decision_buttons",
          shinyWidgets::actionGroupButtons(
            inputIds = c("Reject", "NoDecision", "Accept"),
            labels = list("Reject", "No Decision", "Accept"),
            status = c("danger", "primary", "success"),
            size = "normal"
          )
          ),
          shiny::div(
            style = "margin-top: 10px;",
            shinyjs::hidden(
              shinyWidgets::prettyCheckboxGroup(
                inputId = "reject.reason",
                label = NULL,
                choices = ""
              )
            ),
            shinyjs::hidden(
              shinyWidgets::textInputIcon(
                "comments",
                placeholder = "Screening comments",
                label = NULL
              )
            )
          )
        ),
        shiny::column(
          width = 3,
          bslib::card(
            fill = FALSE,
            bslib::card_body(
              max_height = 500,
              style = "overflow-y: auto;",
              shiny::uiOutput("ref_list")
            )
          )
        )
      )
    )
  )
}
