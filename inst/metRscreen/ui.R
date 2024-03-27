# UI function

#UI##########
ui <- function(){
  shinymaterial::material_page(
  font_color = "green",
  primary_theme_color = "darkgreen",
  secondary_theme_color = "darkgreen",
  shinyjs::useShinyjs(),
  title = shiny::tags$img(src="logo/metRscreen.png", height = 85),
  br(),
  shinyWidgets::actionBttn(
    inputId = "help",
    label = "Help",
    style = "minimal",
    color = "royal"),
  # Sidebar
  shinymaterial::material_row(
    shinymaterial::material_column(
      width = 4,
      shinymaterial::material_card(
        depth = 2,
        shinyWidgets::awesomeCheckbox(
          inputId = "hide_name",
          label = "Hide author names and journal?",
          value = F,
          status = "info"
        ),
        shinymaterial::material_file_input("Ref", label = "Import a .csv"),
        shiny::tags$h6(shiny::htmlOutput("progress"))),
      shinymaterial::material_card(
        depth = 2,
        shinymaterial::material_text_box("search1", "Green Keyword:", value = "", color = "green"),
        shinymaterial::material_text_box("search2","Red Keyword:", value =  "", color = "red"),
        shinymaterial::material_text_box("search3", "Purple Keyword:", value =  "", color = "purple"),
        shinymaterial::material_text_box("search4", "Orange Keyword:", value =  "", color = "orange"),
        shinymaterial::material_text_box("search5", "Blue Keyword:", value =  "", color = "blue"),
      ),
      shinymaterial::material_card(
        width = 2,
        depth = 2,
        shinyWidgets::actionBttn(
          inputId = "Next",
          label = "Next Study",
          style = "bordered",
          color = "success",
          block = T,
          size = "sm"),
        shinyWidgets::actionBttn(
          inputId = "Previous",
          label = "Previous Study",
          style = "bordered",
          color = "royal",
          block = T,
          size = "sm")),
      shiny::textOutput("info"),
      shiny::textInput("Study", "study", value = 1)
    ),

    #main panel
    shinymaterial::material_column(
      width = 8,
      shinymaterial::material_card(
        depth = 2,
        shiny::splitLayout(
          shinyWidgets::actionBttn(
            inputId = "Accept",
            label = "Accept",
            style = "pill",
            color = "success",
            block = T,
            size = "sm"),
          shinyWidgets::actionBttn(
            inputId = "NoDecision",
            label = "No Decision",
            style = "pill",
            color = "primary",
            block = T,
            size = "sm"),
          shinyWidgets::actionBttn(
            inputId = "Reject",
            label = "Reject",
            style = "pill",
            color = "warning",
            block = T,
            size = "sm"))),
      shinymaterial::material_card(
        depth = 2,
        shiny::tags$h6(shiny::htmlOutput("overview")),
        shiny::br(),
        shiny::tags$h6(shiny::br(), shiny::htmlOutput("abstract")),
        shiny::br(),
        shiny::tags$h6(shiny::br(), shiny::htmlOutput("keyword")))
    )
  )
)
}
