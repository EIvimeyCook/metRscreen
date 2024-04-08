# server function
server <- function(input, output, session) {
  # reactive objects########

  # settings object
  settings.store <- shiny::reactiveValues(
    reject.list = NULL,
    counter = NULL,
    datapath = NULL,
    new.data = NULL,
    inputs = NULL
  )

  # create a counter for the total
  countertot <- shiny::reactiveValues(total = 1)

  # create a counter for the total
  import <- shiny::reactiveValues(check = FALSE)

  # create a new dataframe based on the old data in a reactive ovject
  original <- shiny::reactiveValues(newData = NULL)

  # counter to move studies and subset, counter values change depedning on next/previous
  counter <- shiny::reactiveValues(countervalue = 0)

  # help tips when help is pressed######
  shiny::observeEvent(input$help, {
    shinyalert::shinyalert("Tips",
      "1. In order to use a .csv file, export references from Zotero. <br>
                          <br>
                           2. To highlight multiple words, separate each string with a comma and no space. <br>
                           <br>
                           3. To use a wildcard search, enter the * character after the appropriate string. <br>
                          <br>
                          4. If you want to see author/title/journal/year info, select each component from the checkbox. These are blinded by default",
      type = "info",
      html = T,
      confirmButtonText = "OK"
    )
  })

  # cite me action button#######
  shiny::observeEvent(input$citeme, {
    shinyalert::shinyalert(
      title = "metRscreen",
      text = paste(shiny::tags$h5("Made by Ed Ivimey-Cook and Joel Pick")),
      size = "l",
      closeOnClickOutside = FALSE,
      html = TRUE,
      type = "",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      animation = TRUE,
      imageUrl = "logo/metRscreen.png",
      imageHeight = "88",
      imageWidth = "80"
    )
  })

  # input dataframe#######
  datasetInput <- shiny::reactive({
    shinyFiles::shinyFileChoose(input, "ref",
      roots = shinyFiles::getVolumes(), session = session,
      filetype = "csv"
    )
    if (length(shinyFiles::parseFilePaths(shinyFiles::getVolumes(), input$ref)$datapath) > 0) {
      settings.store$datapath <- shinyFiles::parseFilePaths(shinyFiles::getVolumes(), input$ref)$datapath
      read.csv(shinyFiles::parseFilePaths(shinyFiles::getVolumes(), input$ref)$datapath)
    }
  })

  # counter total######
  shiny::observeEvent(input$ref, {
    countertot$total <- nrow(datasetInput())
  })


  # save files########
  shiny::observe({
    if (length(metRDS > 0) & import$check == "FALSE") {
      settings.store <<- do.call("reactiveValues", readRDS(metRDS))
      countertot$total <- nrow(settings.store$new.data)
      reject.list <<- settings.store$reject.list
      counter$countervalue <- settings.store$counter
      shinyWidgets::updateCheckboxGroupButtons(
        session = session,
        inputId = "show_fields",
        label = character(0),
        choices = c("Title", "Author", "Year", "Journal"),
        status = "primary",
        selected = settings.store$inputs,
        justified = "TRUE",
        checkIcon = list(
          yes = shiny::icon("square-check"),
          no = shiny::icon("square")
        ),
        size = "sm"
      )
      import$check <- TRUE
    }
  })

  # update radiogroup with improted reasons####
  shiny::observe({
    if (length(reject.list > 0)) {
      shinyWidgets::updatePrettyRadioButtons(
        session = session,
        inputId = "reject.reason",
        choices = c(reject.list),
        selected = character(0),
        inline = TRUE,
        prettyOptions = list(
          icon = icon("check"),
          bigger = TRUE,
          status = "info",
          animation = "jelly"
        )
      )
      settings.store$reject.list <- reject.list
    }
  })

  # progress displayed based on counter and percentage #######
  output$progress <- shiny::renderText({
    percent <- round(counter$countervalue / countertot$total * 100, 0)
    paste0("<font color=\"#ff3333\"><b>", percent, "%", " ", "screened", "</b></font> <font color=\"#ff3333\"><b>", "(Paper No = ", counter$countervalue, "</b>) </font>")
  })

  # progress showing######
  shiny::observe({
    if (isTruthy(input$ref) | (length(metRDS > 0))) {
      shinyjs::show("progress")
      shinyjs::show("reject.reason")
    }
  })

  # set boundaries for the counter based on total and reaching zero
  shiny::observeEvent(counter$countervalue, {
    if (counter$countervalue == 0) {
      counter$countervalue <- counter$countervalue + 1

      if (counter$countervalue > countertot$total) {
        counter$countervalue <- countertot$total
      }
    }
  })


  # the dataset is then subsetted to represent the counter #######
  StudyData <- shiny::reactive({
    if (isTruthy(input$ref)) {
      datasetInput()[counter$countervalue, ]
    } else if (length(metRDS > 0)) {
      settings.store$new.data[counter$countervalue, ]
    }
  })

  # render abstract text highlighted based on search########
  output$abstract <- shiny::renderText({
    highlight_text(as.character(StudyData()$Abstract), search = list(input$search1, input$search2, input$search3, input$search4, input$search5))
  })

  # render keyword text highlighted based on search######
  output$keyword <- shiny::renderText({
    highlight_text(as.character(StudyData()$Manual.Tags), search = list(input$search1, input$search2, input$search3, input$search4, input$search5))
  })

  # outputs for each section #######
  output$title <- shiny::renderUI({
    shiny::HTML(paste("<b>Title:</b>", as.character(StudyData()$Title)))
  })

  output$author <- shiny::renderUI({
    shiny::HTML(paste("<b>Author:</b>", as.character(StudyData()$Author)))
  })

  output$year <- shiny::renderUI({
    shiny::HTML(paste("<b>Year:</b>", as.character(StudyData()$Publication.Year)))
  })

  output$journal <- shiny::renderUI({
    shiny::HTML(paste("<b>Journal:</b>", as.character(StudyData()$Publication.Title)))
  })

  shiny::observeEvent(input$show_fields,
    {
      settings.store$inputs <- input$show_fields
      if ("Journal" %in% input$show_fields) {
        shinyjs::show("journal")
      } else {
        shinyjs::hide("journal")
      }
      if ("Year" %in% input$show_fields) {
        shinyjs::show("year")
      } else {
        shinyjs::hide("year")
      }
      if ("Author" %in% input$show_fields) {
        shinyjs::show("author")
      } else {
        shinyjs::hide("author")
      }
      if ("Title" %in% input$show_fields) {
        shinyjs::show("title")
      } else {
        shinyjs::hide("title")
      }
    },
    ignoreNULL = FALSE
  )

  # create a newversion of the data frame######
  shiny::observeEvent(input$ref, {
    original$newData <- cbind(datasetInput(),
      Screen = "To be screened",
      Reason = "No reason given"
    )
  })

  # change and save with accept/reject and nodecision######
  shiny::observeEvent(input$Accept, {
    if (isTruthy(input$ref)) {
      original$newData[counter$countervalue, ]$Screen <- "Accept"

      counter$countervalue <- counter$countervalue + 1
      settings.store$counter <- counter$countervalue

      shinyWidgets::updatePrettyRadioButtons(
        session = session,
        inputId = "reject.reason",
        choices = c(reject.list),
        selected = character(0),
        inline = TRUE,
        prettyOptions = list(
          icon = icon("check"),
          bigger = TRUE,
          status = "info",
          animation = "jelly"
        )
      )
      settings.store$new.data <- original$newData
      write.csv(as.data.frame(shiny::reactiveValuesToList(original)),
        file = paste(shinyFiles::parseFilePaths(shinyFiles::getVolumes(), input$ref)$datapath, "Screened.csv")
      )

      print(shiny::reactiveValuesToList(settings.store))
      saveRDS(shiny::reactiveValuesToList(settings.store),
        file = paste(shinyFiles::parseFilePaths(shinyFiles::getVolumes(), input$ref)$datapath, "settings.rds")
      )
    } else if (length(metRDS > 0)) {
      settings.store$new.data[counter$countervalue, ]$Screen <- "Accept"
      counter$countervalue <- counter$countervalue + 1
      settings.store$counter <- counter$countervalue

      shinyWidgets::updatePrettyRadioButtons(
        session = session,
        inputId = "reject.reason",
        choices = c(reject.list),
        selected = character(0),
        inline = TRUE,
        prettyOptions = list(
          icon = icon("check"),
          bigger = TRUE,
          status = "info",
          animation = "jelly"
        )
      )
      original$newData <- settings.store$new.data
      write.csv(as.data.frame(shiny::reactiveValuesToList(original)),
        file = paste(settings.store$datapath, "Screened.csv")
      )
      saveRDS(shiny::reactiveValuesToList(settings.store),
        file = paste(settings.store$datapath, "settings.rds")
      )
    }
  })


  shiny::observeEvent(input$Reject, {
    if (isTruthy(input$ref)) {
      original$newData[counter$countervalue, ]$Screen <- "Reject"

      counter$countervalue <- counter$countervalue + 1
      settings.store$counter <- counter$countervalue

      shinyWidgets::updatePrettyRadioButtons(
        session = session,
        inputId = "reject.reason",
        choices = c(reject.list),
        selected = character(0),
        inline = TRUE,
        prettyOptions = list(
          icon = icon("check"),
          bigger = TRUE,
          status = "info",
          animation = "jelly"
        )
      )
      settings.store$new.data <- original$newData
      write.csv(as.data.frame(shiny::reactiveValuesToList(original)),
        file = paste(shinyFiles::parseFilePaths(shinyFiles::getVolumes(), input$ref)$datapath, "Screened.csv")
      )

      print(shiny::reactiveValuesToList(settings.store))
      saveRDS(shiny::reactiveValuesToList(settings.store),
        file = paste(shinyFiles::parseFilePaths(shinyFiles::getVolumes(), input$ref)$datapath, "settings.rds")
      )
    } else if (length(metRDS > 0)) {
      settings.store$new.data[counter$countervalue, ]$Screen <- "Reject"
      counter$countervalue <- counter$countervalue + 1
      settings.store$counter <- counter$countervalue

      shinyWidgets::updatePrettyRadioButtons(
        session = session,
        inputId = "reject.reason",
        choices = c(reject.list),
        selected = character(0),
        inline = TRUE,
        prettyOptions = list(
          icon = icon("check"),
          bigger = TRUE,
          status = "info",
          animation = "jelly"
        )
      )
      original$newData <- settings.store$new.data
      write.csv(as.data.frame(shiny::reactiveValuesToList(original)),
        file = paste(settings.store$datapath, "Screened.csv")
      )
      saveRDS(shiny::reactiveValuesToList(settings.store),
        file = paste(settings.store$datapath, "settings.rds")
      )
    }
  })

  shiny::observeEvent(input$NoDecision, {
    if (isTruthy(input$ref)) {
      original$newData[counter$countervalue, ]$Screen <- "No Decision"

      counter$countervalue <- counter$countervalue + 1
      settings.store$counter <- counter$countervalue

      shinyWidgets::updatePrettyRadioButtons(
        session = session,
        inputId = "reject.reason",
        choices = c(reject.list),
        selected = character(0),
        inline = TRUE,
        prettyOptions = list(
          icon = icon("check"),
          bigger = TRUE,
          status = "info",
          animation = "jelly"
        )
      )
      settings.store$new.data <- original$newData
      write.csv(as.data.frame(shiny::reactiveValuesToList(original)),
        file = paste(shinyFiles::parseFilePaths(shinyFiles::getVolumes(), input$ref)$datapath, "Screened.csv")
      )

      print(shiny::reactiveValuesToList(settings.store))
      saveRDS(shiny::reactiveValuesToList(settings.store),
        file = paste(shinyFiles::parseFilePaths(shinyFiles::getVolumes(), input$ref)$datapath, "settings.rds")
      )
    } else if (length(metRDS > 0)) {
      settings.store$new.data[counter$countervalue, ]$Screen <- "No Decision"
      counter$countervalue <- counter$countervalue + 1
      settings.store$counter <- counter$countervalue

      shinyWidgets::updatePrettyRadioButtons(
        session = session,
        inputId = "reject.reason",
        choices = c(reject.list),
        selected = character(0),
        inline = TRUE,
        prettyOptions = list(
          icon = icon("check"),
          bigger = TRUE,
          status = "info",
          animation = "jelly"
        )
      )
      original$newData <- settings.store$new.data
      write.csv(as.data.frame(shiny::reactiveValuesToList(original)),
        file = paste(settings.store$datapath, "Screened.csv")
      )
      saveRDS(shiny::reactiveValuesToList(settings.store),
        file = paste(settings.store$datapath, "settings.rds")
      )
    }
  })
}
