# server function
server <- function(input, output, session) {
  # reactive objects########

  # settings object
  settings.store <- shiny::reactiveValues(
    reject.list = NULL,
    counter = NULL,
    datapath = NULL,
    new.data = NULL,
    inputs = NULL,
    search1 = NULL,
    search2 = NULL,
    search3 = NULL,
    search4 = NULL,
    search5 = NULL
  )

  # create a counter for the total
  countertot <- shiny::reactiveValues(total = 1)

  # create a counter for the total
  import <- shiny::reactiveValues(check = FALSE)

  # create a new dataframe based on the old data in a reactive ovject
  original <- shiny::reactiveValues(new.data = NULL)

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
    if(is.null(screen.history)) {
      cat("\nReading in new screening file\n")
    return(read.csv(screen.file))
    } else if(!is.null(screen.history)) {
      settings.store <<- do.call("reactiveValues", readRDS(screen.history))
      cat("\nReading in saved screening file\n")
      return(settings.store$new.data)
    }

  })

  # counter total######
  shiny::observe({
    countertot$total <- nrow(datasetInput())
  })

  # change the study with next and previous#######
  shiny::observeEvent(input$Next, {
    counter$countervalue <- counter$countervalue + 1
  })

  shiny::observeEvent(input$Previous, {
    counter$countervalue <- counter$countervalue - 1
  })

  # set boundaries for the counter based on total and reaching zero#######
  shiny::observeEvent(counter$countervalue, {
    if (counter$countervalue == 0) {
      shinyjs::disable("Previous")
      counter$countervalue <- counter$countervalue + 1
    } else {
      shinyjs::enable("Previous")
    }

    if (counter$countervalue == countertot$total) {
      shinyjs::disable("Next")
      counter$countervalue <- countertot$total
    } else {
      shinyjs::enable("Next")
    }
  })

  # save files loading ########

  shiny::observe({
    if (!is.null(screen.history) & import$check == "FALSE") {
      shinyjs::show("previous.decisions")
      # update params
      reject.list <<- settings.store$reject.list
      counter$countervalue <- settings.store$counter

      shinyWidgets::updateCheckboxGroupButtons(
        session = session,
        inputId = "show.fields",
        selected = settings.store$inputs
      )
      import$check <- TRUE

    if(!is.null(settings.store$search1)){
      shinyWidgets::updateTextInputIcon(
        session = session,
        inputId = "search1",
        value = settings.store$search1
      )
    }

    if(!is.null(settings.store$search2)){
      shinyWidgets::updateTextInputIcon(
        session = session,
        inputId = "search2",
        value = settings.store$search2
      )
    }

    if(!is.null(settings.store$search3)){
      shinyWidgets::updateTextInputIcon(
        session = session,
        inputId = "search3",
        value = settings.store$search3
      )
    }

    if(!is.null(settings.store$search4)){
      shinyWidgets::updateTextInputIcon(
        session = session,
        inputId = "search4",
        value = settings.store$search4
      )
    }

    if(!is.null(settings.store$search5)){
      shinyWidgets::updateTextInputIcon(
        session = session,
        inputId = "search5",
        value = settings.store$search5
      )
    }
}

  })

  # create a newversion of the data frame######
  shiny::observe({
    if(is.null(screen.history)){
    original$new.data <- cbind(
      datasetInput(),
      Screen = "To be screened",
      Reason = "No reason given"
    )
    cat("\nCreating new screening output\n")
    } else {
      original$new.data <- datasetInput()
      cat("\nUsing existing screening output\n")
    }
  })


  # update radiogroup with imported reasons####
  shiny::observe({
    if (length(reject.list > 0)) {
      shinyjs::show("reject.reason")
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

  # progress showing######
      shinyjs::show("progress")

  # the dataset is then subsetted to represent the counter #######
  StudyData <- shiny::reactive({
    return(datasetInput()[counter$countervalue, ])
  })


  # render abstract text highlighted based on search########
  output$abstract <- shiny::renderText({
    highlight_text(as.character(StudyData()$Abstract),
                   search = list(input$search1, input$search2, input$search3, input$search4, input$search5))
  })

  # render keyword text highlighted based on search######
  output$keyword <- shiny::renderText({
    highlight_text(as.character(StudyData()$Manual.Tags),
                   search = list(input$search1, input$search2, input$search3, input$search4, input$search5))
  })

  # outputs for each section #######
  output$title <- shiny::renderUI({
    shiny::HTML(paste("<b>Title:</b>", as.character(StudyData()$Title)))
  })

  output$hist.reason <- shiny::renderUI({
    if(StudyData()$Screen == "Reject"){
      shiny::HTML(paste("<b>Reject Reason:</b>", as.character(StudyData()$Reason)))
    } else {
      shiny::HTML("")
    }
  })

  output$hist.screen <- shiny::renderUI({
    shiny::HTML(paste("<b>Screen:</b>", as.character(StudyData()$Screen)))
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

  # input strings are saved#######
  shiny::observeEvent(input$search1, {
  settings.store$search1 <- input$search1
    })

  shiny::observeEvent(input$search2, {
    settings.store$search2 <- input$search2
  })

  shiny::observeEvent(input$search3, {
    settings.store$search3 <- input$search3
  })

  shiny::observeEvent(input$search4, {
    settings.store$search4 <- input$search4
  })

  shiny::observeEvent(input$search5, {
    settings.store$search5 <- input$search5
  })

  # hide or show fields depedning on input#####
  shiny::observeEvent(input$show.fields, {

       if ("Journal" %in% input$show.fields) {
        shinyjs::show("journal")
      } else {
        shinyjs::hide("journal")
      }

      if ("Year" %in% input$show.fields) {
        shinyjs::show("year")
      } else {
        shinyjs::hide("year")
      }

      if ("Author" %in% input$show.fields) {
        shinyjs::show("author")
      } else {
        shinyjs::hide("author")
      }

      if ("Title" %in% input$show.fields) {
        shinyjs::show("title")
      } else {
        shinyjs::hide("title")
      }
    settings.store$inputs <<- input$show.fields
    },
    ignoreNULL = FALSE
  )

  # change and save with accept/reject and nodecision######

  #accept
  shiny::observeEvent(input$Accept, {

      original$new.data[counter$countervalue, ]$Screen <- "Accept"
      counter$countervalue <- counter$countervalue + 1

      #save data from orginial
      screen.dat <- as.data.frame(shiny::reactiveValuesToList(original)) |>
        dplyr::rename_all(~ gsub("new.data.", "", .))

      write.csv(screen.dat, file = paste0(screen.file, "_Screened.csv"), row.names = FALSE)

      #update buttons on press
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

      #storing data fro later
      settings.store$counter <- counter$countervalue
      settings.store$new.data <- original$new.data
      saveRDS(shiny::reactiveValuesToList(settings.store),
        file = paste0(screen.file, "_history.rds")
      )
  })


  # reject
  shiny::observeEvent(input$Reject, {
      original$new.data[counter$countervalue, ]$Screen <- "Reject"
      if (length(input$reject.reason > 0)) {
        original$new.data[counter$countervalue, ]$Reason <- input$reject.reason
      }

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
      settings.store$new.data <- original$new.data

      screen.dat <- as.data.frame(shiny::reactiveValuesToList(original)) |>
        dplyr::rename_all(~ gsub("new.data.", "", .))
      write.csv(screen.dat, file = paste0(screen.file, "_Screened.csv"), row.names = FALSE)

      saveRDS(shiny::reactiveValuesToList(settings.store),
        file = paste0(screen.file, "_history.rds")
      )
  })

 # no decision
  shiny::observeEvent(input$NoDecision, {
      original$new.data[counter$countervalue, ]$Screen <- "No Decision"
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
      settings.store$new.data <- original$new.data

      screen.dat <- as.data.frame(shiny::reactiveValuesToList(original)) |>
        dplyr::rename_all(~ gsub("new.data.", "", .))
      write.csv(screen.dat, file = paste0(screen.file, "_Screened.csv"), row.names = FALSE)

      saveRDS(shiny::reactiveValuesToList(settings.store),
        file = paste0(screen.file, "_history.rds")
      )
  })

  # progress displayed based on counter and percentage #######
  output$progress <- shiny::renderText({
    percent <- round(sum(original$new.data$Screen != "To be screened") / countertot$total * 100, 0)
    paste0("<font color=\"#ff3333\"><b>", percent, "%", " ", "screened", "</b></font> <font color=\"#ff3333\"><b>", "(Paper No = ", counter$countervalue, "</b>) </font>")
  })

  # app stop on session end######
  session$onSessionEnded(function() {
    stopApp()
  })
}
