# server function
server <- function(input, output, session) {
  # reactive objects########

  # settings object
  settings.store <- shiny::reactiveValues(
    reject.vec = NULL,
    counter = NULL,
    datapath = NULL,
    new.data = NULL,
    inputs = NULL,
    search1 = NULL,
    search2 = NULL,
    search3 = NULL,
    search4 = NULL,
    search5 = NULL,
    screen.comments = NULL
  )

  # create a counter for the total
  countertot <- shiny::reactiveValues(total = 1)

  # counter to move studies and subset, counter values change depedning on next/previous
  counter <- shiny::reactiveValues(countervalue = 0)

  # create a counter for the total
  import <- shiny::reactiveValues(first.import = FALSE, first.load = TRUE)

  # create a new dataframe based on the old data in a reactive ovject
  original <- shiny::reactiveValues(new.data = NULL)

  # create a temp data file
  temp <- shiny::reactiveValues(import.data = NULL)


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

  # UI showing######
  shinyjs::show("progress")
  shinyjs::show("comments")
  shinyjs::show("previous.decisions")


  # input dataframe and create saving empty template#######
  shiny::observe({
    if (is.null(screen.history) & import$first.load == TRUE) {
      original$new.data <- cbind(
        read.csv(screen.file),
        Screen = "To be screened",
        Reason = "No reason given",
        Comment = "No comments given"
      )
      import$first.load <- FALSE
      cat("\nReading in new screening file and creating new screening output\n")
    } else if (!is.null(screen.history) & import$first.load == TRUE) {
      settings.store <<- do.call("reactiveValues", readRDS(screen.history))
      original$new.data <- settings.store$new.data
      cat("\nReading in saved screening file and using existing screening output\n")
      import$first.load <- FALSE
      import$first.import <- TRUE
    }

    # save a temp output
    screen.dat <- as.data.frame(shiny::reactiveValuesToList(original)) |>
      dplyr::rename_all(~ gsub("new.data.", "", .))
    write.csv(screen.dat, file = paste0(screen.file, "_Screened.csv"), row.names = FALSE)

    saveRDS(shiny::reactiveValuesToList(settings.store),
      file = paste0(screen.file, "_history.rds")
    )
  })

  # update radiogroup with imported reasons####
  shiny::observe({
    if (length(reject.vec > 0)) {
      shinyjs::show("reject.reason")

      shinyWidgets::updatePrettyRadioButtons(
        session = session,
        inputId = "reject.reason",
        choices = c(reject.vec),
        selected = character(0),
        inline = TRUE,
        prettyOptions = list(
          icon = icon("check"),
          bigger = TRUE,
          status = "info",
          animation = "jelly"
        )
      )

      settings.store$reject.vec <- reject.vec
    }
  })




  # save files loading ########
  shiny::observe({
    if (!is.null(screen.history) & import$first.import == "TRUE") {
      # update params
      if(!identical(reject.vec,settings.store$reject.vec) & !is.null(reject.vec)){
        reject.vec <<- reject.vec
      } else {
        reject.vec <<- settings.store$reject.vec
      }

      counter$countervalue <- settings.store$counter

      shinyWidgets::updateCheckboxGroupButtons(
        session = session,
        inputId = "show.fields",
        selected = settings.store$inputs
      )

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
      import$first.import <- FALSE
    }
  })

  # counter total######
  shiny::observe({
    countertot$total <- nrow(original$new.data)
  })

  # change the study with next and previous#######
  shiny::observeEvent(input$Next, {
    counter$countervalue <- counter$countervalue + 1
  })

  shiny::observeEvent(input$Previous, {
    counter$countervalue <- counter$countervalue - 1
  })

  # the dataset is then subsetted to represent the counter #######
  StudyData <- shiny::reactive({
    return(original$new.data[counter$countervalue, ])
  })


  # render abstract text highlighted based on search########
  output$abstract <- shiny::renderUI({
    shiny::HTML(highlight_text(as.character(StudyData()$Abstract),
                               search = list(input$search1,
                                             input$search2,
                                             input$search3,
                                             input$search4,
                                             input$search5)))
  })

  # render keyword text highlighted based on search######
  output$keyword <- shiny::renderUI({
    shiny::HTML(highlight_text(as.character(StudyData()$Manual.Tags),
                               search = list(input$search1,
                                             input$search2,
                                             input$search3,
                                             input$search4,
                                             input$search5)))
  })

  # outputs for each section #######
  output$title <- shiny::renderUI({
    shiny::HTML(paste("<p>",
                      "<b>Title:</b>",
                      as.character(StudyData()$Title),
                      "</p>"
    ))
  })

  output$hist.reason <- shiny::renderUI({
    if(StudyData()$Screen == "Reject"){
      shiny::HTML(paste(
        "<p>",
        "<b>Reject Reason:</b>",
        as.character(StudyData()$Reason),
        "</p>"
      ))
    } else {
      shiny::HTML("")
    }
  })

  output$hist.screen <- shiny::renderUI({
    shiny::HTML(paste("<p>",
                      "<b>Screen:</b>",
                      as.character(StudyData()$Screen),
                      "</p>"
    ))
  })

  output$screen.comment <- shiny::renderUI({
    if(nchar(StudyData()$Comment) == 0){
      shiny::HTML("")
    } else {
    shiny::HTML(paste("<p>",
                      "<b>Comment:</b>",
                      as.character(StudyData()$Comment),
                      "</p>"
    ))
                      }
  })

  output$author <- shiny::renderUI({
    shiny::HTML(paste("<p>",
                      "<b>Author:</b>",
                      as.character(StudyData()$Author),
                      "</p>"
    ))
  })

  output$year <- shiny::renderUI({
    shiny::HTML(paste("<p>",
                      "<b>Year:</b>",
                      as.character(StudyData()$Publication.Year),
                      "</p>"
    ))
  })

  output$journal <- shiny::renderUI({
    shiny::HTML(paste("<p>",
                      "<b>Journal:</b>",
                      as.character(StudyData()$Publication.Title),
                      "</p>"
    ))
  })
  # change and save with accept/reject and nodecision######

  #accept
  shiny::observeEvent(input$Accept, {

    original$new.data[counter$countervalue, ]$Screen <- "Accept"
    original$new.data[counter$countervalue, ]$Comment <- input$comments

    counter$countervalue <- counter$countervalue + 1
    
    if (counter$countervalue > countertot$total) {
      counter$countervalue <- 0
    }
    if (counter$countervalue == 0) {
      counter$countervalue <- counter$countervalue + 1
    }
    
    


    #save data from orginial
    screen.dat <- as.data.frame(shiny::reactiveValuesToList(original)) |>
      dplyr::rename_all(~ gsub("new.data.", "", .))

    write.csv(screen.dat, file = paste0(screen.file, "_Screened.csv"), row.names = FALSE)

    #update buttons on press to nothing
    shinyWidgets::updatePrettyRadioButtons(
      session = session,
      inputId = "reject.reason",
      choices = c(reject.vec),
      selected = character(0),
      inline = TRUE,
      prettyOptions = list(
        icon = icon("check"),
        bigger = TRUE,
        status = "info",
        animation = "jelly"
      )
    )

    shinyWidgets::updateTextInputIcon(
      session = session,
      inputId = "comments",
      value = character(0),
      placeholder = "Screening comments",
      label = NULL
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
    original$new.data[counter$countervalue, ]$Comment <- input$comments

    counter$countervalue <- counter$countervalue + 1
    settings.store$counter <- counter$countervalue
    
    if (counter$countervalue > countertot$total) {
      counter$countervalue <- 0
    }
    if (counter$countervalue == 0) {
      counter$countervalue <- counter$countervalue + 1
    }
    
    

    shinyWidgets::updatePrettyRadioButtons(
      session = session,
      inputId = "reject.reason",
      choices = c(reject.vec),
      selected = character(0),
      inline = TRUE,
      prettyOptions = list(
        icon = icon("check"),
        bigger = TRUE,
        status = "info",
        animation = "jelly"
      )
    )

    shinyWidgets::updateTextInputIcon(
      session = session,
      inputId = "comments",
      value = character(0),
      placeholder = "Screening comments",
      label = NULL
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
    original$new.data[counter$countervalue, ]$Comment <- input$comments

    counter$countervalue <- counter$countervalue + 1
    settings.store$counter <- counter$countervalue
    
    if (counter$countervalue > countertot$total) {
      counter$countervalue <- 0
    }
    if (counter$countervalue == 0) {
      counter$countervalue <- counter$countervalue + 1
    }

    

    shinyWidgets::updatePrettyRadioButtons(
      session = session,
      inputId = "reject.reason",
      choices = c(reject.vec),
      selected = character(0),
      inline = TRUE,
      prettyOptions = list(
        icon = icon("check"),
        bigger = TRUE,
        status = "info",
        animation = "jelly"
      )
    )


    shinyWidgets::updateTextInputIcon(
      session = session,
      inputId = "comments",
      value = character(0),
      placeholder = "Screening comments",
      label = NULL
    )

    settings.store$new.data <- original$new.data

    screen.dat <- as.data.frame(shiny::reactiveValuesToList(original)) |>
      dplyr::rename_all(~ gsub("new.data.", "", .))
    write.csv(screen.dat, file = paste0(screen.file, "_Screened.csv"), row.names = FALSE)

    saveRDS(shiny::reactiveValuesToList(settings.store),
            file = paste0(screen.file, "_history.rds")
    )
  })
  # hide or show fields depending on input#####
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
    settings.store$inputs <- input$show.fields
  },
  ignoreNULL = FALSE
  )

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
  # progress displayed based on counter and percentage #######
  output$progress <- shiny::renderText({
    percent <- round(sum(original$new.data$Screen != "To be screened") / countertot$total * 100, 0)
    paste0("<p>",
           "<font color=\"#ff3333\"><b>",
           percent,
           "%",
           " ",
           "screened",
           "</b></font> <font color=\"#ff3333\"><b>",
           "(Paper No = ",
           counter$countervalue,
           "</b>) </font>",
           "</p>")
  })
  
  # set boundaries for the counter based on total and reaching zero#######
  shiny::observeEvent(counter$countervalue, {
    if (counter$countervalue == 0) {
      shinyjs::disable("Previous")
      counter$countervalue <- counter$countervalue + 1
    } else {
      shinyjs::enable("Previous")
    }
    
    if (counter$countervalue > countertot$total) {
      shinyjs::disable("Next")
      counter$countervalue <- countertot$total
    } else {
      shinyjs::enable("Next")
    }
  })

  # app stop on session end######
  session$onSessionEnded(function() {
    stopApp()
  })
}
