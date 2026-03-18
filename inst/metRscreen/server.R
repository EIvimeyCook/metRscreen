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
    search5 = NULL,
    screen.comments = NULL,
    collab.names = NULL
  )

  # create a counter for the total
  countertot <- shiny::reactiveValues(total = 1)

  # counter to move studies and subset, counter values change depedning on next/previous
  counter <- shiny::reactiveValues(countervalue = 0)

  # create a counter for the total
  import <- shiny::reactiveValues(first.import = FALSE, first.load = TRUE)

  # create a new dataframe based on the old data in a reactive ovject
  original <- shiny::reactiveValues(new.data = NULL)
  
  # create a new dataframe for hcecking
  check_dat <- shiny::reactiveValues(check = NULL)

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
        Comment = "No comments given",
        Screen.Name = "No screener name given"
      )
      
      import$first.load <- FALSE
      countertot$total <- nrow(original$new.data)
      cat("\nReading in new screening file and creating new screening output(s)\n")
      
    } else if (!is.null(screen.history) & import$first.load == TRUE) {
      
      settings.store <<- do.call("reactiveValues", readRDS(screen.history))
      check_dat$check <- read.csv(screen.file)
      countertot$total <- nrow(settings.store$new.data)
      counter$countervalue <- settings.store$counter

      
      if (isTRUE(all.equal(settings.store$new.data$Title, check_dat$check$Title))) {
        
        original$new.data <- settings.store$new.data
        
        import$first.load <- FALSE
        import$first.import <- TRUE
        cat("\nReading in saved screening file and using existing screening output\n")
        

      } else if(!isTRUE(all.equal(settings.store$new.data$Title, check_dat$check$Title))) {
      
        cat("\nData frame inconsistencies between saved and loaded data frames - please revert to previous version\n")
        shinyalert::shinyalert(
          title = "Warning",
          text = "Data frame inconsistencies between saved and loaded data frames - please revert to previous version",
          size = "m",
          closeOnClickOutside = FALSE,
          html = TRUE,
          type = "warning",
          showConfirmButton = TRUE,
          showCancelButton = FALSE,
          confirmButtonText = "OK",
          confirmButtonCol = "#AEDEF4",
          animation = TRUE,
          imageHeight = "88",
          imageWidth = "80"
        )
        shiny::stopApp()
      }
    }


    # save a temp output
    settings.store$counter <- counter$countervalue
    settings.store$new.data <- original$new.data

    screen.dat <- as.data.frame(shiny::reactiveValuesToList(original)) |>
      dplyr::rename_all(~ gsub("new.data.", "", .))
    write.csv(screen.dat, file = paste0(screen.file, "_Screened.csv"), row.names = FALSE)

    saveRDS(shiny::reactiveValuesToList(settings.store),
      file = paste0(screen.file, "_history.rds")
    )
  })


  # save files loading ########
  shiny::observe({
    if (!is.null(screen.history) & import$first.import == "TRUE") {
      # update params
      if (!identical(reject.list, settings.store$reject.list) & !is.null(reject.list)) {
        reject.list <<- reject.list
      } else {
        reject.list <<- settings.store$reject.list
      }

      if (!identical(collab.names, settings.store$collab.names) & !is.null(collab.names)) {
        collab.names <<- collab.names
      } else {
        collab.names <<- settings.store$collab.names
      }

      counter$countervalue <- settings.store$counter

      shinyWidgets::updateCheckboxGroupButtons(
        session = session,
        inputId = "show.fields",
        selected = settings.store$inputs
      )

      if (!is.null(settings.store$search1)) {
        shinyWidgets::updateTextInputIcon(
          session = session,
          inputId = "search1",
          value = settings.store$search1
        )
      }

      if (!is.null(settings.store$search2)) {
        shinyWidgets::updateTextInputIcon(
          session = session,
          inputId = "search2",
          value = settings.store$search2
        )
      }

      if (!is.null(settings.store$search3)) {
        shinyWidgets::updateTextInputIcon(
          session = session,
          inputId = "search3",
          value = settings.store$search3
        )
      }

      if (!is.null(settings.store$search4)) {
        shinyWidgets::updateTextInputIcon(
          session = session,
          inputId = "search4",
          value = settings.store$search4
        )
      }

      if (!is.null(settings.store$search5)) {
        shinyWidgets::updateTextInputIcon(
          session = session,
          inputId = "search5",
          value = settings.store$search5
        )
      }
      import$first.import <- FALSE
    }
  })

  # update collab names########
  shiny::observe({
    if (length(collab.names) > 0) {
      shinyjs::show("choose.collab")
      shiny::updateRadioButtons(
        session = session,
        choices = collab.names,
        inputId = "choose.collab",
        selected = character(0)
      )
      settings.store$collab.names <- collab.names
    }
  })




  # update radiogroup with imported reasons####
  shiny::observe({
    if (length(reject.list > 0)) {
      shinyjs::show("reject.reason")
      shinyWidgets::updatePrettyCheckboxGroup(
        session = session,
        inputId = "reject.reason",
        choices = reject.list,
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
      search = list(
        input$search1,
        input$search2,
        input$search3,
        input$search4,
        input$search5
      )
    ))
  })

  # render keyword text highlighted based on search######
  output$keyword <- shiny::renderUI({
    shiny::HTML(highlight_text(as.character(StudyData()$Manual.Tags),
      search = list(
        input$search1,
        input$search2,
        input$search3,
        input$search4,
        input$search5
      )
    ))
  })

  # render title text highlighted based on search######
  output$title <- shiny::renderUI({
    shiny::HTML(paste(
      "<b>",
      highlight_text(as.character(StudyData()$Title),
        search = list(
          input$search1,
          input$search2,
          input$search3,
          input$search4,
          input$search5
        )
      ),
      "</b>"
    ))
  })

  # error
  output$hist.reason <- shiny::renderUI({
    if (StudyData()$Screen == "Reject") {
      shiny::HTML(paste(
        "<p>",
        "<b>Reject Reason:</b>",
        as.character(StudyData()$Reason),
        "</p>"
      ))
    }
  })

  output$hist.screen <- shiny::renderUI({
    shiny::HTML(paste(
      "<p>",
      "<b>Screen:</b>",
      as.character(StudyData()$Screen),
      "</p>"
    ))
  })

  output$screen.comment <- shiny::renderUI({
    shiny::HTML(paste(
      "<p>",
      "<b>Comment:</b>",
      as.character(StudyData()$Comment),
      "</p>"
    ))
  })

  output$name.screener <- shiny::renderUI({
    shiny::HTML(paste(
      "<p>",
      "<b>Screener:</b>",
      as.character(StudyData()$Screen.Name),
      "</p>"
    ))
  })

  output$author <- shiny::renderUI({
    shiny::HTML(paste(
      "<p>",
      "<b>Author:</b>",
      as.character(StudyData()$Author),
      "</p>"
    ))
  })

  output$year <- shiny::renderUI({
    shiny::HTML(paste(
      "<p>",
      "<b>Year:</b>",
      as.character(StudyData()$Publication.Year),
      "</p>"
    ))
  })

  output$journal <- shiny::renderUI({
    shiny::HTML(paste(
      "<p>",
      "<b>Journal:</b>",
      as.character(StudyData()$Publication.Title),
      "</p>"
    ))
  })
  # change and save with accept/reject and nodecision######
  # accept
  shiny::observeEvent(input$Accept, {
    original$new.data[counter$countervalue, ]$Screen <- "Accept"
    if (input$comments != "") {
      original$new.data[counter$countervalue, ]$Comment <- input$comments
    }



    if (length(collab.names) > 0) {
      original$new.data[counter$countervalue, ]$Screen.Name <- input$choose.collab
      if (is.null(input$choose.collab)) {
        shiny::showNotification("No screener chosen")
      }
    }

    counter$countervalue <- counter$countervalue + 1

    if (counter$countervalue > countertot$total) {
      shinyalert::shinyalert(
        title = "Congratulations",
        text = "You've finished screening all papers!",
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = FALSE,
        type = "success",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      cat(paste("\nCongratulations - you have finished screening", countertot$total, "papers \n"))
      counter$countervalue <- countertot$total
    }
    if (counter$countervalue == 0) {
      counter$countervalue <- counter$countervalue + 1
    }

    # save data from orginial
    screen.dat <- as.data.frame(shiny::reactiveValuesToList(original)) |>
      dplyr::rename_all(~ gsub("new.data.", "", .))

    write.csv(screen.dat, file = paste0(screen.file, "_Screened.csv"), row.names = FALSE)

    # update buttons on press to nothing
    shinyWidgets::updatePrettyCheckboxGroup(
      session = session,
      inputId = "reject.reason",
      choices = reject.list,
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

    # storing data fro later
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
      original$new.data[counter$countervalue, ]$Reason <- paste(input$reject.reason, collapse = "; ")
    }
    if (input$comments != "") {
      original$new.data[counter$countervalue, ]$Comment <- input$comments
    }

    if (length(collab.names) != 0) {
      original$new.data[counter$countervalue, ]$Screen.Name <- input$choose.collab
      if (is.null(input$choose.collab)) {
        shiny::showNotification("No screener chosen")
      }
    }
    counter$countervalue <- counter$countervalue + 1
    settings.store$counter <- counter$countervalue

    if (counter$countervalue > countertot$total) {
      shinyalert::shinyalert(
        title = "Congratulations",
        text = "You've finished screening all papers!",
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = FALSE,
        type = "success",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      cat(paste("\nCongratulations - you have finished screening", countertot$total, "papers \n"))
      counter$countervalue <- countertot$total
    }
    if (counter$countervalue == 0) {
      counter$countervalue <- counter$countervalue + 1
    }



    shinyWidgets::updatePrettyCheckboxGroup(
      session = session,
      inputId = "reject.reason",
      choices = reject.list,
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

    if (input$comments != "") {
      original$new.data[counter$countervalue, ]$Comment <- input$comments
    }

    if (length(collab.names) != 0) {
      original$new.data[counter$countervalue, ]$Screen.Name <- input$choose.collab
      if (is.null(input$choose.collab)) {
        shiny::showNotification("No screener chosen")
      }
    }

    counter$countervalue <- counter$countervalue + 1
    settings.store$counter <- counter$countervalue

    if (counter$countervalue > countertot$total) {
      shinyalert::shinyalert(
        title = "Congratulations",
        text = "You've finished screening all papers!",
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = FALSE,
        type = "success",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      cat(paste("\nCongratulations - you have finished screening", countertot$total, "papers \n"))
      counter$countervalue <- countertot$total
    }
    if (counter$countervalue == 0) {
      counter$countervalue <- counter$countervalue + 1
    }



    shinyWidgets::updatePrettyCheckboxGroup(
      session = session,
      inputId = "reject.reason",
      choices = reject.list,
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
  shiny::observeEvent(input$show.fields,
    {
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
    data <- original$new.data

    
    screened     <- sum(data$Screen != "To be screened", na.rm = TRUE)
    percent      <- round(screened / countertot$total * 100, 0)
    
    n_accept     <- sum(data$Screen == "Accept", na.rm = TRUE)
    n_reject     <- sum(data$Screen == "Reject", na.rm = TRUE)
    n_nodecision <- sum(data$Screen == "No Decision", na.rm = TRUE)
    
    accept_str <- if (isTRUE(screened > 0)) {
      pct_accept <- round(n_accept / screened * 100, 0)
      paste0("<br><font color=\"#2ecc71\"><b>Accept: ", n_accept, " (", pct_accept, "%)</b></font>")
    } else ""
    
    reject_str <- if (isTRUE(screened > 0)) {
      pct_reject <- round(n_reject / screened * 100, 0)
      paste0("<br><font color=\"#e74c3c\"><b>Reject: ", n_reject, " (", pct_reject, "%)</b></font>")
    } else ""
    
    nodecision_str <- if (isTRUE(screened > 0)) {
      pct_nodecision <- round(n_nodecision / screened * 100, 0)
      paste0("<br><font color=\"#3498db\"><b>No Decision: ", n_nodecision, " (", pct_nodecision, "%)</b></font>")
    } else ""
    
    paste0(
      "<p>",
      "<font color=\"#ff3333\"><b>", percent, "% screened",
      " (Paper No = ", counter$countervalue, ")</b></font>",
      accept_str,
      reject_str,
      nodecision_str,
      "</p>"
    )
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
  
  
  #reference list on side
  output$ref_list <- shiny::renderUI({
    data <- original$new.data
    shiny::req(data, nrow(data) > 0)
    
    screened <- data |>
      dplyr::filter(Screen != "To be screened") |>
      dplyr::mutate(row_idx = dplyr::row_number())
    
    if (nrow(screened) == 0) return(shiny::p("No papers screened yet."))
    
    purrr::pmap(screened, function(Title, Author, Screen, row_idx, ...) {
      colour <- switch(Screen,
                       "Accept"      = "#2ecc71",
                       "Reject"      = "#e74c3c",
                       "No Decision" = "#3498db"
      )
      shiny::tags$div(
        style = "margin-bottom: 6px;",
        shiny::actionButton(
          inputId = paste0("ref_", row_idx),
          label   = shiny::tags$span(
            shiny::tags$b(style = paste0("color:", colour), Screen),
            shiny::tags$br(),
            shiny::tags$small(paste0(Author, " (", data$Publication.Year[row_idx], ")"))
          ),
          style = "width: 100%; text-align: left; background: white; border: 1px solid #ddd;"
        )
      )
    })
  })
  
  shiny::observe({
    data <- original$new.data
    screened_rows <- which(data$Screen != "To be screened")
    
    purrr::walk(screened_rows, function(i) {
      shiny::observeEvent(input[[paste0("ref_", i)]], {
        counter$countervalue <- i
      }, ignoreInit = TRUE)
    })
  })

  # app stop on session end######
  session$onSessionEnded(function() {
    stopApp()
  })
}
