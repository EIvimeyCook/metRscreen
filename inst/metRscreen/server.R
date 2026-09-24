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

  # collaborative mode ########
  # when screener names are supplied, each screener gets their own files
  # (see utilities/collab_functions.R) and can switch between screeners
  collab_mode <- length(collab.names) > 0
  active <- shiny::reactiveValues(user = NULL)

  # save the current screening state to the right file(s)
  save_state <- function(summary = FALSE) {
    if (collab_mode) {
      if (is.null(active$user)) {
        return(invisible(NULL))
      }
      save_user_state(screen.file, active$user, shiny::reactiveValuesToList(settings.store))
      if (summary) write_collab_summary(screen.file, collab.names)
    } else {
      screen.dat <- as.data.frame(shiny::reactiveValuesToList(original)) |>
        dplyr::rename_all(~ gsub("new.data.", "", .))
      write.csv(screen.dat, file = paste0(screen.file, "_Screened.csv"), row.names = FALSE)
      saveRDS(shiny::reactiveValuesToList(settings.store),
        file = paste0(screen.file, "_history.rds")
      )
    }
  }

  # in collaborative mode a screener has to be chosen before making decisions
  screener_chosen <- function() {
    if (collab_mode && is.null(active$user)) {
      shiny::showNotification("Choose who is screening before making a decision",
        type = "warning"
      )
      return(FALSE)
    }
    TRUE
  }

  # restore saved searches and visible fields
  apply_saved_settings <- function(s) {
    shinyWidgets::updateCheckboxGroupButtons(
      session = session,
      inputId = "show.fields",
      selected = if (is.null(s$inputs)) character(0) else s$inputs
    )
    for (i in 1:5) {
      value <- s[[paste0("search", i)]]
      if (!is.null(value)) {
        shinyWidgets::updateTextInputIcon(
          session = session,
          inputId = paste0("search", i),
          value = value
        )
      }
    }
  }


  # help tips when help is pressed######
  shiny::observeEvent(input$help, {
    shinyalert::shinyalert("Tips",
      "1. In order to use a .csv file, export references from Zotero. <br>
                          <br>
                           2. To highlight multiple words, separate each string with a comma and no space. <br>
                           <br>
                           3. To use a wildcard search, enter the * character after the appropriate string. <br>
                          <br>
                          4. If you want to see author/title/journal/year info, select each component from the checkbox. These are blinded by default.
      <br>
      <br>
      5. You can now make decisions using keyboard shortcuts: y = accept, m = no decision, n = decline
      <br>
      <br>
      6. Collaborative screening: supply collab.names to metRscreen(). Each screener gets their own files and you can switch screener with 'Who is screening?'. Other screeners' decisions are hidden unless you turn on 'Show other screeners' decisions'. A combined file (_Collab_Summary.csv) flags agreements and conflicts.",
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
    if (collab_mode & import$first.load == TRUE) {
      # show the papers straight away; each screener's decisions are loaded
      # once they are chosen in "Who is screening?"
      original$new.data <- blank_screen(screen.file)
      import$first.load <- FALSE
      countertot$total <- nrow(original$new.data)
      cat("\nCollaborative mode: choose who is screening to load their screening file\n")
    } else if (is.null(screen.history) & import$first.load == TRUE) {
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
      } else if (!isTRUE(all.equal(settings.store$new.data$Title, check_dat$check$Title))) {
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

    save_state()
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
      shinyjs::show("collab.panel")
      shiny::updateRadioButtons(
        session = session,
        choices = collab.names,
        inputId = "choose.collab",
        selected = character(0)
      )
      settings.store$collab.names <- collab.names
    }
  })

  # switch screener: save the current screener, then load the chosen one ########
  shiny::observeEvent(input$choose.collab, {
    new_user <- input$choose.collab
    # the radio buttons start with a blank choice - ignore anything that isn't a screener
    if (!collab_mode || length(new_user) != 1 || !new_user %in% collab.names ||
      identical(new_user, active$user)) {
      return()
    }

    loaded <- load_user_state(screen.file, new_user)

    if (is.null(loaded$settings)) {
      shinyalert::shinyalert(
        title = "Warning", text = loaded$message, type = "warning",
        closeOnClickOutside = FALSE, confirmButtonText = "OK"
      )
      # go back to whoever was screening before
      shiny::updateRadioButtons(
        session = session, inputId = "choose.collab",
        selected = if (is.null(active$user)) character(0) else active$user
      )
      return()
    }

    # make sure the outgoing screener's latest state is on disk
    if (!is.null(active$user)) {
      settings.store$counter <- counter$countervalue
      settings.store$new.data <- original$new.data
      save_state()
    }

    s <- loaded$settings
    # screener-specific settings (reject reasons and screeners are project-wide)
    for (nm in c("new.data", "counter", "inputs", "screen.comments")) {
      settings.store[[nm]] <- s[[nm]]
    }
    # keyword searches: use the screener's saved ones, otherwise keep what is on screen
    for (nm in paste0("search", 1:5)) {
      settings.store[[nm]] <- if (is.null(s[[nm]])) input[[nm]] else s[[nm]]
    }
    if (is.null(reject.list) && !is.null(s$reject.list)) {
      reject.list <<- s$reject.list
      settings.store$reject.list <- reject.list
      shinyjs::show("reject.reason")
      shinyWidgets::updatePrettyCheckboxGroup(
        session = session,
        inputId = "reject.reason",
        choices = reject.list,
        selected = character(0),
        inline = TRUE,
        prettyOptions = list(icon = icon("check"), bigger = TRUE, status = "info", animation = "jelly")
      )
    }

    active$user <- new_user
    original$new.data <- s$new.data
    countertot$total <- nrow(s$new.data)
    counter$countervalue <- max(1, min(s$counter, nrow(s$new.data)))
    apply_saved_settings(s)

    # don't let a half-written comment or ticked reason carry over to the next screener
    shinyWidgets::updateTextInputIcon(
      session = session, inputId = "comments", value = character(0),
      placeholder = "Screening comments", label = NULL
    )
    shinyWidgets::updatePrettyCheckboxGroup(
      session = session, inputId = "reject.reason", choices = reject.list,
      selected = character(0), inline = TRUE,
      prettyOptions = list(icon = icon("check"), bigger = TRUE, status = "info", animation = "jelly")
    )

    cat(paste0("\n", loaded$message, "\n"))
    shiny::showNotification(loaded$message, type = "message")
  })

  # other screeners' decisions for the current paper (hidden by default) ########
  output$others.decisions <- shiny::renderUI({
    shiny::req(collab_mode, isTRUE(input$show.others))
    # pick up decisions from screeners working at the same time in a shared folder
    shiny::invalidateLater(5000)

    i <- counter$countervalue
    shiny::req(i >= 1)
    others <- setdiff(collab.names, active$user)
    if (length(others) == 0) {
      return(shiny::p("No other screeners."))
    }

    title <- as.character(original$new.data$Title[i])
    own <- if (is.null(active$user)) NA else as.character(original$new.data$Screen[i])
    decisions <- own

    rows <- lapply(others, function(u) {
      d <- read_user_decisions(screen.file, u)
      if (is.null(d) || nrow(d) < i || !identical(as.character(d$Title[i]), title)) {
        decisions <<- c(decisions, NA)
        return(shiny::tags$p(shiny::tags$b(paste0(u, ":")), shiny::tags$i("not started")))
      }
      decisions <<- c(decisions, d$Screen[i])
      colour <- switch(as.character(d$Screen[i]),
        "Accept" = "#2ecc71",
        "Reject" = "#e74c3c",
        "No Decision" = "#3498db",
        "#6c757d"
      )
      show_reason <- isTRUE(d$Screen[i] == "Reject" && !is.na(d$Reason[i]) && d$Reason[i] != "No reason given")
      show_comment <- isTRUE(!is.na(d$Comment[i]) && d$Comment[i] != "No comments given")
      shiny::tags$p(
        shiny::tags$b(paste0(u, ":")),
        shiny::tags$span(style = paste0("color:", colour, "; font-weight: bold;"), d$Screen[i]),
        if (show_reason) shiny::tags$br(),
        if (show_reason) shiny::tags$small("Reason: ", d$Reason[i]),
        if (show_comment) shiny::tags$br(),
        if (show_comment) shiny::tags$small("Comment: ", d$Comment[i])
      )
    })

    status <- agreement_status(matrix(decisions, nrow = 1))
    status_colour <- switch(status,
      "Agree" = "#2ecc71",
      "Conflict" = "#e74c3c",
      "#6c757d"
    )
    shiny::tagList(
      rows,
      shiny::tags$p(shiny::tags$b("Agreement: "), shiny::tags$span(style = paste0("color:", status_colour, "; font-weight: bold;"), status))
    )
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
    if (!screener_chosen()) {
      return()
    }
    original$new.data[counter$countervalue, ]$Screen <- "Accept"
    if (input$comments != "") {
      original$new.data[counter$countervalue, ]$Comment <- input$comments
    }

    if (collab_mode) {
      original$new.data[counter$countervalue, ]$Screen.Name <- active$user
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
    save_state(summary = TRUE)
  })


  # reject
  shiny::observeEvent(input$Reject, {
    if (!screener_chosen()) {
      return()
    }
    original$new.data[counter$countervalue, ]$Screen <- "Reject"

    if (length(input$reject.reason > 0)) {
      original$new.data[counter$countervalue, ]$Reason <- paste(input$reject.reason, collapse = "; ")
    }
    if (input$comments != "") {
      original$new.data[counter$countervalue, ]$Comment <- input$comments
    }

    if (collab_mode) {
      original$new.data[counter$countervalue, ]$Screen.Name <- active$user
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
    save_state(summary = TRUE)
  })

  # no decision
  shiny::observeEvent(input$NoDecision, {
    if (!screener_chosen()) {
      return()
    }
    original$new.data[counter$countervalue, ]$Screen <- "No Decision"

    if (input$comments != "") {
      original$new.data[counter$countervalue, ]$Comment <- input$comments
    }

    if (collab_mode) {
      original$new.data[counter$countervalue, ]$Screen.Name <- active$user
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
    save_state(summary = TRUE)
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


    screened <- sum(data$Screen != "To be screened", na.rm = TRUE)
    percent <- round(screened / countertot$total * 100, 0)

    n_accept <- sum(data$Screen == "Accept", na.rm = TRUE)
    n_reject <- sum(data$Screen == "Reject", na.rm = TRUE)
    n_nodecision <- sum(data$Screen == "No Decision", na.rm = TRUE)

    accept_str <- if (isTRUE(screened > 0)) {
      pct_accept <- round(n_accept / screened * 100, 0)
      paste0("<br><font color=\"#2ecc71\"><b>Accept: ", n_accept, " (", pct_accept, "%)</b></font>")
    } else {
      ""
    }

    reject_str <- if (isTRUE(screened > 0)) {
      pct_reject <- round(n_reject / screened * 100, 0)
      paste0("<br><font color=\"#e74c3c\"><b>Reject: ", n_reject, " (", pct_reject, "%)</b></font>")
    } else {
      ""
    }

    nodecision_str <- if (isTRUE(screened > 0)) {
      pct_nodecision <- round(n_nodecision / screened * 100, 0)
      paste0("<br><font color=\"#3498db\"><b>No Decision: ", n_nodecision, " (", pct_nodecision, "%)</b></font>")
    } else {
      ""
    }

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


  # reference list on side
  output$ref_list <- shiny::renderUI({
    data <- original$new.data
    shiny::req(data, nrow(data) > 0)

    # row_idx must be the paper's row in the full data so the button jumps to it
    screened <- data |>
      dplyr::mutate(row_idx = dplyr::row_number()) |>
      dplyr::filter(Screen != "To be screened")

    if (nrow(screened) == 0) {
      return(shiny::p("No papers screened yet."))
    }

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
          label = shiny::tags$span(
            shiny::tags$b(style = paste0("color:", colour), Screen),
            shiny::tags$br(),
            shiny::tags$small(paste0(Author, " (", data$Publication.Year[row_idx], ")"))
          ),
          style = "width: 100%; text-align: left; background: white; border: 1px solid #ddd;"
        )
      )
    })
  })

  # one observer per reference button (only created once, not on every decision)
  ref_observers <- integer(0)
  shiny::observe({
    data <- original$new.data
    screened_rows <- which(data$Screen != "To be screened")
    new_rows <- setdiff(screened_rows, ref_observers)
    ref_observers <<- c(ref_observers, new_rows)

    purrr::walk(new_rows, function(i) {
      shiny::observeEvent(input[[paste0("ref_", i)]],
        {
          counter$countervalue <- i
        },
        ignoreInit = TRUE
      )
    })
  })

  # app stop on session end######
  session$onSessionEnded(function() {
    stopApp()
  })
}
