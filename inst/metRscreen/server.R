#server function
server <- function(input, output, session){
  #reactive objects########
  #create a counter for the total
  countertot <- shiny::reactiveValues(total = 1)

  #create a new dataframe based on the old data in a reactive ovject
  original <- shiny::reactiveValues(oldData = NULL, newData = NULL)

  #counter to move studies and subset, counter values change depedning on next/previous
   counter <- shiny::reactiveValues(countervalue = 0, next_count = 0)

  #help tips when help is pressed######
  shiny::observeEvent(input$help, {
    shinyalert::shinyalert("Tips",
               "1. In order to use a .csv file, export references from Zotero. <br>
                          <br>
                           2. To highlight multiple words, separate each string with a comma and no space. <br>
                           <br>
                           3. To use a wildcard search, enter the * character after the appropriate string. <br>
                          <br>
                          4. If you want to hide author names and journal (blind) then select the checkbox prior to importing ",
               type = "info",
               html = T,
               confirmButtonText = "OK")
  })

  #cite me action button#######
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
      imageUrl =  "logo/metRscreen.png",
      imageHeight = "88",
      imageWidth = "80"
    )
  })

  #input dataframe#######
  datasetInput <- shiny::reactive({
    shiny::req(input$ref)
    shinyFiles::shinyFileChoose(input,'ref', roots = shinyFiles::getVolumes(), session = session,
                                filetype = "csv")
    if(length(shinyFiles::parseFilePaths(shinyFiles::getVolumes(), input$ref)$datapath) > 0) {
   read.csv(shinyFiles::parseFilePaths(shinyFiles::getVolumes(), input$ref)$datapath)
    }
  })

  #counter total######
  shiny::observeEvent(input$ref,{
    countertot$total <- nrow(datasetInput())
  })

  #update radiogroup with improted reasons####
   shiny::observe({
     if(length(reject.list > 0)){
     shinyWidgets::updatePrettyRadioButtons(
       session = session,
       inputId = "reject.reason",
       choices = c(reject.list),
       selected = character(0),
       inline = TRUE,
       prettyOptions = list(icon = icon("check"),
                            bigger = TRUE,
                            status = "info",
                            animation = "jelly")
     )
       shinyjs::show("reject.reason")
}
   })

  #progress displayed based on counter and percentage #######
   output$progress <- shiny::renderText({
     shiny::req(input$ref)
     percent = round(counter$countervalue/countertot$total*100, 0)
     paste0("<font color=\"#ff3333\"><b>",percent,"%", " ", "screened","</b></font> <font color=\"#ff3333\"><b>","(Paper No = ", counter$countervalue,"</b>) </font>")
   })

  #change the study with next and previous#######
   shiny::observeEvent(input$Next, {
     counter$countervalue <- counter$countervalue + 1
   })

   shiny::observeEvent(input$Previous, {
     counter$countervalue <- counter$countervalue - 1
   })

   #set boundaries for the counter based on total and reaching zero
   shiny::observeEvent(counter$countervalue, {
     if(counter$countervalue == 0) {
       counter$countervalue <- counter$countervalue + 1
     }})

   shiny::observeEvent(counter$countervalue, {
     if(counter$countervalue > countertot$total) {
       counter$countervalue <- countertot$total
     }
   })

  #the dataset is then subsetted to represent the counter #######
   StudyData <-  shiny::reactive({
     datasetInput()[counter$countervalue,]
   })


  #render abstract text highlighted based on search########
   output$abstract <- shiny::renderText({
     highlight_text(as.character(StudyData()$Abstract), search = list(input$search1, input$search2, input$search3, input$search4, input$search5))
   })

  #render keyword text highlighted based on search######
   output$keyword <- shiny::renderText({
     highlight_text(as.character(StudyData()$Manual.Tags), search = list(input$search1, input$search2, input$search3, input$search4, input$search5))
   })

  #outputs for each section #######
   output$title <- shiny::renderUI({
     shiny::req(input$ref)
     shiny::req(input$show_fields)
     if("Title" %in% input$show_fields){
       shiny::HTML(paste("<b>Title:</b>", as.character(StudyData()$Title)))
       } else {
         shiny::HTML(paste())
         }
     })

   output$author <- shiny::renderUI({
     shiny::req(input$ref)
     shiny::req(input$show_fields)
     if("Author" %in% input$show_fields){
       shiny::HTML(paste("<b>Author:</b>", as.character(StudyData()$Author)))
     } else {
       shiny::HTML(paste())
     }
   })

   output$year <- shiny::renderUI({
     shiny::req(input$ref)
     shiny::req(input$show_fields)
     if("Year" %in% input$show_fields){
       shiny::HTML(paste("<b>Year:</b>", as.character(StudyData()$Publication.Year)))
     } else {
       shiny::HTML(paste())
     }
   })

   output$journal <- shiny::renderUI({
     shiny::req(input$ref)
     shiny::req(input$show_fields)
     if("Journal" %in% input$show_fields){
       shiny::HTML(paste("<b>Journal:</b>", as.character(StudyData()$Publication.Title)))
     } else {
       shiny::HTML(paste())
     }
   })

  #create a newversion of the data frame######
  shiny::observeEvent(input$ref,{
      original$oldData <- datasetInput()
      original$newData <- cbind(original$oldData, Screen = "To be screened",
                                Reason = "No reason given")
  })

  #change and save with accept/reject and nodecision######
  shiny::observeEvent(input$Accept, {
    original$newData[counter$countervalue,]$Screen <- "Accept"
    counter$countervalue <- counter$countervalue + 1
    write.csv(as.data.frame(shiny::reactiveValuesToList(original)),
              file = paste(shinyFiles::parseFilePaths(shinyFiles::getVolumes(), input$ref)$datapath, "Screened.csv"))
  })

  shiny::observeEvent(input$Reject, {
    original$newData[counter$countervalue,]$Screen <- "Reject"
    if(length(input$reject.reason > 0)){
      original$newData[counter$countervalue,]$Reason <- input$reject.reason
    }
    counter$countervalue <- counter$countervalue + 1
    write.csv(as.data.frame(shiny::reactiveValuesToList(original)),
              file = paste(shinyFiles::parseFilePaths(shinyFiles::getVolumes(), input$ref)$datapath, "Screened.csv"))
  })

  shiny::observeEvent(input$NoDecision, {
    original$newData[counter$countervalue,]$Screen <- "No Decision"
    counter$countervalue <- counter$countervalue + 1
    write.csv(as.data.frame(shiny::reactiveValuesToList(original)),
              file = paste(shinyFiles::parseFilePaths(shinyFiles::getVolumes(), input$ref)$datapath, "Screened.csv"))
  })
}


