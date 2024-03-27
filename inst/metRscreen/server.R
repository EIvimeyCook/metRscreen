#server function
server <- function(input, output, session){
  
  #alert for intro message
  shinyalert::shinyalert(
    title = "Welcome to metRscreen",
    text = "by Ed Ivimey-Cook and Joel Pick",
    closeOnEsc = TRUE,
    closeOnClickOutside = FALSE,
    html = FALSE,
    type = "",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "OK",
    confirmButtonCol = "#AEDEF4",
    timer = 200000,
    imageUrl = "",
    animation = TRUE
  )
  
  #help tips when help is pressed
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
  
  #hide the study section
  shinyjs::hide("Study")
  
  #create a reactive dataframe that changes depending on reference file added    
  datasetInput <- shiny::reactive({
    shiny::req(input$Ref)
    read.csv(input$Ref$datapath)
  })
  #create a counter for the total
  countertot <- shiny::reactiveValues(total = 1)
  
  #observe when a dataframe is added and create a total counter that is the length
  shiny::observeEvent(input$Ref,{
    print(input$Ref$datapath)
    dat<-read.csv(input$Ref$datapath)
    countertot$total <- nrow(dat)
  })
  
  #create a new dataframe based on the old data in a reactive ovject
  original <- shiny::reactiveValues(oldData = NULL, newData = NULL)
  
  #add some new columns to the existing data
  shiny::observeEvent(input$Ref, {
    theFile <- input$Ref
    if(is.null(theFile)) {
      original$oldData <- NULL
    } else {
      original$oldData <- read.csv(theFile$datapath, 1) 
      original$newData <- rep("To Be Screened", times = nrow(original$oldData))
    }
  })
  
  #progress displayed based on counter and percentage
  output$progress <- shiny::renderText({
    percent = round(counter$countervalue/nrow(datasetInput())*100, 0)
    paste0("<font color=\"#ff3333\"><b>",percent,"%", " ", "screened","</b></font> <font color=\"#ff3333\"><b>","(Paper No = ", counter$countervalue,"</b>) </font>")
  }) 
  
  #counter to move studies and subset, counter values change depedning on next/previous
  counter <- shiny::reactiveValues(countervalue = 1) 
  
  #change the study with next and previous
  shiny::observeEvent(input$Next, {
    counter$countervalue <- counter$countervalue + 1  
  })
  
  shiny::observeEvent(input$Previous, {
    counter$countervalue <- counter$countervalue - 1  
  })
  
  #change and save with accept/reject and nodecision
  shiny::observeEvent(input$Accept, {
    original$newData[counter$countervalue] <- "Accept"
    counter$countervalue <- counter$countervalue + 1  
    write.csv(as.data.frame(shiny::reactiveValuesToList(original)), file = file.path(here::here(),"ScreenedData.csv"))
  })
  
  shiny::observeEvent(input$Reject, {
    original$newData[counter$countervalue] <- "Reject"
    counter$countervalue <- counter$countervalue + 1  
    write.csv(as.data.frame(shiny::reactiveValuesToList(original)), file = file.path(here::here(),"ScreenedData.csv"))
  })
  
  shiny::observeEvent(input$NoDecision, {
    original$newData[counter$countervalue] <- "No Decision"
    counter$countervalue <- counter$countervalue + 1
    write.csv(as.data.frame(shiny::reactiveValuesToList(original)), file = file.path(here::here(),"ScreenedData.csv"))
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
  
  #the dataset is then subsetted to represent the counter 
  StudyData <-  shiny::reactive({
    datasetInput()[counter$countervalue,]
  })    
  
  #reference that is shown changes depedning on studydata and if you press hide name
  output$overview <-  shiny::renderUI({
    str3 <- paste("<b>Title:</b>", as.character(StudyData()$Title))
    str1 <- paste("<b>Author(s):</b>",as.character(StudyData()$Author))
    str1b <- paste("<b>Author(s):</b> Hidden")
    str2 <- paste("<b>Year:</b>", as.character(StudyData()$Publication.Year))
    str4 <- paste("<b>Journal:</b>", as.character(StudyData()$Publication.Title))
    str4b <- paste("<b>Journal:</b> Hidden")
    if(input$hide_name){
      shiny::tags$HTML(paste(str3, str1b, str2,  str4b,  sep = '<br/><br/>'))
    } else {shiny::tags$HTML(paste(str3, str1, str2,  str4, sep = '<br/><br/>'))}
  })
  
  #render abstract text highlighted based on search       
  output$abstract <- shiny::renderText({
    highlight_text(as.character(StudyData()$Abstract), search = list(input$search1, input$search2, input$search3, input$search4, input$search5))
  })
  
  #render keyword text highlighted based on search       
  output$keyword <- shiny::renderText({
    highlight_text(as.character(StudyData()$Manual.Tags), search = list(input$search1, input$search2, input$search3, input$search4, input$search5))
  })
  
  
}


