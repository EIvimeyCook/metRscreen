#load packages
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(tidyr)
library(dplyr)
library(data.table)
library(shinythemes)
library(stringr)
library(ggvis)
library(shinyalert)
library(shinyjs)
library(bslib)
library(shinymaterial)
library(shinyalert)

#highlight function
highlightjoel <- function(text,search){
    
    colours <- c("#52D017","#DC143C","#AF33FF","#FFB133","#334DFF")
    
    for(j in 1:length(search)){
        if(!is.null(search[[j]])){
            split_search <- strsplit(search[[j]],",")[[1]]
            split_search <- gsub("\\*","[a-zA-Z]+",split_search)
            
            for(i in 1:length(split_search)){
                search_term <- paste0("(^|\\W)((?i)",split_search[i],")(\\W|$)")
                text <- gsub(search_term,paste0("\\1<font color=\"",colours[j],"\"><b>\\2</b></font>\\3"),text)
            }
        }     
    }
    return(text)
}

#save data function
saveData <- function(data) {
    data <- as.data.frame(t(data))
    if (exists("Screen")) {
        Screen <<- rbind(Screen, data)
    } else {
        Screen <<- data
    }
}


#needed for form saving
fields <- c("Study", "Decision")

#UI##########
ui <- material_page(
    font_color = "green",
    useShinyalert(),
    primary_theme_color = "darkgreen",
    secondary_theme_color = "darkgreen",
    useShinyjs(),
    title= img(src="meta.png", height = 85),
    br(),
    actionBttn(
        inputId = "help",
        label = "Help", 
        style = "minimal",
        color = "royal"),

    # Sidebar 
    material_row(
        material_column(
            width = 4,
            material_card(
                depth = 2,
        material_file_input("Ref", label = "Import a .csv")),
        h5(htmlOutput("progress")),
        material_card(
            depth = 2,
        material_text_box("search1", "Green Keyword:", value = "   ", color = "green"),
        material_text_box("search2","Red Keyword:",value =  "   ", color = "red"),
        material_text_box("search3", "Purple Keyword:",value =  "   ", color = "purple"),
        material_text_box("search4", "Orange Keyword:",value =  "   ", color = "orange"),
        material_text_box("search5", "Blue Keyword:",value =  "   ", color = "blue"),
        ),
        material_card(
            width = 2,
            depth = 2,
            actionBttn(
                inputId = "Next",
                label = "Next Study",
                style = "fill", 
                color = "success",
                block = T, 
                size = "sm"),
            actionBttn(
                inputId = "Previous",
                label = "Previous Study",
                style = "fill", 
                color = "royal",
                block = T, 
                size = "sm")),
        downloadButton("downloadData", "Download Data"),
        textInput("Study", "study", value = 1)
                ),
        
        #main panel
            material_column(
                width = 8,
                material_card(
                    depth = 2,
                h6(htmlOutput("overview")), 
                br(), br(),
                h6(br(), htmlOutput("abstract")),
                br(), br(),
                h6( br(), htmlOutput("keyword"))),
            material_card(
                depth = 2,
                radioButtons(
                inputId = "Decision",
                label = NULL, 
                choices = c("No Decision Made", "Accept", "Maybe", " Reject"),
                selected = "No Decision Made",
                width = "500px")
            ),
        )
)
)
#Server########
server <- function(input, output, session) { 
    #create a reactive that changes depending on reference file added    
    datasetInput <- reactive({
        req(input$Ref)
        read.csv(input$Ref$datapath)
})
    
    countertot <- reactiveValues(total = 1)
    
    observeEvent(input$Ref,{
        dat<-read.csv(input$Ref$datapath)
        countertot$total <- nrow(dat)
    })
    
    #counter to move studies and subset, counter values change depedning on next/previous
    counter <- reactiveValues(countervalue = 1) 

    observeEvent(input$Next, {
        counter$countervalue <- counter$countervalue + 1  
})
    
    observeEvent(input$Previous, {
        counter$countervalue <- counter$countervalue - 1
})
    
    observeEvent(counter$countervalue, {
        if(counter$countervalue == 0) {
            counter$countervalue <- counter$countervalue + 1
        }})
    
    observeEvent(counter$countervalue, {
        if(counter$countervalue > countertot$total) {
            counter$countervalue <- countertot$total
        }
    })

     #the dataset is then subsetted to represent the counter 
    StudyData <- reactive({
          datasetInput()[counter$countervalue,]
})    
    
    observeEvent(input$Next, {
        updateTextInput(session, "Study", value = counter$countervalue)
        shinyjs::reset("Decision")
})
    shinyjs::hide("Study")

    
    #reference that is shown changes depedning on studydata
    output$overview <- renderUI({
        str3 <- paste("<b>Title:</b>", as.character(StudyData()$Title))
        str1 <- paste("<b>Author(s):</b>",as.character(StudyData()$Author))
        str2 <- paste("<b>Year:</b>", as.character(StudyData()$Publication.Year))
        str4 <- paste("<b>Journal:</b>", as.character(StudyData()$Publication.Title))
        HTML(paste(str3, str1, str2,  str4, sep = '<br/><br/>'))
})
       
    output$abstract <- renderText({
        highlightjoel(as.character(StudyData()$Abstract), search = list(input$search1, input$search2, input$search3, input$search4, input$search5))
})
    
        output$keyword <- renderText({
            highlightjoel(as.character(StudyData()$Manual.Tags), search = list(input$search1, input$search2, input$search3, input$search4, input$search5))
})

        
        #create a reactive form to save data from
        formData <- reactive({
            data <- sapply(fields, function(x) input[[x]])
            data
})
        
        #pressing next saves the form
        observeEvent(input$Next, {
            saveData(formData())
})
        
            #alert for intro
            shinyalert(
                title = "Welcome to MetaScreen",
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
            
            #download button handler
            output$downloadData <- downloadHandler(
                filename = function() {
                    paste0("newData - ", Sys.Date(), ".csv")
                },
                
                content = function(con) {
                    if (!is.null(original$newData)) {
                        dataSave <- original$newData
                    } else {
                        dataSave <- original$oldData
                    }
                        write.csv(dataSave, con, row.names = F)
})
    

            #help tips
            observeEvent(input$help, {
                shinyalert("Tips",
                          "1. In order to use a .csv file, export references from Zotero. <br>
                          <br>
                           2. To highlight multiple words, separate each string with a comma and no space. <br>
                           <br>
                           3. To use a wildcard search, enter the * character after the appropriate string.",
                           type = "info",
                           html = T,
                           confirmButtonText = "OK")
})
            
            original <- reactiveValues()
            
            observeEvent(input$Ref, {
                theFile <- input$Ref
                if(is.null(theFile)) {
                    original$oldData <- NULL
                } else {
                    original$oldData <- read.csv(theFile$datapath, 1) 
                    original$oldData$"ID" <- c(1:nrow(original$oldData))
                    original$oldData$"Screen" <- rep("NA", times = nrow(original$oldData))
                    
                }
})
            observeEvent(input$Next, {
            original$newData <- original$oldData
            if(input$Study !="") {
                for(i in 1:nrow(original$oldData)){
                    if(input$Study == original$oldData$"ID"[i]){
                        
                        if(!is.na(input$Decision)){
                            original$newData$"Screen"[i] <- input$Decision
                        }
                    }
                }
                original$oldData<-original$newData  
            }
})
            output$progress <- renderText({
                percent = round(counter$countervalue/nrow(datasetInput())*100, 0)
                paste0("<font color=\"#ff3333\"><b>",percent,"%", " ", "screened","</b></font>")
                
            })                
            
            
}
        
#Run##########
shinyApp(ui = ui, server = server)

