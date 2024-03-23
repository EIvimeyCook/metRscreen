#load packages
library(shiny)
library(shinyWidgets)
library(tidyr)
library(dplyr)
library(data.table)
library(shinythemes)
library(stringr)
library(shinyalert)
library(shinyjs)
library(bslib)
library(shinymaterial)
library(shinyalert)


#highlight function
#create a vector of colour
#split the string for each search box and add  a possible wild card, then paste  
highlightjoel <- function(text,search){
    
    colours <- c("#52D017","#DC143C","#AF33FF","#FFB133","#334DFF")

    for(j in 1:length(search)){
        if(nchar(search[[j]]) !=0){
            split_search <- strsplit(as.character(search),", ")[[j]]
            split_search <- gsub("\\*","[a-zA-Z]+",split_search)

            for(i in 1:length(split_search)){
                search_term <- paste0("(^|\\W)(",split_search[i],")(\\W|$)")
                text <- gsub(search_term,paste0("\\1<font color=\"",colours[j],"\"><b>\\2</b></font>\\3"),text, ignore.case = T)
            }
        }    
    }
    return(text)
}


#UI##########
ui = material_page(
    font_color = "green",
    primary_theme_color = "darkgreen",
    secondary_theme_color = "darkgreen",
    useShinyjs(),
    title = img(src="meta.png", height = 85),
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
                awesomeCheckbox(
                    inputId = "hide_name",
                    label = "Hide author names and journal?", 
                    value = F,
                    status = "info"
                ),
        material_file_input("Ref", label = "Import a .csv"),
        h6(htmlOutput("progress"))),
        material_card(
            depth = 2,
        material_text_box("search1", "Green Keyword:", value = "", color = "green"),
        material_text_box("search2","Red Keyword:", value =  "", color = "red"),
        material_text_box("search3", "Purple Keyword:", value =  "", color = "purple"),
        material_text_box("search4", "Orange Keyword:", value =  "", color = "orange"),
        material_text_box("search5", "Blue Keyword:", value =  "", color = "blue"),
        ),
        material_card(
            width = 2,
            depth = 2,
            actionBttn(
                inputId = "Next",
                label = "Next Study",
                style = "bordered", 
                color = "success",
                block = T, 
                size = "sm"),
            actionBttn(
                inputId = "Previous",
                label = "Previous Study",
                style = "bordered", 
                color = "royal",
                block = T, 
                size = "sm")),
        textOutput("info"),
        textInput("Study", "study", value = 1)
                ),
        
        #main panel
            material_column(
                width = 8,
                material_card(
                    depth = 2,
                    splitLayout(
                        actionBttn(
                            inputId = "Accept",
                            label = "Accept",
                            style = "pill", 
                            color = "success",
                            block = T, 
                            size = "sm"),
                        actionBttn(
                            inputId = "NoDecision",
                            label = "No Decision",
                            style = "pill", 
                            color = "primary",
                            block = T, 
                            size = "sm"),
                        actionBttn(
                            inputId = "Reject",
                            label = "Reject",
                            style = "pill", 
                            color = "warning",
                            block = T, 
                            size = "sm"))),
                material_card(
                    depth = 2,
                h6(htmlOutput("overview")), 
                br(),
                h6(br(), htmlOutput("abstract")),
                br(),
                h6( br(), htmlOutput("keyword")))
        )
)
)

#Server########
server = function(input, output, session) { 
    
    #alert for intro message
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
    
    #help tips when help is pressed
    observeEvent(input$help, {
        shinyalert("Tips",
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
    datasetInput <- reactive({
        req(input$Ref)
        read.csv(input$Ref$datapath)
})
    #create a counter for the total
    countertot <- reactiveValues(total = 1)
    
    #observe when a dataframe is added and create a total counter that is the length
    observeEvent(input$Ref,{
        print(input$Ref$datapath)
        dat<-read.csv(input$Ref$datapath)
        countertot$total <- nrow(dat)
    })
    
    #create a new dataframe based on the old data in a reactive ovject
    original <- reactiveValues(oldData = NULL, newData = NULL)
    
    #add some new columns to the existing data
    observeEvent(input$Ref, {
        theFile <- input$Ref
        if(is.null(theFile)) {
            original$oldData <- NULL
        } else {
            original$oldData <- read.csv(theFile$datapath, 1) 
            original$newData <- rep("To Be Screened", times = nrow(original$oldData))
        }
    })
    
    #progress displayed based on counter and percentage
    output$progress <- renderText({
        percent = round(counter$countervalue/nrow(datasetInput())*100, 0)
        paste0("<font color=\"#ff3333\"><b>",percent,"%", " ", "screened","</b></font> <font color=\"#ff3333\"><b>","(Paper No = ", counter$countervalue,"</b>) </font>")
    }) 
    
    #counter to move studies and subset, counter values change depedning on next/previous
    counter <- reactiveValues(countervalue = 1) 

    #change the study with next and previous
    observeEvent(input$Next, {
        counter$countervalue <- counter$countervalue + 1  
})
    
    observeEvent(input$Previous, {
        counter$countervalue <- counter$countervalue - 1  
    })
    
    #change and save with accept/reject and nodecision
    observeEvent(input$Accept, {
        original$newData[counter$countervalue] <- "Accept"
        counter$countervalue <- counter$countervalue + 1  
        write.csv(as.data.frame(shiny::reactiveValuesToList(original)), file = file.path(here::here(),"ScreenedData.csv"))
    })

    observeEvent(input$Reject, {
        original$newData[counter$countervalue] <- "Reject"
        counter$countervalue <- counter$countervalue + 1  
        write.csv(as.data.frame(shiny::reactiveValuesToList(original)), file = file.path(here::here(),"ScreenedData.csv"))
        })
    
    observeEvent(input$NoDecision, {
        original$newData[counter$countervalue] <- "No Decision"
        counter$countervalue <- counter$countervalue + 1
        write.csv(as.data.frame(shiny::reactiveValuesToList(original)), file = file.path(here::here(),"ScreenedData.csv"))
        })
    
    #set boundaries for the counter based on total and reaching zero
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

    #reference that is shown changes depedning on studydata and if you press hide name
    output$overview <- renderUI({
        str3 <- paste("<b>Title:</b>", as.character(StudyData()$Title))
        str1 <- paste("<b>Author(s):</b>",as.character(StudyData()$Author))
        str1b <- paste("<b>Author(s):</b> Hidden")
        str2 <- paste("<b>Year:</b>", as.character(StudyData()$Publication.Year))
        str4 <- paste("<b>Journal:</b>", as.character(StudyData()$Publication.Title))
        str4b <- paste("<b>Journal:</b> Hidden")
        if(input$hide_name){
        HTML(paste(str3, str1b, str2,  str4b,  sep = '<br/><br/>'))
        } else {HTML(paste(str3, str1, str2,  str4, sep = '<br/><br/>'))}
})

    #render abstract text highlighted based on search       
    output$abstract <- renderText({
        highlightjoel(as.character(StudyData()$Abstract), search = list(input$search1, input$search2, input$search3, input$search4, input$search5))
})
    
    #render keyword text highlighted based on search       
        output$keyword <- renderText({
            highlightjoel(as.character(StudyData()$Manual.Tags), search = list(input$search1, input$search2, input$search3, input$search4, input$search5))
})


}


#Run##########
shinyApp(ui = ui, server = server)
