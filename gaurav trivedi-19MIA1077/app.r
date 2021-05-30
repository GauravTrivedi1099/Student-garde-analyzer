library(DT)
library(shiny)
library(readr)
library(vioplot)
library(rsconnect)
library(knitr)

ui <- fluidPage(
        navbarPage(h2("Download and Use Grades.csv only!!"),
           sidebarPanel(
                downloadButton("downloadData", "grades.csv"),
               HTML('<center><img src="c.png" width="400"></center>'),
               fileInput('file', 'Choose file', accept = "text/csv/xlsx"),
               textInput("NIU", "REGISTRATION NUMBER:", "100.."),
               actionButton("go", "Go!"),
               br(),
               br(),
               mainPanel(
                 tabsetPanel(
                   tabPanel("View",
                            DT::dataTableOutput('grades'),
                   ),
                   tabPanel("Summary",
                            uiOutput('variables'),
                            uiOutput('ifBins'),
                            verbatimTextOutput('summaryGroup'),
                            plotOutput(outputId = "plotGroup")
                   )
                 )
               )
             )
  )
)


server <- function(input, output) {
  data <- read_csv("grades.csv")
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("grades", ".csv", sep="")
    },
    content = function(file) {
      write.csv(data, file)
    }
  )
  
  grades <- eventReactive(input$go, {
    read_csv(input$file$datapath)
  })
  
  output$grades <- DT::renderDataTable({
    grades()
  })
  
  output$variables <- renderUI({
    selectInput('variables', 'Select a Variable', choices = as.character(colnames(grades()[,2:dim(grades())[2]])), selected = 'continuousAssessment')
  })
  
  output$ifBins <- renderUI({
    var <- grades()[,input$variables]
    if(is.character(var[[1]])){
      return()
    }else{
      sliderInput("bins", "Bins",
                  min = 1, max = 130, value = 10)
    }
  })
  
  
  
  output$plotGroup <- renderPlot({
    var <- grades()[,input$variables]
    if(is.character(var[[1]])){
      barplot(table(var), col='yellow')
    }else{
      barplot(table(var), col='yellow')
      hist(var[[1]], col='yellow', breaks = input$bins)
    }
  })
  
  
  
}

shinyApp(ui = ui, server = server)