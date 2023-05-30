# Import libraries
library(shiny)
library(data.table)
library(randomForest)

# Read in the RF model
model <- readRDS("model.rds")


####################################
# User interface                   #
####################################

ui <- pageWithSidebar(
  
  # Page header
  headerPanel('Model Predictor'),
  
  # Input values
  sidebarPanel(
    #HTML("<h3>Input parameters</h3>"),
    tags$label(h3('Input parameters')),
    numericInput("xbox", 
                 label = "xbox", 
                 value = 2),
    numericInput("ybox", 
                 label = "ybox", 
                 value = 2),
    numericInput("width", 
                 label = "width", 
                 value = 2),
    numericInput("height", 
                 label = "height", 
                 value = 2),
    numericInput("onpix", 
                 label = "onpix", 
                 value = 2),
    numericInput("xbar", 
                 label = "xbar", 
                 value = 2),
    numericInput("ybar", 
                 label = "ybar", 
                 value = 2),
    numericInput("x2bar", 
                 label = "x2bar", 
                 value = 2),
    numericInput("y2bar", 
                 label = "y2bar", 
                 value = 2),
    numericInput("xybar", 
                 label = "xybar", 
                 value = 2),
    numericInput("x2ybar", 
                 label = "x2ybar", 
                 value = 2),
    numericInput("xy2bar", 
                 label = "xy2bar", 
                 value = 2),
    numericInput("xedge", 
                 label = "xedge", 
                 value = 2),
    numericInput("xedgey", 
                 label = "xedgey", 
                 value = 2),
    numericInput("yedge", 
                 label = "yedge", 
                 value = 2),
    numericInput("yedgex", 
                 label = "yedgex", 
                 value = 2),
    
    actionButton("submitbutton", "Submit", 
                 class = "btn btn-primary")
  ),
  
  mainPanel(
    tags$label(h3('Status/Output')), # Status/Output Text Box
    verbatimTextOutput('contents'),
    tableOutput('tabledata') # Prediction results table
    
  )
)

####################################
# Server                           #
####################################

server<- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    df <- data.frame(
      Name = c("xbox",
               "ybox",
               "width",
               "height",
               "onpix",
               "xbar",
               "ybar",
               "x2bar",
               "y2bar",
               "x2ybar",
               "xy2bar",
               "xybar",
               "xedge",
               "xedgey",
               "yedge",
               "yedgex"),
      Value = as.character(c(input$xbox,
                             input$ybox,
                             input$width,
                             input$height,
                             input$onpix,
                             input$xbar,
                             input$ybar,
                             input$x2bar,
                             input$y2bar,
                             input$x2ybar,
                             input$xy2bar,
                             input$xybar,
                             input$xedge,
                             input$xedgey,
                             input$yedge,
                             input$yedgex)),
      stringsAsFactors = FALSE)
    
    letter <- 0
    df <- rbind(df, letter)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    predictions <- predict(model,test)
    Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 3))
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)