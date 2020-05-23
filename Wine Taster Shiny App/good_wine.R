# Import libraries
library(shiny)
library(shinythemes)
library(data.table)
library(RCurl)
library(randomForest)

# Read data
wine <- read.csv(text = getURL("https://raw.githubusercontent.com/richardcsuwandi/datasets/master/wine.csv") )
wine_reduced <- wine[,c("fixed.acidity", "volatile.acidity", "chlorides", "alcohol", "pH", "citric.acid", "residual.sugar", "quality")]
# Build model
model <- randomForest(quality ~ ., data = wine_reduced, ntree = 500, mtry = 4, importance = TRUE)

# User Interface (UI)               
ui <- fluidPage(theme = shinytheme("united"),
  navbarPage(
    tabPanel('Good Wine?',
    
      # Input values
      sidebarPanel(
        HTML("<h3>Input Parameters</h3>"),
        
        sliderInput("fixed", "Alcohol:",
                    min = 8.4, max = 14.9,
                    value = 10.2),
        sliderInput("pH", "pH:",
                    min = 2.74, max = 4.01,
                    value = 3.31),
        sliderInput("citric.acid", "Citric Acid:",
                    min = 0.0, max = 1.0,
                    value = 0.25),
        sliderInput("alcohol", "Alcohol:",
                    min = 8.4, max = 14.9,
                    value = 10.2),
        sliderInput("pH", "pH:",
                    min = 2.74, max = 4.01,
                    value = 3.31),
        sliderInput("citric.acid", "Citric Acid:",
                    min = 0.0, max = 1.0,
                    value = 0.25),
        sliderInput("residual.sugar", "Residual Sugar:",
                    min = 0.9, max = 15.4,
                    value = 2.2),
        
        actionButton("submitbutton", "Submit", class = "btn btn-primary")
      ),
      
      mainPanel(
        tags$label(h3('Output')), # Output text Box
        verbatimTextOutput('contents'),
        tableOutput('tabledata') # Prediction results table
        
      )
    )
  )
)

# Server                           
server <- function(input, output, session) {

  # Input Data
  datasetInput <- reactive({  
    
  df <- data.frame(
    Name = c("alcohol",
             "pH",
             "citric.acid",
             "residual.sugar"),
    Value = as.character(c(input$alcohol,
                           input$pH,
                           input$citric.acid,
                           input$residual.sugar)),
    stringsAsFactors = FALSE)
  
  quality <- "quality"
  df <- rbind(df, quality)
  input <- transpose(df)
  write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
  
  test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
  
  Output <- data.frame(Quality = predict(model,test))
  print(Output)
  
  })
  
  # Output text box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Wine tasting complete.") 
    } else {
      return("Server is ready for wine tasting.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}


# Create the shiny app
shinyApp(ui = ui, server = server)
