library(shiny)

# Custom CSS
style <- function() {
  return(
    tags$style(HTML(
      "hr { border-top: 1px solid grey; } 
      .tableTitle { font-weight:bold; color:black; }
      .pCrit {font-weight:bold; color:red; }
      .result {padding:10px; width:90%; text-align:center;}"
    ))
  )
}

side <- sidebarLayout(
  sidebarPanel(
    fileInput("myFile", "Choose CSV File",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
    ),
    tags$hr(),    # Draw line
    checkboxInput("header", "Header", TRUE),  # Check if file has header
    radioButtons("separator", "Separator",
                 choices = c(Comma = ",",
                             Semicolon = ";",
                             Tab = "\t"),
                 selected = ";"),
    radioButtons("quote", "Quote",           # Select separator to delimit strings
                 choices = c(None = "",
                             "Double Quote" = '"',
                             "Single Quote" = "'"),
                 selected = '"'),
    selectInput("columnId", label="Select column to import", choices=seq_len(50), width="50%", selected="2")
  ),
  mainPanel(
    fluidRow(
      htmlOutput("error")
    )
  )  
)

ui <- fluidPage(style(), side)

server <- function(input, output) {
  observeEvent(input$myFile, {
    print(paste("New file upload:",input$myFile$datapath ))
  }, once=FALSE) 
  
  getData <- reactive({
    inFile = input$myFile
    
    data = read.csv(inFile$datapath, sep = input$separator, header = input$header, quote = input$quote)
    val <- unlist(data[strtoi(input$columnId)])
  })
  
}

shinyApp(ui, server)