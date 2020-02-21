# Module UI function
csvFileInput <- function(id, label = "Choose CSV File", chooseCol=FALSE, defaultSelectedCol="1") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    fileInput(ns("file"), label,
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
    ),
    tags$hr(), # Draw line
    checkboxInput(ns("header"), "Header", TRUE),  # Check if file has header
    radioButtons(ns("separator"), "Separator",
                 choices = c(Comma = ",",
                             Semicolon = ";",
                             Tab = "\t"),
                 selected = ";"),
    radioButtons(ns("quote"), "Quote",           # Select separator to delimit strings
                 choices = c(None = "",
                             "Double Quote" = '"',
                             "Single Quote" = "'"),
                 selected = '"'),
    buildColSelect(ns, chooseCol, defaultSelectedCol)  
  )
}

buildColSelect <- function(ns, chooseCol, defaultSelectedCol) {
  if (chooseCol) {
    return(
      selectInput(ns("columnId"), label="Select column to import", choices=seq_len(50), width="50%", selected=defaultSelectedCol)
    )   
  }
}

# Module server function
csvFile <- function(input, output, session) {
  inFile <- reactiveVal({
    # If no file is selected, don't do anything
    # validate(need(input$file, message = NULL))
    input$file
  })

  if (is.null(inFile())) return(NULL)

  data = read.csv(inFile()$datapath, sep = input$separator, header = input$header, quote = input$quote, fileEncoding="latin1")
  
  if (is.null(input$columnId)) {
    return(data)
  } 
  val <- unlist(data[strtoi(input$columnId)])
  if (class(val) != "numeric") {
    stop("Header or column ID issue")
  }
  return(val)

}


