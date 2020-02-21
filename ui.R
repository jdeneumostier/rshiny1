# Custom CSS
# style <- function() {
#   return(
#     tags$style(HTML(
#       "hr { border-top: 1px solid grey; } 
#       .tableTitle { font-weight:bold; color:black; }
#       .pCrit {font-weight:bold; color:red; }
#       .result {padding:10px; width:90%; text-align:center;}"
#     ))
#   )
# }

side <- sidebarLayout(
  sidebarPanel(
    csvFileInput("myFile", chooseCol=TRUE, defaultSelectedCol=2),  # loaded from moduleCSV.R
    tags$hr(),    # Draw line
    sliderInput("alpha", label=HTML("Significance Level (&alpha;)"), min=0, max=0.3, step=0.01, value=0.05),
    tags$hr(),
    htmlOutput("t_test")
  ),
  mainPanel(
    fluidRow(
      htmlOutput("error")
    ),
    fluidRow(
      column(
        width = 6,
        plotOutput("histo")
      ),
      column(
        width = 6,
        plotOutput("qqplot")
      )
    ),
    fluidRow(
      column(
        width = 6,
        tableOutput("stats")
      ),
      column(
        width = 6,
        fluidRow(
          tableOutput("norm"),
          htmlOutput("norm_result")
        )
      )
    ),
    fluidRow(
      htmlOutput("t_test_result")
    )
  )  
)

#ui <- fluidPage(style(), side)
ui <- fluidPage(theme = "custom.css", tags$script(src = "test.js"), side)

