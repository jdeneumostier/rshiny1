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
    selectInput("columnId", label="Select column to import", choices=seq_len(50), width="50%", selected="2"),
    tags$hr(),    # Draw line
    sliderInput("alpha", label=HTML("Significance Level (&alpha;)"), min=0, max=0.3, step=0.01, value=0.05),
    #numericInput("alpha", "alpha value", 0.05, step=0.01, min=0.01, max=0.3, width="50%"),
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
ui <- fluidPage(theme = "custom.css", side)

server <- function(input, output) {
  # Calcul des stats descriptives
  stats <- reactiveValues()
  computeStats <- function(values, alpha) {
    n <- length(values)
    m <- mean(values)
    md <- median(values)
    std <- sd(values)
    stderr <- std/sqrt(length(values))
    
    ci = c();
    ci[1] <- (m - qt(1-alpha/2, n-1) * std/sqrt(n))
    ci[2] <- (m + qt(1-alpha/2, n-1) * std/sqrt(n))
    
    stats <- c(n, m, md, std, stderr, ci[1], ci[2])
    names(stats) <- c("N", "Mean", "Median", "Standard deviation", "Standard error", 
                      paste("Lower ",1-alpha,"% CL for Mean", sep=""), 
                      paste("Upper ",1-alpha,"% CL for Mean", sep=""))
    stats  
  }
  
  # Tests de normalité (Kolmorogorov, Shapiro)
  normCheck <- reactiveVal(FALSE);
  normality <- function(values) {
    # Ne pas afficher le warning: ties should not be present for the Kolmogorov-Smirnov test
    kTest = suppressWarnings(ks.test(values, "pnorm", mean=mean(values), sd=sd(values)))
    sTest = shapiro.test(values)
    
    normCheck(kTest$p.value > input$alpha && sTest$p.value > input$alpha)

    results <- data.frame(
      "test" = c("Kolmogorov-Smirnov", "Shapiro-Wilk"),
      "statistic" = c(paste("D = ",round(kTest$statistic, 5)), paste("W = ",round(sTest$statistic, 5)) ),
      "pvalue" = c(kTest$p.value, sTest$p.value)
    )
    names(results) <- c("Test", "Statistic", "P value")
    return(results)
  }  
  
  errorMsg <-reactiveVal(NULL);
  
  # Load CSV file
  getData <- reactive({
    inFile <- input$myFile
    if (is.null(inFile)) return(NULL)
    validate(
      tryCatch(
        {
          errorMsg(NULL)
          data = read.csv(inFile$datapath, sep = input$separator, header = input$header, quote = input$quote)
          val <- unlist(data[strtoi(input$columnId)])
          if (class(val) != "numeric") {
            stop("Header or column ID issue")
            return(null)
          }
        return(val)
        },
        error = function(e) {
          errorMsg(paste("Error: ", e$message))
          return(NULL)
        },
        warning = function(w) {
          errorMsg(w$message)
          return(NULL)
        }
      )
    )
  })
  
  output$error <- renderUI(
    if (!is.null(errorMsg())) {
      p(errorMsg(), class="bg-danger text-danger result")
    }
  )

  # Affichage des stats
  output$stats <- renderTable(
    {
      mydata = getData()
      
      if (is.null(mydata)) return(NULL)
      computeStats(mydata, input$alpha);               
    }, 
    rownames = TRUE, colnames = FALSE, spacing = c("l"), digits=2, align = "?",
    caption="<span class='tableTitle'>Descriptive statistics</span>", 
    caption.placement = getOption("xtable.caption.placement", "top")
  )
  
  # Histogramme
  output$histo <- renderPlot({
    mydata = getData()
    if (is.null(mydata)) return(NULL)
    hist(mydata, col = "#75AADB", border = "white", main="Distribution", xlab="Values")
  })
  
  # QQplot
  output$qqplot <- renderPlot({
    mydata = getData()
    if (is.null(mydata)) return(NULL)
    qqnorm(mydata, frame=FALSE)
    qqline(mydata, col="#75AADB", lwd=2)    
  })

  # Tests de normalité
  output$norm <- renderTable(
    {
      mydata = getData()
      if (is.null(mydata)) return(NULL)    
      normality(mydata)
    },
    rownames = FALSE, colnames = TRUE, digits=4,
    caption="<span class='tableTitle'>Tests for Normality</span>", 
    caption.placement = getOption("xtable.caption.placement", "top")
  )
  
  # Ccl tests normalité
  output$norm_result <- renderUI({
    mydata = getData()
    if (is.null(mydata)) return(NULL) 

    if (normCheck()) {
      p("According to the tests, data is normally distributed", class="bg-success text-success result")      
    } else {
      p("According to the tests, data is not normally distributed", class="bg-danger text-danger result")    
    }
  })
  
  # T-test form
  output$t_test <- renderUI({
    mydata = getData()
    if (is.null(mydata)) return(NULL)

    tagList(
      tags$h4(HTML(paste("T-test for Mean (&alpha;=",input$alpha,")",sep=""))),
      numericInput("m0", "Hypothesized Mean (H0)", value=isolate(input$m0), width="50%"),
      selectInput("alternative", label="Alernative hypothesis (H1)", 
                  choices=c("lower"="less", "greater"="greater", "2-sided"="two.sided"), 
                  selected=isolate(input$alternative)),
      actionButton("test_mean", label="Proceed")  
    )
  })
  
  # Ajout d'un listener sur le bouton "test_mean"
  observeEvent(input$test_mean, {
    mydata = getData()
    if (is.null(mydata)) return(NULL)
    result <- t.test(mydata, mu=input$m0, alternative=input$alternative, conf.level=1-input$alpha)
    if (result$p.value < 0.0001) {
      result$p.value = "< 0.0001"
    } else {
      result$p.value = toString(round(result$p.value, 4))
    }
    tTest = data.frame(
      "df" = result$parameter,
      "statistic" = result$statistic,
      "pvalue" = result$p.value
    )
    names(tTest) = c("DF", "T Statistic", "P value")

    output$t_test_result <- renderTable({
      if (is.null(errorMsg())) {
        tTest
      }
      else return()     
    },
      caption="<span class='tableTitle'>Test Mean</span>", 
      caption.placement = getOption("xtable.caption.placement", "top")
    )
  })
}



shinyApp(ui, server)



