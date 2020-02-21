source("utils.R")

server <- function(input, output, session) {
  # Tests de normalite (Kolmorogorov, Shapiro)
  normCheck <- reactiveVal(FALSE);
  normality <- reactive( {
    # Ne pas afficher le warning: ties should not be present for the Kolmogorov-Smirnov test
    values <- getData()
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
  }) 
   
  errorMsg <-reactiveVal(NULL); 
  
  getData <- reactive({
      tryCatch(
        {
          data <- callModule(csvFile, "myFile")
          errorMsg(NULL)
          return(data)            
        },
          error = function(e) {
            errorMsg(paste("Error:", e$message))
            return(NULL)
          },
          warning = function(w) {
            errorMsg(paste("Warning:", w$message))
            return(NULL)
          }
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

      if (!is.null(mydata)) computeStats(mydata, input$alpha);
    },
    rownames = TRUE, colnames = FALSE, spacing = c("l"), digits=2, align = "?",
    caption="<span class='tableTitle'>Descriptive statistics</span>",
    caption.placement = getOption("xtable.caption.placement", "top")
  )

  # # Histogramme
  output$histo <- renderPlot({
    mydata = getData()
    if (!is.null(mydata)) {
      hist(mydata, col = "#75AADB", border = "white", main="Distribution", xlab="Values")
    }
  })

  # QQplot
  output$qqplot <- renderPlot({
    mydata = getData()
    if (is.null(mydata)) return(NULL)
    qqnorm(mydata, frame=FALSE)
    qqline(mydata, col="#75AADB", lwd=2)
  })

  # Tests de normalite
  output$norm <- renderTable(
    {
      mydata = getData()
      if (!is.null(mydata)) {
        normality()
      }
    },
    rownames = FALSE, colnames = TRUE, digits=4,
    caption="<span class='tableTitle'>Tests for Normality</span>",
    caption.placement = getOption("xtable.caption.placement", "top")
  )

  # Ccl tests normalite
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
      selectInput("alternative", label="Alternative hypothesis (H1)",
                  choices=c("less then "="less", "greater then"="greater", "2-sided"="two.sided"),
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
