
# Title: Investment Simulations
# Description: A Shiny App that simulates investments based on monthly or yearly periodic contributions. 
# Author: Melisa Esqueda 
# Date: 4/14/2021

library(dplyr)
library(ggplot2)
library(reshape2)


ui <- fluidPage(
    
    titlePanel("Investment Simulations"),
    fluidRow(
        column(3,
               sliderInput(inputId = "Initial", label = "Initial Investment Amount ($)", min = 1, 
                           max = 15000, value = 1000, step = 100, pre = '$', sep = ','),
               sliderInput(inputId = "Years", label = "Number of Years", min = 0, 
                           max = 100, value = 10, step = 1, sep = ',')),
        
        column(3,
               sliderInput(inputId = "P_amount", label = "Amount of Periodic Contributions", min = 1, 
                           max = 1000, value = 360, step = 1, sep = ','), 
               radioButtons(inputId = "Type", label = "Type of Periodic Contribution",
                            choices = list("At The End of Each Month" = 1, 
                                           "At The End of Each Year" = 2), 
                            selected = 2)
        ),
        
        column(3,
               sliderInput(inputId = "Rate", label = "Average Annual Return Rate (%)", min = 0, 
                           max = 30, value = 10, step = 1),
               sliderInput(inputId = "Volatility", label = "Average Annual Volatility", min = 1, 
                           max = 70, value = 18, step = 1, sep = ',')
        ),
        
        column(3,
               sliderInput(inputId = "Simulations", label = "Number of Simulations", min = 1, 
                           max = 1000, value = 50, step = 1, sep = ','),
               numericInput(inputId = "Seed", label = "Random Seed Value", value = 12345)
        )
    ),
    mainPanel(
    hr(),
    h4("Invesment's Future Values Plot"),
    plotOutput('distPlot'),
    
    hr(),
    h4("Numeric Summary of Investment's Future Value"),
    verbatimTextOutput('tables'),
    
    hr(),
    h4("Quantile Values"),
    verbatimTextOutput('table_2')

))

server <- function(input, output) {
    
    result <- reactive({
      set.seed(input$Seed)
      n = input$Years
      p = input$Initial
      repetitions = input$Simulations
      pmt = input$P_amount
      rate_mean  = input$Rate/100
      rate_sd = input$Volatility/100
        
        #Define periods based on type
        k = 1
        if (input$Type == "At The End of Each Month"){
            k = 12
        }
        #Set matrix
        tbl = matrix(p, nrow = n*k+1, ncol = repetitions)
        
        for (reps in 1:repetitions){
        amt = rep(p, n*k+1)
        step = 0
        
        for (period in 1:(n*k)){
            step = step + 1
            if (step == 1 | step %% (k+1) == 0){
                rate = rnorm(1, mean = rate_mean, sd = rate_sd)
            }
            amt[period+1] = input$P_amount + amt[period]*(1+ rate/k)
        }
        tbl[ ,reps] = amt
        }
        complete_tbl= melt(tbl, varnames = c('n', 'repetitions'))
        complete_tbl
       
    })
  

    # code for graph
    output$distPlot <- renderPlot({
      repetitions = input$Simulations
      years = input$Years
      plot_invest =  ggplot(data = result(), aes(x = n, y = value, group = repetitions, color = repetitions)) +
        geom_line() + labs(title = "Investment Simulations")  + geom_line(data = result(), aes(x = n, y = input$Initial + (input$P_amount*n)), color = "violet") + 
        geom_line(data = result() %>% group_by(n) %>% summarize(ninety = quantile(value, probs = 0.90)), aes(x = n, y = ninety), size  = 1.3, color = "red") + 
        geom_line(data = result() %>% group_by(n) %>% summarize(ten = quantile(value, probs = 0.10)), aes(x = n, y = ten), size  = 1.3, color = "orange")
        
      
      
      if (input$Type == "At The End of Each Month"){
        plot_invest + xlab("Months")
      } else{
        plot_invest + xlab("Years")
      }
      
    })
    
    
    
    
    # code for statistics
    output$table_2 <- renderPrint({
      #Quantiles
      mean_value = mean(result()$value)
      ten_percent = quantile(result()$value, probs = 0.10)
      twentyfive_percent = quantile(result()$value, probs = 0.25)
      fifty_percent = quantile(result()$value, probs = 0.50)
      seventyfive_percent = quantile(result()$value, probs = 0.75)
      ninety_percent = quantile(result()$value, probs = 0.90)
      quants = data.frame(mean_value, ten_percent, twentyfive_percent, fifty_percent, seventyfive_percent, ninety_percent)
      rownames(quants) = c("")
        print(quants, print.gap = 2)
    })
    #Data
    output$tables = renderPrint({
      print(summary(result()), print.gap = 2)
    })
    
}

shinyApp(ui = ui, server = server)

