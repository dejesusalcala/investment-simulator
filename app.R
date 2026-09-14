
# Homework 5: Investment Simulator Shiny App
# Angel De Jesus Alcala Ruiz

# Packages needed 

library(tidyverse)
library(cowplot)
library(shiny)
library(shinythemes)
library(shinyWidgets)

# Table of stock-bond composition

Stocks = seq(from = 0, to = 100, by = 5)
Bonds = seq(from = 100, to = 0, by = -5)
Avg_mu = c(0.0509,0.0540,0.0570,0.0600,0.0629,
           0.0657,0.0684,0.0711,0.0737,0.0762,
           0.0787,0.0811,0.0834,0.0856,0.0878,
           0.0899,0.0919,0.0938,0.0957,0.0974,0.0991)
SD_sigma = c(0.0403,0.0399,0.0411,0.0437,0.0474,
             0.0521,0.0574,0.0632,0.0694,0.0759,
             0.0825,0.0894,0.0964,0.1035,0.1107,
             0.1180,0.1254,0.1328,0.1403,0.1479,0.1555)

StockBondComp = data.frame(Stocks,Bonds,Avg_mu,SD_sigma)


# Our main function  

InvestmentSimulation = function(P = 1000,PMT = 360,n = 10,Portfolio_ratio = 40,Simulations = 50,Target_amount = 5000,Seed = 50000){
  #' @title InvestmentSimulation
  #' 
  #' @description This function computes an investment simulation
  #' 
  #' @params P (principal)
  #' @params PMT (periodic contributions)
  #' @params n (number of years)
  #' @params portfolio_ratio (this number refers to the percentage of stocks in the portfolio
  #'           e.g. portfolio_ratio = 40 means 40% stocks and 60% bonds)
  #' @params Simulations (number of years)
  #' @params Target_amount (the target amount that we'd like to reach given our parameters) 
  #' 
  #' @return a list that contains the following:
  #'          - data frame of simulations over n years 
  #'            with variable annual rates
  #'          - P
  #'          - PMT
  #'          - Simulations
  #'          - Target_amount
  
  # Stopping conditions
  
  
  if (Seed < 0) {
    stop("Random seed must be greater than 0")
  } else {
    Seed = round(Seed)
  }
  
  set.seed(Seed)
  
  if ((P < 0) | (PMT < 0) | (Target_amount < 0)) {
    stop("Initial amount and contribution must be greater than 0")
  }
  
  if (Target_amount < P) {
    stop("Target amount must be greater than the initial amount")
  }
  
  if ((P >= 10^(12)) | (PMT >= 10^(12)) | Target_amount >= 10^(12)) {
    stop("Initial amount, the yearly contribution, and the target amount must all be less than $1 trillion dollars")
  }
  
  k = 1
  YEAR = c(0:n)
  NON_INVEST = c(P)
  non_invest_amount = P
  
  # Get the average rate and sd from stock-bond table 
  
  r = as.numeric(filter(select(StockBondComp,3), Stocks == Portfolio_ratio))
  sigma = as.numeric(filter(select(StockBondComp,4), Stocks == Portfolio_ratio))
  
  # Create a column of non-investment
  
  for (i in 1:n) {
    non_invest_amount = non_invest_amount + PMT
    NON_INVEST = c(NON_INVEST, non_invest_amount)
  }
  
  Invest_Table = data.frame(YEAR,NON_INVEST)
  
  # The main algorithm for each simulation
  
  for (simulation in 1:Simulations){
    amount0 = P
    SIM = c(P)
    for (year in 1:n) {
      rate = rnorm(1,mean = r, sd = sigma)
      amount1 = amount0*(1 + rate) + PMT
      SIM = c(SIM,amount1)
      amount0 = amount1
    }
    Invest_Table = data.frame(Invest_Table,SIM)
  }
  
  # Compute the average and add the new column to Invest_Table
  
  Average = Invest_Table %>%
    select(3:(Simulations + 2)) %>%
    rowMeans()
  
  Invest_Table = data.frame(Invest_Table,Average)
  
  OutPut = lst(Invest_Table,P,PMT,n,Simulations,Target_amount)
  
  return(OutPut)
}

# My colors 

gray = "#cdcdcd"
dark_gray = "#4C4C4C"
pink = "#B00067"

light_pink1 = "#F8A5D9"
pink1 = "#EC0094"
orange1 = "#FF8B00"
blue1 = "#0492DF"
green1 = "#AFFA00"

pink3 = "#F25EBB"
pink4 = "#B90074"

ui = fluidPage(
    
    theme = shinytheme("cyborg"),
    titlePanel("Investment Simulator"),
    tags$head(
      tags$style(HTML("hr {border-top:1.8px solid #4c4c4c;}"))
    ),
    
    hr(),
    
    h3("Inputs"),
    
    fluidRow(
      setSliderColor(c(pink, pink, pink), c(1, 2, 3)),
        column(width = 2,
               numericInput(inputId = "initial_investment", 
                            label = "Initial investment",
                            value = 1000,
                            min = 0),
               numericInput(inputId = "periodic_contribution", 
                            label = "Yearly contribution",
                            value = 360),
               numericInput(inputId = "target_amount", 
                            label = "Target amount",
                            value = 5000)
                 ),
          column(width = 3,
                 sliderInput(inputId = "number_of_years",
                             label = "Input the number of years",
                             min = 1,
                             max = 40,
                             value = 10),
                 sliderInput(inputId = "stock_bond_ratio",
                             label = "Adjust the % of stocks in stock-bond ratio",
                             min = 0,
                             max = 100,
                             value = 60,
                             step = 5)
                 ),
          column(width = 3,
                 sliderInput(inputId = "number_of_simulations",
                             label = "Number of simulations",
                             min = 10,
                             max = 300,
                             value = 50,
                             step = 10),
                 numericInput(inputId = "set_seed", 
                          label = "Set the random seed",
                          value = 12345)
                 ),
          column(width = 3,
                 checkboxGroupInput(inputId = "lines_of_interest", 
                                    label = "Select lines of interest", 
                                    choices = list("Maximum line" = 1,
                                                   "Minimum line" = 2,
                                                   "Target amount" = 3,
                                                   "Average line" = 4,
                                                   "Average time to reach the target amount" = 5,
                                                   "Non-investment line" = 6),
                                    selected = c(1,2,3,4,5)),
          )
                 
        ),
    
    hr(),
    
    h3("Graphs"),

    plotOutput(outputId = "InvestmentPlot"),
    
    br(),
    
    plotOutput(outputId = "ProbabilityPlot"),
    
    hr(),
    
    h3("Summary Statistics"),
    
    fluidRow(
      column(width = 4,
             align = "center",
             h5("Summary Table 1"),
             p("Summary statistics of all simulations"),
             tableOutput(outputId = "table1")
      ),
      column(width = 4,
             align = "center",
             h5("Summary Table 2"),
             p("Simulations greater than non-investment amount"),
             tableOutput(outputId = "table2")
      ),
      column(width = 4,
             align = "center",
             h5("Summary Table 3"),
             p("Simulations greater than initial investment"),
             tableOutput(outputId = "table3")
             ),
    ),
    
    hr(),
    
    h3("Some Important Information"),
    
    h5("Stock-Bond Ratio"),
    
    textOutput(outputId = "Paragraph1"),
    
    h5("Non-Investment"),
    
    textOutput(outputId = "Paragraph2"),
    
    h5("Random Seed"),
    
    textOutput(outputId = "Paragraph3"),
    
    hr()
    
)


server <- function(input, output) {
  
  InvestmentLIST = reactive({InvestmentSimulation(P = input$initial_investment,
                                        PMT = input$periodic_contribution,
                                        n = input$number_of_years,
                                        Portfolio_ratio = input$stock_bond_ratio,
                                        Simulations = input$number_of_simulations,
                                        Target_amount = input$target_amount,
                                        Seed = input$set_seed)})

    output$InvestmentPlot = renderPlot({
      
        # Create InvestmentDF
      
        InvestmentDF = InvestmentLIST()$Invest_Table
        
        # Find the number of rows
        
        number_of_rows = nrow(InvestmentDF)
        
        # Change the name of the columns
        
        HeaderName = c("YEAR","NON_INVEST",paste0("SIM",1:(InvestmentLIST()$Simulations)),"AVERAGE")
        
        colnames(InvestmentDF) = HeaderName
        
        # Create pivoted InvestmentDF
        
        PivotedInvestmentDF = pivot_longer(InvestmentDF, 
                                           cols = starts_with("sim"),
                                           names_to = "SIMULATIONS", 
                                           values_to = "AMOUNT")
        
        # For graphing purposes
        #   - Find the column that contains max and min after the investment period
        
        col_with_max = which(InvestmentDF == max(InvestmentDF[number_of_rows,3:(InvestmentLIST()$Simulations + 2)]), arr.ind = TRUE)[2]
        col_with_min = which(InvestmentDF == min(InvestmentDF[number_of_rows,3:(InvestmentLIST()$Simulations + 2)]), arr.ind = TRUE)[2]
        
        # For graphing purposes
        #   - Find the quartiles at the end of the nth-year 
        
        probs_for_quantiles = c(.25,.5,.75)
        quantiles = quantile(InvestmentDF[number_of_rows,3:InvestmentLIST()$Simulations],probs_for_quantiles)
    
        # Find the intersection of the average line and the target amount using point-slope form
        
        for (j in 1:(length(InvestmentDF$AVERAGE) - 1)){
          
          if (InvestmentDF$AVERAGE[number_of_rows] < InvestmentLIST()$Target_amount) {
            stop("Target amount is not within reach")
          }
          
          if (between(InvestmentLIST()$Target_amount, InvestmentDF$AVERAGE[j],InvestmentDF$AVERAGE[j + 1])) {
            index1 = j
            index2 = j + 1
            break
          }
        }
        
        x1 = InvestmentDF$YEAR[index1]
        x2 = InvestmentDF$YEAR[index2]
        
        y1 = InvestmentDF$AVERAGE[index1]
        y2 = InvestmentDF$AVERAGE[index2]
        
        y_target = InvestmentLIST()$Target_amount
        m = (y2 - y1)/(x2 - x1)
      
        x_target = (y_target - y1)/m + x1 
        
        # Main graph with all the simulations
        
        InvestmentGraph = ggplot(data = NULL) +
          geom_line(data = PivotedInvestmentDF,
                    mapping = aes(x = YEAR, y = AMOUNT, group = SIMULATIONS),
                    color = gray,
                    alpha = .5,
                    size = .8) +
          labs(title = "Investment Simulation",
               subtitle = paste0(InvestmentLIST()$Simulations,
                                 " simulations with a ",
                                 as.numeric(filter(StockBondComp, Stocks == input$stock_bond_ratio)[1]),"%-",
                                 as.numeric(filter(StockBondComp, Stocks == input$stock_bond_ratio)[2]),"% stock-bond portfolio,",
                                 "\nan initial investment of $", format(round(InvestmentLIST()$P, digits = 2), big.mark = ",", scientific = FALSE),
                                 " and yearly contributions of $",format(round(InvestmentLIST()$PMT, digits = 2), big.mark = ",", scientific = FALSE)),
               x = "Year",
               y = "Balance in U.S. $")  +
          scale_x_continuous(breaks = seq(0,InvestmentLIST()$n,by = 1)) +
          theme(plot.title = element_text(size = 22,
                                          color = gray),
                plot.subtitle = element_text(size = 18,
                                             color = gray),
                plot.margin = unit(c(.5,.5,2.6,.5), "cm"),
                axis.ticks = element_blank(),
                axis.line = element_line(color = gray),
                panel.background = element_rect(fill = "black"),
                plot.background = element_rect(fill = "black"),
                panel.grid.major.y = element_line(color = dark_gray,
                                                  size = .2,
                                                  linetype = "solid"),
                panel.grid.minor.y = element_blank(),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                axis.text = element_text(size = 14, color = gray),
                axis.title.y = element_text(size = 14, color = gray),
                axis.title.x = element_text(size = 14, color = gray)) +
          coord_cartesian(xlim = c(0,InvestmentLIST()$n), ylim = c(0,max(InvestmentDF)), clip = "off")
        
        # Graph the lines of interest
        
        if (1 %in% input$lines_of_interest){
          InvestmentGraph = InvestmentGraph +
              geom_line(data = InvestmentDF,
                        mapping = aes(x = YEAR, y = InvestmentDF[,col_with_max]),
                        color = blue1,
                        size = 1.5,
                        alpha = 1) 
          } else {
          InvestmentGraph
        }
          
       if (2 %in% input$lines_of_interest) {
          InvestmentGraph = InvestmentGraph +
            geom_line(data = InvestmentDF, 
                      mapping = aes(x = YEAR, y = InvestmentDF[,col_with_min]),
                      color = orange1, 
                      size = 1.5,
                      alpha = 1)
          } else {
          InvestmentGraph
          }
        
        if (3 %in% input$lines_of_interest) {
          InvestmentGraph = InvestmentGraph + 
            geom_hline(yintercept = InvestmentLIST()$Target_amount,
                      color = light_pink1,
                      size = 1.5,
                      alpha = 1,
                      linetype = "dashed")
        } else {
          InvestmentGraph
        }
        
        if (4 %in% input$lines_of_interest){
          InvestmentGraph = InvestmentGraph + 
            geom_line(data = InvestmentDF,
                      mapping = aes(x = YEAR, y = AVERAGE),
                      size = 1.8,
                      alpha = 1,
                      color = pink1,
                      linetype = "solid")
        } else {
          InvestmentGraph
        }
        
        if (5 %in% input$lines_of_interest) {
          InvestmentGraph = InvestmentGraph + 
            geom_vline(xintercept = x_target,
                       color = light_pink1,
                       size = 1.5,
                       alpha = 1,
                       linetype = "dashed")
        } else {
          InvestmentGraph
        }
        
        if (6 %in% input$lines_of_interest) {
          InvestmentGraph = InvestmentGraph + 
            geom_line(data = InvestmentDF,
                      mapping = aes(x = YEAR, y = NON_INVEST),
                      color = green1,
                      size = 1.5,
                      alpha = 1,
                      linetype = "solid")
        } else {
          InvestmentGraph
        }
        
        if ((3 %in% input$lines_of_interest) & (4 %in% input$lines_of_interest) & (5 %in% input$lines_of_interest)) {
          InvestmentGraph = InvestmentGraph + 
            geom_point(data = NULL, 
                       mapping = aes(x = x_target, y = InvestmentLIST()$Target_amount),
                       size = 10,
                       color = pink1,
                       alpha = .4)
        } else {
          InvestmentGraph
        }
        
        # Annotations at the bottom of the graph
        
        InvestmentGraph = ggdraw(InvestmentGraph) +
          draw_label("Statistics at the end of the investment period:", x = .1325, y = .15, size = 13, color = gray, fontface = "bold") +
          draw_label("Time to reach target", x = .07, y = .1, color = light_pink1, fontface = "bold") +
          draw_label("Average amount", x = .21, y = .1, size = 13, color = pink1, fontface = "bold") +
          draw_label("Non-investment", x = .35, y = .1, color = green1, fontface = "bold") +
          draw_label("0th %ile (Min)", x = .48, y = .1, size = 13, color = orange1, fontface = "bold") +
          draw_label("25th %ile", x = .59, y = .1, size = 13, color = gray, fontface = "bold") +
          draw_label("50th %ile", x = .70, y = .1, size = 13, color = gray, fontface = "bold") +
          draw_label("75th %ile", x = .81, y = .1, size = 13, color = gray, fontface = "bold") + 
          draw_label("100th %ile (Max)", x = .93, y = .1, size = 13, color = blue1, fontface = "bold") +
          draw_label(paste(round(x_target,digits = 2)," years"), 
                     x = .07, 
                     y = .05,
                     color = light_pink1,
                     fontface = "bold") +
          draw_label(paste0("$",format(round(last(InvestmentDF$AVERAGE), digits = 2),big.mark=",",scientific=FALSE)),
                     x = .21,
                     y = .05,
                     size = 13,
                     color = pink1,
                     fontface = "bold") +
          draw_label(paste0("$", format(round(InvestmentLIST()$P + (InvestmentLIST()$PMT)*(InvestmentLIST()$n), digits = 2),big.mark = ",",scientific = FALSE)),
                     x = .35,
                     y = .05,
                     size = 13,
                     color = green1,
                     fontface = "bold") +
          draw_label(paste0("$",format(round(min(InvestmentDF[number_of_rows,3:InvestmentLIST()$Simulations]), digits = 2),big.mark = ",",scientific = FALSE)),
                     x = .48,
                     y = .05,
                     size = 13,
                     color = orange1,
                     fontface = "bold") +
          draw_label(paste0("$",format(round(quantiles[[1]], digits = 2),big.mark = ",",scientific = FALSE)), 
                     x = .59,
                     y = .05,
                     size = 13, 
                     color = gray,
                     fontface = "bold") +
          draw_label(paste0("$",format(round(quantiles[[2]], digits = 2),big.mark = ",",scientific = FALSE)),
                     x = .70,
                     y = .05,
                     size = 13,
                     color = gray,
                     fontface = "bold") +
          draw_label(paste0("$",format(round(quantiles[[3]], digits = 2),big.mark = ",",scientific = FALSE)),
                     x = .81,
                     y = .05,
                     size = 13,
                     color = gray,
                     fontface = "bold") + 
          draw_label(paste0("$",format(round(max(InvestmentDF[number_of_rows,3:InvestmentLIST()$Simulations]), digits = 2),big.mark=",",scientific=FALSE)),
                     x = .93,
                     y = .05,
                     size = 13,
                     color = blue1,
                     fontface = "bold")
        
        InvestmentGraph
        })
    
    output$ProbabilityPlot = renderPlot({
      
      InvestmentDF = InvestmentLIST()$Invest_Table
      
      # Find the intersection of the average line and the target amount using point-slope form
      
      number_of_rows = nrow(InvestmentDF)
      
      # Change the name of the columns
      
      HeaderName = c("YEAR","NON_INVEST",paste0("SIM",1:(InvestmentLIST()$Simulations)),"AVERAGE")
      
      colnames(InvestmentDF) = HeaderName
      
      for (j in 1:(length(InvestmentDF$AVERAGE) - 1)) {
        
        if (InvestmentDF$AVERAGE[number_of_rows] < InvestmentLIST()$Target_amount) {
          stop("Target amount is not within reach")
        }
        
        if (between(InvestmentLIST()$Target_amount, InvestmentDF$AVERAGE[j],InvestmentDF$AVERAGE[j + 1])) {
          index1 = j
          index2 = j + 1
          break
        }
      }
      
      x1 = InvestmentDF$YEAR[index1]
      x2 = InvestmentDF$YEAR[index2]
      
      y1 = InvestmentDF$AVERAGE[index1]
      y2 = InvestmentDF$AVERAGE[index2]
      
      y_target = InvestmentLIST()$Target_amount
      m = (y2 - y1)/(x2 - x1)
      
      x_target = (y_target - y1)/m + x1
      
      lower_x_target = floor(x_target)
      upper_x_target = ceiling(x_target)
      
      YEAR = 0:InvestmentLIST()$n
      PROPORTIONS = rep(0,InvestmentLIST()$n + 1)
      
      for (row in 1:(InvestmentLIST()$n + 1)) {
        
        amounts_in_year = slice(select(InvestmentDF,3:(InvestmentLIST()$Simulations + 2)),row)
        count = sum(amounts_in_year >= InvestmentLIST()$Target_amount)
        prob = count/(InvestmentLIST()$Simulations)
        
        PROPORTIONS[row] = prob 
      }
      
      ProbabilityDF = data.frame(YEAR,PROPORTIONS)
      
      ProbabilityGraph = ggplot(data = ProbabilityDF) +
        geom_col(aes(x = YEAR,
                     y = PROPORTIONS), 
                 fill = gray) +
        geom_col(data = filter(ProbabilityDF,YEAR == lower_x_target),
                 aes(x = YEAR,
                     y = PROPORTIONS),
                 fill = pink3) +
        geom_col(data = filter(ProbabilityDF,YEAR == upper_x_target),
                 aes(x = YEAR,
                     y = PROPORTIONS),
                 fill = pink4) +
        labs(title = "Probability of Reaching Target Amount",
             subtitle = paste0("Based on the average of ", 
                               InvestmentLIST()$Simulations," simulations and the target being $",
                               format(round(InvestmentLIST()$Target_amount, digits = 2), big.mark = ",", scientific = FALSE)),
             x = "Year",
             y = "Probability") + 
        scale_x_discrete(limits = c(0:InvestmentLIST()$n)) +
        scale_y_continuous(limits = c(0,1),
                           labels = c("0%","25%","50%","75%","100%")) +
        theme(plot.title = element_text(size = 22,
                                        color = gray),
              plot.subtitle = element_text(size = 18,
                                           color = gray),
              plot.margin = unit(c(.5,.5,2.6,.5), "cm"),
              axis.ticks = element_blank(),
              axis.line = element_line(color = gray),
              panel.background = element_rect(fill = "black"),
              plot.background = element_rect(fill = "black"),
              panel.grid.major.y = element_line(color = dark_gray,
                                                size = .2,
                                                linetype = "solid"),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              axis.text = element_text(size = 14, color = gray),
              axis.title.y = element_text(size = 14, color = gray),
              axis.title.x = element_text(size = 14, color = gray))
      
      ProbabilityGraph = ggdraw(ProbabilityGraph) +
        draw_label(paste0("Probability at year ",
                          lower_x_target," is about ",
                          round((ProbabilityDF$PROPORTIONS[index1])*100, digits = 0),"%"),
                   x = .16,
                   y = .15,
                   size = 16,
                   color = pink3,
                   fontface = "bold") + 
        draw_label(paste0("Probability at year ",
                          upper_x_target," is about ",
                          round((ProbabilityDF$PROPORTIONS[index2])*100, digits = 0),"%"),
                   x = .16,
                   y = .08,
                   size = 16,
                   color = pink4,
                   fontface = "bold")
      
      ProbabilityGraph
        
    })
    
    output$table1 = renderTable({
      
      InvestmentDF = InvestmentLIST()$Invest_Table
      
      # Change the name of the columns
      
      HeaderName = c("YEAR","NON_INVEST",paste0("SIM",1:(InvestmentLIST()$Simulations)),"AVERAGE")
      colnames(InvestmentDF) = HeaderName
      
      # Create pivoted InvestmentDF
      
      PivotedInvestmentDF = pivot_longer(InvestmentDF, 
                                         cols = starts_with("sim"),
                                         names_to = "SIMULATIONS", 
                                         values_to = "AMOUNT")
      # Create Summary Table 1
      
      PivotedInvestmentDF %>%
        group_by(YEAR) %>%
        summarise(MINIMUM = paste0("$", format(round(min(AMOUNT), digits = 2), big.mark = ",", scientific = FALSE)),
                  MAXIMUM = paste0("$", format(round(max(AMOUNT), digits = 2), big.mark = ",", scientific = FALSE)),
                  AVERAGE = paste0("$", format(round(mean(AMOUNT), digits = 2), big.mark = ",", scientific = FALSE))
        )
    })
    
    output$table2 = renderTable({
      
      InvestmentDF = InvestmentLIST()$Invest_Table
      
      # Change the name of the columns
      
      HeaderName = c("YEAR","NON_INVEST",paste0("SIM",1:(InvestmentLIST()$Simulations)),"AVERAGE")
      colnames(InvestmentDF) = HeaderName
      
      # Create pivoted InvestmentDF
      
      PivotedInvestmentDF = pivot_longer(InvestmentDF, 
                                         cols = starts_with("sim"),
                                         names_to = "SIMULATIONS", 
                                         values_to = "AMOUNT")
      
      # Create Summary Table 2
      
      PivotedInvestmentDF %>% 
        group_by(YEAR) %>%
        summarise(
          COUNT = sum(AMOUNT > NON_INVEST),
          PROPORTION = paste0(round((COUNT / InvestmentLIST()$Simulations)*100,digits = 1),"%")
          )
    })
    
    output$table3 = renderTable({
      
      InvestmentDF = InvestmentLIST()$Invest_Table
      
      # Change the name of the columns
      
      HeaderName = c("YEAR","NON_INVEST",paste0("SIM",1:(InvestmentLIST()$Simulations)),"AVERAGE")
      colnames(InvestmentDF) = HeaderName
      
      # Create pivoted InvestmentDF
      
      PivotedInvestmentDF = pivot_longer(InvestmentDF, 
                                         cols = starts_with("sim"),
                                         names_to = "SIMULATIONS", 
                                         values_to = "AMOUNT")
      
      PivotedInvestmentDF %>% 
        group_by(YEAR) %>%
        summarise(
          COUNT = sum(AMOUNT > InvestmentLIST()$P),
          PROPORTION = paste0(round((COUNT / InvestmentLIST()$Simulations)*100,digits = 1),"%")
        )
      
      
    })
    
    output$Paragraph1 = renderText({
      
      text1 = "
      The above simulations are generated by an algorithm that uses random 
      annual rates. These rates mostly depend on the ratio of the stock-bond
      portfolio. For example, a 40%-60% stock-bond portfolio is an investment
      portfolio with 40% stocks and 60% bonds. In addition, a 40%-60% stock-bond
      portfolio will have an average annual rate of about 7.3% with a standard 
      deviation of about 6.9%. In general the higher percentage of stocks in the
      stock-bond portfolio will mean that both the annual average rate and the 
      standard deviation will be higher. Therefore a 20%-80% stock-bond
      portfolio will grow at a more consistent rate over time as opposed to an
      80%-20% stock-bond portfolio because the standard deviation will be lower 
      on the 20%-80% portfolio. On the contrary, this also means that the growth
      on the 20%-80% portfolio will be much slower, and it may mean that it will 
      take longer to reach a specific target amount than the 80%-20% portfolio.
      " 
      
      text1
    })
    
    output$Paragraph2 = renderText({
      
      text2 = "
      The non-investment line refers to the case in that there's 
      no investment into a stock-bond portfolio. This results in a linear
      growth that begins with the initial amount and then each year there's 
      a contribution that's added to the previous amount.
      "
      
      text2
    })
    
    output$Paragraph3 = renderText({
      
      text3 = "
      The random seed is generally used for reproducibility purposes. What this
      means is that the same random numbers can be generated while using the 
      same seed and the same arguments as before. In addition, two people on
      different computers can generate the same random numbers if they both set 
      the same seed and are using the same arguments in their functions. For the 
      purposes of this application the user can expect the same graphs and 
      summary statistics if they keep all of the arguments the same. If the user
      wants to generate different graphs with different statistics while 
      keeping all of the other arguments the same, then the seed can be changed 
      to a different positive integer.
      "
      
      text3
    })
}

# Run the application 

shinyApp(ui = ui, server = server)
