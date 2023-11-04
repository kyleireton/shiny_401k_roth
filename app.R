# necessary packages

library(shiny)
library(ggplot2)
library(dplyr)

# ui for inputs


ui <- fluidPage(
  titlePanel("Roth 401(k) Paycheck Predictor"),
  sidebarLayout(
    sidebarPanel(
      numericInput(
        "salary",
        "Current Salary",
        value = 100000,
        min = 0,
        max = 1000000
      ),
      
      sliderInput(
        "roth_contribution",
        "Roth 401(k) Contribution Rate",
        value = 15,
        min = 0,
        max = 100
      ),
      
      sliderInput(
        "income_tax",
        "Income Tax before 401(k) Contribution",
        value = 33,
        min = 0,
        max = 100
      ),
      
      numericInput(
        "pay_periods",
        "Paychecks per year (usually 12 vs 26)",
        value = 12,
        min = 12,
        max = 26
      ),
      
      actionButton("calculate", "Calculate")
      
    ),
    
    mainPanel(plotOutput("paycheck_plot"),
              plotOutput("annual_totals_plot"))
  )
)

# server for reactive logic

server <- function(input, output) {
  observeEvent(input$calculate,
               
               {
                 salary <- input$salary
                 
                 roth_rate <- input$roth_contribution / 100
                 
                 roth_contribution_annual <- salary * roth_rate
                 
                 pay_periods <- input$pay_periods
                 
                 roth_contribution_paycheck <-
                   round(roth_contribution_annual / pay_periods)
                 
                 income_tax <- input$income_tax / 100
                 
                 annual_pay <- salary * (1 - income_tax)
                 
                 paycheck_max <- round(annual_pay / pay_periods)
                 
                 paycheck_final <-
                   round(paycheck_max - roth_contribution_paycheck)
                 
                 output$paycheck_plot <- renderPlot({
                   df <- data.frame(
                     Description = c("Net Paycheck",
                                     
                                     "Roth 401(k) Contribution",
                                     
                                     "Max paycheck"),
                     
                     Amount = c(
                       paycheck_final,
                       
                       roth_contribution_paycheck,
                       
                       paycheck_max
                     )
                   )
                   
                   # specify order as factor
                   
                   df$Description <- factor(
                     df$Description,
                     
                     levels = c("Net Paycheck",
                                
                                "Roth 401(k) Contribution",
                                
                                "Max paycheck")
                   )
                   
                   ggplot(df,
                          
                          aes(x = Description,
                              
                              y = Amount,
                              
                              fill = Description)) +
                     
                     geom_bar(stat = "identity") +
                     
                     geom_text(aes(label = Amount), nudge_y = 250) +
                     
                     labs(title = "Paycheck Breakdown",
                          
                          y = "Amount") +
                     
                     scale_fill_manual(values = c("forestgreen", "blue", "dodgerblue")) +
                     
                     scale_y_continuous(breaks = seq(0, paycheck_max, by = 500)) +
                     
                     theme_classic() +
                     
                     theme(legend.position = "none")
                   
                 })
                 
                 
                 output$annual_totals_plot <- renderPlot({
                   df <- data.frame(
                     Description = c("Net Pay",
                                     
                                     "Roth 401(k) Contribution",
                                     
                                     "Max Pay"),
                     
                     Amount = c(
                       paycheck_final*pay_periods,
                       
                       roth_contribution_annual,
                       
                       annual_pay
                     )
                   )
                   
                   # specify order as factor
                   
                   df$Description <- factor(
                     df$Description,
                     
                     levels = c("Net Pay",
                                
                                "Roth 401(k) Contribution",
                                
                                "Max Pay")
                   )
                   
                   ggplot(df,
                          
                          aes(x = Description,
                              
                              y = Amount,
                              
                              fill = Description)) +
                     
                     geom_bar(stat = "identity") +
                     
                     geom_text(aes(label = Amount), nudge_y = 2500) +
                     
                     labs(title = "Annual Breakdown",
                          
                          y = "Amount") +
                     
                     scale_fill_manual(values = c("forestgreen", "blue", "dodgerblue")) +
                     
                     scale_y_continuous(breaks = seq(0, paycheck_max*pay_periods, by = 10000)) +
                     
                     theme_classic() +
                     
                     theme(legend.position = "none")
                   
                 })
                 
               })
}

# run the app

shinyApp(ui = ui,
         
         server = server)
