# necessary packages

library(scales)
library(shiny)
library(ggplot2)
library(dplyr)

# function for finding progressive tax by income
# fed + state (CA)

tax_total <- function(income) {
  
  # fed
  
  fed_tax <- case_when(
    
    income <= 11000                  ~  income * 0.10,
    income > 11000 & income <= 44725 ~ ((income - 11000) * 0.12) + 1100,
    income > 44725 & income <= 95736 ~ ((income - 44725) * 0.22) + 5367,
    income > 95736                   ~ ((income - 95736) * 0.24) + 20983
    
  )
  
  fed_tax
  
  # state
  
  ca_tax <- case_when(
    
    income <= 10099                  ~  income * 0.01,
    income > 10100 & income <= 23942 ~ ((income - 10100) * 0.02) + 101,
    income > 23942 & income <= 37788 ~ ((income - 23942) * 0.04) + 378,
    income > 37799 & income <= 52455 ~ ((income - 37788) * 0.06) + 932,
    income > 52465 & income <= 66295 ~ ((income - 52455) * 0.08) + 1912,
    income > 66296                   ~ ((income - 66295) * 0.093) + 2919,
    
  )
  
  ca_tax
  
  
  # combined
  
  tax_total <- fed_tax + ca_tax
  
  return(tax_total)
}


# ui for inputs

ui <- fluidPage(
  titlePanel("401(k) Predictor: Traditional vs Roth"),
  sidebarLayout(
    sidebarPanel(
      
      sliderInput(
        "yrs_pre_retire",
        "Years until retirement",
        value = 30,
        min = 1,
        max = 50
      ),
      
      sliderInput(
        "income_base",
        "Annual income",
        value = 100000,
        min = round(23000 * 4/3),
        max = 250000
      ),
      
      sliderInput(
        "withhold_401k",
        "401k Contribution %",
        value = 20,
        min = 1,
        max = 50
      ),
      
      sliderInput(
        "withhold_benefits",
        "Benefits Contribution %",
        value = 5,
        min = 1,
        max = 10
      ),
      
      sliderInput(
        "percent_growth",
        "Average Stock Growth %",
        value = 7,
        min = 0,
        max = 15
      ),
      
      sliderInput(
        "yrs_post_retire",
        "Years After Retirement (no other income)",
        value = 15,
        min = 1,
        max = 40
      ),
      
      sliderInput(
        "withdrawal",
        "Annual Withdrawal",
        value = 100000,
        min = 0,
        max = 250000
      ),
      
      actionButton("calculate", "Calculate")
      
    ),
    
    mainPanel(plotOutput("savings_plot"),
              plotOutput("income_plot"))
  )
)

# server for reactive logic

server <- function(input, output) {
  
  observeEvent(input$calculate,
               
               {
                 
     # calculations, variables
                 
                 # calculate annual growth 
                 
                 ROI <- input$percent_growth / 100
                 
                 yrs_growth <- input$yrs_pre_retire
                
                 # income
                  
                 income_base <- input$income_base
                 
                 # encoding tax rates
                 
                 withhold_401k <- input$withhold_401k / 100
                 
                 benefits <- input$withhold_benefits / 100
                 
                 
                 income_trad <- income_base * (1 - (withhold_401k + benefits))
                 
                 income_roth <- income_base * (1 - (benefits))
                 
                 
                 # Calculate taxes paid for both incomes
                 
                 roth_income <- tax_total(income_roth)
                 
                 trad_income <- tax_total(income_trad)
                 
                 
                 tax_savings <- roth_income - trad_income
                 
                 
                 # how much will tax savings generate in extra investment growth? 
                 
                 compound_trad <- ((withhold_401k * income_base) + tax_savings) *
                   
                   (((1 + ROI) ^ yrs_growth - 1) / ROI)
                 
                 trad_save <- round(compound_trad)
                 
                 trad_save
                 
                 
                 compound_roth <- (withhold_401k * income_base) *
                   
                   (((1 + ROI) ^ yrs_growth - 1) / ROI)
                 
                 roth_save <- round(compound_roth)
                 
                 roth_save
                 
                 
      # output plot 1 : accumulated growth
                 
                 output$savings_plot <- renderPlot({
                   
                   df1 <- data.frame(
                     
                     Description = c("Traditional",
                                     
                                     "Roth",
                                     
                                     "Difference"),
                     
                     Amount = c(
                       
                       trad_save,
                       
                       roth_save,
                       
                       trad_save - roth_save
                     )
                   )
                   
                   # specify order as factor
                   
                   df1$Description <- factor(
                     
                     df1$Description,
                     
                     levels = c("Traditional",
                                
                                "Roth",
                                
                                "Difference")
                   )
                   
                   ggplot(df1,
                          
                          aes(x = Description,
                              
                              y = Amount,
                              
                              fill = Description)) +
                     
                     geom_bar(stat = "identity") +
                     
                     geom_text(aes(label = label_dollar()(Amount)),
                               
                               vjust = 1.5,
                               
                               size = 6) +
                   
                   scale_y_continuous(#breaks = seq(0, (max(df1$Amount) + 500000), by = 250000),
                                      
                                      labels = label_dollar()) +
                     
                     labs(title = "401k At Retirement",
                          
                          y = "Amount") +
                     
                     scale_fill_manual(values = c("forestgreen", "dodgerblue", "orange")) +
                     
                     # scale_y_continuous() +
                     
                     theme_classic() +
                     
                     theme(legend.position = "none") +
                     
                     theme(text = element_text(size=20),
                           
                           axis.title.x=element_blank(),
                           
                           axis.title.y=element_blank())
                   
                 })
                
     # post retirement
                 
                 yrs_post_retire <- input$yrs_post_retire
                 
                 withdrawal <- input$withdrawal
                 
                 tax_withdrawal <- tax_total(withdrawal)
                 
                 # roth
                 
                 withdraw_roth <- roth_save * (1 + ROI) ^ yrs_post_retire -
                   
                   (withdrawal / ROI) *
                   
                   ((1 + ROI) ^ yrs_post_retire - 1)
                 
                 
                 # trad
                 
                 withdraw_trad <- trad_save * (1 + ROI) ^ yrs_post_retire -
                   
                   ((withdrawal + (tax_withdrawal)) / ROI) *
                   
                   ((1 + ROI) ^ yrs_post_retire - 1)
                 
                  
    # output plot 2 : retirement income

                 output$income_plot <- renderPlot({
                   
                   df2 <- data.frame(
                     
                     Description = c("Traditional",

                                     "Roth",
                                     
                                     "Difference"),

                     Amount = c(

                       withdraw_trad,

                       withdraw_roth,
                       
                       withdraw_trad - withdraw_roth
                     )
                   )

                   # specify order as factor

                   df2$Description <- factor(

                     df2$Description,

                     levels = c("Traditional",

                                "Roth",
                                
                                "Difference")
                   )

                   ggplot(df2,

                          aes(x = Description,

                              y = Amount,

                              fill = Description)) +

                     geom_bar(stat = "identity") +

                     geom_text(aes(label = label_dollar()(Amount)),
                               
                               vjust = 1.5,
                               
                               size = 6) +

                     scale_y_continuous(#breaks = seq(0, (max(df2$Amount)+ 500000), by = 250000),

                                        labels = label_dollar()) +

                     labs(title = "401k After Retirement",

                          y = "Amount") +

                     scale_fill_manual(values = c("forestgreen", "dodgerblue", "orange")) +

                     # scale_y_continuous() +

                     theme_classic() +

                     theme(legend.position = "none") +
                     
                     theme(text = element_text(size=20),
                           
                           axis.title.x=element_blank(),
                           
                           axis.title.y=element_blank())

                 })
                 
               })
  
}

# run the app

shinyApp(ui = ui,
         
         server = server)
