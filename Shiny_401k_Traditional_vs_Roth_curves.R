  # necessary packages
  
  library(scales)
  library(shiny)
  library(tidyr)
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
        "yrs_contributing",
        "Years making contributions",
        value = 30,
        min = 1,
        max = 50
      ),
      
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
        "Years After Retirement",
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
                 
                 yrs_contributing <- input$yrs_contributing
                
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
                 
                 
                 # let's make it a function to plot over all years
                 
                 traditional_growth <-
                   function(withhold_401k,
                            income_base,
                            tax_savings,
                            ROI,
                            yrs_contributing,
                            yrs_growth) {
                     
                     compound_trad_contrib <-
                       
                       ((withhold_401k * income_base) + tax_savings) *
                       
                       (((1 + ROI) ^ yrs_contributing - 1) / ROI)
                     
                     compound_trad_mature <- 
                       
                       ((compound_trad_contrib * (1 + ROI) ^ (yrs_growth - yrs_contributing)) -
                       
                       compound_trad_contrib)
                     
                     compound_trad <-
                       
                       compound_trad_contrib + compound_trad_mature
                     
                     trad_save <- round(compound_trad)
                     
                     return(trad_save)
                     
                   }
                 
                 roth_growth <-
                   function(withhold_401k,
                            income_base,
                            tax_savings,
                            ROI,
                            yrs_contributing,
                            yrs_growth) {
                     
                     compound_roth_contrib <- (withhold_401k * income_base) *
                       
                       (((1 + ROI) ^ yrs_contributing - 1) / ROI)
                     
                     compound_roth_mature <- ((
                       
                       compound_roth_contrib *
                         
                         (1 + ROI) ^ (yrs_growth - yrs_contributing)
                     ) -
                       
                       compound_roth_contrib)
                     
                     compound_roth <-
                       
                       compound_roth_contrib + compound_roth_mature
                     
                     roth_save <- round(compound_roth)
                     
                     return(roth_save)
                     
                   }
                 
      
      # let's make a dataframe for all the years of growth pre-retirement
                 
                 df_retire <- data.frame(
                   
                   Years = c(1:yrs_growth)) %>% 
                   
                   mutate(
                     
                     Traditional = traditional_growth(withhold_401k, income_base, tax_savings, ROI, yrs_contributing, Years),
                     
                     Roth = roth_growth(withhold_401k, income_base, tax_savings, ROI, yrs_contributing, Years),
                     
                     Difference = Traditional - Roth
                     
                   ) %>% 
                   
                   pivot_longer(cols = Traditional:Difference,
                                
                                names_to = "Type",
                                
                                values_to = "Value")
                 
                 df_retire$Type <- factor(
                   
                   df_retire$Type,
                   
                   levels = c("Traditional",
                              
                              "Roth",
                              
                              "Difference")
                 )
                 
      # output plot 1 : accumulated growth
                 
                 output$savings_plot <- renderPlot({
                   
                   ggplot(df_retire,
                          
                          aes(x = Years,
                              
                              y = Value,
                              
                              color = Type)) +
                     
                     geom_line(linewidth = 2) +
                   
                   scale_y_continuous(labels = label_dollar()) +
                     
                     labs(title = "401k Approaching Retirement",
                          
                          y = "Amount") +
                     
                     scale_color_manual(values = c("forestgreen", "dodgerblue", "orange")) +
                     
                     theme_minimal() +
                     
                     theme(text = element_text(size=20),
                           
                           axis.title.y=element_blank())
                   
                 })
                
     # post retirement
                 
                 yrs_post_retire <- input$yrs_post_retire
                 
                 withdrawal <- input$withdrawal
                 
                 tax_withdrawal <- tax_total(withdrawal)
                 
                 trad_save <- traditional_growth(withhold_401k, income_base, tax_savings, ROI,
                                                 
                                                 yrs_contributing, 
                                                 
                                                 yrs_growth)
                 
                 roth_save <- roth_growth(withhold_401k, income_base, tax_savings, ROI,
                                                 
                                                 yrs_contributing,
                                                 
                                                 yrs_growth)
                 
                 # trad
                 
             traditional_withdraw <- function(ROI, yrs_post_retire, withdrawal, tax_withdrawal){
                 
                 withdraw_trad <- trad_save * (1 + ROI) ^ yrs_post_retire -
                   
                   ((withdrawal + tax_withdrawal) / ROI) *
                   
                   ((1 + ROI) ^ yrs_post_retire - 1)
                
                 return(withdraw_trad)
                  
               }
                 
                 
                 # roth
             
             roth_withdraw <- function(ROI, yrs_post_retire, withdrawal){
                 
                 withdraw_roth <- roth_save * (1 + ROI) ^ yrs_post_retire -
                   
                   (withdrawal / ROI) *
                   
                   ((1 + ROI) ^ yrs_post_retire - 1)
              
                 return(withdraw_roth)
                    
             }
                 
       # let's make a dataframe for the years after retirement
             
             df_withdraw <- data.frame(
               
               Years = c(1:yrs_post_retire)) %>% 
               
               mutate(
                 
                 Traditional = traditional_withdraw(ROI, Years, withdrawal, tax_withdrawal),
                 
                 Roth = roth_withdraw(ROI, Years, withdrawal),
                 
                 Difference = Traditional - Roth
                 
               ) %>% 
               
               pivot_longer(cols = Traditional:Difference,
                            
                            names_to = "Type",
                            
                            values_to = "Value")
             
             df_withdraw$Type <- factor(
               
               df_withdraw$Type,
               
               levels = c("Traditional",
                          
                          "Roth",
                          
                          "Difference")
             )
             
                 
                  
    # output plot 2 : retirement income

             output$income_plot <- renderPlot({
               
               ggplot(df_withdraw,
                      
                      aes(x = Years,
                          
                          y = Value,
                          
                          color = Type)) +
                 
                 geom_line(linewidth = 2) +
                 
                 scale_y_continuous(labels = label_dollar()) +
                 
                 labs(title = "401k After Retirement",
                      
                      y = "Amount") +
                 
                 scale_color_manual(values = c("forestgreen", "dodgerblue", "orange")) +
                 
                 theme_minimal() +
                 
                 theme(text = element_text(size=20),
                       
                       axis.title.y=element_blank())
               
             })
                 
               })
  
}

# run the app

shinyApp(ui = ui,
         
         server = server)
