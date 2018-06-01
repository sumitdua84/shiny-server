#**********************************************************************************************************
# Project      :  R ShinyApp Demo                                                                       
# Name         :  app.R
# Description  :  
# Input        :  
#               
# Usage notes  :  1. Install R packages if they do not exist in the environment
#                 2. Setup working directory in the beginning of the program
#                 3. Setup UI
#                 4. Pre Process Data
#                 
#                 
#
#**********************************************************************************************************
# Programmer   :  Sumit Dua
# Date         :  31 May 2018 
# Change Log   :  
# Text         :  
#**********************************************************************************************************

# STEP 1 - Install Packages - Only if necessary

#install.packages("shiny")
#install.packages("haven")

library(shiny)
library(haven)
#**********************************************************************************************************

# STEP 2 - Setup working directory

#setwd("K:\\data")
setwd("C:\\Users\\Sumit Dua\\Desktop\\Desktop\\shinyapp")
#**********************************************************************************************************


# STEP 3 - Setup UI


ui <- fluidPage(
  
  # App title ----
  titlePanel("KIT AHS Survey"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for variable to plot against contraceptive prevalence rate ----
      selectInput("variable", "Variable:",
                  c("Province" = "cyl",
                    "Sex" = "am",
                    "Age" = "gear")),
      
      # Input: Checkbox for whether outliers should be included ----
      checkboxInput("outliers", "Show outliers", TRUE)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption")),
      
      # Output: Plot of the requested variable against mpg ----
      plotOutput("mpgPlot")
      
    )
  )
)

#**********************************************************************************************************
#Step 4 - Pre process Data 

#fert <- read.dta("4 women 2018-02-08.dta")
#
#myvars <- c("q501", "lc", "q507a", "q507b", "q507c", "q507d", "q507e", "q507f", "q507g", "q507h", "q507xother")
#
#fert_1 <- fert[myvars]
#
#colnames(fert_1)[colnames(fert_1) == 'q501']      <- 'preg'
#colnames(fert_1)[colnames(fert_1) == 'lc']        <- 'urban_rural'
#colnames(fert_1)[colnames(fert_1) == 'q507a']     <- 'femster'
#colnames(fert_1)[colnames(fert_1) == 'q507b']     <- 'iud'
#colnames(fert_1)[colnames(fert_1) == 'q507c']     <- 'pill'
#colnames(fert_1)[colnames(fert_1) == 'q507d']     <- 'injection	'
#colnames(fert_1)[colnames(fert_1) == 'q507e']     <- 'condom	'
#colnames(fert_1)[colnames(fert_1) == 'q507f']     <- 'lact_amen	'
#colnames(fert_1)[colnames(fert_1) == 'q507g']     <- 'per_abst	'
#colnames(fert_1)[colnames(fert_1) == 'q507h']     <- 'withdrawal'
#colnames(fert_1)[colnames(fert_1) == 'q507i']     <- 'other'
#

#**********************************************************************************************************








	



#**********************************************************************************************************

mpgData <- mtcars
mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))


# Define server logic to plot various variables against COntraceptive Prevalence Rate ----
server <- function(input, output) {
  
  # Compute the formula text ----
  # This is in a reactive expression since it is shared by the
  # output$caption and output$mpgPlot functions
  formulaText <- reactive({
    paste("mpg ~", input$variable)
  })
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  
  # Generate a plot of the requested variable against mpg ----
  # and only exclude outliers if requested
  output$mpgPlot <- renderPlot({
    boxplot(as.formula(formulaText()),
            data = mpgData,
            outline = input$outliers,
            col = "#75AADB", pch = 19)
  })
  
}



shinyApp(ui, server)




























