# Loading required libraries
library(shiny)
library(shinythemes)
library(shinyjs)

# Setting the path
path <- "D:/Studies/Materials/Second-cycle/I year/I semester/Coding for DS and DM/R/r-project"

# Loading the fitted model
fit.reduced <- readRDS("fit.reduced.rds")

# Shiny App
ui <- fluidPage(
  theme = shinytheme("lumen"),
  useShinyjs(),
  tags$head(
    # To disable the up-down arrow in a numeric input
  tags$style(HTML(".shiny-input-container input[type=number]::-webkit-inner-spin-button,
  .shiny-input-container input[type=number]::-webkit-outer-spin-button {
  -webkit-appearance: none;
  margin: 0;}"))),
  titlePanel("Default Prediction App"),
  sidebarLayout(
    sidebarPanel(
      h4("Enter Customer Information"),
      numericInput("age", "Age:", 25),
      selectInput("ed", "Education:", c(
        "Did not complete high school",
        "High school degree",
        "Some college",
        "College degree",
        "Post-undergraduate degree"
        )),
      numericInput("employ", "Years with Current Employer:", 2),
      numericInput("address", "Years at Current Address:", 3),
      numericInput("income", "Household Income (in thousands):", 50),
      numericInput("debtinc", "Debt to Income Ratio (in percentage):", 20),
      numericInput("creddebt", "Credit Card Debt (in thousands):", 5),
      numericInput("othdebt", "Other Debt (in thousands):", 10),
      actionButton("predictBtn", "Predict")
      ),
    mainPanel(
      h4("Prediction Result"),
      textOutput("predictionText"),
      textOutput("probabilityText")
      )
    )
  )

# Function to validate input fields
validateInput <- function(inputValue, minValue, maxValue, message) {
  if (inputValue < minValue || inputValue > maxValue) {
    showNotification(message, type = "warning")
    return(TRUE)
    }
  return(FALSE)
  }

server <- function(input, output) {
  observeEvent(input$predictBtn, {
    if(
      validateInput(input$age, 18, 100, "Age must be between 18 and 100.") ||
      validateInput(input$employ, 0, Inf, "Years with current employer must be non-negative.") ||
      validateInput(input$address, 0, Inf, "Years at current address must be non-negative.") ||
      validateInput(input$income, 0, Inf, "Household income must be non-negative.") ||
      validateInput(input$debtinc, 0, Inf, "Debt to income ratio must be non-negative.") ||
      validateInput(input$creddebt, 0, Inf, "Credit card debt must be non-negative.") ||
      validateInput(input$othdebt, 0, Inf, "Other debt must be non-negative.")
      ){
      return(NULL)
      }
    
    new_customer <- data.frame(
      age = input$age,
      ed_2 = ifelse(input$ed == "High school degree", 1, 0),
      ed_3 = ifelse(input$ed == "Some college", 1, 0),
      ed_4 = ifelse(input$ed == "College degree", 1, 0),
      ed_5 = ifelse(input$ed == "Post-undergraduate degree", 1, 0),
      employ = input$employ,
      address = input$address,
      income = input$income,
      debtinc = input$debtinc,
      creddebt = input$creddebt,
      othdebt = input$othdebt
      )
    prediction <- predict(fit.reduced, newdata = new_customer, type = "response")

    output$predictionText <- renderText({
      ifelse(round(prediction) == 1, "Prediction: Will Default", "Prediction: Will Not Default")
      })

    output$probabilityText <- renderText({
      paste("Probability of Default:", round(prediction, 3))
    })
    # Scroll to the top using shinyjs
    shinyjs::runjs('window.scrollTo(0,0);')
  })
}

shinyApp(ui, server)