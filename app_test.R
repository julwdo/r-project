# Loading required library
library(shiny)
library(shinythemes)
library(shinyjs)

# Shiny App
ui <- fluidPage(
  useShinyjs(),
  theme = shinytheme("flatly"),
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
      numericInput("debtinc", "Debt to Income Ratio (x100):", 20),
      numericInput("creddebt", "Credit Card Debt (in thousands):", 5),
      numericInput("othdebt", "Other Debt (in thousands):", 10),
      actionButton("predictBtn", "Predict")
    ),
    mainPanel(
      h4("Prediction Result"),
      textOutput("predictionText"),
      textOutput("probabilityText"),
      shinyjs::hidden(
        div(id = "loading_spinner", class = "shiny-progress"),
        h5("Loading...", align = "center")
      )
    )
  )
)

server <- function(input, output) {
  observeEvent(input$predictBtn, {
    # Prepare input data for prediction
    new_customer <- data.frame(
      age = input$age,
      ed_2 = as.factor(ifelse(input$ed == "High school degree", 1, 0)),
      employ = input$employ,
      address = input$address,
      income = input$income,
      debtinc = input$debtinc,
      creddebt = input$creddebt,
      othdebt = input$othdebt
    )
    
    # Make predictions
    prediction <- predict(lr.reduced, newdata = new_customer, type = "response")
    
    # Display prediction
    output$predictionText <- renderText({
      if (prediction > 0.5) {
        return("Prediction: Will Default")
      } else {
        return("Prediction: Will Not Default")
      }
    })
    
    # Display probability
    output$probabilityText <- renderText({
      paste("Probability of Default:", round(prediction, 3))
    })
  })
}

shinyApp(ui, server)

