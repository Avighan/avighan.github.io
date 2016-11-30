library(shiny)
library(ggplot2)
require(graphics)
data <- mtcars

server <- 
shinyServer(function(input, output) {
  myXY <- reactive({
    paste("mpg ~", "as.integer(", input$x,")")
  })
  
  myFit <- reactive({
    lm(as.formula(myXY()),data=data)
  })
  
  output$summary <- renderPrint({
    predictor <- input$x
    if(predictor == "cyl")
      summary(mtcars$cyl)
    else if(predictor == "disp")
      summary(mtcars$disp)
    else if(predictor == "hp")
      summary(mtcars$hp)
    else if(predictor == "drat")
      summary(mtcars$drat)
    else if(predictor == "wt")
      summary(mtcars$wt)
    else if(predictor == "qsec")
      summary(mtcars$qsec)
    else if(predictor == "vs")
      summary(mtcars$vs)
    else if(predictor == "am")
      summary(mtcars$am)
    else if(predictor == "gear")
      summary(mtcars$gear)
    else if(predictor == "carb")
      summary(mtcars$carb)
    else if(predictor == "mpg")
      summary(mtcars$mpg)
    
  })
  
  
  output$text <- renderText({
    paste("Regression Model:", "mpg ~", input$x)
  })
  
  output$myPlot <- renderPlot ({
    with(data, {plot(as.formula(myXY()),xlab=input$x,ylab="mpg")
      abline(myFit(), col=input$color)
    })
  })
})



ui <- 

shinyUI(fluidPage(
  headerPanel(img(src = "ddp.jpg"),
              h2("Developing Data Products: Courserea Project") ),
  
  # description and input
  sidebarPanel(
    h3('Input Panel'),
    p('Here we can play with mtcars dataset.'),
    p('Select the predictor variable with mpg as outcome:'),
    selectInput('x', label='Predictor', selected='cyl', choices=names(mtcars)),
    p('It is possible to choose the line color.'),
    selectInput('color', label='Color', choices=palette()),
    h3('Documentation'),
    p("This project uses the 'mtcars' dataset to plot the",
      "regression model using two variables of this dataset:",
      "one as predictor, what we have to choose, and the",
      "other is 'mpg' as the outcome."),
    
    p("It is simple to use and when the predictor is selected",
      "the plot is updated putting the new fit.")),
  
  # plot and output
  mainPanel(
    div(h2("Developing Data Products: Coursera Project", style="color:#ff6600;margin:-65px 0px 10px 20px;")),
    h4('Summary of Selected Attribute'),
    h4(verbatimTextOutput("summary")),
    h4(textOutput('text')),
    plotOutput('myPlot'))
  
))
shinyApp(ui = ui, server = server)

