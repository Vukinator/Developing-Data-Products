library(shiny)

ui= fluidPage(
  titlePanel("Predicting the stopping distance with speed for 1920's cars."),
  sidebarLayout(
    sidebarPanel(
      h3("Slide me!"),
      sliderInput("sliderSpeed","Speed(mph)", 0, 60, value = 15)
    ),
    mainPanel(
      plotOutput("plot"),
      h3("Stopping distance prediction based on speed."),
      textOutput("pred")
  )
))

server= function(input, output){
  
  data= datasets::cars
  model= lm(dist~speed, data)
  pred= reactive({
    speedInput= input$sliderSpeed
    predict(model, newdata= data.frame(speed= speedInput))
  })
  output$plot= renderPlot({
    speedInput= input$sliderSpeed
    plot(data$speed, data$dist,
         xlab= "MPH", ylab="Distance (ft)",
         bty= "n", xlim= c(0,200), ylim= c(0,200))
    abline(model, col= "blue", lwd= 1)
    points(speedInput, pred(), col= "red", pch=19)
  })
  output$predDist= renderText({
    pred()
  })
}
shinyApp(ui= ui, server= server)