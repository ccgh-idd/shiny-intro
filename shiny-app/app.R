# A very basic shiny app layout

library(shiny)

ui <- fluidPage(
  titlePanel("Simple Shiny App"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("x", "X:", min = 1, max = 10, value = 5),
      sliderInput("y", "Y:", min = 1, max = 10, value = 5)
    ),
    mainPanel(
      h2("Product:"),
      verbatimTextOutput("product")
    )
  )
)

server <- function(input, output, session) {
  output$product <- renderText({
    input$x * input$y
  })
}

shinyApp(ui, server)
