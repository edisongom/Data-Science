library(shiny)
library(dygraphs)
library(datasets)
#install.packages("dygraphs")
ui <- fluidPage(
  
  titlePanel("Muertes previstas por enfermedad pulmonar (UK)"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("months", label = "Meses a prediccion", 
                   value = 72, min = 12, max = 144, step = 12),
      selectInput("interval", label = "Intervalo de prediccion",
                  choices = c("0.80", "0.90", "0.95", "0.99"),
                  selected = "0.95"),
      checkboxInput("showgrid", label = "Mostrar Grid", value = TRUE),
      hr(),
      div(strong("Desde: "), textOutput("from", inline = TRUE)),
      div(strong("Hasta: "), textOutput("to", inline = TRUE)),
      div(strong("Fecha Seleccionada: "), textOutput("clicked", inline = TRUE)),
      div(strong("Punto mas cercano seleccionado: "), textOutput("point", inline = TRUE)),
      br(),
      helpText("Haga clic y arrastre para acercar (haga doble clic para alejar).")
    ),
    mainPanel(
      dygraphOutput("dygraph")
    )
  )
)
server <- function(input, output) {
  
  predicted <- reactive({
    hw <- HoltWinters(ldeaths)
    predict(hw, n.ahead = input$months, 
            prediction.interval = TRUE,
            level = as.numeric(input$interval))
  })
  
  output$dygraph <- renderDygraph({
    dygraph(predicted(), main = "Prediccion muertes/meses") %>%
      dySeries(c("lwr", "fit", "upr"), label = "Deaths") %>%
      dyOptions(drawGrid = input$showgrid)
  })
  
  output$from <- renderText({
    strftime(req(input$dygraph_date_window[[1]]), "%d %b %Y")      
  })
  
  output$to <- renderText({
    strftime(req(input$dygraph_date_window[[2]]), "%d %b %Y")
  })
  
  output$clicked <- renderText({
    strftime(req(input$dygraph_click$x), "%d %b %Y")
  })
  
  output$point <- renderText({
    paste0('X = ', strftime(req(input$dygraph_click$x_closest_point), "%d %b %Y"), 
           '; Y = ', req(input$dygraph_click$y_closest_point))
  })
}
# Run the application 
shinyApp(ui = ui, server = server)

