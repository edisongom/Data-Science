library(shiny)
library(lubridate)
#setwd("C:/Users/ediso/Downloads/Notebooks/R/shiny1/Shiny_ejemplo")

shinyUI(fluidPage(

    titlePanel("Catálogo de inputs de Shiny"),

    sidebarLayout(
        sidebarPanel(
            sliderInput("slider_input","Seleccione un valor:",
                        min=0,
                        max=100,
                        value=25,
                        animate = TRUE,
                        step = 1,
                        post = "%"),
            sliderInput("range_input","Seleccione un rango",
                        min = 0,
                        max = 100,
                        value = c(0,100)),
            numericInput("numeric_input","Seleccione un número",
                         value = 10,
                         min = 0,
                         max = 100),
            dateInput("date_input","Seleccione Fecha",
                      language = "es",
                      value = today(),
                      min = today()-365,
                      max = today(),
                      weekstart = 1),
            dateRangeInput("date_range_input", "Seleccione un rango",
                           language = "es",
                           start=today()-30,
                           end = today(),separator = "a"),
            checkboxInput("single_chk_box","Desea recibir información:",
                          value = FALSE),
            checkboxGroupInput("group_chk_box", "Seleccione tipo:",
                               choices = c("Carro","Tractor","Bici","Moto"),
                               selected = NULL),
            radioButtons("radio_input","Seleccione una:",
                         choices = c("Básica","Media","Universitaria"),
                         selected = NULL),
            actionButton("action_input","Siguiente"),
            actionButton("action_salir","Salir"),
            submitButton("Ejecutar")
      ),

        mainPanel(
            h1("Salidas de los inputs de Shiny"),
            h2("Slider IO"),
            verbatimTextOutput("single_slider_io"),
            h2("Slider con rango"),
            verbatimTextOutput("range_slider_io"),
            h2("Salida Numeric Input"),
            verbatimTextOutput("numeric_io"),
            h2("Salida Date Input"),
            verbatimTextOutput("date_io"),
            h2("Salida Check Box"),
            verbatimTextOutput("chk_box_io"),
            h2("Salida Check Box Grupos"),
            verbatimTextOutput("group_chk_box_io"),
            h2("Salida Action button"),
            verbatimTextOutput("Action_button_io")
                    )
    )
))
