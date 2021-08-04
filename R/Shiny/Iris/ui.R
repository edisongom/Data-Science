shinyUI(pageWithSidebar(
    headerPanel("Data Iris"),
    sidebarPanel(
        helpText("El iris es un tipo de flor. Se ha construido un modelo predictivo simple para determinar las
        especies de iris (setosa, versicolor o virginica) observando solo las dimensiones del sepalo y el petalo.
        Con los controles deslizantes puede elegir el valor para cada dimension y el modelo predecira con 
        precision que especie de iris es "),
        sliderInput('sl', 'Sepal Length (cm)',value = 6.1, min = 4.3, max = 7.9, step = 0.05,),
        sliderInput('sw', 'Sepal Width (cm)',value = 3.2, min = 2, max = 4.4, step = 0.05,),
        sliderInput('pl', 'Petal Length (cm)',value = 3.95, min = 1, max = 6.9, step = 0.05,),
        sliderInput('pw', 'Petal Width (cm)',value = 1.3, min = 0.1, max = 2.5, step = 0.05,)
    ),

    mainPanel(
        h4('Los inputs que ingresaste:'),
        verbatimTextOutput("inputs"),
        h4('El tipo de iris que el modelo predice:'),
        verbatimTextOutput("prediction"),
        h4('Detalles acerca del modelo:'),
        p("Se utilizo Random Forest para construir el modelo predictivo y se ve que la precision es de
          alrededor del 96% con muy pocas clasificaciones erroneas."),
        verbatimTextOutput("model")
    )
))