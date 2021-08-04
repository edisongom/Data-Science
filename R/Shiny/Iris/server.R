
library(randomForest)
data(iris)
set.seed(12345)
modFit <- randomForest(Species ~., data=iris)

shinyServer(
    function(input, output) {
        
        output$inputs <- renderPrint({
            sl <- input$sl
            sw <- input$sw
            pl <- input$pl
            pw <- input$pw
            
            dataframe <- data.frame(Sepal.Length = sl, Sepal.Width = sw, Petal.Length = pl, Petal.Width = pw)
            dataframe})
        
        output$prediction <- renderPrint({
            sl <- input$sl
            sw <- input$sw
            pl <- input$pl
            pw <- input$pw
            
            dataframe <- data.frame(Sepal.Length = sl, Sepal.Width = sw, Petal.Length = pl, Petal.Width = pw)
            
            as.character(predict(modFit, dataframe))            
            })
        
        output$model <- renderPrint({
            modFit
        })
        
        
    }
)
