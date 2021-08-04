#library(shiny)

shinyServer(function(input, output) {

  output$single_slider_io=renderPrint({
    print(input$slider_input)
    #print(str(input$slider_input))
  })
  output$range_slider_io=renderPrint({
    input$range_input
  })
  output$numeric_io=renderPrint({
    input$numeric_input
  })
  output$date_io=renderPrint({
    input$date_input
  })
  output$range_date_io=renderPrint({
    input$date_range_input
  })
  output$chk_box_io=renderPrint({
    input$single_chk_box
  })
  output$group_chk_box_io=renderPrint({
    input$group_chk_box
  })
  output$radio_button_io=renderPrint({
    input$radio_input
  })
  output$Action_button_io=renderPrint({
    input$action_input
  })
})
