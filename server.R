shinyServer(
  function(input, output) {
    
    output$text1 <- renderText({ 
      paste("you have selected: ", input$player)
    })
   #transtree(input$player) 
  }
)