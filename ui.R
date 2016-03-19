shinyUI(fluidPage(
  titlePanel("Player Transaction Vis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Input a player who's transaction tree you want to see"),
      
      textInput("player", "Player:", "Please use a BBRef ID"),
      submitButton("Submit")
    ),
    
    mainPanel(
      textOutput("text1")
    )
  )
))