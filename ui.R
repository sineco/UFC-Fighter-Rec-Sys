library(shiny)


shinyUI(
  pageWithSidebar(
    # Application title
    headerPanel("UFC Fighter Recommendation System"),
    sidebarPanel(
       uiOutput("choose_fighter"),
       actionButton('goButton', 'Go!'),
       p("Click the button to recommendation of similar fighters to follow.")
       
    ),
    
    mainPanel(
    
      h4('You entered'),
      verbatimTextOutput('fighter_out'),
      
      h4('Information about the selected fighter '),
      tableOutput('view'),
      
      h4('Fighter record'),
      tableOutput('fighter_record'),
      
      h3('Results of Recommendation'),
      h4('Based on your favorite fighter, you may also like these fighters'),
      tableOutput('view_recommended')
      
   
      
    )
  )
)