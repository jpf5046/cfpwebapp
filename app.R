

#

# This is a Shiny web application. You can run the application by clicking

# the 'Run App' button above.

#

# Find out more about building applications with Shiny here:

#

#    http://shiny.rstudio.com/

#



library(shiny)



# Define UI for application that draws a histogram

ui <- fluidPage(
  
  titlePanel("CFP Possible Outcomes"),
  
  
  
  # Create a new Row in the UI for selectInputs
  
  fluidRow(
    
    column(4,
           
           selectInput("secchamp",
                       
                       "SEC Champion:",
                       
                       c("Select", "Alabama","Florida"))
           
    ),
    
    column(4,
           
           selectInput("BigTenchamp",
                       
                       "Big Ten Champion:",
                       
                       c("Select","Wisconsin", "Penn State"))
           
    ),
    
    column(4,
           
           selectInput("ACCchamp",
                       
                       "ACC Champion:",
                       
                       c("Select","Clemson", "Virginia Tech"))
           
    ),
    
    column(4,
           
           selectInput("bigtwelvechamp",
                       
                       "Big 12 Champion:",
                       
                       c("Select","Oklahoma", "Oklahoma State"))
           
    ),
    
    column(4,
           
           selectInput("pactwelvechamp",
                       
                       "Pac 12 Champion:",
                       
                       c("Select","Washington", "Colorado"))
           
    )
    
  ),
  
  # Create a new row for the table.
  
  fluidRow(
    
    dataTableOutput("table"),
    
    textOutput("text1")
    
  )
  
)



# Define server logic required to draw a histogram

server <- function(input, output) {
  
  
  
  output$table <- renderDataTable({
    
    
    
    df <- cbind(' ' = c('1st','2nd','3rd','4th','1st out','2nd out'), 'Team' = c("Select", "Select", "Select", "Select", "Select", "Select"))
    
    if(  # iteration 1
      
      input$secchamp == "Alabama" &
      
      input$BigTenchamp == "Wisconsin" &
      
      input$ACCchamp == "Clemson" &
      
      input$bigtwelvechamp == "Oklahoma" &
      
      input$pactwelvechamp == "Washington"
      
    ) {
      
      df <- cbind(' ' = c('1st',
                          
                          '2nd',
                          
                          '3rd',
                          
                          '4th',
                          
                          '1st out',
                          
                          '2nd out'), 
                  
                  'Team' = c("Alabama", 
                             
                             "Clemson", 
                             
                             "Ohio State", 
                             
                             "Washington",
                             
                             "Wisconsin",
                             
                             "Oklahoma"))
      
      output$text1 <- renderText({ 
        
        "Ohio State getins in over Wisconsin. Has better record than Wisconsin and has H2H advantage."
        
      })
      
    }
    
    if(  # iteration 2
      
      input$secchamp == "Alabama" &
      
      input$BigTenchamp == "Wisconsin" &
      
      input$ACCchamp == "Clemson" &
      
      input$bigtwelvechamp == "Oklahoma" &
      
      input$pactwelvechamp == "Colorado"
      
    ) {
      
      df <- cbind(' ' = c('1st',
                          
                          '2nd',
                          
                          '3rd',
                          
                          '4th',
                          
                          '1st out',
                          
                          '2nd out'), 
                  
                  'Team' = c("Alabama", 
                             
                             "Clemson", 
                             
                             "Ohio State", 
                             
                             "Wisconsin",
                             
                             "Oklahoma",
                             
                             "Michigan"))
      
      output$text1 <- renderText({ 
        
        "Ohio State and Big Ten Champ are in becasue of Ohio State's record and Big Ten has more top ten teams."
        
      })
      
    }
    
    if(  # iteration 3
      
      input$secchamp == "Alabama" &
      
      input$BigTenchamp == "Wisconsin" &
      
      input$ACCchamp == "Clemson" &
      
      input$bigtwelvechamp == "Oklahoma State" &
      
      input$pactwelvechamp == "Washington"
      
    ) {
      
      df <- cbind(' ' = c('1st',
                          
                          '2nd',
                          
                          '3rd',
                          
                          '4th',
                          
                          '1st out',
                          
                          '2nd out'), 
                  
                  'Team' = c("Alabama", 
                             
                             "Clemson", 
                             
                             "Ohio State", 
                             
                             "Washington",
                             
                             "Wisconsin",
                             
                             "Michigan"))
      
      output$text1 <- renderText({ 
        
        "Ohio State gets in over Wisconsin becasue of H2H win over Wisconsin."
        
      })
      
    }
    
    if(  # iteration 4
      
      input$secchamp == "Alabama" &
      
      input$BigTenchamp == "Wisconsin" &
      
      input$ACCchamp == "Clemson" &
      
      input$bigtwelvechamp == "Oklahoma State" &
      
      input$pactwelvechamp == "Colorado"
      
    ) {
      
      df <- cbind(' ' = c('1st',
                          
                          '2nd',
                          
                          '3rd',
                          
                          '4th',
                          
                          '1st out',
                          
                          '2nd out'), 
                  
                  'Team' = c("Alabama", 
                             
                             "Clemson", 
                             
                             "Ohio State", 
                             
                             "Wisconsin",
                             
                             "Michigan",
                             
                             "Colorado"))
      
      output$text1 <- renderText({ 
        
        "Wisconsin gets in over Michigan because of a better winning percentage and confrence champ."
        
      })
      
    }
    
    if(  # iteration 5
      
      input$secchamp == "Alabama" &
      
      input$BigTenchamp == "Wisconsin" &
      
      input$ACCchamp == "Virginia Tech" &
      
      input$bigtwelvechamp == "Oklahoma" &
      
      input$pactwelvechamp == "Washington"
      
    ) {
      
      df <- cbind(' ' = c('1st',
                          
                          '2nd',
                          
                          '3rd',
                          
                          '4th',
                          
                          '1st out',
                          
                          '2nd out'), 
                  
                  'Team' = c("Alabama", 
                             
                             "Ohio State", 
                             
                             "Washington", 
                             
                             "Wisconsin",
                             
                             "Oklahoma",
                             
                             "Michigan"))
      
      output$text1 <- renderText({ 
        
        "Big Ten Champ gets in over Big Twelve. More teams in top ten. "
        
      })
      
    }
    
    if(  # iteration 6
      
      input$secchamp == "Alabama" &
      
      input$BigTenchamp == "Wisconsin" &
      
      input$ACCchamp == "Virginia Tech" &
      
      input$bigtwelvechamp == "Oklahoma" &
      
      input$pactwelvechamp == "Colorado"
      
    ) {
      
      df <- cbind(' ' = c('1st',
                          
                          '2nd',
                          
                          '3rd',
                          
                          '4th',
                          
                          '1st out',
                          
                          '2nd out'), 
                  
                  'Team' = c("Alabama", 
                             
                             "Ohio State", 
                             
                             "Wisconsin", 
                             
                             "Oklahoma",
                             
                             "Michigan",
                             
                             "Colorado"))
      
      output$text1 <- renderText({ 
        
        "Big Ten and Big Twelve Champ gets in."
        
      })
      
    }
    
    if(  # iteration 7
      
      input$secchamp == "Alabama" &
      
      input$BigTenchamp == "Wisconsin" &
      
      input$ACCchamp == "Virginia Tech" &
      
      input$bigtwelvechamp == "Oklahoma State" &
      
      input$pactwelvechamp == "Washington"
      
    ) {
      
      df <- cbind(' ' = c('1st',
                          
                          '2nd',
                          
                          '3rd',
                          
                          '4th',
                          
                          '1st out',
                          
                          '2nd out'), 
                  
                  'Team' = c("Alabama", 
                             
                             "Ohio State", 
                             
                             "Washington", 
                             
                             "Wisconsin",
                             
                             "Michigan",
                             
                             "Clemson"))
      
      output$text1 <- renderText({ 
        
        "Michigan first team out. Big Team Champ is in, Ohio State is two above Pac 12 Champ."
        
      })
      
    }
    
    if(  # iteration 8
      
      input$secchamp == "Alabama" &
      
      input$BigTenchamp == "Wisconsin" &
      
      input$ACCchamp == "Virginia Tech" &
      
      input$bigtwelvechamp == "Oklahoma State" &
      
      input$pactwelvechamp == "Colorado"
      
    ) {
      
      df <- cbind(' ' = c('1st',
                          
                          '2nd',
                          
                          '3rd',
                          
                          '4th',
                          
                          '1st out',
                          
                          '2nd out'), 
                  
                  'Team' = c("Alabama", 
                             
                             "Ohio State", 
                             
                             "Wisconsin", 
                             
                             "Michigan",
                             
                             "Clemson",
                             
                             "Colorado"))
      
      output$text1 <- renderText({ 
        
        "Three big ten teams if Oklahoma, Washington, and Clemson lose. "
        
      })
      
    }
    
    if(  # iteration 9
      
      input$secchamp == "Alabama" &
      
      input$BigTenchamp == "Penn State" &
      
      input$ACCchamp == "Clemson" &
      
      input$bigtwelvechamp == "Oklahoma" &
      
      input$pactwelvechamp == "Washington"
      
    ) {
      
      df <- cbind(' ' = c('1st',
                          
                          '2nd',
                          
                          '3rd',
                          
                          '4th',
                          
                          '1st out',
                          
                          '2nd out'), 
                  
                  'Team' = c("Alabama", 
                             
                             "Clemson", 
                             
                             "Ohio State", 
                             
                             "Washington",
                             
                             "Penn State",
                             
                             "Oklahoma"))
      
      output$text1 <- renderText({ 
        
        "Ohio State has better record than Penn State and Oklahoma -- even though PSU has H2H-- OSU gets in."
        
      })
      
    }
    
    if(  # iteration 10
      
      input$secchamp == "Alabama" &
      
      input$BigTenchamp == "Penn State" &
      
      input$ACCchamp == "Clemson" &
      
      input$bigtwelvechamp == "Oklahoma" &
      
      input$pactwelvechamp == "Colorado"
      
    ) {
      
      df <- cbind(' ' = c('1st',
                          
                          '2nd',
                          
                          '3rd',
                          
                          '4th',
                          
                          '1st out',
                          
                          '2nd out'), 
                  
                  'Team' = c("Alabama", 
                             
                             "Clemson", 
                             
                             "Ohio State", 
                             
                             "Penn State",
                             
                             "Oklahoma",
                             
                             "Michigan"))
      
      output$text1 <- renderText({ 
        
        "Penn State gets in over Oklahoma becasue of H2H over OSU -- even though OU and PSU have same record."
        
      })
      
    }
    
    if(  # iteration 11
      
      input$secchamp == "Alabama" &
      
      input$BigTenchamp == "Penn State" &
      
      input$ACCchamp == "Clemson" &
      
      input$bigtwelvechamp == "Oklahoma State" &
      
      input$pactwelvechamp == "Washington"
      
    ) {
      
      df <- cbind(' ' = c('1st',
                          
                          '2nd',
                          
                          '3rd',
                          
                          '4th',
                          
                          '1st out',
                          
                          '2nd out'), 
                  
                  'Team' = c("Alabama", 
                             
                             "Clemson", 
                             
                             "Ohio State", 
                             
                             "Washington",
                             
                             "Penn State",
                             
                             "Michigan"))
      
      output$text1 <- renderText({ 
        
        "Ohio State gets in before Big Ten Champ becasue of better record. Michigan second team out."
        
      })
      
    }
    
    if(  # iteration 12
      
      input$secchamp == "Alabama" &
      
      input$BigTenchamp == "Penn State" &
      
      input$ACCchamp == "Clemson" &
      
      input$bigtwelvechamp == "Oklahoma State" &
      
      input$pactwelvechamp == "Colorado"
      
    ) {
      
      df <- cbind(' ' = c('1st',
                          
                          '2nd',
                          
                          '3rd',
                          
                          '4th',
                          
                          '1st out',
                          
                          '2nd out'), 
                  
                  'Team' = c("Alabama", 
                             
                             "Clemson", 
                             
                             "Ohio State", 
                             
                             "Penn State",
                             
                             "Michigan",
                             
                             "Colorado"))
      
      output$text1 <- renderText({ 
        
        "Colorado is second team out. Two Big Ten teams make CFP. "
        
      })
      
    }
    
    if(  # iteration 13
      
      input$secchamp == "Alabama" &
      
      input$BigTenchamp == "Penn State" &
      
      input$ACCchamp == "Virginia Tech" &
      
      input$bigtwelvechamp == "Oklahoma" &
      
      input$pactwelvechamp == "Washington"
      
    ) {
      
      df <- cbind(' ' = c('1st',
                          
                          '2nd',
                          
                          '3rd',
                          
                          '4th',
                          
                          '1st out',
                          
                          '2nd out'), 
                  
                  'Team' = c("Alabama", 
                             
                             "Ohio State", 
                             
                             "Washington", 
                             
                             "Penn State",
                             
                             "Oklahoma",
                             
                             "Michigan"))
      
      output$text1 <- renderText({ 
        
        "Penn State gets in over Oklahoma due to H2H win over OSU. "
        
      })
      
    }
    
    if(  # iteration 14
      
      input$secchamp == "Alabama" &
      
      input$BigTenchamp == "Penn State" &
      
      input$ACCchamp == "Virginia Tech" &
      
      input$bigtwelvechamp == "Oklahoma" &
      
      input$pactwelvechamp == "Colorado"
      
    ) {
      
      df <- cbind(' ' = c('1st',
                          
                          '2nd',
                          
                          '3rd',
                          
                          '4th',
                          
                          '1st out',
                          
                          '2nd out'), 
                  
                  'Team' = c("Alabama", 
                             
                             "Ohio State", 
                             
                             "Penn State", 
                             
                             "Oklahoma",
                             
                             "Michigan",
                             
                             "Clemson"))
      
      output$text1 <- renderText({ 
        
        "Oklahoma gets in over Michigan becasue of Big Twelve championship. "
        
      })
      
    }
    
    if(  # iteration 15
      
      input$secchamp == "Alabama" &
      
      input$BigTenchamp == "Penn State" &
      
      input$ACCchamp == "Virginia Tech" &
      
      input$bigtwelvechamp == "Oklahoma State" &
      
      input$pactwelvechamp == "Washington"
      
    ) {
      
      df <- cbind(' ' = c('1st',
                          
                          '2nd',
                          
                          '3rd',
                          
                          '4th',
                          
                          '1st out',
                          
                          '2nd out'), 
                  
                  'Team' = c("Alabama", 
                             
                             "Ohio State", 
                             
                             "Washington", 
                             
                             "Penn State",
                             
                             "Michigan",
                             
                             "Clemson"))
      
      output$text1 <- renderText({ 
        
        "Ohio State is above Wisconsin due to strength of schedule. Washington ranked higher than PSU due to better record."
        
      })
      
    }
    
    if(  # iteration 16
      
      input$secchamp == "Alabama" &
      
      input$BigTenchamp == "Penn State" &
      
      input$ACCchamp == "Virginia Tech" &
      
      input$bigtwelvechamp == "Oklahoma State" &
      
      input$pactwelvechamp == "Colorado"
      
    ) {
      
      df <- cbind(' ' = c('1st',
                          
                          '2nd',
                          
                          '3rd',
                          
                          '4th',
                          
                          '1st out',
                          
                          '2nd out'), 
                  
                  'Team' = c("Alabama", 
                             
                             "Ohio State", 
                             
                             "Michigan", 
                             
                             "Penn State",
                             
                             "Clemson",
                             
                             "Colorado"))
      
      output$text1 <- renderText({ 
        
        "Clemson has a better record than Michigan. Michigan gets in with better resume. Penn State gets in as Big Ten Champ. CFPC switches Michigan to three to have Mich-OSU rematch."
        
      })
      
    }
    
    if(  # iteration 17
      
      input$secchamp == "Florida" &
      
      input$BigTenchamp == "Wisconsin" &
      
      input$ACCchamp == "Clemson" &
      
      input$bigtwelvechamp == "Oklahoma" &
      
      input$pactwelvechamp == "Washington"
      
    ) {
      
      df <- cbind(' ' = c('1st',
                          
                          '2nd',
                          
                          '3rd',
                          
                          '4th',
                          
                          '1st out',
                          
                          '2nd out'), 
                  
                  'Team' = c("Alabama", 
                             
                             "Clemson", 
                             
                             "Ohio State", 
                             
                             "Washington",
                             
                             "Wisconsin",
                             
                             "Oklahoma"))
      
      output$text1 <- renderText({ 
        
        "Alabama is still top dog if Florida wins.  "
        
      })
      
    }
    
    if(  # iteration 18
      
      input$secchamp == "Florida" &
      
      input$BigTenchamp == "Wisconsin" &
      
      input$ACCchamp == "Clemson" &
      
      input$bigtwelvechamp == "Oklahoma" &
      
      input$pactwelvechamp == "Colorado"
      
    ) {
      
      df <- cbind(' ' = c('1st',
                          
                          '2nd',
                          
                          '3rd',
                          
                          '4th',
                          
                          '1st out',
                          
                          '2nd out'), 
                  
                  'Team' = c("Alabama", 
                             
                             "Clemson", 
                             
                             "Ohio State", 
                             
                             "Wisconsin",
                             
                             "Oklahoma",
                             
                             "Michigan"))
      
      output$text1 <- renderText({ 
        
        "Big Ten Champ gets in over Big Twelve due to more teams in the top ten. Michigan is second team out."
        
      })
      
    }
    
    if(  # iteration 19
      
      input$secchamp == "Florida" &
      
      input$BigTenchamp == "Wisconsin" &
      
      input$ACCchamp == "Clemson" &
      
      input$bigtwelvechamp == "Oklahoma State" &
      
      input$pactwelvechamp == "Washington"
      
    ) {
      
      df <- cbind(' ' = c('1st',
                          
                          '2nd',
                          
                          '3rd',
                          
                          '4th',
                          
                          '1st out',
                          
                          '2nd out'), 
                  
                  'Team' = c("Alabama", 
                             
                             "Clemson", 
                             
                             "Ohio State", 
                             
                             "Washington",
                             
                             "Wisconsin",
                             
                             "Michigan"))
      
      output$text1 <- renderText({ 
        
        "Alabama still is top. Ohio State gets in over Wisconsin due to H2H win."
        
      })
      
    }
    
    if(  # iteration 20
      
      input$secchamp == "Florida" &
      
      input$BigTenchamp == "Wisconsin" &
      
      input$ACCchamp == "Clemson" &
      
      input$bigtwelvechamp == "Oklahoma State" &
      
      input$pactwelvechamp == "Colorado"
      
    ) {
      
      df <- cbind(' ' = c('1st',
                          
                          '2nd',
                          
                          '3rd',
                          
                          '4th',
                          
                          '1st out',
                          
                          '2nd out'), 
                  
                  'Team' = c("Alabama", 
                             
                             "Clemson", 
                             
                             "Ohio State", 
                             
                             "Wisconsin",
                             
                             "Michigan",
                             
                             "Colorado"))
      
      output$text1 <- renderText({ 
        
        "Two Big Ten teams. Michigan is first team out. "
        
      })
      
    }
    
    if(  # iteration 21
      
      input$secchamp == "Florida" &
      
      input$BigTenchamp == "Wisconsin" &
      
      input$ACCchamp == "Virginia Tech" &
      
      input$bigtwelvechamp == "Oklahoma" &
      
      input$pactwelvechamp == "Washington"
      
    ) {
      
      df <- cbind(' ' = c('1st',
                          
                          '2nd',
                          
                          '3rd',
                          
                          '4th',
                          
                          '1st out',
                          
                          '2nd out'), 
                  
                  'Team' = c("Alabama", 
                             
                             "Washington", 
                             
                             "Ohio State", 
                             
                             "Wisconsin",
                             
                             "Oklahoma",
                             
                             "Michigan"))
      
      output$text1 <- renderText({ 
        
        "Big Ten Champ gets in over Big Twelve due to more teams in the top ten."
        
      })
      
    }
    
    if(  # iteration 22
      
      input$secchamp == "Florida" &
      
      input$BigTenchamp == "Wisconsin" &
      
      input$ACCchamp == "Virginia Tech" &
      
      input$bigtwelvechamp == "Oklahoma" &
      
      input$pactwelvechamp == "Colorado"
      
    ) {
      
      df <- cbind(' ' = c('1st',
                          
                          '2nd',
                          
                          '3rd',
                          
                          '4th',
                          
                          '1st out',
                          
                          '2nd out'), 
                  
                  'Team' = c("Alabama", 
                             
                             "Ohio State", 
                             
                             "Wisconsin", 
                             
                             "Oklahoma",
                             
                             "Michigan",
                             
                             "Clemson"))
      
      output$text1 <- renderText({ 
        
        "Oklahoma is in. Michigan first team out. Two Big Ten teams. "
        
      })
      
    }
    
    if(  # iteration 23
      
      input$secchamp == "Florida" &
      
      input$BigTenchamp == "Wisconsin" &
      
      input$ACCchamp == "Virginia Tech" &
      
      input$bigtwelvechamp == "Oklahoma State" &
      
      input$pactwelvechamp == "Washington"
      
    ) {
      
      df <- cbind(' ' = c('1st',
                          
                          '2nd',
                          
                          '3rd',
                          
                          '4th',
                          
                          '1st out',
                          
                          '2nd out'), 
                  
                  'Team' = c("Alabama", 
                             
                             "Ohio State", 
                             
                             "Washington", 
                             
                             "Wisconsin",
                             
                             "Michigan",
                             
                             "Clemson"))
      
      output$text1 <- renderText({ 
        
        "Michigan first team out. Better loses than Clemson. Two Big Ten team. Washington and Ohio State are interchangeable. "
        
      })
      
    }
    
    if(  # iteration 24
      
      input$secchamp == "Florida" &
      
      input$BigTenchamp == "Wisconsin" &
      
      input$ACCchamp == "Virginia Tech" &
      
      input$bigtwelvechamp == "Oklahoma State" &
      
      input$pactwelvechamp == "Colorado"
      
    ) {
      
      df <- cbind(' ' = c('1st',
                          
                          '2nd',
                          
                          '3rd',
                          
                          '4th',
                          
                          '1st out',
                          
                          '2nd out'), 
                  
                  'Team' = c("Alabama", 
                             
                             "Ohio State", 
                             
                             "Wisconsin", 
                             
                             "Michigan",
                             
                             "Clemson",
                             
                             "Colorado"))
      
      output$text1 <- renderText({ 
        
        "Michigan gets in over Clemson, better loses. Three Big Ten teams."
        
      })
      
    }
    
    if(  # iteration 25
      
      input$secchamp == "Florida" &
      
      input$BigTenchamp == "Penn State" &
      
      input$ACCchamp == "Clemson" &
      
      input$bigtwelvechamp == "Oklahoma" &
      
      input$pactwelvechamp == "Washington"
      
    ) {
      
      df <- cbind(' ' = c('1st',
                          
                          '2nd',
                          
                          '3rd',
                          
                          '4th',
                          
                          '1st out',
                          
                          '2nd out'), 
                  
                  'Team' = c("Alabama", 
                             
                             "Clemson", 
                             
                             "Ohio State", 
                             
                             "Washington",
                             
                             "Penn State",
                             
                             "Oklahoma"))
      
      output$text1 <- renderText({ 
        
        "OSU is in before PSU due to better record."
        
      })
      
    }
    
    if(  # iteration 26
      
      input$secchamp == "Florida" &
      
      input$BigTenchamp == "Penn State" &
      
      input$ACCchamp == "Clemson" &
      
      input$bigtwelvechamp == "Oklahoma" &
      
      input$pactwelvechamp == "Colorado"
      
    ) {
      
      df <- cbind(' ' = c('1st',
                          
                          '2nd',
                          
                          '3rd',
                          
                          '4th',
                          
                          '1st out',
                          
                          '2nd out'), 
                  
                  'Team' = c("Alabama", 
                             
                             "Clemson", 
                             
                             "Ohio State", 
                             
                             "Penn State",
                             
                             "Oklahoma",
                             
                             "Michigan"))
      
      output$text1 <- renderText({ 
        
        "Big Ten Champ gets in over Big twelve due to more teams in the Big Ten in the top ten."
        
      })
      
    }
    
    if(  # iteration 27
      
      input$secchamp == "Florida" &
      
      input$BigTenchamp == "Penn State" &
      
      input$ACCchamp == "Clemson" &
      
      input$bigtwelvechamp == "Oklahoma State" &
      
      input$pactwelvechamp == "Washington"
      
    ) {
      
      df <- cbind(' ' = c('1st',
                          
                          '2nd',
                          
                          '3rd',
                          
                          '4th',
                          
                          '1st out',
                          
                          '2nd out'), 
                  
                  'Team' = c("Alabama", 
                             
                             "Clemson", 
                             
                             "Ohio State", 
                             
                             "Washington",
                             
                             "Penn State",
                             
                             "Michigan"))
      
      output$text1 <- renderText({ 
        
        "Ohio State gets in ahead of Penn State due to better record. Big Ten first two teams out."
        
      })
      
    }
    
    if(  # iteration 28
      
      input$secchamp == "Florida" &
      
      input$BigTenchamp == "Penn State" &
      
      input$ACCchamp == "Clemson" &
      
      input$bigtwelvechamp == "Oklahoma State" &
      
      input$pactwelvechamp == "Colorado"
      
    ) {
      
      df <- cbind(' ' = c('1st',
                          
                          '2nd',
                          
                          '3rd',
                          
                          '4th',
                          
                          '1st out',
                          
                          '2nd out'), 
                  
                  'Team' = c("Alabama", 
                             
                             "Clemson", 
                             
                             "Ohio State", 
                             
                             "Penn State",
                             
                             "Michigan",
                             
                             "COlorado"))
      
      output$text1 <- renderText({ 
        
        "OSU gets in over PSU due to better record. "
        
      })
      
    }
    
    if(  # iteration 29
      
      input$secchamp == "Florida" &
      
      input$BigTenchamp == "Penn State" &
      
      input$ACCchamp == "Virginia Tech" &
      
      input$bigtwelvechamp == "Oklahoma" &
      
      input$pactwelvechamp == "Washington"
      
    ) {
      
      df <- cbind(' ' = c('1st',
                          
                          '2nd',
                          
                          '3rd',
                          
                          '4th',
                          
                          '1st out',
                          
                          '2nd out'), 
                  
                  'Team' = c("Alabama", 
                             
                             "Ohio State", 
                             
                             "Washington", 
                             
                             "Penn State",
                             
                             "Oklahoma",
                             
                             "Michigan"))
      
      output$text1 <- renderText({ 
        
        "Big Ten Champ gets in. Oklahoma jumps Michigan after winning the Big Twelve."
        
      })
      
    }
    
    if(  # iteration 30
      
      input$secchamp == "Florida" &
      
      input$BigTenchamp == "Penn State" &
      
      input$ACCchamp == "Virginia Tech" &
      
      input$bigtwelvechamp == "Oklahoma" &
      
      input$pactwelvechamp == "Colorado"
      
    ) {
      
      df <- cbind(' ' = c('1st',
                          
                          '2nd',
                          
                          '3rd',
                          
                          '4th',
                          
                          '1st out',
                          
                          '2nd out'), 
                  
                  'Team' = c("Alabama", 
                             
                             "Ohio State", 
                             
                             "Penn State", 
                             
                             "Oklahoma",
                             
                             "Michigan",
                             
                             "Clemson"))
      
      output$text1 <- renderText({ 
        
        "Oklahoma jumps Michigan after winning the Big Twelve."
        
      })
      
    }
    
    if(  # iteration 31
      
      input$secchamp == "Florida" &
      
      input$BigTenchamp == "Penn State" &
      
      input$ACCchamp == "Virginia Tech" &
      
      input$bigtwelvechamp == "Oklahoma State" &
      
      input$pactwelvechamp == "Washington"
      
    ) {
      
      df <- cbind(' ' = c('1st',
                          
                          '2nd',
                          
                          '3rd',
                          
                          '4th',
                          
                          '1st out',
                          
                          '2nd out'), 
                  
                  'Team' = c("Alabama", 
                             
                             "Ohio State", 
                             
                             "Washington", 
                             
                             "Penn State",
                             
                             "Michigan",
                             
                             "Clemson"))
      
      output$text1 <- renderText({ 
        
        "Two Big Ten teams. Alabama still gets in."
        
      })
      
    }
    
    if(  # iteration 32
      
      input$secchamp == "Florida" &
      
      input$BigTenchamp == "Penn State" &
      
      input$ACCchamp == "Virginia Tech" &
      
      input$bigtwelvechamp == "Oklahoma State" &
      
      input$pactwelvechamp == "Colorado"
      
    ) {
      
      df <- cbind(' ' = c('1st',
                          
                          '2nd',
                          
                          '3rd',
                          
                          '4th',
                          
                          '1st out',
                          
                          '2nd out'), 
                  
                  'Team' = c("Alabama", 
                             
                             "Ohio State", 
                             
                             "Michigan", 
                             
                             "Penn State",
                             
                             "Colorado",
                             
                             "Clemson"))
      
      output$text1 <- renderText({ 
        
        "Michigan jumps Penn State to have a rematch of The Game. Three Big Ten teams."
        
      })
      
    }
    
    df
    
  }
  
  )
  
  
  
}



# Run the application 

shinyApp(ui = ui, server = server)