#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(data.tools)
library(bs4Dash)
ui <- dataentryui(
  analysistab = bs4Dash::tabItem(tabName = 'nl',
                            uiOutput('choiz'),
                            uiOutput('plot')
  )
)


server <- function(input, output, session){


  dataentryserver(input = input,output = output,session = session,
                    authenticate = T)
  onSessionEnded(session = session,
                 output$ended<-renderUI({
                   'hi'}))
  output$choiz <- renderUI({
    x <- usesactivedata(input = input,session = session)
    selectInput('chois', label = 'choose variables', choices = names(x))
  })

  output$plot<-renderUI({
    ''
  })

}


shinyApp(ui, server)
