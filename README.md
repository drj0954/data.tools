---
title: "data.tools"
author: "Dr. Jhansi Chengala"
date: "2023-04-15"
output: html_document
---

# data.tools

data.tools enables you to create, upload, edit, analyze, store and share datasets in SQLite databases and other storage tools directly from shiny app. To install this package from GitHub run;
```
devtools::install_github('drj0954/data.tools')
```

## Using data.tools!
```
library(shiny)
library(data.tools)
library(bs4Dash)
ui <- dataentryui(title = 'data.tools',
  analysistabItem = bs4Dash::tabItem(tabName = 'autonomous',
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
    x <- useactivedata(input = input,session = session)
    selectInput('chois', label = 'choose variables', choices = names(x))
  })

  output$tabl<-renderTable({
    mtcars
  })

}


shinyApp(ui, server)

```
You can also use the demo app to build and manage your custom databases by running;
```
shinySQLite::runExample(example = 'app.R')
```
It is easy to call ```data.tools``` functions in your shinyapp! 
```dataentryui``` is a shiny UI element where all data.tools outputs and inputs are rendered. The arguments to be considered in this function are the title and your additional tabItem (```analysistab```). See the documentation for more info.

```dataentryserver``` is a server functions that should be called inside your shiny app server. See dataentryserver documentation for more info.

Other important functions include ```useactivedata``` which allows you to carry out data analysis on the currently opened dataset based on your functions of interest such as plots, regression or tables e.t.c. Its output should be rendered in the ```analysistab```.  

You can create, retrieve and update multiple databases and tables anytime within the interface.
Thank you!
