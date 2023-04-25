# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' @import shiny
#' @import bs4Dash
#' @import jsonlite
#' @import fresh
#' @import bslib
#' @import sourcetools
#' @import dplyr
#' @import rhandsontable
#' @import shinyWidgets
#' @import plotly
#' @import ggplot2
#' @import shinyAce
#' @import dashboardthemes
#' @import shinyjs
#' @import RSQLite
#' @import DBI
#' @export
secureapp <- function (credentials, input, output, session)
{
  credentials<-data.frame(username=c('Abednego'),password=c(1234),previllage='Admin',recovery='#m245mm')
  credentials$password <- paste0('#',credentials$password)
  con <- DBI:::dbConnect(SQLite(),'cred')

  if(dbExistsTable(con,'credDdata')==F){
    dbWriteTable(con, 'credData', credentials, append = T)

    DBI:::dbBegin(con)
    DBI:::dbSendStatement(con,
                         'DELETE FROM credData
WHERE rowid NOT IN (
  SELECT MIN(rowid)
  FROM credData
  GROUP BY password
)')
    DBI:::dbCommit(con)
  }else{
    DBI:::dbBegin(con)
    DBI:::dbSendStatement(con,
                         'DELETE FROM credData
WHERE rowid NOT IN (
  SELECT MIN(rowid)
  FROM credData
  GROUP BY password
)')
    DBI:::dbCommit(con)
  }

  dbcreddf<-reactive({
    req(length(input$sgn)!=0)
    req(rvlue() == 1)
    con <- DBI:::dbConnect(SQLite(),'cred')
    rd<-dbReadTable(con, 'credData')
    vt=seq(0,99)
    special<-c('*','#','$','@','&','!','%')
    lts<-LETTERS
    smplts<-sample(lts,10)
    ltsb<-letters
    smpltsb<-sample(lts,10)



    smp<-c(paste0(smplts,sample(vt,10),smpltsb,sample(special,1)),paste0(smplts,smpltsb,sample(vt,10),sample(special,1)),paste0(sample(vt,10),smplts,sample(special,1),smpltsb))
    if(all(rd$recovery%in%smp)==T){
      v<-smp[-which(smp==rd$recovery)]
      x<-c(paste0(smplts,sample(v,1),smpltsb),paste0(smplts,smpltsb,sample(v,1)),paste0(sample(v,1),smplts,smpltsb))
      df <- data.frame(username = input$user, password = paste0('#',input$passwod),previllage='user',
                       recovery=sample(x,1))

      dbWriteTable(con, 'credData',df, append = T)
    }

    else{
      v<-smp
      x<-c(paste0(smplts,sample(v,1),smpltsb),paste0(smplts,smpltsb,sample(v,1)),paste0(sample(v,1),smplts,smpltsb))
      df <- data.frame(username = input$user, password = paste0('#',input$passwod),previllage='user',
                       recovery=sample(x,1))

      dbWriteTable(con, 'credData',df, append = T)}


    mydata <- dbReadTable(con, 'credData')
    return(mydata)
  })
  loginTM<-data.frame(openlogTM='',closelogTM='')
  if(dbExistsTable(con,'admintools')==F){
    dbWriteTable(con, 'admintools', loginTM, overwrite = T)}
  logTMfun =function(){
    if(length(input$setlogTM) != 0){
      if(input$setlogTM==T){
        reactiveValues(data = isolate(
          dflogtools()))}else{
            reactiveValues(data = dbReadTable(con, 'admintools'))
          }
    }else{
      reactiveValues(data = dbReadTable(con, 'admintools'))
    }
  }



  dflogtools<-reactive({
    if(input$setlogTM==T){
      if (!is.null(input$toolstbl)){
        DFf <<- rhandsontable::hot_to_r(input$toolstbl)
        dbWriteTable(con, 'admintools',DFf, overwrite = T)}
    }


    mydata <- dbReadTable(con, 'admintools')
    return(mydata)
  })

  output$toolstbl<-rhandsontable:::renderRHandsontable({
    X<-logTMfun()
    rhandsontable(X$data)
  })

  credent <- function(){
    con <- DBI:::dbConnect(SQLite(),'cred')
    data = reactiveValues(dbReadTable(con, 'credData'))
    return(data)
  }
  con <- DBI:::dbConnect(SQLite(),'cred')
  cre<-isolate({dbReadTable(con, 'credData')})
  credentialz <- reactiveVal(cre)
  observe({
    input$sgn
    con <- DBI:::dbConnect(SQLite(),'cred')
    data <- isolate({dbcreddf()})
    isolate({
      credentialz(data)
    })
  })
  observe({
    req(length(input$loginok)!=0)
    input$loginok
    con <- DBI:::dbConnect(SQLite(),'cred')
    data <- dbReadTable(con, 'credData')
    isolate({
      credentialz(data)
    })
  })
  observe({
    req(rule()==F)
    req(input$passwod!=input$againpasswod)
    con <- DBI:::dbConnect(SQLite(),'cred')
    data <- isolate({remodub()})
    isolate({
      credentialz(data)
    })
  })
  remodub<-reactive({
    input$sgnup
    con <- DBI:::dbConnect(SQLite(),'cred')
    DBI:::dbBegin(con)
    DBI:::dbSendStatement(con,
                         'DELETE FROM credData
WHERE rowid NOT IN (
  SELECT MIN(rowid)
  FROM credData
  GROUP BY password
)')
    DBI:::dbCommit(con)

    data = dbReadTable(con, 'credData')
  })

  shiny:::observeEvent(input$loginok, {
    crd <- credentialz()
    if (input$username %in% crd$username == T) {
      datcred <- crd %>% dplyr:::filter(username ==
                                         input$username) %>% select(password)
      if (paste0('#',input$password) %in% datcred$password == T) {
        req(paste0('#',input$password) == crd %>% filter(username ==
                                                           input$username) %>% dplyr:::filter(password ==
                                                                                               paste0('#',input$password)) %>% select(password))
        shinyjs:::delay(2000, output$UIlogsecuredpage <- shiny:::renderUI({
          shiny:::div(style = paste0("font-size:", input$inpuT,
                                    "%"), securedPage)
        }))
      }
    }
  })
  output$userloggedin <- shiny:::renderUI({
    input$username
  })
  shiny:::observeEvent(input$loginok, {
    output$UIlogfailed <- shiny:::renderUI({
      crd <- credentialz()
      if (input$username %in% crd$username == T) {
        datcred <- crd %>% filter(username ==
                                    input$username) %>% select(password)
        if (paste0('#',input$password) %in% datcred$password == T) {
          req(paste0('#',input$password) == crd %>% filter(username ==
                                                             input$username) %>% filter(password == paste0('#',input$password)) %>%
                select(password))
        }
        else {
          span(style = "color:red",
               div("Invalid credentials. Try again with the correct credentials or click on forgot username or password or sign-up if you're a new user.",style='text-align center'))
        }
      }
      else
        if (all((input$username %in% crd$username == F),
                (paste0('#',input$password) %in% crd$password == F))==T) {
          span(style = "color:red",
               div("Invalid credentials. Try again with the correct credentials or click on forgot username|password or sign-up if you're a new user.",style='text-align center'))
        }else
          if (all((input$username %in% crd$username == F),
                  (paste0('#',input$password) %in% crd$password == T))==T) {
            span(style = "color:red",
                 div("Invalid credentials. Try again with the correct credentials or click on forgot username|password or sign-up if you're a new user.",style='text-align center'))
          }
    })
  })
  shiny:::observeEvent(input$loginok, {

    crd <- credentialz()
    if (input$username %in% crd$username == T) {
      datcred <- crd %>% filter(username == input$username) %>%
        select(password)
      if (paste0('#',input$password) %in% datcred$password == T) {
        req(paste0('#',input$password) == crd %>% filter(username ==
                                                           input$username) %>% filter(password == paste0('#',input$password)) %>%
              select(password))
        shinyjs:::toggle("logintosecure")
      }
    }
  })

  output$okbtn <- renderUI({
    actionLink('sgn',span('sign up',style='color:green'))
  })


  output$cbtn <- renderUI({
    input$sgn
    req(rvlue() == 1)
    req(paste0('#',input$passwod)==paste0('#',input$againpasswod))
    dp<-dbcreddf()[seq(0,nrow(dbcreddf())),]

    credential<-dbcreddf()

    rcv<-credential[nrow(credential),]%>%select(recovery)
    if(rule()==T){fluidPage( fresh:::use_theme(fresh:::create_theme(

      fresh:::bs_vars_font(
        family_sans_serif = "'Saira Stencil One', cursive"
      )
    )),
    HTML(paste(span('success! credentials added, redirecting you to login page in',style='color:green'),span(paste0(45-rtnn(),'s'),style='color:gray'))),uiOutput('rbtn'),hr(),
    HTML(paste('NB: Your recovery code is ',span(rcv,style='color:fuchsia'), 'in case you forget your password. Save it somewhere.')))}else
    {span('not succsessful! some credentials already exist try again with different credentials',style='color:red')}
  })
  output$rbtn <- renderUI({
    input$sgn
    req(rvlue() == 1)
    req(paste0('#',input$passwod)==paste0('#',input$againpasswod))

    if(rule()==T){
      actionLink('rfrpgbtn',HTML(paste(icon('refresh'),'or click me to return to login page')))}
  })

  observeEvent(input$rfrpgbtn,{
    refresh()
  })

  rule <- reactive({
    input$sgn
    req(rvlue() == 1)
    req(paste0('#',input$passwod)==paste0('#',input$againpasswod))
    dp<-dbcreddf()
    dp<-dbcreddf()[seq(0,nrow(dbcreddf())-1),]
    if(paste0('#',input$passwod)%in%dp$password==F){
      return(T)}else{
        FALSE
      }
  })

  rvlue <- reactiveVal(0)

  shiny:::observe({
    input$sgn
    req(input$user != '' && input$passwod != '')
    req(input$againpasswod == input$passwod)
    isolate({
      rvlue(rvlue()+1)})
  })
  shiny:::observe({
    input$sgn
    req(input$user != '' && input$passwod != '')
    req(input$againpasswod != input$passwod)
    isolate({
      rvlue(-1)})
  })

  shiny:::observe({
    req(input$user == '' && input$passwod == '')
    isolate({
      rvlue(0)})
  })

  shiny:::observe({
    input$sgn
    req(isolate({rule()})==F)
    isolate({
      rvlue(0)})
  })
  shiny:::observe({
    input$sgn
    req(rvlue() == 1)
    req(isolate({rule()})==T)
    delay(45000,
          refresh())
  })
  rtnn<-reactiveVal(0)
  observe({
    input$sgn
    req(rvlue() == 1)
    invalidateLater(1000,session = session)
    isolate({
      rtnn(rtnn()+1)
    })
  })


  output$crednew<-rhandsontable:::renderRHandsontable({
    input$sgn
    req(rule()==T)
    credential<-dbcreddf()
    credential$password <- substr(credential$password,2,90)
    dp<-dbcreddf()[seq(0,nrow(dbcreddf())-1),]
    req(input$passwod%in%dp$password==F)
    rhandsontable(
      credential[nrow(credential),])
  })

  shiny:::observeEvent(input$sgnp, {
    showModal(modalDialog(easyClose = F,footer = '',fade = F,
                          title = 'create account',
                          fluidPage( fresh:::use_theme(fresh:::create_theme(

                            fresh:::bs_vars_font(
                              family_sans_serif = "'Saira Stencil One', cursive"
                            )
                          )),
                          rhandsontable:::rHandsontableOutput('crednew'),
                          uiOutput('cbtn'),
                          textInput('user',placeholder = 'type username','enter username:'),
                          textInput('passwod',placeholder = 'type password','enter password:'),
                          textInput('againpasswod',placeholder = 'confirm password',''),
                          hr(),uiOutput('okbtn')

                          )
    ))
  })
  jscodelog <- function(){

    '$(document).keyup(function(e) {
    if (e.key == "Enter") {
    $("#loginok").click();
}});'}
  lrvlue <- reactiveVal(0)
  vrv<-reactive({
    input$loginok
    crd <- credentialz()

    req(input$username != '' && input$password != '')
    req(paste0('#',input$password) == crd %>% filter(username ==
                                                       input$username) %>% filter(password == paste0('#',input$password)) %>%
          select(password))
    return(T)

  })

  vrvr<-reactive({
    input$loginok
    crd <- credentialz()

    req(input$username != '' && input$password != '')
    req(paste0('#',input$password) != crd %>% filter(username ==
                                                       input$username) %>% filter(password == paste0('#',input$password)) %>%
          select(password))
    return(T)

  })



  shiny:::observe({
    input$loginok
    req(length(input$username) != 0 && length(input$password) != 0)
    req(isolate({vrv()})==T)
    isolate({
      lrvlue(lrvlue()+1)})
  })




  observeEvent(input$loginok,{
    crd <- credentialz()
    if(length(paste0('#',input$password) == crd %>% filter(username ==
                                                           input$username) %>% filter(password == paste0('#',input$password)) %>%
              select(password))==0){

      delay(2500,refresh())}
  })


  output$rcvd<-renderUI({
    crd <- credentialz()
    if(input$rcver%in%crd$recovery==T)
      dt<-crd%>%filter(recovery==input$rcver)
    usernm=dt$username
    passwd=dt$password
    fluidPage( fresh:::use_theme(fresh:::create_theme(

      fresh:::bs_vars_font(
        family_sans_serif = "'Saira Stencil One', cursive"
      )
    )),
    'Hi, these are your credentials',br(),
    paste('username','=',usernm),
    paste('password','=',passwd)
    )
  })


  observeEvent(input$fgt,{

    showModal(modalDialog(footer = '',title = '#recovery page',

                          fluidRow(
                            fresh:::use_theme(fresh:::create_theme(

                              fresh:::bs_vars_font(
                                family_sans_serif = "'Saira Stencil One', cursive"
                              )
                            )),
                            textInput('rcver',placeholder = 'enter recovery code',''),
                            uiOutput('rcvd')

                          )
    )
    )

  })


  configEvent <- function (event.starting_time = "", event.closing_time = "",input, output, session){
    if (lubridate:::seconds_to_period(as.POSIXct(paste(paste0(lubridate:::year(Sys.time()),
                                                             "-", lubridate:::month(Sys.time()), "-",
                                                             lubridate:::day(Sys.time())), event.closing_time))) >
        lubridate:::seconds_to_period(as.POSIXct(paste(paste0(lubridate:::year(Sys.time()),
                                                             "-", lubridate:::month(Sys.time()), "-",
                                                             lubridate:::day(Sys.time())), event.starting_time)))) {
      if(lubridate:::seconds_to_period(as.POSIXct(paste(paste0(lubridate:::year(Sys.time()),
                                                              "-", lubridate:::month(Sys.time()), "-",
                                                              lubridate:::day(Sys.time())), event.starting_time))) <
         lubridate:::seconds_to_period(Sys.time()) &&
         lubridate:::seconds_to_period(as.POSIXct(paste(paste0(lubridate:::year(Sys.time()),
                                                              "-", lubridate:::month(Sys.time()), "-",
                                                              lubridate:::day(Sys.time())), event.closing_time))) >
         lubridate:::seconds_to_period(Sys.time())){
        return('run')}else{
          return('dont')
        }

    }else
      if (lubridate:::seconds_to_period(as.POSIXct(paste(paste0(lubridate:::year(Sys.time()),
                                                               "-", lubridate:::month(Sys.time()), "-",
                                                               lubridate:::day(Sys.time())), event.closing_time))) <
          lubridate:::seconds_to_period(as.POSIXct(paste(paste0(lubridate:::year(Sys.time()),
                                                               "-", lubridate:::month(Sys.time()), "-",
                                                               lubridate:::day(Sys.time())), event.starting_time)))) {
        if(lubridate:::seconds_to_period(as.POSIXct(paste(paste0(lubridate:::year(Sys.time()),
                                                                "-", lubridate:::month(Sys.time()), "-",
                                                                lubridate:::day(Sys.time())), event.closing_time))) >
           lubridate:::seconds_to_period(Sys.time())){
          return('run')
        }else
          if(lubridate:::seconds_to_period(as.POSIXct(paste(paste0(lubridate:::year(Sys.time()),
                                                                  "-", lubridate:::month(Sys.time()), "-",
                                                                  lubridate:::day(Sys.time())), event.starting_time))) <
             lubridate:::seconds_to_period(Sys.time()) &&
             lubridate:::seconds_to_period(as.POSIXct(paste(paste0(lubridate:::year(Sys.time()),
                                                                  "-", lubridate:::month(Sys.time()), "-",
                                                                  lubridate:::day(Sys.time())), '23:59:59'))) >
             lubridate:::seconds_to_period(Sys.time())){
            return('run')
          }else{
            return('dont')
          }
      }
  }



  observe({
    xvlue<-isolate(logTMfun())
    x<-xvlue$data
    req(all((x[,1]!=''),(x[,2]!='')))
    if(lubridate:::seconds_to_period(as.POSIXct(paste(paste0(lubridate:::year(Sys.time()),
                                                            "-", lubridate:::month(Sys.time()), "-", lubridate:::day(Sys.time())),
                                                     x[,1])))>lubridate:::seconds_to_period(Sys.time())){
      shinyjs:::delay(as.numeric(lubridate:::seconds(round(lubridate:::seconds_to_period(as.POSIXct(paste(paste0(lubridate:::year(Sys.time()),
                                                                                                              "-", lubridate:::month(Sys.time()), "-", lubridate:::day(Sys.time())),
                                                                                                       x[,1]))) - lubridate:::seconds_to_period(Sys.time()))) *
                                  1000), shinyjs:::runjs("function reload_page() { window.location.reload(); setTimeout(reload_page, 1000); } setTimeout(reload_page, 1000); "))
    }else
      if(lubridate:::seconds_to_period(as.POSIXct(paste(paste0(lubridate:::year(Sys.time()),
                                                              "-", lubridate:::month(Sys.time()), "-", lubridate:::day(Sys.time())),
                                                       x[,2])))>lubridate:::seconds_to_period(Sys.time())){
        shinyjs:::delay(as.numeric(lubridate:::seconds(round(lubridate:::seconds_to_period(as.POSIXct(paste(paste0(lubridate:::year(Sys.time()),
                                                                                                                "-", lubridate:::month(Sys.time()), "-", lubridate:::day(Sys.time())),
                                                                                                         x[,2]))) - lubridate:::seconds_to_period(Sys.time()))) *
                                    1000), shinyjs:::runjs("function reload_page() { window.location.reload(); setTimeout(reload_page, 1000); } setTimeout(reload_page, 1000); "))
      }
  })
  xvlue<-isolate({logTMfun()})
  x<-isolate({xvlue$data})
  uptime <- reactiveVal({
    if (lubridate:::seconds_to_period(as.POSIXct(paste(paste0(lubridate:::year(Sys.time()),
                                                             "-", lubridate:::month(Sys.time()), "-",
                                                             lubridate:::day(Sys.time())), x[,2]))) >
        lubridate:::seconds_to_period(as.POSIXct(paste(paste0(lubridate:::year(Sys.time()),
                                                             "-", lubridate:::month(Sys.time()), "-",
                                                             lubridate:::day(Sys.time())), x[,1])))) {
      if (lubridate:::seconds_to_period(Sys.time()) <
          lubridate:::seconds_to_period(as.POSIXct(paste(paste0(lubridate:::year(Sys.time()),
                                                               "-", lubridate:::month(Sys.time()), "-", lubridate:::day(Sys.time())),
                                                        x[,1])))) {
        round(as.numeric(as.POSIXct(paste(paste0(lubridate:::year(Sys.time()),
                                                 "-", lubridate:::month(Sys.time()), "-", lubridate:::day(Sys.time())
        ), x[,1]))) - as.numeric(Sys.time()))
      }else
        if (lubridate:::seconds_to_period(Sys.time()) >
            lubridate:::seconds_to_period(as.POSIXct(paste(paste0(lubridate:::year(Sys.time()),
                                                                 "-", lubridate:::month(Sys.time()), "-", lubridate:::day(Sys.time())),
                                                          x[,1])))) {
          round(as.numeric(as.POSIXct(paste(paste0(lubridate:::year(Sys.time()),
                                                   "-", lubridate:::month(Sys.time()), "-", lubridate:::day(Sys.time())
          ), x[,2]))) - as.numeric(Sys.time()))
        }
    }else
      if (lubridate:::seconds_to_period(as.POSIXct(paste(paste0(lubridate:::year(Sys.time()),
                                                               "-", lubridate:::month(Sys.time()), "-",
                                                               lubridate:::day(Sys.time())), x[,2]))) <
          lubridate:::seconds_to_period(as.POSIXct(paste(paste0(lubridate:::year(Sys.time()),
                                                               "-", lubridate:::month(Sys.time()), "-",
                                                               lubridate:::day(Sys.time())), x[,1])))) {
        if (lubridate:::seconds_to_period(Sys.time()) >
            lubridate:::seconds_to_period(as.POSIXct(paste(paste0(lubridate:::year(Sys.time()),
                                                                 "-", lubridate:::month(Sys.time()), "-", lubridate:::day(Sys.time())),
                                                          x[,1])))) {
          round(as.numeric(as.POSIXct(paste(paste0(lubridate:::year(Sys.time()),
                                                   "-", lubridate:::month(Sys.time()), "-", lubridate:::day(Sys.time())+1
          ), x[,2]))) - as.numeric(Sys.time()))
        }else
          if (lubridate:::seconds_to_period(Sys.time()) <
              lubridate:::seconds_to_period(as.POSIXct(paste(paste0(lubridate:::year(Sys.time()),
                                                                   "-", lubridate:::month(Sys.time()), "-", lubridate:::day(Sys.time())),
                                                            x[,2])))) {
            round(as.numeric(as.POSIXct(paste(paste0(lubridate:::year(Sys.time()),
                                                     "-", lubridate:::month(Sys.time()), "-", lubridate:::day(Sys.time())+1
            ), x[,2]))) - as.numeric(Sys.time()))
          }else
            if (lubridate:::seconds_to_period(Sys.time()) <
                lubridate:::seconds_to_period(as.POSIXct(paste(paste0(lubridate:::year(Sys.time()),
                                                                     "-", lubridate:::month(Sys.time()), "-", lubridate:::day(Sys.time())),
                                                              x[,1]))) &&
                lubridate:::seconds_to_period(Sys.time()) >
                lubridate:::seconds_to_period(as.POSIXct(paste(paste0(lubridate:::year(Sys.time()),
                                                                     "-", lubridate:::month(Sys.time()), "-", lubridate:::day(Sys.time())),
                                                              x[,2])))) {
              round(as.numeric(as.POSIXct(paste(paste0(lubridate:::year(Sys.time()),
                                                       "-", lubridate:::month(Sys.time()), "-", lubridate:::day(Sys.time())
              ), x[,1]))) - as.numeric(Sys.time()))
            }
      }
  })

  observe({
    invalidateLater(1000,session = session)

    isolate({

      uptime(uptime()-1)

    })

  })

  observe({
    invalidateLater(1000,session = session)
    req(isolate({lubridate:::seconds_to_period(uptime())})==0)
    shinyjs:::runjs("function reload_page() { window.location.reload(); setTimeout(reload_page, 1000); } setTimeout(reload_page, 1000); ")
  })


  output$uptm<-renderUI({
    paste("closed. opens at",x[,1], '|','Time left:',lubridate:::seconds_to_period(uptime()))
  })

  output$logmod<-renderUI({
    xvlue<-logTMfun()
    x<-xvlue$data

    if(all((x[,1]!=''),(x[,2]!=''))){

      if(configEvent(event.starting_time = x[,1], event.closing_time = x[,2],
                     input, output, session)=='run'){
        shiny:::tabsetPanel(
          miniUI:::miniTabPanel(tagList(icon('user',style='font-size:150%'),''),useShinyjs(),
                               fluidPage(if(length(input$sgnp)==0){tags$head(tags$script(HTML(jscodelog())))},
                                         fluidRow(column(1),
                                                  column(10,div(style = "border-style: solid; border-radius: 4px; border-color: blue;",fluidPage(

                                                    textInput('username',placeholder = 'type username',label = 'enter username:',''),
                                                    passwordInput('password',placeholder = 'type password','enter password:'),

                                                    shinyWidgets:::actionBttn('loginok','login',style = 'fill',color = 'su',
                                                               size = 'sm'),
                                                    hr(),actionLink('fgt',span('forgot password?',style='color:brown')),'|',
                                                    actionLink('sgnp',span('sign-up',style='color:blue'))

                                                  )))),
                                         fluidRow(
                                           uiOutput("UIlogfailed")
                                         )
                               )

          ),
          miniUI:::miniTabPanel(icon = icon('wrench'),'Tools',
                               div(style = "border-style: solid; border-radius: 4px; border-color: gray;",'# set application login time. where, openlogTM is the time where the login page will open while closelogTM is the time where the login page will close daily. NB: time formarts should be as follows; Hour:Minute:Second, e.g., 08:00:00',
                                   rhandsontable:::rHandsontableOutput('toolstbl'),
                                   checkboxInput('setlogTM','type in the table and click me',value=F)))

        )}else
          if(configEvent(event.starting_time = x[,1], event.closing_time = x[,2],
                         input, output, session)=='dont'){
            fluidPage(shiny:::tabsetPanel(
              miniUI:::miniTabPanel(icon = icon('user'),'login',useShinyjs(),
                                   uiOutput('uptm')),
              miniUI:::miniTabPanel(icon = icon('wrench'),'Tools',
                                   div(style = "border-style: solid; border-radius: 4px; border-color: gray;",'# set application login time. where, openlogTM is the time where the login page will open while closelogTM is the time where the login page will close daily. NB: time formarts should be as follows; Hour:Minute:Second, e.g., 08:00:00',
                                       rhandsontable:::rHandsontableOutput('toolstbl'),
                                       checkboxInput('setlogTM','type in the table and click me',value=F)))
            )
            )
          }}else{fluidPage(
            shiny:::tabsetPanel(
              miniUI:::miniTabPanel(tagList(icon('user',style='font-size:150%'),''),useShinyjs(),
                                   fluidRow(if(length(input$sgnp)==0){tags$head(tags$script(HTML(jscodelog())))},

                                            column(12,div(style = "border-style: solid; border-radius: 4px; border-color: blue;",fluidPage(

                                              textInput('username',placeholder = 'type username','enter username:'),
                                              passwordInput('password',placeholder = 'type password','enter password:'),
                                              shinyWidgets:::actionBttn('loginok','login',style = 'fill',color = 'su',
                                                         size = 'sm'),
                                              hr(),actionLink('fgt',span('forgot password?',style='color:brown')),'|',
                                              actionLink('sgnp',span('sign-up',style='color:blue')),
                                              uiOutput("UIlogfailed")

                                            ))))
              ),
              miniUI:::miniTabPanel(icon = icon('wrench'),'Tools',
                                   div(style = "border-style: solid; border-radius: 4px; border-color: yellow;",'# set application login time. where, openlogTM is the time where the login page will open while closelogTM is the time where the login page will close daily. NB: time formarts should be as follows; Hour:Minute:Second, e.g., 08:00:00',
                                       rhandsontable:::rHandsontableOutput('toolstbl'),
                                       checkboxInput('setlogTM','type in the table and click me',value=F)))
            )
          )
          }
  })



  output$cloxe<-renderUI({
    input$loginok
    req(lrvlue()>=2)
    crd <- credentialz()
    req(paste0('#',input$password) == crd %>% filter(username ==
                                                       input$username) %>% filter(password == paste0('#',input$password)) %>%
          select(password))
    modalButton('close login page')
  })



}



user <- function ()
{
  tags$ul(shiny:::uiOutput("userloggedin"))
}




#' The server function of shinySQLite
#'
#' This is function a server function that renders the outputs as well as sending statements to SQLite database.
#'
#' This function is used together with it's ui companion,  dataentryui(mytheme)
#'
#' @examples library(shiny)
#' library(RSQLite)
#' library(DT)
#' library(shinySQLite)
#' ui <- dataentryui(title='myapp',analysistab='tab')
#' server<-function(input,output,session){
#' dataentryserver(input = input,output = output,session = session)
#' }
#' shinyApp(ui, server)
#'
#' @param input Shiny input object passed from the server.
#' @param output Shiny output object passed from the server.
#' @param session Shiny session object passed from the server.
#'        See \link{https://github.com/nasilabednego/shinySQLite} for more information.
#' @import shiny
#' @export
dataentryserver <- function(input, output, session, authenticate = F) {
  users<-reactiveValues(count=0)

  credentials<-data.frame(username=c('Abednego'),password=c(1234))
  credentials$password <- paste0('#',credentials$password)

  secureapp(credentials=credentials, input, output, session=session
  )
  output$tf<-renderText({
    T
  })
  rvlue <- reactiveVal(0)
  con <- DBI:::dbConnect(SQLite(),'cred')
  cre<-isolate({dbReadTable(con, 'credData')})
  shiny:::observe({
    input$sgn
    req(input$user != '' && paste0('#',input$passwod) != '')
    req(input$againpasswod == paste0('#',input$passwod))
    isolate({
      rvlue(rvlue()+1)})
  })
  shiny:::observe({
    input$sgn
    req(input$user != '' && paste0('#',input$passwod) != '')
    req(input$againpasswod != paste0('#',input$passwod))
    isolate({
      rvlue(-1)})
  })

  shiny:::observe({
    req(input$user == '' && paste0('#',input$passwod) == '')
    isolate({
      rvlue(0)})
  })

  shiny:::observe({
    input$sgn
    req(rvlue() == 1)
    req(isolate({rule()})==T)
    delay(3500,
          refresh())
  })
  shiny:::observe({
    input$sgn
    req(isolate({rule()})==T)
    isolate({
      rvlue(0)})
  })
  dbcreddf<-reactive({
    req(length(input$sgn)!=0)
    input$sgn
    req(rvlue() == 1)
    con <- DBI:::dbConnect(SQLite(),'cred')
    df <- data.frame(username = input$user, password = paste0('#',input$passwod))

    dbWriteTable(con, 'credData',df, append = T)



    data = dbReadTable(con, 'credData')
    return(data)
  })

  credent <- function(){
    con <- DBI:::dbConnect(SQLite(),'cred')
    data = dbReadTable(con, 'credData')
    return(data)
  }

  credentialz <- reactiveVal(cre)
  observe({
    input$sgn
    con <- DBI:::dbConnect(SQLite(),'cred')
    data <- isolate({dbcreddf()})
    isolate({
      credentialz(data)
    })
  })
  observe({
    input$loginok
    con <- DBI:::dbConnect(SQLite(),'cred')
    data <- dbReadTable(con, 'credData')
    isolate({
      credentialz(data)
    })
  })
  observe({
    req(rule()==F)
    req(paste0('#',input$passwod)!=input$againpasswod)
    con <- DBI:::dbConnect(SQLite(),'cred')
    data <- isolate({remodub()})
    isolate({
      credentialz(data)
    })
  })
  remodub<-reactive({
    req(rule()==F)
    req(paste0('#',input$passwod)!=input$againpasswod)
    con <- DBI:::dbConnect(SQLite(),'cred')
    DBI:::dbBegin(con)
    DBI:::dbSendStatement(con,
                         'DELETE FROM credData
WHERE rowid NOT IN (
  SELECT MIN(rowid)
  FROM credData
  GROUP BY password
)')
    DBI:::dbCommit(con)

    data = dbReadTable(con, 'credData')
  })

  rule <- reactive({
    input$sgn
    req(rvlue() == 1)
    req(paste0('#',input$passwod)==input$againpasswod)
    dp<-dbcreddf()
    dp<-dbcreddf()[seq(0,nrow(dbcreddf())-1),]
    if(paste0('#',input$passwod)%in%dp$password==F){
      return(T)}else{
        FALSE
      }
  })
  lrvlue <- reactiveVal(0)
  vrv<-reactive({
    input$loginok
    crd <- isolate({credentialz()})

    req(length(input$username) != 0 && length(input$password) != 0)
    req(paste0('#',input$password) == crd %>% filter(username ==
                                                       input$username) %>% filter(password == paste0('#',input$password)) %>%
          select(password))


  })


  shiny:::observe({
    input$loginok
    req(input$username != '' && input$password != '')
    req(isolate({vrv()})==T)
    isolate({
      lrvlue(lrvlue()+1)})
  })

  shiny:::observe({
    input$loginok
    req(input$username != '' && input$password == '')
    isolate({
      lrvlue(0)})
  })


  crd=isolate({credentialz()})


  output$page<-renderUI({

    shiny:::tabsetPanel(id='href.link',
                       tabPanel(p(id='dbes',tagList(icon=icon('file'),
                                                    title = "Files")),
                                if(length(input$initdb)!=0){fluidRow(
                                  column(6,
                                         uiOutput('dbcreatdbui')),
                                  column(6,
                                         uiOutput('uidb'))


                                )}else{fluidRow(column(3),
                                                column(6,
                                                       uiOutput('dbcreatdbui')
                                                ))}




                       ),
                       tabPanel(p(id='dfrem',tagList(icon = icon('pencil-alt'),'edit_Table')),
                                fluidRow(bs4Dash:::box(width = 4,title = tagList(span(style='color:olive',icon('wrench',style='font-size:150%')),span(style='color:black','# controls & inputs')),
                                                      div(uiOutput('editoption'),class="bg-gray-light")),
                                         bs4Dash:::box(width = 8,title=tagList(span(style='color:red',icon('pencil_ellipsis_rectangle',style='font-size:150%')),span(style='color:black','# edit & outputs')),
                                                      uiOutput('uitbls')
                                         ))))
  }
  )




  jscodecode <- '$(document).keyup(function(e) {
    if (e.key == "Enter") {
    $("#code").click();
}});'


  output$codeoutput<-renderUI({
    fluidPage(
      tags$head(tags$script(HTML(jscodecode))),
      sidebarLayout(sidebarPanel(radioButtons(inline = T,'outputoptions',choices = c('Print','Plot'),'Output options'),
                                 shinyWidgets:::actionBttn('code','Run',color = 'suc',size = 'sm',style = 'fil')
      ),
      mainPanel(
        shinyAce:::aceEditor(height = '100px',
                            autoScrollEditorIntoView = T,                                                                                     outputId = "myeditor",                                                                                             placeholder = 'code',                                                                                                                                                                                 mode = "r",value = '',
                            autoComplete = "live",
                            autoCompleteList = names(mtcars),                                                                                                       autoCompleters="snippet" ),
        verbatimTextOutput('myrenderprint'),plotly:::plotlyOutput('myrenderplot'),uiOutput('myuioutput'))
      )
    )
  })

  evl<-reactiveVal(0)

  observe({
    input$code
    isolate({
      evl(input$myeditor)
    })
  })


  output$myrenderprint<-renderPrint({

    req(input$outputoptions=='Print')
    if(input$outputoptions=='Print'){
      value<-eval(parse(text=evl()))
      return(value)}
  })
  output$myrenderplot<-plotly:::renderPlotly({

    req(input$outputoptions=='Plot')

    if(input$outputoptions=='Plot'){
      value<-eval(parse(text=evl()))
      return(value)}
  })
  output$myrenderui<-renderUI({

    req(input$outputoptions=='UI')

    if(input$outputoptions=='UI'){
      value<-eval(parse(text=evl()))
      return(value)}
  })

  output$availbledbs<-renderUI({
    req(length(input$initdb)!=0)

    checkboxInput('see','See databases',value = F)
  })


  output$tbl.title<-renderUI({
    req(length(input$initdb)!=0)

    'data.frame'
  })




  output$uidlnd<-renderUI({

    sidebarLayout(sidebarPanel(uiOutput("downlodui")),
                  mainPanel(rhandsontable:::rHandsontableOutput("dbtable")))

  })

  output$dui<-renderUI({
    req(length(input$initdb)!=0)
    uiOutput('uidlnd')
  })






  ####################################-------------- false connection
  con <- DBI:::dbConnect(SQLite(), 'Processing...')
  mydbstore <- data.frame(DBname = "", created.on = "",created.by='',check.names = F)
  if(DBI:::dbExistsTable(con,'masterdb')==F){
    RSQLite:::dbCreateTable(con, "masterdb", mydbstore)}
  ###################################--------------



  tm <- data.frame(logs = "",check.names = F)
  conb <- DBI:::dbConnect(SQLite(), 'timdb')
  if(DBI:::dbExistsTable(conb,'dbdfb')==F){
    RSQLite:::dbCreateTable(conb, "dbdfb", tm)}



  logsdb<- reactive({
    req(length(input$initdb)!=0)

    tm <- data.frame(logs = input$opened,check.names = F)
    conb <- DBI:::dbConnect(SQLite(), 'timdb')
    DBI:::dbWriteTable(conb, "dbdfb", tm,check.names=F, append = T)


    tbo<-DBI:::dbReadTable(conb,'dbdfb',check.names=F)
    return(tbo)
  })


  lenr<-eventReactive(input$opened,{
    req(length(input$initdb)!=0)
    se<-logsdb()
    sel<-se[,1]
    lensel<-length(sel)
    return(
      lensel)

  })





  observe({

    input$loginok
    req(length(input$initdb)!=0)


    selc<-logsdb()[numeric(lenr()-1),]

    read <- awesomedt()%>%dplyr:::filter(database==input$dbstorage)
    choices <- read$dataframe
    #updateSelectInput(session,"opened", uiOutput("opnddb"),choices = choices,selected = selc)

  })




  observe({
    input$loginok
    req(length(input$initdb)!=0)
    req(input$dbstorage)

    mydbstore <- data.frame(DBname = "", created.on = "",created.by=input$username,check.names = F)
    con <- DBI:::dbConnect(SQLite(), input$dbstorage)
    DBI:::dbWriteTable(con, "masterdb", mydbstore,check.names=F, append = TRUE)
    DBI:::dbBegin(con)
    rs <- DBI:::dbSendStatement(con, paste("DELETE from masterdb WHERE DBname = '' "))
    DBI:::dbGetRowsAffected(rs)
    DBI:::dbClearResult(rs)
    DBI:::dbCommit(con)



  })



  masterdbdt <- reactive({
    input$loginok
    req(length(input$initdb)!=0)

    req(length(input$username) != 0 && length(paste0('#',input$password)) != 0)
    con <- DBI:::dbConnect(con, isolate({input$dbstorage}))
    if (input$dbcreatenew == T) {

      DBI:::dbWriteTable(con, "masterdb", data.frame(DBname = input$dbaddnewdb,
                                                    created.on = paste(Sys.time()),created.by=input$username,check.names = F),check.names=F, append = TRUE)
      DBI:::dbBegin(con)
      rs <- DBI:::dbSendStatement(con, paste("DELETE from masterdb WHERE DBname = '' "))
      DBI:::dbGetRowsAffected(rs)
      DBI:::dbClearResult(rs)
      DBI:::dbCommit(con)
      DBI:::dbBegin(con)
      DBI:::dbSendStatement(con,
                           'DELETE FROM masterdb
WHERE rowid NOT IN (
  SELECT MIN(rowid)
  FROM masterdb
  GROUP BY DBname
)')
      DBI:::dbCommit(con)
    }
    data <- DBI:::dbReadTable(con, "masterdb",check.names=F)
    return(data)
  })

  output$dbsavailble<-renderUI({

    req(input$see==T)
    rhandsontable:::rHandsontableOutput('myavailableDBs')
  })


  output$myavailableDBs <- rhandsontable:::renderRHandsontable({
    input$loginok
    req(length(input$initdb)!=0)
    req(input$see==T)
    dt<-masterdbdt()
    db.state<-masterdbdt()[,1]
    notopend<-which(db.state!=input$dbstorage)
    db.state[notopend]<-'open corresponding db'
    opend<-which(db.state!='open corresponding db')
    db.state[opend]<-paste(timer(),'minutes','running')

    bind<-cbind(masterdbdt(),db.state)
    finedb<-data.frame(bind,check.names = F)
    finedb

    rhandsontable:::rhandsontable(finedb)

  })

  inputdb<-reactiveVal(selectInput("dbstorage", 'you have selected this database',
                                   choices = 'Processing...'))
  output$dbopendbase <- renderUI({
    input$loginok
    req(isolate({lrvlue()})>=1)
    inputdb()

  })

  output$logoutBtn<-renderUI({
    input$loginok
    req(isolate({lrvlue()})>=1)

    actionLink('logout',span(icon('lock',style='font-size:150%'),style='color:yellow','logout'))
  })
  output$nBtn<-renderUI({
    input$loginok
    req(isolate({lrvlue()})>=1)
    tooltip(placement = 'top',
            actionLink('initdb',span(icon('fire',style='font-size:150%'),style='color:orange','init')),
            title='# _app initialize')
  })
  observe({
    input$loginok
    req(length(input$initdb)!=0)
    choice<-masterdbdt()[,1]
    choices <- choice
    updateSelectInput("dbstorage", ' open any database',
                      choices = choices,session = session)
    shinyjs:::toggle('logmod')
  })

  output$crt<-renderUI({
    input$loginok
    req(isolate({lrvlue()})>=1)

    fluidPage(
      textInput('dbaddnewdb',placeholder='type dbname','',width = 125),
      checkboxInput("dbcreatenew", 'create database',
                    value = F))
  })
  output$dbcreatdbuig <- renderUI({

    fluidRow(
      inputPanel(

        uiOutput('crt'),
        uiOutput('dbopendbase'),
        uiOutput('stru')),
      inputPanel(
        uiOutput("creatdbui"),inputPanel(uiOutput("opendb")),
        uiOutput('dui')),
      uiOutput('dbsavailble'))
  })

  output$dbcreatdbui <- renderUI({

    fluidPage(

      bs4Dash:::box(width = 12,title = tagList(span(icon('folder',style='font-size:150%'),style='color:olive'),

                                              if(length(input$initdb)!=0){span(style='color:olive',actionLink(inputId = "note",label = '# database'))}
                                              else{span(style='color:black','# login to database')}),solidHeader = T,
                   if(length(input$initdb)==0){uiOutput('logmod')},

                   fluidRow(
                     uiOutput('crt'),
                     uiOutput('dbopendbase'),
                     tags$a(HTML('<a cursor: pointer;" onclick="updateNavbarTheme(&#39;navbar-gray&#39;);"</a>')#uiOutput('stru')
                     )
                   )
      ),
      if(length(input$initdb)!=0){
        bs4Dash:::box(width = 12,solidHeader = T,title = tagList(span(icon('table',style='font-size:150%'),style='color:#175BD6'),span(style='color:black','# create tables')),
                     fluidPage(fluidRow(
                       uiOutput("creatdbui"),div(style = "border-style: solid; border-radius: 4px; border-color: silver;",uiOutput("opendb"))),
                       uiOutput('dbsavailble')))})
  })
  output$mandb<-renderUI({if(length(input$initdb)!=0){
    actionLink('man',span(icon('shield',style='font-size:150%; color:fuchsia','manage db'),
                          span(style='font-size:90%; color:fuchsia','manage db')))
  }})

  observeEvent(input$man,{
    shinyWidgets:::ask_confirmation(
      inputId = 'confirm_man',
      type = 'question',
      text = fluidPage('# Not available now but development in progress...')
    )
  })
  observeEvent(input$moretips,{
    shinyWidgets:::show_alert(
      title = '# shinySQLite daily tips',
      inputId = 'confirm_man',
      text = fluidPage('# Not available now but development in progress...')
    )
  })
  activatedbs<-reactiveVal({0})
  output$stru<-renderUI({

    tooltip(checkboxInput('loginok',uiOutput('strudfn'),value = F),title = 'connect to your databases')
  })

  output$strudfn<-renderUI({

    if(input$dbcreatenew==T){
      paste(input$dbaddnewdb,'db has been created successfully')}else
        'connect'
  })

  observe({
    req(input$dbcreatenew==T)
    shinyjs:::toggleState('loginok')
  })



  observe({
    invalidateLater(1000,session)
    isolate({
      activatedbs(activatedbs()+1)
    })
  })












  #mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm







  observe({
    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    mydb <- data.frame(dataframe = "",database='', created.on = "",created.by=input$username,check.names = F)
    con <- DBI:::dbConnect(SQLite(), input$dbstorage)
    DBI:::dbWriteTable(con, "awesome", mydb,check.names=F, append = TRUE)
    DBI:::dbBegin(con)
    rs <- DBI:::dbSendStatement(con, paste("DELETE from awesome WHERE dataframe = '' "))
    DBI:::dbGetRowsAffected(rs)
    DBI:::dbClearResult(rs)
    DBI:::dbCommit(con)


  })

  output$availbledt<-renderUI({
    req(length(input$initdb)!=0)
    input$dbstorage
    tags$b(span(style='color:black',span("# Available data frames and their respective databases",style='font-size:78%')))
  })

  output$uplodmerge <- renderUI({
    req(length(input$initdb)!=0)
    input$dbstorage
    input$opened
    fluidPage(radioButtons("fileType_merge", label = h4("Choose File type"),
                           choices = list(`.csv/txt` = 1, .xlsx = 2),
                           selected = 1, inline = TRUE), fileInput("fileb",
                                                                   h4("Upload csv/xlsx"), accept = c("text/csv",
                                                                                                     "text/comma-separated-values,text/plain",
                                                                                                     ".csv", ".xlsx")), hr(), checkboxInput("merge",
                                                                                                                                            "bind the uploaded data to your table", value = F))

  })
  Xbind =function(){

    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    input$opened
    req(input$opened!='')
    input$fileb
    req(length(input$fileb) != 0)
    req(input$fileb!='')
    reactiveValues(data = isolate(
      uplobind()))
  }


  observe({
    input$dbstorage
    input$opened
    input$fileb
    req(input$fileb!='')
    X=X()
    len=length(X$data[,1])

    lenb=length(isolate({uplobind()})[,1])
    nm=names(X$data)
    nmb=names(isolate({uplobind()}))
    vek<-nmb%in%nm
    aTRUE<-T%in%vek
    fTRUE<-F%in%vek
    if(len==lenb && all(aTRUE)==F){
      if(aTRUE==T && fTRUE==T){
        shinyWidgets:::ask_confirmation(
          inputId = 'conf',
          title = '#Bind table options',
          text = fluidPage('Choose how to bind your tables, e.g whether to bind rows or columns'),
          btn_labels = c('Bind_columns','Bind_rows'),
          btn_colors = c('blue','green')

        )}}
  })

  output$uplodui <- renderUI({
    fluidPage(radioButtons("fileType_Input", label = h4("Choose File type"),
                           choices = list(`.csv/txt` = 1, .xlsx = 2),
                           selected = 1, inline = TRUE), fileInput("file1",
                                                                   h4("Upload csv/xlsx"), accept = c("text/csv",
                                                                                                     "text/comma-separated-values,text/plain",
                                                                                                     ".csv", ".xlsx")), hr(), textInput("saveas",
                                                                                                                                        "save your file as in the db", placeholder = "type the file name here"))

  })

  output$uploduistate <- renderUI({
    if(input$createnewb==T){
      'sucessfully created! open to edit in the select input '}
  })
  uplobind <- reactive({

    inFile <- input$fileb
    if (is.null(inFile)) {
      return(NULL)
    }
    if (input$fileType_merge == "1") {
      read.csv(inFile$datapath, header = TRUE, stringsAsFactors = FALSE)
    }
    else {
      readxl:::read_xlsx(inFile$datapath)
    }
  })

  uploadeddat <- reactive({
    if(input$createnewb == T){
      inFile <- input$file1
      if (is.null(inFile)) {
        return(NULL)
      }
      if (input$fileType_Input == "1") {
        read.csv(inFile$datapath, header = TRUE, stringsAsFactors = FALSE)
      }
      else {
        readxl:::read_xlsx(inFile$datapath)
      }}
  })

  uploadeddata<-reactive({
    head<-uploadeddat()[1,]
    head[]<-'#'
    rbind(head,uploadeddat())
  })


  redrec <- reactiveVal(0)
  observe({
    req(input$createnew == T)
    invalidateLater(1000, session)
    isolate({
      redrec(redrec() + 1)
    })
  })
  output$rid <- rhandsontable:::renderRHandsontable({
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$creatnewb==T)
    rhandsontable(uploadeddata()[redrec(), ])
  })
  dbtbl <- reactive({
    req(length(input$initdb)!=0)
    input$dbstorage
    con <- DBI:::dbConnect(SQLite(), input$dbstorage)
    if (input$createnew == T) {
      mydb <- data.frame(COL1 = "", COL2 = "",COL3 = "",check.names = F)
      DBI:::dbWriteTable(con, input$addnewdb, mydb,check.names=F, append = TRUE)

    }
    data <- DBI:::dbReadTable(con, input$addnewdb,check.names=F)
    return(data)
  })
  dbtbl2 <- reactive({
    req(length(input$initdb)!=0)
    input$dbstorage
    con <- DBI:::dbConnect(SQLite(), input$dbstorage)
    if (input$createnewb == T) {

      myd <- uploadeddata()[-1,]
      DBI:::dbWriteTable(con, input$saveas, myd,check.names=F, append = F,
                        overwrite = T)

    }
    data <- DBI:::dbReadTable(con, input$saveas,check.names=F)
    return(data)
  })
  output$dt <- rhandsontable:::renderRHandsontable({
    input$loginok
    req(length(input$initdb)!=0)
    if (input$createnew == T) {
      rhandsontable(dbtbl()[1, ])

    }
  })
  output$dt2 <- rhandsontable:::renderRHandsontable({
    input$loginok
    req(length(input$initdb)!=0)
    if (input$createnewb == T) {
      rhandsontable(dbtbl2()[redrec(), ])

    }
  })
  output$checkdb <- renderUI({
    input$loginok
    req(length(input$initdb)!=0)
    if (input$createnew == T) {
      HTML(paste("You have just added", span(style = "color:fucshsia",
                                             input$addnewdb), "data frame in",input$dbstorage, "db. To open it, type its name in the select input above"))
    }
  })
  choiseread<-reactive({
    req(lrvlue()>=0)
    req(input$dbstorage!='')
    input$dbstorage
    con <- DBI:::dbConnect(con, input$dbstorage)
    read <- awesomedt()%>%dplyr:::filter(database==input$dbstorage)

    choices <- c(read$dataframe)
    return(choices)
  })







  output$opendb <- renderUI({
    req(lrvlue()>=0)
    req(input$dbstorage!='')
    input$dbstorage
    con <- DBI:::dbConnect(con, input$dbstorage)
    read <- awesomedt()%>%dplyr:::filter(database==input$dbstorage)

    choices <- c(read$dataframe)
    fluidPage(selectInput("opened", uiOutput("opnddb"),
                          choices = choices,width = 150),
              textOutput('rctv'))
  })











  output$opnddb <- renderUI({
    if (input$opened != "") {
      HTML(paste(
        span(span(style = "color:green", input$opened),style='font-size:85%'),span('in',style='font-size:70%'),span(span(style = "color:green", input$dbstorage),style='font-size:85%'),span("db",style='font-size:70%'),
        span("is open edit in;",style='font-size:70%'),span(actionLink('link_to_edit','edit_Table'),style='font-size:85%')
      ))
    }
    else{
      req(input$dbstorage=='Processing...')

      HTML(paste(span(style = "color:navy",'create/upload the data.frame in opened database below')))
    }
  })
  output$checkcontrol <- renderUI({
    if (input$createnew == F) {
      "check to create"
    }
    else {
      span(style = "color:green", "Success! Your new dataframe is created in",input$dbstorage,"database")
    }
  })
  observe({
    req(input$createnew==T)
    updateCheckboxInput(session,checkboxInput("createnew",
                                              span(style = "color:green", "Success! Your new dataframe is created in",input$dbstorage,"database"),
                                              value = T))
  })


  output$editdbname <- renderUI({
    if (input$createnew == F) {
      textInput("addnewdb", placeholder = "type table name here",
                "",width = 130)
    }

  })
  output$uistmnt<-renderUI({

    if(req(length(isolate({input$dbstorage}))!=0)==T){

      if(input$dbstorage=='Processing...'){
        'create a database first'
      }
      else
        fluidPage( span("CREATE A NEW DATA FRAME OR UPLOAD .CSV/.XLSX ",style='font-size:80%'),
        )}else
          'click on begin connections'
  })
  output$uidbc<-renderUI({
    req(length(input$initdb)!=0)
    input$dbstorage
    fluidPage(
      inputPanel(                                                              fluidPage( uiOutput("checkdb"), rhandsontable:::rHandsontableOutput("dt"),
                                                                                          rhandsontable:::rHandsontableOutput("dt2")),rhandsontable:::rHandsontableOutput("rid")),wellPanel(uiOutput('availbledt'),
                                                                                                                                                                                          rhandsontable:::rHandsontableOutput("myavailabledbs")))

  })
  output$uidb<-renderUI({
    req(length(input$initdb)!=0)

    input$dbstorage

    fluidPage(
      bs4Dash:::box(width = 12,
                   title = tagList(span(style='color:green',icon('tree',style='font-size:150%')),span(style='color:green','# environment')),
                   shiny:::tabsetPanel(
                     tabPanel(icon = icon('memory'),'',
                              uiOutput('availbledt'),
                              rhandsontable:::rHandsontableOutput("myavailabledbs"),
                              uiOutput('availbledbs')),

                     tabPanel(icon = icon('share'),'export',
                              uiOutput('dui')),
                     tabPanel(tagList('a',icon('th',style='font-size:130%'),'b'),'',
                              '# rename table',
                              uiOutput('uirnmdf')),
                     tabPanel(icon('trash',style='font-size:130%'),'',
                              uiOutput('uidltdf')
                     ))),
      bs4Dash:::box(width = 12,title = tagList(span(style='color:#112446',icon('microscope',style='font-size:150%')),span(style='color:black','# tip of the day')),
                   fluidPage(fluidRow(span(style='color:black','# some activities in this pane are server only'),uiOutput("checkdb"),fluidPage( rhandsontable:::rHandsontableOutput("dt"),
                                                                                                                                                rhandsontable:::rHandsontableOutput("dt2")),rhandsontable:::rHandsontableOutput("rid"))))

    )})

  footeritem<-reactiveVal(0)
  observe({
    invalidateLater(60000,session = session)
    isolate({
      footeritem(footeritem()+1)
    })
  })
  semifooteritem<-reactiveVal(0)
  observe({
    invalidateLater(30000,session = session)
    isolate({
      semifooteritem(semifooteritem()+1)
    })
  })
  quotafooteritem<-reactiveVal(0)
  observe({
    invalidateLater(15000,session = session)
    isolate({
      quotafooteritem(quotafooteritem()+1)
    })
  })
  output$itmft<-renderUI({
    req(length(input$loginok)!=0)
    uiOutput('footitm')})


  output$footitm<-renderUI({
    req(length(input$loginok)!=0)
    if(footeritem()%%2==0){
      if(semifooteritem()%%2==1){
        span(style='color:black',paste('shinySQLite app',
                                       paste0('@',lubridate:::year(Sys.time()))),'v0.1.0')}else{
                                         if(quotafooteritem()%%2==1){
                                           span(style='color:black','Build & manage databases!')}else{
                                             span(style='color:black','Build, manage & share tables!')
                                           }
                                       }
    }else{
      if(semifooteritem()%%2==1){
        if(quotafooteritem()%%2==1){
          span(style='color:black','App Developed By: Dr Jhansi & Nasila')}else{
            tags$a(span(style='color:black','Email me;','drjhansi707@gmail.com'),mailto='drjhansi707@gmail.com')
          }
      }else{

        tags$a(span(style='color:black','Follow |'), 'https://github.com/nasilabednego',href='https://github.com/nasilabednego')
      }
    }
  })
  tbs<-reactiveVal(0)
  observe({
    req(length(input$initdb)!=0)
    tbs( shiny:::tabsetPanel(
      tabPanel(p(id='bld',tagList(icon = icon('hammer'),'New')),

               uiOutput("editdbname"),
               checkboxInput("createnew", 'check to create',
                             value = F)),
      tabPanel(p(id='dok',tagList(icon = icon('docker'),'Import')),

               uiOutput("uplodui"),uiOutput('uploduistate'),
               checkboxInput("createnewb", 'check to create',
                             value = F))
    ))
  })

  output$creatdbui <- renderUI({
    req(length(input$initdb)!=0)
    input$dbstorage
    tbs()
  })


  observe({
    req(input$createnewb == T)
    delay(1000,
          hide('uplodui')
    )
  })
  observe({
    req(input$createnewb == F)
    show('uplodui')

  })
  observe({
    req(input$createnewb == T)
    delay(3000,
          updateCheckboxInput(session,"createnewb", 'check to create',
                              value = F))

  })

  output$uidltdf<-renderUI({
    req(lrvlue()>=0)
    input$dbstorage
    dt<-awesomedt()
    x<-dt[,1]
    fluidPage(
      pickerInput('dfram',choices = x,label = 'select table'),
      checkboxInput('dltb',value = F,label = 'delete selected table')
    )
  })
  output$uirnmdf<-renderUI({
    req(lrvlue()>=0)
    input$dbstorage
    dt<-awesomedt()
    x<-dt[,1]
    fluidPage(
      fluidRow(column(5,
                      pickerInput('dfrnm',choices = x,label = 'select table to rename'),
                      textInput('rnmto','rename table to:'),
                      checkboxInput('rnmtb',value = F,label = 'rename selected table')),
               column(7,
                      rhandsontable:::rHandsontableOutput('mynmtb')
               ))
    )
  })
  rmytb<-reactiveVal(0)
  observe({
    req(input$rnmtb==T)

    isolate({
      rmytb(input$dfrnm)
    })
  })
  rnmed<-reactiveVal(0)
  observe({
    req(input$rnmtb==T)

    isolate({
      rnmed(input$rnmto)
    })
  })

  awesomedt <- reactive({
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$dbstorage!='')
    con <- DBI:::dbConnect(con, input$dbstorage)

    if (input$createnew == T) {
      DBI:::dbWriteTable(con, "awesome", data.frame(dataframe = input$addnewdb,database=input$dbstorage,
                                                   created.on = paste(Sys.time()),created.by=input$username,check.names = F),check.names=F, append = TRUE)
      DBI:::dbBegin(con)
      rs <- DBI:::dbSendStatement(con, paste("DELETE from awesome WHERE dataframe = '' "))
      DBI:::dbGetRowsAffected(rs)
      DBI:::dbClearResult(rs)
      DBI:::dbCommit(con)
      DBI:::dbBegin(con)
      DBI:::dbSendStatement(con,
                           'DELETE FROM awesome
WHERE rowid NOT IN (
  SELECT MIN(rowid)
  FROM awesome
  GROUP BY dataframe, database
)')
      DBI:::dbCommit(con)

    }
    else if (input$createnewb == T) {
      DBI:::dbWriteTable(con, "awesome", data.frame(dataframe = input$saveas,database=input$dbstorage,
                                                   created.on = paste(Sys.time()),created.by=input$username,check.names = F),check.names=F, append = TRUE)
      DBI:::dbBegin(con)
      rs <- DBI:::dbSendStatement(con, paste("DELETE from awesome WHERE dataframe = '' "))
      DBI:::dbGetRowsAffected(rs)
      DBI:::dbClearResult(rs)
      DBI:::dbCommit(con)
      DBI:::dbBegin(con)
      DBI:::dbSendStatement(con,
                           'DELETE FROM awesome
WHERE rowid NOT IN (
  SELECT MIN(rowid)
  FROM awesome
  GROUP BY dataframe, database
)')
      DBI:::dbCommit(con)

    }else{
      if(length(input$dltb)!=0){
        if(input$dltb==T){
          DBI:::dbBegin(con)


          rs <- DBI:::dbSendStatement(con, paste("DELETE from",
                                                "awesome",
                                                "WHERE", 'dataframe', '=',
                                                paste0("'", input$dfram,"'")))
          DBI:::dbGetRowsAffected(rs)
          DBI:::dbClearResult(rs)
          DBI:::dbCommit(con)
        }}else
          if(length(input$finlise)!=0){
            if(input$finlise==T){

              DBI:::dbBegin(con)
              DBI:::dbSendStatement(con, paste("UPDATE", "awesome",
                                              "SET", 'dataframe', "=", paste0("'",rnmed(),"'"), "WHERE",
                                              'dataframe', " =", paste0("'",rmytb(),"'")))
              DBI:::dbCommit(con)




            }}}
    data <- DBI:::dbReadTable(con, "awesome",check.names=F)
    return(data)
  })




  initm<-(-1)
  fn<-function(input){
    re<-eventReactive(input$dbstorage!='',{
      0
    })
  }

  timerun<-reactiveVal({
    initm

  })

  observe({

    invalidateLater(60000,session = session)

    isolate({

      timerun(timerun()+1)})

  })


  timerb<- eventReactive(input$dbstorage,{

    vlb<-isolate({timerun()})
    return(vlb)
  })

  timer<- reactive({

    vl<-(timerun()-timerb())
    return(vl)
  })

  timerc<- eventReactive(input$opened,{

    vlb<-isolate({timerun()})
    return(vlb)
  })

  timerdf<- reactive({

    vl<-(timerun()-timerc())
    return(vl)
  })

  output$myavailabledbs <- rhandsontable:::renderRHandsontable({
    req(lrvlue()>=0)
    dt<-awesomedt()
    db.status<-awesomedt()[,2]
    notopend<-which(db.status!=input$dbstorage)
    db.status[notopend]<-'open corresponding db'
    opend<-which(db.status!='open corresponding db')
    db.status[opend]<-paste('select dataframe')
    df.stat<-awesomedt()[,1]
    df.sta<-replace(df.stat,which(db.status=='open corresponding db'),'open db')
    df.state<-replace(df.sta,which(db.status=='select dataframe'),'select dataframe')
    opendf<-which(df.stat==input$opened)
    df.state[opendf]<-paste(timerdf(),"' running")

    bind<-cbind(awesomedt(),df.state)
    fine<-data.frame(bind,check.names = F)
    rhandsontable:::rhandsontable(fine,readOnly = T)
  })
  output$downlodui <- renderUI({
    req(length(input$initdb)!=0)
    input$dbstorage
    con <- DBI:::dbConnect(SQLite(), input$dbstorage)
    read <- awesomedt()
    choices <- read$dataframe
    fluidPage(selectInput("savethisdb", "Choose a data frame to download:",
                          choices = choices), downloadLink("downloaddb",
                                                           "Download data frame"))
  })
  mydbtosave <- reactive({
    req(length(input$initdb)!=0)
    input$dbstorage
    con <- DBI:::dbConnect(SQLite(), input$dbstorage)
    dat <- data.frame(DBI:::dbReadTable(con, input$savethisdb),check.names = F)
    return(dat)
  })
  output$dbtable <- rhandsontable:::renderRHandsontable({
    rhandsontable(mydbtosave())
  })
  output$downloaddb <- downloadHandler(filename = function() {
    paste(paste0(input$savethisdb, ".csv"), ".csv",
          sep = "")
  }, content = function(file) {
    write.csv(mydbtosave(), file, row.names = FALSE)
  })


  observe({
    req(length(input$removclm)!=0)
    req(input$removclm == T)

    myda <- removcol()

    updateSelectInput(session,"column", label = "filter column to append a value in",
                      choices = names(myda))
  })


  observe({
    req(length(input$makechanges)!=0)
    req(input$makechanges == T)

    myda <- changecolname()

    updateSelectInput(session,"column", label = "filter column to append a value in",
                      choices = names(myda))
  })


  observe({
    req(length(input$confirm)!=0)
    req(input$confirm == T)

    myda <- dbaddedcol()

    updateSelectInput(session,"column", label = "filter column to append a value in",
                      choices = names(myda))
  })

  output$selected <- renderUI({
    req(lrvlue()>=0)
    input$dbstorage
    con <- DBI:::dbConnect(SQLite(), input$dbstorage)
    if (input$opened == "") {
      "You have not opened any data.frame. First open any of your data.frames or create or upload one if you don't have. See tabPanel 1"
    }
    else {
      req(input$opened %in% awesomedt()[, 1] == T)
      req(length(input$initdb)!=0)
      input$dbstorage
      con <- DBI:::dbConnect(SQLite(), input$dbstorage)



      myda <- mydatareactived()
      selectInput("column", label = "filter column to append a value in",
                  choices = names(myda))


    }
  })


  output$controls<-renderUI({
    if(input$editintb=='home'){
      fluidPage(
        uiOutput("valuestoappend2"),
        uiOutput('uihelp'))
    }else

      if(input$editintb=='bind.table'){fluidPage(
        uiOutput('uplodmerge')
      )
      }
    else
      if(input$editintb=='renameCOLS'){
        uiOutput("selectclm")
      }else
        if(input$editintb=='removeCOLS'){
          uiOutput("columremoveui")
        }else{
          'You can edit in multiple cells based on similar filtered column conditions by controlling the filters and typing in the textInput'
        }
  })
  output$appendRV<-renderUI(
    {req(input$value!='')
      fluidPage(
        span(style='color:green',
             "This is the row and value in",input$column,"column you are likely to append to the df"),
        tableOutput("reactdf"), hr(), "You can type in cells and click commit to save your records"
      )
    }
  )
  output$editcellcon<-renderUI({
    req(length(input$initdb)!=0)
    input$dbstorage
    if (input$opened == "") {
    }
    else {myda <- mydatareactive()
    cols<-chois()
    fluidRow(
      selectInput("myfilter", label = "filter column",
                  choices = names(myda),width = 130),uiOutput("conditvalue"),
      selectInput("col", label = "edit in column;",
                  choices = cols,width = 130), inputPanel(textInput("changes",
                                                                    "", placeholder = paste('type here'),label = 'write changes'), checkboxInput("commitchanges",
                                                                                                                                                 label = "save inputs", value = F))
    )}
  })
  col2<-reactive({
    names(mydatareactive())
  })




  output$uitbls<-renderUI({
    req(length(input$initdb)!=0)

    if(input$editintb=='home'){fluidPage(div('type in this table and click enter key/commit button, to add row to table2',rhandsontable:::rHandsontableOutput("minitable"),class='bg-gray-light'),br(),
                                         rhandsontable:::rHandsontableOutput("dbaddedcolstable"),br())
    }else
      if(input$editintb=='renameCOLS'){
        rhandsontable:::rHandsontableOutput("changedcoltable")
      }else
        if(input$editintb=='bind.table'){
          rhandsontable:::rHandsontableOutput("rcoltbl")
        }
    else
      if(input$editintb=='removeCOLS'){
        rhandsontable:::rHandsontableOutput("rcoltbl")
      }else{
        fluidPage(
          shiny:::tabsetPanel(tabPanel('edit_Cells',
                                      uiOutput('editcellcon'),
                                      rhandsontable:::rHandsontableOutput("editedcells")
          ),tabPanel('delete_rows',
                     uiOutput("checktodel"),
                     rhandsontable:::rHandsontableOutput("rowreduceddt")

          )
          ))
      }

  })
  k<-uiOutput('append_r_c')
  output$editoption<-renderUI({
    fluidPage(
      radioButtons(inline=T,'editintb',choices=list(
        'home','filter','renameCOLS','removeCOLS','bind.table'),shiny:::HTML(paste('edit options for',span(tags$b(tags$u(input$opened)),'data.frame',style='color:green')))),
      uiOutput('controls')
    )
  })

  jscode <- '$(document).keyup(function(e) {
    if (e.key == "Enter") {
    $("#commit").click();
}});'



  jscodeb <- '$(document).keyup(function(e) {
    if (e.key == "Enter") {
    $("#commitb").click();
}});'
  jskod<-reactiveVal(jscodeb)

  observe({
    input$opened
    req(vlued()==0)
    isolate(jskod(''))
  })
  observe({
    input$opened
    req(vlued()==1)
    isolate(jskod(jscodeb))
  })

  extracol<-reactiveVal(0)
  initmini<-reactive({
    req(length(input$initdb)!=0)
    input$dbstorage
    input$opened


    con <- DBI:::dbConnect(SQLite(), input$dbstorage)
    req(length(input$commitb)!=0)
    dtvlue<-mydatareact()
    if(input$commitb==T){
      dtr<-dtvlue[1,]
      dtr[]<-''
      return(dtr)}else
        if(input$commitb==F){
          dtr<-dtvlue[1,]
          dtr[]<-''
          return(dtr)
        }else
          if(input$makechanges==T){
            dtr<-dtvlue[1,]
            dtr[]<-''
            return(dtr)
          }
  })
  minifn=function(){
    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    input$opened
    req(input$dbstorage!='')


    con <- DBI:::dbConnect(SQLite(), input$dbstorage)
    if(input$editintb=='home'){
      req(length(input$commitb)!=0)
      if(input$commitb==T){
        reactiveValues(data = isolate(
          initmini()))}else
            if(input$commitb==F){
              reactiveValues(data = isolate(
                initmini()))}
    }
  }
  onSessionStart=shinyjs:::reset('dbstorage')

  onSessionStart=isolate({
    users$count=users$count+1
  })

  onSessionEnded(
    function(){
      isolate({
        users$count=users$count-1
      })
    }
  )
  output$printcd<-renderUI({
    paste(users$count,'users')})
  output$conU<-renderPrint({


    cat(paste(users$count,'connected users'))

  })
  output$minitable <- rhandsontable:::renderRHandsontable({

    req(length(input$initdb)!=0)
    input$dbstorage
    input$opened
    req(input$dbstorage!='')
    con <- DBI:::dbConnect(SQLite(), input$dbstorage)
    req(length(input$commitb)!=0)
    X<-minifn()
    if(all(('Password'%in%names(mydatareactived())==T),('date'%in%names(mydatareactived())==T))){
      rhandsontable(X$data)%>%
        rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
        rhandsontable::hot_col(col = 'Password',type = 'password')%>%
        rhandsontable::hot_cell(col = 'date',X$data$date[which(X$data$date!='')],type = 'date')}else
          if(all(('password'%in%names(mydatareactived())==T),('date'%in%names(mydatareactived())==T))){
            rhandsontable(X$data)%>%
              rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
              rhandsontable::hot_col(col = 'password',type = 'password')%>%
              rhandsontable::hot_col(col = 'date',X$data$date[which(X$data$date!='')],type = 'date')}else
                if(all(('Password'%in%names(mydatareactived())==T),('Date'%in%names(mydatareactived())==T))){
                  rhandsontable(X$data)%>%
                    rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                    rhandsontable::hot_col(col = 'Password',type = 'password')%>%
                    rhandsontable::hot_col(col = 'Date',X$data$Date[which(X$data$Date!='')],type = 'date')}else
                      if(all(('password'%in%names(mydatareactived())==T),('Date'%in%names(mydatareactived())==T))){
                        rhandsontable(X$data)%>%
                          rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                          rhandsontable::hot_col(col = 'password',type = 'password')%>%
                          rhandsontable::hot_col(col = 'Date',X$data$Date[which(X$data$Date!='')],type = 'date')}else
                            if('Password'%in%names(mydatareactived())==T){
                              rhandsontable(X$data)%>%
                                rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                rhandsontable::hot_col(col = 'Password',type = 'password')}else
                                  if('password'%in%names(mydatareactived())==T){
                                    rhandsontable(X$data)%>%
                                      rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                      rhandsontable::hot_col(col = 'password',type = 'password')}else
                                        if('date'%in%names(mydatareactived())==T){
                                          rhandsontable(X$data)%>%
                                            rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                            rhandsontable::hot_col(col = 'date',X$data$date[which(X$data$date!='')],type = 'date')}else
                                              if('Date'%in%names(mydatareactived())==T){
                                                rhandsontable(X$data)%>%
                                                  rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                  rhandsontable::hot_col(col = 'Date',X$data$Date[which(X$data$Date!='')],type = 'date')}else
                                                    if(all(('password '%in%names(mydatareactived())==T),('Date '%in%names(mydatareactived())==T))){
                                                      rhandsontable(X$data)%>%
                                                        rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                        rhandsontable::hot_col(col = 'password ',type = 'password')%>%
                                                        rhandsontable::hot_col(col = 'Date ',X$data$date[which(X$data$date!='')],type = 'date')}else
                                                          if(all(('Password '%in%names(mydatareactived())==T),('Date '%in%names(mydatareactived())==T))){
                                                            rhandsontable(X$data)%>%
                                                              rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                              rhandsontable::hot_col(col = 'Password ',type = 'password')%>%
                                                              rhandsontable::hot_col(col = 'Date ',X$data$date[which(X$data$date!='')],type = 'date')}else
                                                                if(all(('password '%in%names(mydatareactived())==T),('date '%in%names(mydatareactived())==T))){
                                                                  rhandsontable(X$data)%>%
                                                                    rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                                    rhandsontable::hot_col(col = 'password ',type = 'password')%>%
                                                                    rhandsontable::hot_col(col = 'date ',X$data$Date[which(X$data$Date!='')],type = 'date')}else
                                                                      if(all(('Password '%in%names(mydatareactived())==T),('date '%in%names(mydatareactived())==T))){
                                                                        rhandsontable(X$data)%>%
                                                                          rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                                          rhandsontable::hot_col(col = 'Password ',type = 'password')%>%
                                                                          rhandsontable::hot_col(col = 'date ',X$data$Date[which(X$data$Date!='')],type = 'date')}else
                                                                            if('password '%in%names(mydatareactived())==T){
                                                                              rhandsontable(X$data)%>%
                                                                                rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                                                rhandsontable::hot_col(col = 'password ',type = 'password')}else
                                                                                  if('Password '%in%names(mydatareactived())==T){
                                                                                    rhandsontable(X$data)%>%
                                                                                      rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                                                      rhandsontable::hot_col(col = 'Password ',type = 'password')}else
                                                                                        if('Date '%in%names(mydatareactived())==T){
                                                                                          rhandsontable(X$data)%>%
                                                                                            rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                                                            rhandsontable::hot_col(col = 'Date ',X$data$date[which(X$data$date!='')],type = 'date')}else
                                                                                              if('date '%in%names(mydatareactived())==T){
                                                                                                rhandsontable(X$data)%>%
                                                                                                  rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                                                                  rhandsontable::hot_col(col = 'date ',X$data$Date[which(X$data$Date!='')],type = 'date')}else
                                                                                                  {rhandsontable(X$data)%>%
                                                                                                      rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)}
  })

  observeEvent(input$minitable$changes$changes,{
    con <- DBI:::dbConnect(SQLite(), input$dbstorage)

    t<-minifn()
    row = input$minitable$changes$changes[[]]
    col = input$minitable$changes$changes[[]]
    value = input$minitable$changes$changes[[]]

    t$data[row,col] = value

  })

  dtrec<-reactive({
    input$dbstorage
    input$opened
    req(length(input$initdb)!=0)
    req(input$dbstorage!='')
    req(input$editintb=='home')
    if(input$commitb==T){
      if (!is.null(input$minitable)){
        rodt <<- rhandsontable::hot_to_r(input$minitable)
        rodt
        return(rodt)}
    }else
      if(input$commitb==F){
        if (!is.null(input$minitable)){
          rodt <<- rhandsontable::hot_to_r(input$minitable)
          rodt
          return(rodt)}}else
            if(input$makechanges==T){
              if (!is.null(input$minitable)){
                rodt <<- rhandsontable::hot_to_r(input$minitable)
                rodt
                return(rodt)
              }
            }
  })

  importvl<-reactiveVal(F)

  observe({
    invalidateLater(length(input$fileType_Input)!=0)
    invalidateLater(1000,session)
    isolate({importvl(T)})})

  observe({
    onclick("bld", isolate({importvl(F)}))})





  vlued<-reactiveVal(1)
  observe({
    onclick("dfrem", isolate({vlued(1)}))})

  observe({
    onclick("dbes", isolate({vlued(0)}))})
  minirow<-reactive({
    req(input$dbstorage!='')
    input$dbstorage
    input$opened
    req(input$opened!='')
    con <- DBI:::dbConnect(SQLite(), input$dbstorage)
    req(vlued()==1)
    req(input$editintb=='home')

    req(length(input$commitb)!=0)
    if(input$commitb==T){
      if(length(input$dbaddedcolstable$changes$changes)==0){
        con <- DBI:::dbConnect(SQLite(), input$dbstorage)

        dtcol<-dtrec()

        DBI:::dbWriteTable(con,input$opened,dtrec(),append=T,check.names=F)}else
          if(length(input$dbaddedcolstable$changes$changes)!=0 &&
             length(input$minitable$changes$changes)!=0){
            con <- DBI:::dbConnect(SQLite(), input$dbstorage)

            dtcol<-dtrec()

            DBI:::dbWriteTable(con,input$opened,dtrec(),append=T,check.names=F)
          }
      else{
        mydatareact()
      }


    }else{
      mydatareact()}
    data<-dbReadTable(con,input$opened,check.names=F)
    return(data)
  })



  rowempty<-reactive({
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened!='')
    con <- DBI:::dbConnect(SQLite(), input$dbstorage)
    req(input$editintb=='append_rows|edit_cells')
    input$dbstorage
    input$opened
    if(length(input$commit)!=0){
      if(input$commit==F){
        req(checkcondition()=='ok')
        req(all(fntrp()=='')==F)
        dtr<-mydatareact()[1,]
        dtr[]<-''
        DBI:::dbWriteTable(con,input$opened,dtr,append=T,check.names=F)}
      else{ mydatareact()}}else{
        mydatareact()
      }




    data<-dbReadTable(con,input$opened,check.names=F)
    return(data)
  })


  fnColemptyb<-reactive({
    req(length(input$initdb)!=0)
    input$dbstorage
    input$opened
    req(length(input$commitb)!=0)
    if(input$commitb==F){
      req(input$opened)
      dt<-mydatareact()
      req(all(dt[,length(names(dt))]=='')==F)


      col<-length(names(dt))+1
      colm<-paste0('COL',col)
      if(colm%in%names(mydatareact())==T){
        return(paste0(colm,'b'))
      }else
        return(colm)}else
        {''}

  })


  fnColempty<-reactive({
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$dbstorage!='')
    input$dbstorage
    input$opened
    req(input$opened!='')
    req(length(input$commit)!=0)
    if(input$commit==F){
      req(input$opened)
      dt<-mydatareact()
      req(all(dt[,length(names(dt))]=='')==F)


      col<-length(names(dt))+1
      colm<-paste('COL',col)
      return(colm)}else
      {''}

  })


  colemptyb<-reactive({
    req(length(input$initdb)!=0)
    input$dbstorage
    con <- DBI:::dbConnect(SQLite(), input$dbstorage)
    input$opened
    req(input$opened!='')
    dt<-mydatareact()
    if(input$editintb=='home'){
      req(all(dt[,length(names(dt))]=='')==F)
      if(input$commitb==F){
        req(input$opened)

        dt<-mydatareact()
        if(all(dt[,length(names(dt))]=='')==F){
          if(dbExistsTable(con,'reactd')==F){
            dbWriteTable(con,'reactd',mydatareact(),check.names=F)

          }
          DBI:::dbBegin(con)
          DBI:::dbSendStatement(con, paste("\n                           ALTER TABLE",
                                          paste0('reactd'), "ADD COLUMN", paste0('[',fnColemptyb(),']'),
                                          "Text;", "''"))
          DBI:::dbCommit(con)

          dtcol<-DBI:::dbReadTable(con, 'reactd',check.names=F)


          DBI:::dbBegin(con)
          DBI:::dbSendStatement(con,

                               paste("CREATE TEMPORARY TABLE", paste0("my_backupdbT(",to_scalar(x=names(dtcol)),')')))
          DBI:::dbCommit(con)
          #########
          DBI:::dbBegin(con)
          DBI:::dbSendStatement(con, paste('INSERT INTO my_backupdbT SELECT', to_scalar(x=names(dtcol)), 'FROM', 'reactd'))
          DBI:::dbCommit(con)
          #########
          DBI:::dbBegin(con)
          DBI:::dbSendStatement(con,
                               paste('DROP TABLE', paste0('"',input$opened,'"')))
          dbCommit(con)
          #########
          DBI:::dbBegin(con)
          dbSendStatement(con,

                          paste("CREATE TABLE", paste0(paste0('"',input$opened,'"'),"(",to_scalar(x=names(dtcol)),')')))
          DBI:::dbCommit(con)

          #########
          DBI:::dbBegin(con)
          DBI:::dbSendStatement(con,
                               paste('INSERT INTO', paste0('"',input$opened,'"'), 'SELECT', to_scalar(x=names(dtcol)), 'FROM', 'my_backupdbT'))
          DBI:::dbCommit(con)
          #########
          DBI:::dbBegin(con)
          DBI:::dbSendStatement(con,
                               'DROP TABLE my_backupdbT')

          DBI:::dbCommit(con)
          #########
          DBI:::dbBegin(con)
          DBI:::dbSendStatement(con,
                               'DROP TABLE reactd')

          DBI:::dbCommit(con)

          dtc<-DBI:::dbReadTable(con, input$opened,check.names=F)
          dtcLc<-dtc[,length(names(dtc))]
          dtcLc[]<-''
          dtfew<-dtc[,-length(names(dtc))]
          dtcolm<-cbind(dtfew,dtcLc)
          final<-dtcolm%>%`colnames<-`(c(names(dtfew),fnColemptyb()))



          DBI:::dbWriteTable(con,'finl',final,check.names=F)
          ######33----------------------------------------------------------------------------
          DBI:::dbBegin(con)

          DBI:::dbSendStatement(con,

                               paste("CREATE TEMPORARY TABLE", paste0("my_backupdbT(",to_scalar(x=names(final)),')')))
          DBI:::dbCommit(con)
          DBI:::dbBegin(con)
          DBI:::dbSendStatement(con, paste('INSERT INTO my_backupdbT SELECT', to_scalar(x=names(final)), 'FROM', 'finl'))


          DBI:::dbCommit(con)
          DBI:::dbBegin(con)


          DBI:::dbSendStatement(con,

                               paste('DROP TABLE', paste0('"',input$opened,'"')))

          dbSendStatement(con,

                          paste("CREATE TABLE", paste0(paste0('"',input$opened,'"'),"(",to_scalar(x=names(final)),')')))
          DBI:::dbCommit(con)
          DBI:::dbBegin(con)
          DBI:::dbSendStatement(con,
                               paste('INSERT INTO', paste0('"',input$opened,'"'), 'SELECT', to_scalar(x=names(final)), 'FROM', 'my_backupdbT'))
          DBI:::dbCommit(con)
          DBI:::dbBegin(con)
          DBI:::dbSendStatement(con,
                               'DROP TABLE my_backupdbT')

          DBI:::dbCommit(con)

          DBI:::dbBegin(con)
          DBI:::dbSendStatement(con,
                               'DROP TABLE finl')

          DBI:::dbCommit(con)


        }}
      else{
        mydatareact()


      }}

    else{
      mydatareact()


    }

    data<-dbReadTable(con,input$opened,check.names=F)
    return(data)
  })


  colempty<-reactive({
    req(length(input$initdb)!=0)
    input$dbstorage
    con <- DBI:::dbConnect(SQLite(), input$dbstorage)
    input$opened
    req(input$opened!='')
    dt<-mydatareact()

    req(all(dt[,length(names(dt))]=='')==F)
    if(input$commit==F){
      req(input$opened)


      if(dbExistsTable(con,'reacol')==F){
        dbWriteTable(con,'reacol',mydatareact(),check.names=F)

      }
      DBI:::dbBegin(con)
      DBI:::dbSendStatement(con, paste("\n                           ALTER TABLE",
                                      paste0('reacol'), "ADD COLUMN", paste0('[',fnColempty(),']'),
                                      "Text;", "''"))
      DBI:::dbCommit(con)

      dtcol<-DBI:::dbReadTable(con, 'reacol',check.names=F)


      DBI:::dbBegin(con)
      DBI:::dbSendStatement(con,

                           paste("CREATE TEMPORARY TABLE", paste0("my_backupdbT(",to_scalar(x=names(dtcol)),')')))
      DBI:::dbCommit(con)
      #########
      DBI:::dbBegin(con)
      DBI:::dbSendStatement(con, paste('INSERT INTO my_backupdbT SELECT', to_scalar(x=names(dtcol)), 'FROM', 'reacol'))
      DBI:::dbCommit(con)
      #########
      DBI:::dbBegin(con)
      DBI:::dbSendStatement(con,
                           paste('DROP TABLE', paste0('"',input$opened,'"')))
      dbCommit(con)
      #########
      DBI:::dbBegin(con)
      dbSendStatement(con,

                      paste("CREATE TABLE", paste0(paste0('"',input$opened,'"'),"(",to_scalar(x=names(dtcol)),')')))
      DBI:::dbCommit(con)

      #########
      DBI:::dbBegin(con)
      DBI:::dbSendStatement(con,
                           paste('INSERT INTO', paste0('"',input$opened,'"'), 'SELECT', to_scalar(x=names(dtcol)), 'FROM', 'my_backupdbT'))
      DBI:::dbCommit(con)
      #########
      DBI:::dbBegin(con)
      DBI:::dbSendStatement(con,
                           'DROP TABLE my_backupdbT')

      DBI:::dbCommit(con)
      #########
      DBI:::dbBegin(con)
      DBI:::dbSendStatement(con,
                           'DROP TABLE reacol')

      DBI:::dbCommit(con)

      ###########-------------------------------------
      ###########
      dtc<-DBI:::dbReadTable(con, input$opened,check.names=F)
      dtcLc<-dtc[,length(names(dtc))]
      dtcLc[]<-''
      dtfew<-dtc[,-length(names(dtc))]
      dtcolm<-cbind(dtfew,dtcLc)
      final<-dtcolm%>%`colnames<-`(c(names(dtfew),fnColempty()))



      DBI:::dbWriteTable(con,'finl',final,check.names=F)
      ######33----------------------------------------------------------------------------
      DBI:::dbBegin(con)

      DBI:::dbSendStatement(con,

                           paste("CREATE TEMPORARY TABLE", paste0("my_backupdbT(",to_scalar(x=names(final)),')')))
      DBI:::dbCommit(con)
      DBI:::dbBegin(con)
      DBI:::dbSendStatement(con, paste('INSERT INTO my_backupdbT SELECT', to_scalar(x=names(final)), 'FROM', 'finl'))


      DBI:::dbCommit(con)
      DBI:::dbBegin(con)


      DBI:::dbSendStatement(con,

                           paste('DROP TABLE', paste0('"',input$opened,'"')))

      dbSendStatement(con,

                      paste("CREATE TABLE", paste0(paste0('"',input$opened,'"'),"(",to_scalar(x=names(final)),')')))
      DBI:::dbCommit(con)
      DBI:::dbBegin(con)
      DBI:::dbSendStatement(con,
                           paste('INSERT INTO', paste0('"',input$opened,'"'), 'SELECT', to_scalar(x=names(final)), 'FROM', 'my_backupdbT'))
      DBI:::dbCommit(con)
      DBI:::dbBegin(con)
      DBI:::dbSendStatement(con,
                           'DROP TABLE my_backupdbT')

      DBI:::dbCommit(con)

      DBI:::dbBegin(con)
      DBI:::dbSendStatement(con,
                           'DROP TABLE finl')

      DBI:::dbCommit(con)

    }
    else{
      mydatareact()


    }

    data<-dbReadTable(con,input$opened,check.names=F)
    return(data)
  })

  checkcondition<-reactive({
    req(length(input$initdb)!=0)
    input$dbstorage
    dt<-mydatareact()
    transdt<-t(dt)
    h=transdt[,ncol(transdt)]
    i<-transdt[,as.numeric(ncol(transdt)-1)]
    tk<-which(i=='')
    if(length(tk)!=length(h)){
      'ok'
    }else
      if(length(tk)==length(h)){

        'no'
      }



  })
  fnt<-reactive({

    req(length(input$initdb)!=0)
    input$dbstorage
    dt<-mydatareact()
    transdt<-t(dt)


    v=mydatareact()[as.numeric(ncol(transdt)-1),]
    glimpse(v)
  })

  fntrp<-reactive({

    req(length(input$initdb)!=0)
    input$dbstorage
    dt<-mydatareact()
    transdt<-t(dt)


    v=mydatareact()[ncol(transdt),]
    glimpse(v)
  })

  output$append_r_c<-renderUI({
    if(input$editintb=='append_rows|edit_cells'){
      span('append_rows|edit_cells',style='color:blue')}else
        'append_rows|edit_cells'
  })

  output$valuestoappend <- renderUI({
    if (input$opened == "") {
    }
    else {

      fluidPage( #tags$head(tags$script(HTML(jscode))),
        #textInput("value", "Type the entry you want to append"),To append multiple rows on-click click',checkboxInput('here','here.',value = F),'
        span('To edit values, type in the cells then click commit to save changes while to append/delete a row, right-click anywhere on any row then select insert/remove row options in the pop-up then click commit to save changes. You can also highlight to delete multiple rows',style='font-size:80%'),
        checkboxInput("commit", label = "commit",
                      value = F))
    }
  })


  output$valuestoappend2 <- renderUI({
    req(length(input$initdb)!=0)
    input$dbstorage
    if (input$opened == "") {
    }
    else {

      fluidPage(tags$head(tags$script(HTML(jskod()))),
                #textInput("value", "Type the entry you want to append"),To append multiple rows on-click click',checkboxInput('here','here.',value = F),'

                checkboxInput("commitb", label = "commit",
                              value = F))
    }
  })


  output$uihelp<-renderUI({

    span('To append row with values to your data(in table2), type in the cells in the table1 then click commit/enter key to save changes. You can also edit cells in table2 by directly typing in it and clicking commit/enter key to save changes',style='font-size:80%')

  })








  ######################## append
  mydatarea <- 0
  mydatareac <- reactiveVal({

    mydatarea}
  )
  observe({

    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(length(input$confirm) != 0)
    req(input$confirm == T)
    isolate({mydatareac(dbaddedcol())})
  })

  observe({

    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(length(input$commitchanges) != 0)
    req(input$commitchanges == T)
    isolate({mydatareac(dbeditcells())})
  })

  observe({
    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(length(input$makechanges) != 0)
    req(input$makechanges == T)
    isolate({mydatareac(changecolname())})
  })



  observe({

    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(length(input$removclm) != 0)
    req(input$removclm == T)
    isolate({
      mydatareac(removcol())})
  })

  observe({

    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(length(input$here == T) != 0)
    req(input$here == T)
    req(input$editintb=='renameCOLS')
    isolate({
      mydatareac(dbrhson())})
  })


  observe({

    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(input$editintb=='bind.table')
    req(length(input$merge == T) != 0)
    req(input$merge == T)
    isolate({
      mydatareac(rbinddb())})
  })
  observe({

    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(length(input$commit == T) != 0)
    req(input$commit == T)
    isolate({
      mydatareac(mynewdb())})
  })
  observe({

    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(length(input$commitb == T) != 0)
    req(input$commitb == T)
    isolate({
      mydatareac(allowmainediting())})
  })


  observe({

    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(length(input$commit == F) != 0)
    req(input$commit == F)

    isolate({
      mydatareac(colempty())})
  })
  observe({

    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(input$editintb=='home')
    req(length(input$commitb) != 0)
    req(input$commitb == F)

    isolate({
      mydatareac(colemptyb())})
  })

  observe({

    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(input$editintb=='append_rows|edit_cells')
    req(length(input$commit)!=0)

    isolate({
      mydatareac(rowempty())})
  })
  observe({

    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(length(input$commitb)!=0)
    req(input$commitb == T)
    isolate({
      mydatareac(minirow())})
  })
  observe({

    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(length(input$commit)!=0)
    req(input$commit == F)
    isolate({
      mydatareac(mydatareact())})
  })

  observe({

    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(length(input$myconfirmation1 == T) != 0)
    req(input$myconfirmation1 == T)
    isolate({
      mydatareac(rowremove())})
  })
  ##################







  observe({

    input$loginok
    req(isolate({lrvlue()})>=1)
    req(input$opened!='')
    input$opened

    con <- DBI:::dbConnect(SQLite(), input$dbstorage)
    req(dbExistsTable(con,input$opened)==T)
    isolate({
      mydatareac(DBI:::dbReadTable(con, input$opened,check.names=F))})
  })
  observe({

    input$loginok
    req(isolate({lrvlue()})>=1)
    req(input$dbstorage!='')
    input$dbstorage

    req(input$opened!='')
    con <- DBI:::dbConnect(SQLite(), input$dbstorage)
    req(dbExistsTable(con,input$opened)==T)
    isolate({
      mydatareac(DBI:::dbReadTable(con, input$opened,check.names=F))})
  })

  observe({
    input$loginok

    req(length(input$initdb)!=0)
    input$opened
    req(input$opened!='')
    con <- DBI:::dbConnect(SQLite(), input$dbstorage)
    req(dbExistsTable(con,input$opened)==T)
    isolate({
      mydatareactiv(DBI:::dbReadTable(con, input$opened,check.names=F))})
  })
  observe({
    input$loginok

    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened!='')
    con <- DBI:::dbConnect(SQLite(), input$dbstorage)
    req(dbExistsTable(con,input$opened)==T)
    isolate({
      mydatareactiv(DBI:::dbReadTable(con, input$opened,check.names=F))})
  })

  mydatareact <- reactive({

    req(length(input$initdb)!=0)

    input$dbstorage
    req(length(choiseread())!=0)
    input$opened
    req(input$opened!='')

    con <- DBI:::dbConnect(SQLite(), input$dbstorage)
    input$dbstorage
    req(input$opened!='')
    input$opened
    req(dbExistsTable(con,input$opened)==T)
    if(class(mydatareac())!='data.frame'){
      mydatc <- data.frame(DBI:::dbReadTable(con, input$opened,check.names=F),check.names = F)
      mydatc }else{
        mydatc <- data.frame(mydatareac(),check.names = F)
        mydatc}


  })


  mydatareactivek <- reactive({
    req(length(input$initdb)!=0)


    input$dbstorage
    con <- DBI:::dbConnect(SQLite(), input$dbstorage)


    mydatr <- data.frame(mydatareact(),check.names = F)
    myda <- mydatr[seq(1,5), ]
    myda[] = ""

    return(myda)


  })

  observeEvent(input$opened,{
    output$reactdf <- renderTable({
      if (input$opened != "") {
        input$opened
        mydatareactive()
      }
    })
  })





  mydat <- 0
  mydatareactiv <- reactiveVal({

    mydat}
  )
  observe({
    input$loginok

    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(openvals$collapsed==T)
    req(length(input$confirm) != 0)
    req(input$confirm == T)
    isolate({mydatareactiv(dbaddedcol())})
  })
  observe({

    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(input$editintb=='bind.table')
    req(length(input$merge == T) != 0)
    req(input$merge == T)
    isolate({
      mydatareactiv(rbinddb())})
  })
  observe({
    req(length(input$initdb)!=0)
    req(length(input$opened)!=0)
    invalidateLater(10000,session)
    isolate({mydatareactiv(mydatareact())})
  })
  observe({
    req(length(input$initdb)!=0)
    req(length(input$opened)!=0)

    invalidateLater(10000,session)
    isolate({mydatareac(mydatareact())})
  })
  observe({
    input$loginok

    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(openvals$collapsed==T)
    req(length(input$commitchanges) != 0)
    req(input$commitchanges == T)
    isolate({mydatareactiv(dbeditcells())})
  })

  observe({
    input$loginok
    req(input$dbstorage!='')
    input$dbstorage
    req(input$opened)
    req(openvals$collapsed==T)
    req(length(input$makechanges) != 0)
    req(input$makechanges == T)
    isolate({mydatareactiv(changecolname())})
  })


  observe({
    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(openvals$collapsed==T)
    req(length(input$removclm) != 0)
    req(input$removclm == T)
    isolate({
      mydatareactiv(removcol())})
  })

  observe({
    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(length(input$commit == F) != 0)
    req(input$commit == F)

    isolate({
      mydatareactiv(colempty())})
  })
  observe({
    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(input$editintb=='home')
    req(length(input$commitb) != 0)
    req(input$commitb == F)

    isolate({
      mydatareactiv(colemptyb())})
  })

  observe({
    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(input$editintb=='append_rows|edit_cells')
    req(length(input$commit)!=0)

    isolate({
      mydatareactiv(rowempty())})
  })
  observe({
    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(length(input$commitb)!=0)
    req(input$commitb == T)
    isolate({
      mydatareactiv(minirow())})
  })
  observe({
    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(openvals$collapsed==T)
    req(input$editintb=='append_rows|edit_cells')
    isolate({
      mydatareactiv(mydatareactived())})
  })

  observe({
    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(openvals$collapsed==T)
    req(length(input$here == T) != 0)
    req(input$here == T)
    req(input$editintb=='renameCOLS')
    isolate({
      mydatareactiv(dbrhson())})
  })



  observe({
    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(openvals$collapsed==T)
    req(length(input$myconfirmation1 == T) != 0)
    req(input$myconfirmation1 == T)
    isolate({
      mydatareactiv(rowremove())})
  })

  mydat <- 0
  mydatareactivb <- reactiveVal({

    mydat}
  )
  observe({
    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(openvals$collapsed==F)
    req(length(input$confirm) != 0)
    req(input$confirm == T)
    isolate({mydatareactivb(dbaddedcol())})
  })

  observe({
    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(openvals$collapsed==F)
    req(length(input$commitchanges) != 0)
    req(input$commitchanges == T)
    isolate({mydatareactivb(dbeditcells())})
  })

  observe({
    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(openvals$collapsed==F)
    req(length(input$makechanges) != 0)
    req(input$makechanges == T)
    isolate({mydatareactivb(changecolname())})
  })

  observe({
    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(openvals$collapsed==F)
    req(length(input$removclm) != 0)
    req(input$removclm == T)
    isolate({
      mydatareactivb(removcol())})
  })
  observe({
    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(openvals$collapsed==F)
    req(length(input$commit == T) != 0)
    req(input$commit == T)
    isolate({
      mydatareactivb(mynewdb())})
  })
  observe({
    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(openvals$collapsed==F)
    req(length(input$commit == T) != 0)
    req(input$commit == T)
    isolate({
      mydatareactivb(mynewdb())})
  })
  observe({
    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(length(input$commitb == T) != 0)
    req(input$commitb == T)
    isolate({
      mydatareactiv(allowmainediting())})
  })
  observe({
    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(length(input$commit == F) != 0)
    req(input$commit == F)

    isolate({
      mydatareactivb(colempty())})
  })
  observe({
    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(length(input$commitb) != 0)
    req(input$commitb == F)

    isolate({
      mydatareactivb(colemptyb())})
  })

  observe({
    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(length(input$commitb)!=0)
    req(input$commitb == T)
    isolate({
      mydatareactivb(minirow())})
  })
  observe({
    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(openvals$collapsed==F)
    req(input$editintb=='append_rows|edit_cells')
    isolate({
      mydatareactivb(mydatareactived())})
  })

  observe({
    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(openvals$collapsed==F)
    req(length(input$here == T) != 0)
    req(input$here == T)
    req(input$editintb=='renameCOLS')
    isolate({
      mydatareactivb(dbrhson())})
  })



  observe({
    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(openvals$collapsed==F)
    req(length(input$myconfirmation1 == T) != 0)
    req(input$myconfirmation1 == T)
    isolate({
      mydatareactivb(rowremove())})
  })







  rctv<-eventReactive(input$opened,{
    1
  })

  output$rctv4<-renderPrint({
    rctv()
  })


  library(rhandsontable)



  mydatareactive <- reactive({
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened!='')

    if (openvals$collapsed==T){
      con <- DBI:::dbConnect(SQLite(), input$dbstorage)

      if(class(mydatareactiv())!='data.frame'){
        mydatc <- data.frame(DBI:::dbReadTable(con, input$opened,check.names=F),check.names = F)

        myda <- mydatc[1, ]
        myda[] = ""
        replcol <- which(names(myda) == input$column)
        myda[, replcol] = input$value
        return(myda)}else{
          mydatc <- data.frame(mydatareactiv(),check.names = F)
          myda <- mydatc[1, ]
          myda[] = ""
          replcol <- which(names(myda) == input$column)
          myda[, replcol] = input$value
          return(myda)
        }}else{
          req(!is.null(mydatareactivb())==T)
          con <- DBI:::dbConnect(SQLite(), input$dbstorage)

          if(class(mydatareactivb())!='data.frame'){
            my <- data.frame(DBI:::dbReadTable(con, input$opened,check.names=F),check.names = F)
            myda <- my[1, ]
            myda[] = ""
            replcol <- which(names(myda) == input$column)
            myda[, replcol] = input$value
            return(myda) }else{
              my <- data.frame(mydatareactivb(),check.names = F)
              myda <- my[1, ]
              myda[] = ""
              replcol <- which(names(myda) == input$column)
              myda[, replcol] = input$value
              return(myda)
            }

        }

  })







  mydatareactived <- reactive({
    req(length(input$initdb)!=0)
    input$dbstorage
    req(length(choiseread())!=0)
    input$opened
    req(input$opened!='')

    req(!is.null(mydatareact())==T)
    con <- DBI:::dbConnect(SQLite(), input$dbstorage)
    req(dbExistsTable(con,input$opened)==T)
    if(class(mydatareact())!='data.frame'){

      my <- data.frame(DBI:::dbReadTable(con, input$opened,check.names=F),check.names = F)

      return(my)

    }else{

      my <- data.frame(mydatareact(),check.names = F)
      DBI:::dbBegin(con)

      return(my)

    }

  })

  X =function(){
    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    input$opened
    req(input$opened!='')
    if(length(input$merge) != 0){
      if(input$merge==T){
        reactiveValues(data = isolate(
          mydatareactived()))}else{
            reactiveValues(data = isolate(
              mydatareactived()))}}else
                if(length(input$commitb) != 0){
                  if(input$commitb==T){
                    reactiveValues(data = isolate(
                      mydatareactived()))}else{
                        reactiveValues(data = isolate(
                          mydatareactived()))}}else
                            if(length(input$commit) != 0){
                              if(input$commit==T){
                                reactiveValues(data = isolate(
                                  mydatareactived()))}else{
                                    req(input$dbstorage)
                                    req(input$opened)
                                    reactiveValues(data = isolate(
                                      mydatareactived()))}}else
                                        if(length(input$commitchanges) != 0){
                                          if(input$commitchanges == T) {
                                            reactiveValues(data = isolate(
                                              mydatareactived()))}else{
                                                reactiveValues(data = isolate(
                                                  mydatareactived()))}}else
                                                    if(length(input$confirm) != 0){
                                                      if(input$confirm == T) {
                                                        reactiveValues(data = isolate(
                                                          mydatareactived()))}else{
                                                            reactiveValues(data = isolate(
                                                              mydatareactived()))}}else
                                                                if(length(input$removeclm) != 0){
                                                                  if(input$removeclm == T) {
                                                                    reactiveValues(data = isolate(
                                                                      mydatareactived()))}else{
                                                                        reactiveValues(data = isolate(
                                                                          mydatareactived()))}}else
                                                                            if(length(input$makechanges) != 0){
                                                                              if(input$makechanges == T) {
                                                                                reactiveValues(data = isolate(
                                                                                  mydatareactived()))}else{
                                                                                    reactiveValues(data = isolate(
                                                                                      mydatareactived()))}}else
                                                                                        if(length(input$myconfirmation1 ) != 0){
                                                                                          if(input$myconfirmation1s == T) {
                                                                                            reactiveValues(data = isolate(
                                                                                              mydatareactived()))}else{
                                                                                                reactiveValues(data = isolate(
                                                                                                  mydatareactived()))}}else
                                                                                                    if(openvals$collapsed==F){mydatareactived()

                                                                                                    }else
                                                                                                      if(openvals$collapsed==T){mydatareactived()

                                                                                                      }else
                                                                                                        if(input$editintb=='append_rows|edit_cells'){mydatareactived()

                                                                                                        }else
                                                                                                        {
                                                                                                          reactiveValues(data = isolate(
                                                                                                            DBI:::dbReadTable(con, input$opened,check.names=F)))}
  }



  ################333/pppppppppppp


  dbrhson <- reactive({
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$dbstorage!='')
    req(input$opened!='')

    con <- DBI:::dbConnect(SQLite(), input$dbstorage)
    if(input$here==T){
      DBI:::dbWriteTable(con, input$opened, mydatareactive(),check.names=F,
                        append = TRUE)}else{
                          mydatareact()}

    data<-RSQLite:::dbReadTable(con,input$opened,check.names==F)
    return(data)
  })








  values <- list()

  setHot <- function(x) {
    values[["hot"]] <<- x }



  observe({
    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$input$dbstorage!='')
    req(input$input$opened!='')
    req(input$commitn==T) # update dataframe file each time the button is pressed
    con <- DBI:::dbConnect(SQLite(), input$dbstorage)

    if (!is.null(values[["hot"]])) { # if there's a table input
      DF <<- values$hot
    }
  })

  observe({
    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$input$dbstorage!='')
    req(input$input$opened!='')
    con <- DBI:::dbConnect(SQLite(), input$dbstorage)

    if (!is.null(input$hot)){
      DF <- (rhandsontable::hot_to_r(input$hot))
      setHot(DF)
    }
  })



  openvals=reactiveValues()
  openvals$collapsed=F
  observeEvent(input$opened,
               { openvals$collapsed=!openvals$collapsed }
  )



  mynewdb <- reactive({

    req(length(input$initdb)!=0)
    input$dbstorage
    input$opened
    req(input$dbstorage!='')
    req(input$opened!='')
    con <- DBI:::dbConnect(SQLite(), input$dbstorage)

    if (input$opened != "") {
      if (input$commit == T) {



        if (!is.null(input$appendeddb)){
          DFf <<- rhandsontable::hot_to_r(input$appendeddb)
          if(dbExistsTable(con,'bvuub')==F){
            dbWriteTable(con,'bvuub',DFf)}





          DBI:::dbBegin(con)
          DBI:::dbSendStatement(con,

                               paste("CREATE TEMPORARY TABLE", paste0("my_backupdbT(",to_scalar(x=names(DFf)),')')))
          DBI:::dbCommit(con)
          #########
          DBI:::dbBegin(con)
          DBI:::dbSendStatement(con, paste('INSERT INTO my_backupdbT SELECT', to_scalar(x=names(DFf)), 'FROM', 'bvuub'))
          DBI:::dbCommit(con)
          #########
          DBI:::dbBegin(con)
          DBI:::dbSendStatement(con,
                               paste('DROP TABLE', paste0('"',input$opened,'"')))
          dbCommit(con)
          #########
          DBI:::dbBegin(con)
          dbSendStatement(con,

                          paste("CREATE TABLE", paste0(paste0('"',input$opened,'"'),"(",to_scalar(x=names(DFf)),')')))
          DBI:::dbCommit(con)

          #########
          DBI:::dbBegin(con)
          DBI:::dbSendStatement(con,
                               paste('INSERT INTO', paste0('"',input$opened,'"'), 'SELECT', to_scalar(x=names(DFf)), 'FROM', 'my_backupdbT'))
          DBI:::dbCommit(con)
          #########
          DBI:::dbBegin(con)
          DBI:::dbSendStatement(con,
                               'DROP TABLE my_backupdbT')

          DBI:::dbCommit(con)
          #########
          DBI:::dbBegin(con)
          DBI:::dbSendStatement(con,
                               'DROP TABLE bvuub')

          DBI:::dbCommit(con)

        }

      }



      else{minirow()}

    }





    data <- DBI:::dbReadTable(con, input$opened,check.names=F)
    return(data)
  })









  output$hot <- rhandsontable:::renderRHandsontable({

    rhandsontable(dbrhson())

  })
  output$appendeddbBn <- rhandsontable:::renderRHandsontable({
    if (input$opened != "") {
      rhandsontable(dbrhson())
    }
  })



  output$appendeddb <- rhandsontable:::renderRHandsontable({
    input$opened
    req(input$opened!='')
    if(input$editintb=='append_rows|edit_cells'){
      if (openvals$collapsed==T){
        con <- DBI:::dbConnect(SQLite(), input$dbstorage)

        X<-X()
        if('Password'%in%names(mydatareactived())==T){
          rhandsontable(X$data)%>%
            rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
            rhandsontable::hot_col(col = 'Password',type = 'password')}else
              if('password'%in%names(mydatareactived())==T){
                rhandsontable(X$data)%>%
                  rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                  rhandsontable::hot_col(col = 'password',type = 'password')}else
                    if('date'%in%names(mydatareactived())==T){
                      rhandsontable(X$data)%>%
                        rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                        rhandsontable::hot_col(col = 'date',as.POSIXct(X$data$date[which(X$data$date!='')]),type = 'date')}else
                          if('Date'%in%names(mydatareactived())==T){
                            rhandsontable(X$data)%>%
                              rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                              rhandsontable::hot_col(col = 'Date',as.POSIXct(X$data$Date[which(X$data$Date!='')]),type = 'date')}else
                                if('Password '%in%names(mydatareactived())==T){
                                  rhandsontable(X$data)%>%
                                    rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                    rhandsontable::hot_col(col = 'Password ',type = 'password')}else
                                      if('password '%in%names(mydatareactived())==T){
                                        rhandsontable(X$data)%>%
                                          rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                          rhandsontable::hot_col(col = 'password ',type = 'password')}else
                                            if('date '%in%names(mydatareactived())==T){
                                              rhandsontable(X$data)%>%
                                                rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                rhandsontable::hot_col(col = 'date ',as.POSIXct(X$data$date[which(X$data$date!='')]),type = 'date')}else
                                                  if('Date '%in%names(mydatareactived())==T){
                                                    rhandsontable(X$data)%>%
                                                      rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                      rhandsontable::hot_col(col = 'Date ',as.POSIXct(X$data$Date[which(X$data$Date!='')]),type = 'date')}
        else
          rhandsontable(X$data)%>%
          rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)
      }else
      {
        X<-X()
        if('Password'%in%names(mydatareactived())==T){
          rhandsontable(X$data)%>%
            rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
            rhandsontable::hot_col(col = 'Password',type = 'password')}else
              if('password'%in%names(mydatareactived())==T){
                rhandsontable(X$data)%>%
                  rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                  rhandsontable::hot_col(col = 'password',type = 'password')}else
                    if('date'%in%names(mydatareactived())==T){
                      rhandsontable(X$data)%>%
                        rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                        rhandsontable::hot_col(col = 'date',as.POSIXct(X$data$date[which(X$data$date!='')]),type = 'date')}else
                          if('Date'%in%names(mydatareactived())==T){
                            rhandsontable(X$data)%>%
                              rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                              rhandsontable::hot_col(col = 'Date',as.POSIXct(X$data$Date[which(X$data$Date!='')]),type = 'date')}else
                                if('Password '%in%names(mydatareactived())==T){
                                  rhandsontable(X$data)%>%
                                    rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                    rhandsontable::hot_col(col = 'Password ',type = 'password')}else
                                      if('password '%in%names(mydatareactived())==T){
                                        rhandsontable(X$data)%>%
                                          rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                          rhandsontable::hot_col(col = 'password ',type = 'password')}else
                                            if('date '%in%names(mydatareactived())==T){
                                              rhandsontable(X$data)%>%
                                                rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                rhandsontable::hot_col(col = 'date ',as.POSIXct(X$data$date[which(X$data$date!='')]),type = 'date')}else
                                                  if('Date '%in%names(mydatareactived())==T){
                                                    rhandsontable(X$data)%>%
                                                      rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                      rhandsontable::hot_col(col = 'Date ',as.POSIXct(X$data$Date[which(X$data$Date!='')]),type = 'date')}
        else
          rhandsontable(X$data)%>%
          rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)}}else{
            con <- DBI:::dbConnect(SQLite(), input$dbstorage)
            if(openvals$collapsed==T){
              X<-X()
              if('Password'%in%names(mydatareactived())==T){
                rhandsontable(X$data)%>%
                  rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                  rhandsontable::hot_col(col = 'Password',type = 'password')}else
                    if('password'%in%names(mydatareactived())==T){
                      rhandsontable(X$data)%>%
                        rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                        rhandsontable::hot_col(col = 'password',type = 'password')}else
                          if('date'%in%names(mydatareactived())==T){
                            rhandsontable(X$data)%>%
                              rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                              rhandsontable::hot_col(col = 'date',as.POSIXct(X$data$date[which(X$data$date!='')]),type = 'date')}else
                                if('Date'%in%names(mydatareactived())==T){
                                  rhandsontable(X$data)%>%
                                    rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                    rhandsontable::hot_col(col = 'Date',as.POSIXct(X$data$Date[which(X$data$Date!='')]),type = 'date')}else
                                      if('Password '%in%names(mydatareactived())==T){
                                        rhandsontable(X$data)%>%
                                          rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                          rhandsontable::hot_col(col = 'Password ',type = 'password')}else
                                            if('password '%in%names(mydatareactived())==T){
                                              rhandsontable(X$data)%>%
                                                rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                rhandsontable::hot_col(col = 'password ',type = 'password')}else
                                                  if('date '%in%names(mydatareactived())==T){
                                                    rhandsontable(X$data)%>%
                                                      rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                      rhandsontable::hot_col(col = 'date ',as.POSIXct(X$data$date[which(X$data$date!='')]),type = 'date')}else
                                                        if('Date '%in%names(mydatareactived())==T){
                                                          rhandsontable(X$data)%>%
                                                            rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                            rhandsontable::hot_col(col = 'Date ',as.POSIXct(X$data$Date[which(X$data$Date!='')]),type = 'date')}
              else
                rhandsontable(X$data)%>%
                rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)}else
                {
                  X<-X()
                  if('Password'%in%names(mydatareactived())==T){
                    rhandsontable(X$data)%>%
                      rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                      rhandsontable::hot_col(col = 'Password',type = 'password')}else
                        if('password'%in%names(mydatareactived())==T){
                          rhandsontable(X$data)%>%
                            rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                            rhandsontable::hot_col(col = 'password',type = 'password')}else
                              if('date'%in%names(mydatareactived())==T){
                                rhandsontable(X$data)%>%
                                  rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                  rhandsontable::hot_col(col = 'date',as.POSIXct(X$data$date[which(X$data$date!='')]),type = 'date')}else
                                    if('Date'%in%names(mydatareactived())==T){
                                      rhandsontable(X$data)%>%
                                        rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                        rhandsontable::hot_col(col = 'Date',as.POSIXct(X$data$Date[which(X$data$Date!='')]),type = 'date')}else
                                          if('Password '%in%names(mydatareactived())==T){
                                            rhandsontable(X$data)%>%
                                              rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                              rhandsontable::hot_col(col = 'Password ',type = 'password')}else
                                                if('password '%in%names(mydatareactived())==T){
                                                  rhandsontable(X$data)%>%
                                                    rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                    rhandsontable::hot_col(col = 'password ',type = 'password')}else
                                                      if('date '%in%names(mydatareactived())==T){
                                                        rhandsontable(X$data)%>%
                                                          rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                          rhandsontable::hot_col(col = 'date ',as.POSIXct(X$data$date[which(X$data$date!='')]),type = 'date')}else
                                                            if('Date '%in%names(mydatareactived())==T){
                                                              rhandsontable(X$data)%>%
                                                                rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                                rhandsontable::hot_col(col = 'Date ',as.POSIXct(X$data$Date[which(X$data$Date!='')]),type = 'date')}
                  else
                    rhandsontable(X$data)%>%
                    rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)}
          }
  })


  observeEvent(input$appendeddb$changes$changes,{

    if(input$editintb=='append_rows|edit_cells'){
      con <- DBI:::dbConnect(SQLite(), input$dbstorage)
      X<-X()
      row = input$appendeddb$changes$changes[[]]
      col = input$appendeddb$changes$changes[[]]
      value = input$appendeddb$changes$changes[[]]

      X$data[row,col] = value}else{
        con <- DBI:::dbConnect(SQLite(), input$dbstorage)
        X<-X()
        row = input$appendeddb$changes$changes[[]]
        col = input$appendeddb$changes$changes[[]]
        value = input$appendeddb$changes$changes[[]]

        X$data[row,col] = value
      }

  })





  output$appendeddbFx <- rhandsontable:::renderRHandsontable({
    if (input$opened != "") {
      rhandsontable(mynewdb())
    }
  })
  output$editincol <- renderUI({
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened!='')
    con <- DBI:::dbConnect(SQLite(), input$dbstorage)
    if (input$opened == "") {
      "You have not opened any data.frame. First open any of your data.frames or create or upload one if you don't have. See tabPanel 1"
    }
    else {
      myda <- DBI:::dbReadTable(con, input$opened,check.names=F)
      wellPanel(selectInput("col", label = "edit cell in this column",
                            choices = names(myda)), textInput("changes",
                                                              "", placeholder = paste("my edit", "|", "Just type anything to add")), checkboxInput("commitchanges",
                                                                                                                                                   label = "commit", value = F)
      )
    }
  })

  quotedchanges <- reactive({
    paste0("'", input$changes, "'")
  })
  quotedcond <- reactive({
    if(all((is.numeric(input$mycond)==F),(is.character(input$mycond)==F))==T){
      input$mycond
    }else
      if(all((is.numeric(input$mycond)==F),(is.character(input$mycond)==F))==F)
        paste0("'", input$mycond, "'")
  })
  output$cuo <- renderText({
    y = cuod()
    return(y)
  })
  output$conditvalue <- renderUI({
    if (input$opened == "") {
    }
    else {
      req(length(input$initdb)!=0)
      input$dbstorage
      con <- DBI:::dbConnect(SQLite(), input$dbstorage)
      myda <- mydatareactived()
      x <- myda%>% dplyr:::select(!!dplyr:::sym(input$myfilter))
      fluidPage(selectInput("mycond", uiOutput('uicon'), choices = x,width = 130))
    }
  })

  dplyrfiter<-reactive({req(length(isolate({input$dbstorage}))!=0)
    input$dbstorage
    con <- DBI:::dbConnect(SQLite(), input$dbstorage)
    myda <- mydatareactived()
    x <- myda%>% dplyr:::select(!!dplyr:::sym(input$myfilter))
    return(x)})

  rowedited<-reactiveVal(1)

  observe({
    req(input$commitchanges==T)
    req(isolate({selccol()})==length(isolate(col2())))
    isolate(rowedited(rowedited()+1))
  })

  observe({
    req(input$commitchanges==T)
    req(isolate({selccol()})==length(isolate(col2())))
    myvl<-isolate({selccol()})
    myvls <- isolate({dplyrfiter()})
    sl<-myvls[isolate({rowedited()})]
    req(myvl==1)
    updateSelectInput(session,"mycond", HTML(
      span("where",input$myfilter, style = "color:red"),
      "=",span(uiOutput('uicon'), style = "color:fuchsia")), choices = myvls,selected = sl)
  })




  selccol<-reactiveVal(1)

  observe({
    req(input$commitchanges==F)
    req(length(isolate({col2()}))<=isolate({selccol()}))
    isolate(selccol(0))
  })

  observe({

    req(input$commitchanges==T)
    req(length(isolate({col2()}))>=isolate({selccol()}))
    isolate(selccol(selccol()+1))
  })
  observe({
    req(input$commitchanges==T)
    mycols <- isolate({col2()})
    slcted<-mycols[isolate({selccol()})]
    updateSelectInput(session,"col", "edit in ccolumn;",
                      choices = mycols,selected = slcted)
  })


  output$uicon <- renderUI({
    div("where;",span(input$myfilter, style = "color:red"),
        "=",
        span(input$mycond, style = "color:fuchsia"), style = 'font-size:85%')
  })
  dbeditcells <- reactive({
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened!='')
    con <- DBI:::dbConnect(SQLite(), input$dbstorage)
    if (input$commitchanges == T) {
      if(input$mycond!='NA'){
        DBI:::dbBegin(con)
        DBI:::dbSendStatement(con, paste("UPDATE", paste0('"',input$opened,'"'),
                                        "SET", paste0('[',input$col,']'), "=", quotedchanges(), "WHERE",
                                        paste0('[',input$myfilter,']'), " =", quotedcond()))
        DBI:::dbCommit(con)}else
        {
          rd<-DBI:::dbReadTable(con, input$opened,check.names=F)
          colrep<-rd[,input$myfilter]
          pos<-which(names(rd)==input$myfilter)
          colrep[is.na(colrep)] <- paste0('"',input$changes,'"')
          rdnopos<-rd[,-pos]
          cbndrd<-cbind(rdnopos,colrep)
          colrN<-data.frame(cbndrd,check.names = F)


          fina<-colrN%>%dplyr:::relocate(colrep,.before = pos)
          final<-fina%>%`colnames<-`(names(rd))

          DBI:::dbWriteTable(con,'finl',final,check.names=F)
          ######33----------------------------------------------------------------------------
          DBI:::dbBegin(con)

          DBI:::dbSendStatement(con,

                               paste("CREATE TEMPORARY TABLE", paste0("my_backupdbT(",to_scalar(x=names(final)),')')))
          DBI:::dbCommit(con)
          DBI:::dbBegin(con)
          DBI:::dbSendStatement(con, paste('INSERT INTO my_backupdbT SELECT', to_scalar(x=names(final)), 'FROM', 'finl'))


          DBI:::dbCommit(con)
          DBI:::dbBegin(con)


          DBI:::dbSendStatement(con,

                               paste('DROP TABLE', paste0('"',input$opened,'"')))

          dbSendStatement(con,

                          paste("CREATE TABLE", paste0(paste0('"',input$opened,'"'),"(",to_scalar(x=names(final)),')')))
          DBI:::dbCommit(con)
          DBI:::dbBegin(con)
          DBI:::dbSendStatement(con,
                               paste('INSERT INTO', paste0('"',input$opened,'"'), 'SELECT', to_scalar(x=names(final)), 'FROM', 'my_backupdbT'))
          DBI:::dbCommit(con)
          DBI:::dbBegin(con)
          DBI:::dbSendStatement(con,
                               'DROP TABLE my_backupdbT')

          DBI:::dbCommit(con)

          DBI:::dbBegin(con)
          DBI:::dbSendStatement(con,
                               'DROP TABLE finl')

          DBI:::dbCommit(con)

        }


    }
    else{
      if(class(mydatareactiv())=='data.frame'){
        mydatareactived()}}







    data <- DBI:::dbReadTable(con, input$opened,check.names=F)
    return(data)
  })
  output$editedcells <- rhandsontable:::renderRHandsontable({
    input$opened
    req(input$opened!='')

    con <- DBI:::dbConnect(SQLite(), input$dbstorage)


    X<-X()
    if(all(('Password'%in%names(mydatareactived())==T),('date'%in%names(mydatareactived())==T))){
      rhandsontable(X$data, readOnly = T)%>%
        rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
        rhandsontable::hot_col(col = 'Password',type = 'password')%>%
        rhandsontable::hot_col(col = 'date',X$data$date[which(X$data$date!='')],type = 'date')}else
          if(all(('password'%in%names(mydatareactived())==T),('date'%in%names(mydatareactived())==T))){
            rhandsontable(X$data, readOnly = T)%>%
              rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
              rhandsontable::hot_col(col = 'password',type = 'password')%>%
              rhandsontable::hot_col(col = 'date',X$data$date[which(X$data$date!='')],type = 'date')}else
                if(all(('Password'%in%names(mydatareactived())==T),('Date'%in%names(mydatareactived())==T))){
                  rhandsontable(X$data, readOnly = T)%>%
                    rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                    rhandsontable::hot_col(col = 'Password',type = 'password')%>%
                    rhandsontable::hot_col(col = 'Date',X$data$Date[which(X$data$Date!='')],type = 'date')}else
                      if(all(('password'%in%names(mydatareactived())==T),('Date'%in%names(mydatareactived())==T))){
                        rhandsontable(X$data, readOnly = T)%>%
                          rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                          rhandsontable::hot_col(col = 'password',type = 'password')%>%
                          rhandsontable::hot_col(col = 'Date',X$data$Date[which(X$data$Date!='')],type = 'date')}else
                            if('Password'%in%names(mydatareactived())==T){
                              rhandsontable(X$data, readOnly = T)%>%
                                rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                rhandsontable::hot_col(col = 'Password',type = 'password')}else
                                  if('password'%in%names(mydatareactived())==T){
                                    rhandsontable(X$data, readOnly = T)%>%
                                      rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                      rhandsontable::hot_col(col = 'password',type = 'password')}else
                                        if('date'%in%names(mydatareactived())==T){
                                          rhandsontable(X$data, readOnly = T)%>%
                                            rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                            rhandsontable::hot_col(col = 'date',X$data$date[which(X$data$date!='')],type = 'date')}else
                                              if('Date'%in%names(mydatareactived())==T){
                                                rhandsontable(X$data, readOnly = T)%>%
                                                  rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                  rhandsontable::hot_col(col = 'Date',X$data$Date[which(X$data$Date!='')],type = 'date')}else
                                                    if(all(('password '%in%names(mydatareactived())==T),('Date '%in%names(mydatareactived())==T))){
                                                      rhandsontable(X$data, readOnly = T)%>%
                                                        rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                        rhandsontable::hot_col(col = 'password ',type = 'password')%>%
                                                        rhandsontable::hot_col(col = 'Date ',X$data$date[which(X$data$date!='')],type = 'date')}else
                                                          if(all(('Password '%in%names(mydatareactived())==T),('Date '%in%names(mydatareactived())==T))){
                                                            rhandsontable(X$data, readOnly = T)%>%
                                                              rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                              rhandsontable::hot_col(col = 'Password ',type = 'password')%>%
                                                              rhandsontable::hot_col(col = 'Date ',X$data$date[which(X$data$date!='')],type = 'date')}else
                                                                if(all(('password '%in%names(mydatareactived())==T),('date '%in%names(mydatareactived())==T))){
                                                                  rhandsontable(X$data, readOnly = T)%>%
                                                                    rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                                    rhandsontable::hot_col(col = 'password ',type = 'password')%>%
                                                                    rhandsontable::hot_col(col = 'date ',X$data$Date[which(X$data$Date!='')],type = 'date')}else
                                                                      if(all(('Password '%in%names(mydatareactived())==T),('date '%in%names(mydatareactived())==T))){
                                                                        rhandsontable(X$data, readOnly = T)%>%
                                                                          rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                                          rhandsontable::hot_col(col = 'Password ',type = 'password')%>%
                                                                          rhandsontable::hot_col(col = 'date ',X$data$Date[which(X$data$Date!='')],type = 'date')}else
                                                                            if('password '%in%names(mydatareactived())==T){
                                                                              rhandsontable(X$data, readOnly = T)%>%
                                                                                rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                                                rhandsontable::hot_col(col = 'password ',type = 'password')}else
                                                                                  if('Password '%in%names(mydatareactived())==T){
                                                                                    rhandsontable(X$data, readOnly = T)%>%
                                                                                      rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                                                      rhandsontable::hot_col(col = 'Password ',type = 'password')}else
                                                                                        if('Date '%in%names(mydatareactived())==T){
                                                                                          rhandsontable(X$data, readOnly = T)%>%
                                                                                            rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                                                            rhandsontable::hot_col(col = 'Date ',X$data$date[which(X$data$date!='')],type = 'date')}else
                                                                                              if('date '%in%names(mydatareactived())==T){
                                                                                                rhandsontable(X$data, readOnly = T)%>%
                                                                                                  rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                                                                  rhandsontable::hot_col(col = 'date ',X$data$Date[which(X$data$Date!='')],type = 'date')}
    else
      rhandsontable(X$data, readOnly = T)%>%
      rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)

  })




  output$addcolsui <- renderUI({
    if (input$opened == "") {
      "You have not opened any data.frame. First open any of your data.frames or create or upload one if you don't have. See tabPanel 1"
    }
    else {
      fluidPage(textInput("addcol", placeholder = "type the column name you would like to add",
                          label = ""), checkboxInput("confirm", value = F,
                                                     "commit"))
    }
  })
  dbaddedcol <- reactive({
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened!='')
    req(vlued()==1)
    con <- DBI:::dbConnect(SQLite(), input$dbstorage)

    if (input$confirm == T) {
      req(input$addcol %in% names(mydatareactive()) == F)
      DBI:::dbBegin(con)
      DBI:::dbSendStatement(con, paste("\n                           ALTER TABLE",
                                      paste0('"',input$opened,'"'), "ADD COLUMN", paste0('[',input$addcol,']'),
                                      "Text; "))
      DBI:::dbCommit(con)
    }
    else{
      if(class(mydatareactiv())=='data.frame'){
        mydatareactiv()}}
    data <- DBI:::dbReadTable(con, input$opened,check.names=F)
    return(data)
  })
  output$dbaddedcolstable <- rhandsontable:::renderRHandsontable({
    input$opened
    req(input$opened!='')

    con <- DBI:::dbConnect(SQLite(), input$dbstorage)


    X<-X()
    if(all(('Password'%in%names(mydatareactived())==T),('date'%in%names(mydatareactived())==T))){
      rhandsontable(X$data)%>%
        rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
        rhandsontable::hot_col(col = 'Password',type = 'password')%>%
        rhandsontable::hot_cell(col = 'date',X$data$date[which(X$data$date!='')],type = 'date')}else
          if(all(('password'%in%names(mydatareactived())==T),('date'%in%names(mydatareactived())==T))){
            rhandsontable(X$data)%>%
              rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
              rhandsontable::hot_col(col = 'password',type = 'password')%>%
              rhandsontable::hot_col(col = 'date',X$data$date[which(X$data$date!='')],type = 'date')}else
                if(all(('Password'%in%names(mydatareactived())==T),('Date'%in%names(mydatareactived())==T))){
                  rhandsontable(X$data)%>%
                    rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                    rhandsontable::hot_col(col = 'Password',type = 'password')%>%
                    rhandsontable::hot_col(col = 'Date',X$data$Date[which(X$data$Date!='')],type = 'date')}else
                      if(all(('password'%in%names(mydatareactived())==T),('Date'%in%names(mydatareactived())==T))){
                        rhandsontable(X$data)%>%
                          rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                          rhandsontable::hot_col(col = 'password',type = 'password')%>%
                          rhandsontable::hot_col(col = 'Date',X$data$Date[which(X$data$Date!='')],type = 'date')}else
                            if('Password'%in%names(mydatareactived())==T){
                              rhandsontable(X$data)%>%
                                rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                rhandsontable::hot_col(col = 'Password',type = 'password')}else
                                  if('password'%in%names(mydatareactived())==T){
                                    rhandsontable(X$data)%>%
                                      rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                      rhandsontable::hot_col(col = 'password',type = 'password')}else
                                        if('date'%in%names(mydatareactived())==T){
                                          rhandsontable(X$data)%>%
                                            rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                            rhandsontable::hot_col(col = 'date',X$data$date[which(X$data$date!='')],type = 'date')}else
                                              if('Date'%in%names(mydatareactived())==T){
                                                rhandsontable(X$data)%>%
                                                  rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                  rhandsontable::hot_col(col = 'Date',X$data$Date[which(X$data$Date!='')],type = 'date')}else
                                                    if(all(('password '%in%names(mydatareactived())==T),('Date '%in%names(mydatareactived())==T))){
                                                      rhandsontable(X$data)%>%
                                                        rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                        rhandsontable::hot_col(col = 'password ',type = 'password')%>%
                                                        rhandsontable::hot_col(col = 'Date ',X$data$date[which(X$data$date!='')],type = 'date')}else
                                                          if(all(('Password '%in%names(mydatareactived())==T),('Date '%in%names(mydatareactived())==T))){
                                                            rhandsontable(X$data)%>%
                                                              rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                              rhandsontable::hot_col(col = 'Password ',type = 'password')%>%
                                                              rhandsontable::hot_col(col = 'Date ',X$data$date[which(X$data$date!='')],type = 'date')}else
                                                                if(all(('password '%in%names(mydatareactived())==T),('date '%in%names(mydatareactived())==T))){
                                                                  rhandsontable(X$data)%>%
                                                                    rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                                    rhandsontable::hot_col(col = 'password ',type = 'password')%>%
                                                                    rhandsontable::hot_col(col = 'date ',X$data$Date[which(X$data$Date!='')],type = 'date')}else
                                                                      if(all(('Password '%in%names(mydatareactived())==T),('date '%in%names(mydatareactived())==T))){
                                                                        rhandsontable(X$data)%>%
                                                                          rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                                          rhandsontable::hot_col(col = 'Password ',type = 'password')%>%
                                                                          rhandsontable::hot_col(col = 'date ',X$data$Date[which(X$data$Date!='')],type = 'date')}else
                                                                            if('password '%in%names(mydatareactived())==T){
                                                                              rhandsontable(X$data)%>%
                                                                                rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                                                rhandsontable::hot_col(col = 'password ',type = 'password')}else
                                                                                  if('Password '%in%names(mydatareactived())==T){
                                                                                    rhandsontable(X$data)%>%
                                                                                      rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                                                      rhandsontable::hot_col(col = 'Password ',type = 'password')}else
                                                                                        if('Date '%in%names(mydatareactived())==T){
                                                                                          rhandsontable(X$data)%>%
                                                                                            rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                                                            rhandsontable::hot_col(col = 'Date ',X$data$date[which(X$data$date!='')],type = 'date')}else
                                                                                              if('date '%in%names(mydatareactived())==T){
                                                                                                rhandsontable(X$data)%>%
                                                                                                  rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                                                                  rhandsontable::hot_col(col = 'date ',X$data$Date[which(X$data$Date!='')],type = 'date')}else
                                                                                                  {rhandsontable(X$data)%>%
                                                                                                      rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)}

  })


  rbinddb <- reactive({

    req(length(input$initdb)!=0)
    input$dbstorage
    input$opened
    req(input$opened!='')
    req(input$dbstorage!='')
    con <- DBI:::dbConnect(SQLite(), input$dbstorage)

    if (input$opened != "") {
      input$opened
      req(vlued()==1)
      req(input$editintb=='bind.table')
      if (input$merge == T) {




        DFf <<- function(){
          X=X()
          len=length(X$data[,1])

          lenb=length(isolate({uplobind()})[,1])
          nm=names(X$data)
          nmb=names(isolate({uplobind()}))
          vek<-nmb%in%nm
          aTRUE<-T%in%vek
          fTRUE<-F%in%vek
          if(len==lenb && all(fTRUE)==T){
            if(aTRUE==F){
              cbind(X$data,uplobind())}else{
                jsonlite:::rbind_pages(list(X$data,uplobind()))
              }
          }else{
            jsonlite:::rbind_pages(list(X$data,uplobind()))
          }
        }
        DFfb<-DFf()
        if(dbExistsTable(con,'bvuub')==F){
          dbWriteTable(con,'bvuub',DFfb)}





        DBI:::dbBegin(con)
        DBI:::dbSendStatement(con,

                             paste("CREATE TEMPORARY TABLE", paste0("my_backupdbT(",to_scalar(x=names(DFfb)),')')))
        DBI:::dbCommit(con)
        #########
        DBI:::dbBegin(con)
        DBI:::dbSendStatement(con, paste('INSERT INTO my_backupdbT SELECT', to_scalar(x=names(DFfb)), 'FROM', 'bvuub'))
        DBI:::dbCommit(con)
        #########
        DBI:::dbBegin(con)
        DBI:::dbSendStatement(con,
                             paste('DROP TABLE', paste0('"',input$opened,'"')))
        dbCommit(con)
        #########
        DBI:::dbBegin(con)
        dbSendStatement(con,

                        paste("CREATE TABLE", paste0(paste0('"',input$opened,'"'),"(",to_scalar(x=names(DFfb)),')')))
        DBI:::dbCommit(con)

        #########
        DBI:::dbBegin(con)
        DBI:::dbSendStatement(con,
                             paste('INSERT INTO', paste0('"',input$opened,'"'), 'SELECT', to_scalar(x=names(DFfb)), 'FROM', 'my_backupdbT'))
        DBI:::dbCommit(con)
        #########
        DBI:::dbBegin(con)
        DBI:::dbSendStatement(con,
                             'DROP TABLE my_backupdbT')

        DBI:::dbCommit(con)
        #########
        DBI:::dbBegin(con)
        DBI:::dbSendStatement(con,
                             'DROP TABLE bvuub')

        DBI:::dbCommit(con)



      }



      else{mydatareact()}

    }


    data <- DBI:::dbReadTable(con, input$opened,check.names=F)
    return(data)
  })
  allowmainediting <- reactive({

    req(length(input$initdb)!=0)
    input$dbstorage
    input$opened
    req(input$opened!='')
    req(input$dbstorage!='')
    con <- DBI:::dbConnect(SQLite(), input$dbstorage)

    if (input$opened != "") {
      input$opened
      req(vlued()==1)
      #req(input$editintb=='home')
      if (input$commitb == T) {



        if (!is.null(input$dbaddedcolstable)){
          DFfb <<- rhandsontable::hot_to_r(input$dbaddedcolstable)
          if(dbExistsTable(con,'bvuub')==F){
            dbWriteTable(con,'bvuub',DFfb)}





          DBI:::dbBegin(con)
          DBI:::dbSendStatement(con,

                               paste("CREATE TEMPORARY TABLE", paste0("my_backupdbT(",to_scalar(x=names(DFfb)),')')))
          DBI:::dbCommit(con)
          #########
          DBI:::dbBegin(con)
          DBI:::dbSendStatement(con, paste('INSERT INTO my_backupdbT SELECT', to_scalar(x=names(DFfb)), 'FROM', 'bvuub'))
          DBI:::dbCommit(con)
          #########
          DBI:::dbBegin(con)
          DBI:::dbSendStatement(con,
                               paste('DROP TABLE', paste0('"',input$opened,'"')))
          dbCommit(con)
          #########
          DBI:::dbBegin(con)
          dbSendStatement(con,

                          paste("CREATE TABLE", paste0(paste0('"',input$opened,'"'),"(",to_scalar(x=names(DFfb)),')')))
          DBI:::dbCommit(con)

          #########
          DBI:::dbBegin(con)
          DBI:::dbSendStatement(con,
                               paste('INSERT INTO', paste0('"',input$opened,'"'), 'SELECT', to_scalar(x=names(DFfb)), 'FROM', 'my_backupdbT'))
          DBI:::dbCommit(con)
          #########
          DBI:::dbBegin(con)
          DBI:::dbSendStatement(con,
                               'DROP TABLE my_backupdbT')

          DBI:::dbCommit(con)
          #########
          DBI:::dbBegin(con)
          DBI:::dbSendStatement(con,
                               'DROP TABLE bvuub')

          DBI:::dbCommit(con)

        }

      }



      else{mydatareact()}

    }


    data <- DBI:::dbReadTable(con, input$opened,check.names=F)
    return(data)
  })









  selcsion<-reactiveVal(1)

  observe({
    req(input$makechanges==F)
    req(length(isolate({chois()}))<=isolate({selcsion()}))
    isolate(selcsion(0))
  })

  observe({

    req(input$makechanges==T)
    req(length(isolate({chois()}))>=isolate({selcsion()}))
    isolate(selcsion(selcsion()+1))
  })
  observe({
    req(input$makechanges==T)
    cols <- isolate({chois()})
    slctd<-cols[isolate({selcsion()})]
    updateSelectInput(session,"selectedcol", "select the column to rename",
                      choices = cols,selected = slctd)
  })
  output$selectclm <- renderUI({
    req(length(input$initdb)!=0)
    input$dbstorage
    con <- DBI:::dbConnect(SQLite(), input$dbstorage)
    cols <- chois()
    fluidPage(selectInput("selectedcol", "select the column to rename",
                          choices = cols), textInput("newname", placeholder = "type the new name here",
                                                     ""), checkboxInput("makechanges", "commit",
                                                                        value = F))
  })
  changecolname <- reactive({
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened!='')
    con <- DBI:::dbConnect(SQLite(), input$dbstorage)
    if (input$makechanges == T) {
      DBI:::dbBegin(con)
      DBI:::dbSendStatement(con, paste("\n                           ALTER TABLE",paste0('"',
                                                                                        input$opened,'"'), "RENAME COLUMN", paste0('[',input$selectedcol,']'),
                                      "TO", paste0(paste0('[',input$newname,']'), ";")))
      DBI:::dbCommit(con)
    } else{
      if(class(mydatareactived())=='data.frame'){
        mydatareact()}}

    data <- DBI:::dbReadTable(con, input$opened,check.names=F)
    return(data)
  })

  remametabl<-reactive({
    req(length(input$initdb)!=0)
    req(input$dbstorage!='')
    input$dbstorage
    con <- DBI:::dbConnect(SQLite(), input$dbstorage)
    req(input$dfrnm!='')
    input$dtfrm
    con <- DBI:::dbConnect(SQLite(), input$dbstorage)
    dtcols <- DBI:::dbReadTable(con, input$dfrnm,check.names=F)
    DBI:::dbBegin(con)
    DBI:::dbSendStatement(con,
                         paste("CREATE TEMPORARY TABLE", paste0("my_backupdbx(",to_scalar(x=names(dtcols)),')')))
    DBI:::dbCommit(con)
    DBI:::dbBegin(con)
    DBI:::dbSendStatement(con, paste('INSERT INTO my_backupdbx SELECT', to_scalar(x=names(dtcols)), 'FROM', paste0('"',input$dfrnm,'"')))
    DBI:::dbCommit(con)
    drd<-DBI:::dbReadTable(con, "my_backupdbx",check.names=F)
    if(input$rnmtb==T){
      if(dbExistsTable(con,rnmed())==F){
        RSQLite:::dbCreateTable(con,rnmed(),drd)
        dbAppendTable(con,rnmed(),drd)
      }
      dt <- DBI:::dbReadTable(con, input$rnmto,check.names=F)
      return(dt) }else
        if(input$finlise==T){
          DBI:::dbBegin(con)
          DBI:::dbSendStatement(con,
                               paste('DROP TABLE', paste("'",rmytb(),"'")))
          dbCommit(con)
          DBI:::dbBegin(con)
          DBI:::dbSendStatement(con,
                               paste('DROP TABLE', "my_backupdbx"))
          dbCommit(con)}
    #########

  })

  output$mynmtb<-rhandsontable:::renderRHandsontable({
    if(input$rnmtb==F){
      shiny:::validate(input$rnmtb==T,'to rename, type new name for the table opened in this pane')
    }else
      rhandsontable(remametabl(),readOnly = T)
  })
  observe({
    req(input$rnmtb==T)
    ask_confirmation(inputId = 'finlise',
                     type = 'warning',
                     text = 'Confirm you want to change table name'
    )
  })
  output$changedcoltable <- rhandsontable:::renderRHandsontable({
    input$opened


    con <- DBI:::dbConnect(SQLite(), input$dbstorage)


    X<-X()
    if(all(('Password'%in%names(mydatareactived())==T),('date'%in%names(mydatareactived())==T))){
      rhandsontable(X$data, readOnly = T)%>%
        rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
        rhandsontable::hot_col(col = 'Password',type = 'password')%>%
        rhandsontable::hot_col(col = 'date',X$data$date[which(X$data$date!='')],type = 'date')}else
          if(all(('password'%in%names(mydatareactived())==T),('date'%in%names(mydatareactived())==T))){
            rhandsontable(X$data, readOnly = T)%>%
              rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
              rhandsontable::hot_col(col = 'password',type = 'password')%>%
              rhandsontable::hot_col(col = 'date',X$data$date[which(X$data$date!='')],type = 'date')}else
                if(all(('Password'%in%names(mydatareactived())==T),('Date'%in%names(mydatareactived())==T))){
                  rhandsontable(X$data, readOnly = T)%>%
                    rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                    rhandsontable::hot_col(col = 'Password',type = 'password')%>%
                    rhandsontable::hot_col(col = 'Date',X$data$Date[which(X$data$Date!='')],type = 'date')}else
                      if(all(('password'%in%names(mydatareactived())==T),('Date'%in%names(mydatareactived())==T))){
                        rhandsontable(X$data, readOnly = T)%>%
                          rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                          rhandsontable::hot_col(col = 'password',type = 'password')%>%
                          rhandsontable::hot_col(col = 'Date',X$data$Date[which(X$data$Date!='')],type = 'date')}else
                            if('Password'%in%names(mydatareactived())==T){
                              rhandsontable(X$data, readOnly = T)%>%
                                rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                rhandsontable::hot_col(col = 'Password',type = 'password')}else
                                  if('password'%in%names(mydatareactived())==T){
                                    rhandsontable(X$data, readOnly = T)%>%
                                      rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                      rhandsontable::hot_col(col = 'password',type = 'password')}else
                                        if('date'%in%names(mydatareactived())==T){
                                          rhandsontable(X$data, readOnly = T)%>%
                                            rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                            rhandsontable::hot_col(col = 'date',X$data$date[which(X$data$date!='')],type = 'date')}else
                                              if('Date'%in%names(mydatareactived())==T){
                                                rhandsontable(X$data, readOnly = T)%>%
                                                  rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                  rhandsontable::hot_col(col = 'Date',X$data$Date[which(X$data$Date!='')],type = 'date')}else
                                                    if(all(('password '%in%names(mydatareactived())==T),('Date '%in%names(mydatareactived())==T))){
                                                      rhandsontable(X$data, readOnly = T)%>%
                                                        rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                        rhandsontable::hot_col(col = 'password ',type = 'password')%>%
                                                        rhandsontable::hot_col(col = 'Date ',X$data$date[which(X$data$date!='')],type = 'date')}else
                                                          if(all(('Password '%in%names(mydatareactived())==T),('Date '%in%names(mydatareactived())==T))){
                                                            rhandsontable(X$data, readOnly = T)%>%
                                                              rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                              rhandsontable::hot_col(col = 'Password ',type = 'password')%>%
                                                              rhandsontable::hot_col(col = 'Date ',X$data$date[which(X$data$date!='')],type = 'date')}else
                                                                if(all(('password '%in%names(mydatareactived())==T),('date '%in%names(mydatareactived())==T))){
                                                                  rhandsontable(X$data, readOnly = T)%>%
                                                                    rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                                    rhandsontable::hot_col(col = 'password ',type = 'password')%>%
                                                                    rhandsontable::hot_col(col = 'date ',X$data$Date[which(X$data$Date!='')],type = 'date')}else
                                                                      if(all(('Password '%in%names(mydatareactived())==T),('date '%in%names(mydatareactived())==T))){
                                                                        rhandsontable(X$data, readOnly = T)%>%
                                                                          rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                                          rhandsontable::hot_col(col = 'Password ',type = 'password')%>%
                                                                          rhandsontable::hot_col(col = 'date ',X$data$Date[which(X$data$Date!='')],type = 'date')}else
                                                                            if('password '%in%names(mydatareactived())==T){
                                                                              rhandsontable(X$data, readOnly = T)%>%
                                                                                rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                                                rhandsontable::hot_col(col = 'password ',type = 'password')}else
                                                                                  if('Password '%in%names(mydatareactived())==T){
                                                                                    rhandsontable(X$data, readOnly = T)%>%
                                                                                      rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                                                      rhandsontable::hot_col(col = 'Password ',type = 'password')}else
                                                                                        if('Date '%in%names(mydatareactived())==T){
                                                                                          rhandsontable(X$data, readOnly = T)%>%
                                                                                            rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                                                            rhandsontable::hot_col(col = 'Date ',X$data$date[which(X$data$date!='')],type = 'date')}else
                                                                                              if('date '%in%names(mydatareactived())==T){
                                                                                                rhandsontable(X$data, readOnly = T)%>%
                                                                                                  rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                                                                  rhandsontable::hot_col(col = 'date ',X$data$Date[which(X$data$Date!='')],type = 'date')}
    else
      rhandsontable(X$data, readOnly = T)%>%
      rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)

  })



  to_scalar <-function(x){
    if(length(x)>1){
      xs=paste0(data.frame(x,check.names = F))

      scalarOutput <- substr(xs,3,nchar(xs)-1)

      return(scalarOutput)}else
      {return(x)}
  }

  chois<-reactive({
    req(length(input$initdb)!=0)
    input$dbstorage
    con <- DBI:::dbConnect(SQLite(), input$dbstorage)


    if(class(mydatareac())=='data.frame'){
      dbcol<-mydatareac()
      chois<-names(dbcol)
      chois}
    else{
      dbcol<-DBI:::dbReadTable(con,input$opened,check.names=F)
      chois<-names(dbcol)
      chois}
  })


  output$columremoveui<-renderUI({
    req(length(input$initdb)!=0)
    input$dbstorage
    con <- DBI:::dbConnect(SQLite(), input$dbstorage)

    fluidPage(
      shinyWidgets:::pickerInput('clchosen',choices = chois(),multiple = T,label =uiOutput('colunchsn') ),
      checkboxInput('removclm','check to confirm the column to remove',value = F),
      uiOutput('scv')
    )


  })

  output$colunchsn<-renderUI({HTML(paste('choose the columns to to remain in the data frame',
                                         span(style='color:red','columns not chosen will be deleted')))
  })

  output$scv<-renderUI({
    to_scalar(x=input$clchosen)})


  outv<-reactive({
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened!='')

    whic<-DBI:::dbReadTable(con,input$opened,check.names=F)
    nms<-names(whic)
    postnl<-which(nms==input$clchosen)
    return(postnl)
  })

  removcol <- reactive({
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened!='')
    con <- DBI:::dbConnect(SQLite(), input$dbstorage)
    if(input$removclm == T){
      whic<-DBI:::dbReadTable(con,input$opened,check.names=F)
      nms<-names(whic)
      postnl<-which(nms==input$clchosen)

      DBI:::dbBegin(con)

      DBI:::dbSendStatement(con,

                           paste("CREATE TEMPORARY TABLE", paste0("my_backupdb(",to_scalar(x=input$clchosen),')')))
      DBI:::dbCommit(con)
      DBI:::dbBegin(con)
      DBI:::dbSendStatement(con, paste('INSERT INTO my_backupdb SELECT', to_scalar(x=input$clchosen), 'FROM', paste0('"',input$opened,'"')))


      DBI:::dbCommit(con)
      DBI:::dbBegin(con)


      DBI:::dbSendStatement(con,

                           paste('DROP TABLE', paste0('"',input$opened,'"')))

      DBI:::dbSendStatement(con,

                           paste("CREATE TABLE", paste0(paste0('"',input$opened,'"'),"(",to_scalar(x=input$clchosen),')')))
      DBI:::dbCommit(con)
      DBI:::dbBegin(con)
      DBI:::dbSendStatement(con,
                           paste('INSERT INTO', paste0('"',input$opened,'"'), 'SELECT',
                                 to_scalar(x=input$clchosen), 'FROM', 'my_backupdb'))
      DBI:::dbCommit(con)
      DBI:::dbBegin(con)
      DBI:::dbSendStatement(con,
                           'DROP TABLE my_backupdb')

      DBI:::dbCommit(con)   }

    else{
      if(class(mydatareactived())=='data.frame'){
        mydatareactived()}}
    data <- DBI:::dbReadTable(con, input$opened,check.names=F)
    return(data)
  })



  output$rcoltbl <- rhandsontable:::renderRHandsontable({
    input$dbstorage
    input$opened
    req(input$opened!='')

    con <- DBI:::dbConnect(SQLite(), input$dbstorage)


    X<-X()
    if(all(('Password'%in%names(mydatareactived())==T),('date'%in%names(mydatareactived())==T))){
      rhandsontable(X$data, readOnly = T)%>%
        rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
        rhandsontable::hot_col(col = 'Password',type = 'password')%>%
        rhandsontable::hot_col(col = 'date',X$data$date[which(X$data$date!='')],type = 'date')}else
          if(all(('password'%in%names(mydatareactived())==T),('date'%in%names(mydatareactived())==T))){
            rhandsontable(X$data, readOnly = T)%>%
              rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
              rhandsontable::hot_col(col = 'password',type = 'password')%>%
              rhandsontable::hot_col(col = 'date',X$data$date[which(X$data$date!='')],type = 'date')}else
                if(all(('Password'%in%names(mydatareactived())==T),('Date'%in%names(mydatareactived())==T))){
                  rhandsontable(X$data, readOnly = T)%>%
                    rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                    rhandsontable::hot_col(col = 'Password',type = 'password')%>%
                    rhandsontable::hot_col(col = 'Date',X$data$Date[which(X$data$Date!='')],type = 'date')}else
                      if(all(('password'%in%names(mydatareactived())==T),('Date'%in%names(mydatareactived())==T))){
                        rhandsontable(X$data, readOnly = T)%>%
                          rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                          rhandsontable::hot_col(col = 'password',type = 'password')%>%
                          rhandsontable::hot_col(col = 'Date',X$data$Date[which(X$data$Date!='')],type = 'date')}else
                            if('Password'%in%names(mydatareactived())==T){
                              rhandsontable(X$data, readOnly = T)%>%
                                rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                rhandsontable::hot_col(col = 'Password',type = 'password')}else
                                  if('password'%in%names(mydatareactived())==T){
                                    rhandsontable(X$data, readOnly = T)%>%
                                      rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                      rhandsontable::hot_col(col = 'password',type = 'password')}else
                                        if('date'%in%names(mydatareactived())==T){
                                          rhandsontable(X$data, readOnly = T)%>%
                                            rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                            rhandsontable::hot_col(col = 'date',X$data$date[which(X$data$date!='')],type = 'date')}else
                                              if('Date'%in%names(mydatareactived())==T){
                                                rhandsontable(X$data, readOnly = T)%>%
                                                  rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                  rhandsontable::hot_col(col = 'Date',X$data$Date[which(X$data$Date!='')],type = 'date')}else
                                                    if(all(('password '%in%names(mydatareactived())==T),('Date '%in%names(mydatareactived())==T))){
                                                      rhandsontable(X$data, readOnly = T)%>%
                                                        rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                        rhandsontable::hot_col(col = 'password ',type = 'password')%>%
                                                        rhandsontable::hot_col(col = 'Date ',X$data$date[which(X$data$date!='')],type = 'date')}else
                                                          if(all(('Password '%in%names(mydatareactived())==T),('Date '%in%names(mydatareactived())==T))){
                                                            rhandsontable(X$data, readOnly = T)%>%
                                                              rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                              rhandsontable::hot_col(col = 'Password ',type = 'password')%>%
                                                              rhandsontable::hot_col(col = 'Date ',X$data$date[which(X$data$date!='')],type = 'date')}else
                                                                if(all(('password '%in%names(mydatareactived())==T),('date '%in%names(mydatareactived())==T))){
                                                                  rhandsontable(X$data, readOnly = T)%>%
                                                                    rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                                    rhandsontable::hot_col(col = 'password ',type = 'password')%>%
                                                                    rhandsontable::hot_col(col = 'date ',X$data$Date[which(X$data$Date!='')],type = 'date')}else
                                                                      if(all(('Password '%in%names(mydatareactived())==T),('date '%in%names(mydatareactived())==T))){
                                                                        rhandsontable(X$data, readOnly = T)%>%
                                                                          rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                                          rhandsontable::hot_col(col = 'Password ',type = 'password')%>%
                                                                          rhandsontable::hot_col(col = 'date ',X$data$Date[which(X$data$Date!='')],type = 'date')}else
                                                                            if('password '%in%names(mydatareactived())==T){
                                                                              rhandsontable(X$data, readOnly = T)%>%
                                                                                rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                                                rhandsontable::hot_col(col = 'password ',type = 'password')}else
                                                                                  if('Password '%in%names(mydatareactived())==T){
                                                                                    rhandsontable(X$data, readOnly = T)%>%
                                                                                      rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                                                      rhandsontable::hot_col(col = 'Password ',type = 'password')}else
                                                                                        if('Date '%in%names(mydatareactived())==T){
                                                                                          rhandsontable(X$data, readOnly = T)%>%
                                                                                            rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                                                            rhandsontable::hot_col(col = 'Date ',X$data$date[which(X$data$date!='')],type = 'date')}else
                                                                                              if('date '%in%names(mydatareactived())==T){
                                                                                                rhandsontable(X$data, readOnly = T)%>%
                                                                                                  rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                                                                  rhandsontable::hot_col(col = 'date ',X$data$Date[which(X$data$Date!='')],type = 'date')}
    else
      rhandsontable(X$data, readOnly = T)%>%
      rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)

  })







  output$checktodel <- renderUI({
    fluidPage(shinyWidgets:::actionBttn("dlt", "DELETE ROW", style = "fil",
                                       size = "sm", color = "da"))
  })
  output$COLcondui <- renderUI({
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened!='')
    con <- DBI:::dbConnect(SQLite(), dbname = input$dbstorage)
    dl <- X()
    xl<-dl$data
    x <-xl %>% dplyr:::select(!!dplyr:::sym(colfilter()))
    selectInput("condt", "enter the column coditioned value to delete row(s)",
                choices = x)
  })
  rowremove <- reactive({
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened!='')
    con <- DBI:::dbConnect(SQLite(), dbname = input$dbstorage)
    input$dlt
    x<-colfilter()
    if (length(input$myconfirmation1 == T)!=0) {
      if (input$myconfirmation1 == T) {
        DBI:::dbBegin(con)
        rs <- DBI:::dbSendStatement(con, paste("DELETE from",
                                              paste0('"',input$opened,'"'), "WHERE",  paste0('[',x,']'), input$bool,
                                              paste0("'", input$condt, "'")))
        DBI:::dbGetRowsAffected(rs)
        DBI:::dbClearResult(rs)
        DBI:::dbCommit(con)
      }else{
        if(class(mydatareactiv())=='data.frame'){
          mydatareactiv()}}}
    else{
      if(class(mydatareactiv())=='data.frame'){
        mydatareactiv()}}
    data <- dbReadTable(con, input$opened,check.names=F)
    return(data)
  })
  colfilter<-reactive({
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened!='')
    input$dlt
    con <- DBI:::dbConnect(SQLite(), dbname = input$dbstorage)
    X<-X()
    dr <- data.frame(X$data,check.names = F)
    if(input$del%in%names(dr)==T){
      return(input$del)
    }else{
      return(
        paste(input$del,''))
    }
  })

  observeEvent(input$dlt, {
    req(length(input$initdb)!=0)
    input$dbstorage

    req(input$opened!='')
    con <- DBI:::dbConnect(SQLite(), dbname = input$dbstorage)
    X<-X()
    dr <- X$data
    shinyWidgets:::ask_confirmation(inputId = "myconfirmation1",
                                   type = "danger", text = fluidPage(selectInput("del",
                                                                                 "", "filter column to delete ", choices = names(dr)),
                                                                     selectInput("bool", "Boolean operator", choices = c("=",
                                                                                                                         ">", "<")), uiOutput("COLcondui"),br(), paste("Do you really want to delete",
                                                                                                                                                                       input$del, "from the db?")))
  })
  observeEvent(input$myonfirmation1,{
    req(length(input$initdb)!=0)
    input$dbstorage
    con <- DBI:::dbConnect(SQLite(), dbname = input$dbstorage)
    dr <- rowremove()
  })

  output$rowreduceddt <- rhandsontable:::renderRHandsontable({
    input$opened


    con <- DBI:::dbConnect(SQLite(), input$dbstorage)


    X<-X()
    if(all(('Password'%in%names(mydatareactived())==T),('date'%in%names(mydatareactived())==T))){
      rhandsontable(X$data, readOnly = T)%>%
        rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
        rhandsontable::hot_col(col = 'Password',type = 'password')%>%
        rhandsontable::hot_col(col = 'date',X$data$date[which(X$data$date!='')],type = 'date')}else
          if(all(('password'%in%names(mydatareactived())==T),('date'%in%names(mydatareactived())==T))){
            rhandsontable(X$data, readOnly = T)%>%
              rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
              rhandsontable::hot_col(col = 'password',type = 'password')%>%
              rhandsontable::hot_col(col = 'date',X$data$date[which(X$data$date!='')],type = 'date')}else
                if(all(('Password'%in%names(mydatareactived())==T),('Date'%in%names(mydatareactived())==T))){
                  rhandsontable(X$data, readOnly = T)%>%
                    rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                    rhandsontable::hot_col(col = 'Password',type = 'password')%>%
                    rhandsontable::hot_col(col = 'Date',X$data$Date[which(X$data$Date!='')],type = 'date')}else
                      if(all(('password'%in%names(mydatareactived())==T),('Date'%in%names(mydatareactived())==T))){
                        rhandsontable(X$data, readOnly = T)%>%
                          rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                          rhandsontable::hot_col(col = 'password',type = 'password')%>%
                          rhandsontable::hot_col(col = 'Date',X$data$Date[which(X$data$Date!='')],type = 'date')}else
                            if('Password'%in%names(mydatareactived())==T){
                              rhandsontable(X$data, readOnly = T)%>%
                                rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                rhandsontable::hot_col(col = 'Password',type = 'password')}else
                                  if('password'%in%names(mydatareactived())==T){
                                    rhandsontable(X$data, readOnly = T)%>%
                                      rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                      rhandsontable::hot_col(col = 'password',type = 'password')}else
                                        if('date'%in%names(mydatareactived())==T){
                                          rhandsontable(X$data, readOnly = T)%>%
                                            rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                            rhandsontable::hot_col(col = 'date',X$data$date[which(X$data$date!='')],type = 'date')}else
                                              if('Date'%in%names(mydatareactived())==T){
                                                rhandsontable(X$data, readOnly = T)%>%
                                                  rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                  rhandsontable::hot_col(col = 'Date',X$data$Date[which(X$data$Date!='')],type = 'date')}else
                                                    if(all(('password '%in%names(mydatareactived())==T),('Date '%in%names(mydatareactived())==T))){
                                                      rhandsontable(X$data, readOnly = T)%>%
                                                        rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                        rhandsontable::hot_col(col = 'password ',type = 'password')%>%
                                                        rhandsontable::hot_col(col = 'Date ',X$data$date[which(X$data$date!='')],type = 'date')}else
                                                          if(all(('Password '%in%names(mydatareactived())==T),('Date '%in%names(mydatareactived())==T))){
                                                            rhandsontable(X$data, readOnly = T)%>%
                                                              rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                              rhandsontable::hot_col(col = 'Password ',type = 'password')%>%
                                                              rhandsontable::hot_col(col = 'Date ',X$data$date[which(X$data$date!='')],type = 'date')}else
                                                                if(all(('password '%in%names(mydatareactived())==T),('date '%in%names(mydatareactived())==T))){
                                                                  rhandsontable(X$data, readOnly = T)%>%
                                                                    rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                                    rhandsontable::hot_col(col = 'password ',type = 'password')%>%
                                                                    rhandsontable::hot_col(col = 'date ',X$data$Date[which(X$data$Date!='')],type = 'date')}else
                                                                      if(all(('Password '%in%names(mydatareactived())==T),('date '%in%names(mydatareactived())==T))){
                                                                        rhandsontable(X$data, readOnly = T)%>%
                                                                          rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                                          rhandsontable::hot_col(col = 'Password ',type = 'password')%>%
                                                                          rhandsontable::hot_col(col = 'date ',X$data$Date[which(X$data$Date!='')],type = 'date')}else
                                                                            if('password '%in%names(mydatareactived())==T){
                                                                              rhandsontable(X$data, readOnly = T)%>%
                                                                                rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                                                rhandsontable::hot_col(col = 'password ',type = 'password')}else
                                                                                  if('Password '%in%names(mydatareactived())==T){
                                                                                    rhandsontable(X$data, readOnly = T)%>%
                                                                                      rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                                                      rhandsontable::hot_col(col = 'Password ',type = 'password')}else
                                                                                        if('Date '%in%names(mydatareactived())==T){
                                                                                          rhandsontable(X$data, readOnly = T)%>%
                                                                                            rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                                                            rhandsontable::hot_col(col = 'Date ',X$data$date[which(X$data$date!='')],type = 'date')}else
                                                                                              if('date '%in%names(mydatareactived())==T){
                                                                                                rhandsontable(X$data, readOnly = T)%>%
                                                                                                  rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)%>%
                                                                                                  rhandsontable::hot_col(col = 'date ',X$data$Date[which(X$data$Date!='')],type = 'date')}
    else
      rhandsontable(X$data, readOnly = T)%>%
      rhandsontable::hot_cols(columnSorting = TRUE,manualColumnMove = T,manualColumnResize = T)

  })


  observe({
    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(length(input$confirm) != 0)
    req(input$confirm == T)
    updateTextInput(session,"addcol", placeholder = "type the column name you would like to add",
                    label = "")
    updateCheckboxInput(session,"confirm", value = F,
                        "commit")
  })

  observe({
    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(length(input$commitb) != 0)
    req(input$commitb == T)

    updateCheckboxInput(session,"commitb", value = F,
                        "commit")
  })

  observe({
    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(length(input$commitchanges) != 0)
    req(input$commitchanges == T)
    updateCheckboxInput(session,"commitchanges",
                        label = "commit", value = F)
  })




  observe({
    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(length(input$makechanges) != 0)
    req(input$makechanges == T)
    updateTextInput(session,"newname", placeholder = "type the new name here",
                    "")
    updateCheckboxInput(session,"makechanges", "commit",
                        value = F)
  })

  observe({
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(length(input$removclm) != 0)
    req(input$removclm == T)
    updateCheckboxInput(session,'removclm','check to confirm the column to remove',value = F)
  })
  observe({
    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(length(input$commit == T) != 0)
    req(input$commit == T)

    updateCheckboxInput(session,"commit", label = "commit",
                        value = F)
  })
  observe({
    input$loginok
    req(length(input$initdb)!=0)
    input$dbstorage
    req(input$opened)
    req(length(input$commitn == T) != 0)
    req(input$commitn == T)

    updateCheckboxInput(session,"commitn", label = "commit",
                        value = F)
  })

  onStop(function(){ dbDisconnect(con)},session = session)
  observeEvent(input$logout,{ shinyWidgets:::ask_confirmation(
    inputId = 'confirmexit',
    type = 'danger',
    text = fluidPage('Do you want to exit the app?',
                     checkboxInput('svtru',label = 'save unsaved changes onExit',value = T)
    )
  )})

  observe({req(input$confirmexit==T)
    refresh()
  })

}
#' Use your opened data frame from shinySQLite
#'
#' You can use shinySQLite data frame in your functions by typing useactivedata(input)
#'
#' You can use useactivedata(input) as your data.frame in functions such as renderPlot, renderPrint ... etc.
#'
#' @examples library(shiny)
#' library(RSQLite)
#' library(DT)
#' library(shinySQLite)
#' ui <- dataentryui(title = 'Biashara Group LTD',
#'           analysistab = bs4Dash:::tabItem(id = 'myAnalysis', tableOutput(dataframe)))
#'
#' server<-function(input,output,session){
#'    dataentryserver(input = input,output = output,session = session)
#'    output$table <- renderTable({
#'       data <- useactivedata(input = input, session = session)
#'       data
#'    })
#' }
#' shinyApp(ui, server)
#'
#' @param input Shiny input object passed from the server.
#'        See \link{https://github.com/nasilabednego/shinySQLite} for more information.
#' @export

useactivedata<-function(
    input,session){
  input$loginok

  input$dbstorage
  input$opened
  input$editintb
  input$makechanges
  input$commitchanges
  con <- DBI:::dbConnect(SQLite(), dbname = input$dbstorage)
  input$commitb
  input$editintb
  input$makechanges
  input$commitchanges
  DBI:::dbReadTable(con, input$opened,check.names=F)

}


#' The ui function of shinySQLite
#'
#' This is function a ui function that displays your shinySQLite databases, data frames and dynamic input controls.
#'
#' This function is used together with it's server companion,  dataentryserver(input, output, session)
#'
#' @examples library(shiny)
#' library(RSQLite)
#' library(DT)
#' library(shinySQLite)
#' ui <- dataentryui(title = 'Kazi database')
#' server<-function(input,output,session){
#'    dataentryserver(input = input,output = output,session = session)
#' }
#' shinyApp(ui, server)
#'
#' @param title Add title to your ui.
#' @param analysistab Add your analysis bs4Dash:::tabItem to existing shinySQLite bs4Dash:::tabItems. This enable you to apply active shinySQLite tables to your data analysis. e.g in server.R you can output$table <- renderTable({useactivedata(input, session)}) and read the table in your analysis bs4Dash:::tabItem e.g tableOutput(dataframe)
#'        See \link{https://github.com/nasilabednego/shinySQLite} for more information.
#' @export


dataentryui<-function (title,analysistab){

  if(missing(analysistab)==T && missing(title)==F){


    dashboardPage(
      header = dashboardHeader(
        title = tags$li(tags$b(span(style="color:orange",title)))
      ),
      dashboardSidebar(disable = T),
      body = bs4Dash:::dashboardBody(
        HTML('<meta name="viewport" content="width=device-width, initial-scale=0.5, user-scalable=yes, maximum-scale=0.6, shrink-to-fit=no">'),
        shinyjs:::useShinyjs(),shinyWidgets:::setBackgroundColor(color = c("#3B414C","#B0BBCD","#8F95A0","#E6EFFF","#3B414C"),direction = 'l',gradient = 'r'),


        bs4Dash:::tabItems(
          bs4Dash:::tabItem(tabName = 'hom',

                  shiny:::tabsetPanel(id='href.link',
                                     tabPanel(p(id='dbes',tagList(icon=icon('file'),
                                                                  title = "Files")),
                                              fluidRow(column(6,
                                                              uiOutput('dbcreatdbui')),
                                                       column(6,
                                                              uiOutput('uidb'))
                                              )




                                     ),
                                     tabPanel(p(id='dfrem',tagList(icon = icon('pencil-alt'),'edit_Table')),
                                              fluidRow(bs4Dash:::box(width = 4,title = tagList(span(style='color:olive',icon('wrench',style='font-size:150%')),span(style='color:black','# controls & inputs')),
                                                                    div(uiOutput('editoption'),class="bg-gray-light")),
                                                       bs4Dash:::box(width = 8,title=tagList(span(style='color:red',icon('pencil-ellipsis',style='font-size:150%')),span(style='color:black','# edit & outputs')),
                                                                    uiOutput('uitbls')
                                                       ))))),

          bs4Dash:::tabItem(tabName='set'
          )
        )

      ),
      title = title,
      controlbar = dashboardControlbar(
        collapsed = T,
        width = '69px',
        overlay = F,
        pinned = F,


        sidebarMenu(
          tooltip(menuItem(NULL,tabName = 'hom',icon = icon('house')),title='home'),

          tooltip(menuItem(NULL,tabName = 'set',icon = icon('cog')),title='settings')

        )

      ))


  }else

    if(all((missing(title)==T),(missing(analysistab)==T))==T){



    }else
      if(missing(title)==T){
        library(sourcetools)

        mid<-substr(analysistab,69,86)
        mId= sourcetools:::tokenize_string(mid)
        mtab<-mId$value[1]

        bs4Dash:::dashboardPage(
          header = bs4Dash:::dashboardHeader(

            status = "dark",
            sidebarIcon = span(icon("th"),style='color:white'),
            controlbarIcon = span(icon("th"),style='color:white'),
            title = span(style="color:white",h4('SQLte')),
            tags$li(span(style="color:white",'shinySQLite'))
          ),
          bs4Dash:::dashboardSidebar(
            disable = F,width = '65px',status = 'orange',collapsed = F,
            actionLink('moretips',tagList(icon('microscope',style='font-size:150%'),span(style='color:white','daily tips'))),
            uiOutput('mandb'),
            br(),uiOutput('nBtn'),
            uiOutput('logoutBtn'),
            uiOutput('tvl'),
            uiOutput('lvlu'),
            uiOutput('printcd')
          ),
          body = bs4Dash:::dashboardBody(HTML('<meta name="viewport" content="width=device-width, initial-scale=0.5, user-scalable=yes, maximum-scale=0.6, shrink-to-fit=no">'),
                                 shinyjs:::useShinyjs(),

                                 shinyWidgets:::setBackgroundColor(color = c("#C8DCFF","#B0BBCD","#C8DCFF","#B0BBCD"),
                                                    direction = 'l',gradient = 'r'),

                                 fresh:::use_theme(fresh:::create_theme(
                                   theme = "default",
                                   bs_vars_global(
                                     body_bg = "#FFF",
                                     text_color = "brown"
                                   ),
                                   bs4dash_status(
                                     primary = "#D25E36",
                                     secondary = "#01DF3A"
                                   ),
                                   bs4dash_yiq(
                                     contrasted_threshold = 180,
                                     text_dark = "#000",
                                     text_light = "#D25E36"
                                     #dc3545"
                                   ),
                                   bs_vars_wells(
                                     bg = "#D25E36"
                                     #2E2E2E"
                                   )
                                 )),
                                 use_googlefont("Saira Stencil One"),

                                 uiOutput('enc'),

                                 bs4Dash:::tabItems(
                                   bs4Dash:::tabItem(tabName = 'hom',
                                           uiOutput('page')),

                                   analysistab,
                                   bs4Dash:::tabItem(tabName = 'use'),
                                   bs4Dash:::tabItem(tabName='lrn'
                                   ),
                                   bs4Dash:::tabItem(tabName='set'
                                   ),
                                   bs4Dash:::tabItem(tabName='sup'
                                   ),
                                   bs4Dash:::tabItem(tabName='clk',
                                           uiOutput('codeoutput')
                                   )
                                 )

          ),
          footer = bs4Dash:::dashboardFooter(fixed = T,miniUI:::miniTitleBar(fluidRow(column(4),tags$ul(uiOutput('itmft'))))),
          controlbar = dashboardControlbar(

            pinned = substr(p(id='tf',T),12,15),
            collapsed = T,
            width = '65px',
            overlay = F,


            fluidPage(theme = bs_add_variables(bs_theme(4,bootswatch = "lum", primary = "orange"),
                                               "body-bg" = "#EEEEEE",
                                               "font-family-base" = "monospace",
                                               "font-size-base" = "1.4rem",
                                               "btn-padding-y" = ".16rem",
                                               "btn-padding-x" = "2rem"
            )),

            sidebarMenu(id='maintbs',
                        tooltip(menuItem(icon('home',style='font-size:150%'),tabName = 'hom'),title='home'),
                        tooltip(menuItem(icon('chart-bar',style='font-size:150%'),tabName = mtab),title='data analysis'),
                        tooltip(menuItem(icon('calculator',style='font-size:150%'),tabName = 'clk'),title='calculator'),
                        tooltip(menuItem(icon('book',style='font-size:150%'),tabName = 'lrn'),title='learn R programming'),
                        tooltip(menuItem(icon('magic',style='font-size:150%'),tabName = 'use'),title = 'usage'),
                        tooltip(menuItem(icon('gear',style='font-size:150%'),tabName = 'set'),title='settings'),
                        tooltip(menuItem(icon('heart',style='font-size:150%'),tabName = 'sup'),title='support')

            ),


            title = 'shinySQLite',
          )

        )
      }

  else
    if(all((missing(title)==F),(missing(analysistab)==F))==T){

      library(sourcetools)
      mid<-substr(analysistab,69,86)
      mId=sourcetools:::tokenize_string(mid)
      mtab<-mId$value[1]

      dashboardPage(
        header = dashboardHeader(
          title = tags$li(tags$b(span(style="color:orange",title)))
        ),
        dashboardSidebar(disable = T),
        body = bs4Dash:::dashboardBody(HTML('<meta name="viewport" content="width=device-width, initial-scale=0.5, user-scalable=yes, maximum-scale=0.6, shrink-to-fit=no">'),
                             shinyjs:::useShinyjs(),


                             bs4Dash:::tabItems(
                               bs4Dash:::tabItem(tabName = 'hom',

                                       shiny:::tabsetPanel(id='href.link',
                                                          tabPanel(p(id='dbes',tagList(icon=icon('file'),
                                                                                       title = "Files")),
                                                                   fluidRow(column(6,
                                                                                   uiOutput('dbcreatdbui')),
                                                                            column(6,
                                                                                   uiOutput('uidb'))
                                                                   )




                                                          ),
                                                          tabPanel(p(id='dfrem',tagList(icon = icon('pencil-alt'),'edit_Table')),
                                                                   fluidRow( bs4Dash:::box(width = 4,title = tagList(span(style='color:olive',icon('gear')),span(style='color:black','# controls & inputs')),
                                                                                          div(uiOutput('editoption'),class="bg-gray-light")),
                                                                             bs4Dash:::box(width = 8,title=tagList(span(style='color:red',icon('table',style='font-size:150%')),span(style='color:black','# edit and save outputs')),
                                                                                          uiOutput('uitbls')
                                                                             ))))),
                               analysistab,
                               bs4Dash:::tabItem(tabName='set'
                               )
                             )

        ),
        title = title,
        controlbar = dashboardControlbar(
          pinned = F,
          collapsed = T,
          width = '69px',
          overlay = F,



          sidebarMenu(
            tooltip(menuItem(NULL,tabName = 'hom',icon = icon('house')),title='home'),
            tooltip(menuItem(NULL,tabName = mtab,icon = icon('chart-bar')),title='data analysis'),
            tooltip(menuItem(NULL,tabName = 'set',icon = icon('cog')),title='settings')

          )

        ))

    }
}











#' Run example shinySQLite app
#'
#' Run example shinySQLite app
#'
#' Quickly launch example app
#'
#' @examples runExample(example = "app.R")
#'

#'        See \link{https://github.com/nasilabednego/shinySQLite} for more information.
#' @export

runExample <- function(example) {
  # locate all the shiny app examples that exist
  validExamples <- list.files(system.file("shiny-examples", package = "shinySQLite"))

  validExamplesMsg <-
    paste0(
      "Valid examples are: '",
      paste(validExamples, collapse = "', '"),
      "'")

  # if an invalid example is given, throw an error
  if (missing(example) || !nzchar(example) ||
      !example %in% validExamples) {
    stop(
      'Please run `runExample()` with a valid example app as an argument.\n',
      validExamplesMsg,
      call. = FALSE)
  }

  # find and launch the app
  appDir <- system.file("shiny-examples", example, package = "shinySQLite")
  shiny:::runApp(appDir, display.mode = "normal")
}
#' to_scalar
#'
#' Stringify vectors or elements of length greater than 1
#'
#' to_scalar(x = c(1,2,3))
#'
#' to_scalar(x = names(mtcars))
#'
#' @export


to_scalar <-function(x){
  if(length(x)>1){
    xs=paste0(data.frame(x,check.names = F))

    scalarOutput <- substr(xs,3,nchar(xs)-1)

    return(scalarOutput)}else
    {return(x)}
}


