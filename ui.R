## ui.app.R ##
library(shiny)
library(shinydashboard)
library(rhandsontable)

load("translation.bin") # contains the dictionary, parsed as a double list

ui <- dashboardPage(skin = "yellow",
  # tags$head(includeScript("google-analytics.js")),
  ## cadastrar o endereço web na conta do google analytics,
  ## e salvar a chave de tracking em arquivo com nome google-analytics.js
  ## na pasta do projeto e sem '<script></script>'
  
  dashboardHeader(title = "Meta Analyzer"),
  dashboardSidebar(

    absolutePanel(width="230px", fixed=TRUE,
      sidebarMenu( id = "menu",
        menuItem("App", tabName = "app", icon = icon("home")),
        #menuItem("Configurações Avançadas", tabName = "config", icon = icon("dashboard")),
        #menuItem("About", tabName = "about", icon = icon("glyphicon glyphicon-info-sign", lib= "glyphicon")),
        #menuItem("Source code", icon = icon("file-code-o"),
                 #href = "https://github.com")
        conditionalPanel(

          condition = "input.menu == 'app'",
          below = "about",
          fluidRow(
            column(
              width = 12,
              uiOutput("test"),
              radioButtons(inputId = "language", label="", choices = c("English" = "en", "Português" = "pt"), selected="en")
  
            )#end column
          )#end fluidrow
        )#end conditional
      )#end sidebarMenu
    )#end absolutPanel
  ),#end sidebar
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(
        tabName = "app",
        uiOutput("desc"),
        uiOutput("painel"),
        uiOutput("results"),
        #conditionalPanel("input.plot == true",
          #uiOutput("results")
        #),
        fluidRow(
        )
      )
      
    # tab about
    #tabItem(tabName = "about",
    #        h2("Projeto: Meta analyzer"),
    #        HTML("<p>Desenvolvedor: Ricardo Manhães Savii <a href = 'http://lattes.cnpq.br/7614391299549728'>(lattes)</a></p>"),
    #        HTML("<p>Orientadora: Profa Dra. Camila Bertini Martins <a href = 'http://lattes.cnpq.br/3770708843269785'>(lattes)</a></p>")
    #)
  )
))
