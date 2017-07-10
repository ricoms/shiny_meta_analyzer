## ui.app.R ##
library(shiny)
library(shinydashboard)
library(rhandsontable)

ui <- dashboardPage(skin = "yellow",
  # tags$head(includeScript("google-analytics.js")),
  ## cadastrar o endereço web na conta do google analytics,
  ## e salvar a chave de tracking em arquivo com nome google-analytics.js
  ## na pasta do projeto e sem '<script></script>'
  
  dashboardHeader(title = "Meta Analyzer"),
  dashboardSidebar(
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
            box(
              title = "Select below to begin", width = 12, background = "orange",
              selectInput(inputId = 'modelo',
                          label = 'Effect Size',
                          choices = c('Proportion'='df_prop',
                                      'Mean Differences'='df_medp',
                                      'Correlation'='df_corr',
                                      'Dichotomous Models'='df_dich'
                          ),
                          width = 200,
                          selected = NULL,
                          multiple = FALSE
              ),
              numericInput(inputId = "alpha", label = "Level of significance",
                           value = 0.05, min = 0.00, max = 1,
                           width = 200, step = 0.01
              )
            ),
            
            box(width = 10,
              actionButton("escolher_modelo", "Define model"),
              background = "orange"
            )

          )#end column
        )#end fluidrow
      )#end conditional
    )#end sidebarMenu
  ),#end sidebar
  
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(
        tabName = "app",
        uiOutput("desc"),
        uiOutput("painel"),
        conditionalPanel("input.plot == true",
          uiOutput("results")
        ),
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
