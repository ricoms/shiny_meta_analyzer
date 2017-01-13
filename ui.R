## ui.app.R ##
library(shiny)
library(shinydashboard)
library(rhandsontable)

ui <- dashboardPage(skin = "yellow",
  # tags$head(includeScript("google-analytics.js")),
  ## cadastrar o endereço web na conta do google analytics,
  ## e salvar a chave de tracking em arquivo com nome google-analytics.js
  ## na pasta do projeto e sem '<script></script>'
  
  dashboardHeader(title = "Metadata analyzer"),
  dashboardSidebar(
    sidebarMenu( id = "menu",
      menuItem("App", tabName = "app", icon = icon("home")),
      #menuItem("Configurações Avançadas", tabName = "config", icon = icon("dashboard")),
      menuItem("About", tabName = "about", icon = icon("glyphicon glyphicon-info-sign", lib= "glyphicon")),
      #menuItem("Source code", icon = icon("file-code-o"),
               #href = "https://github.com")
      
      conditionalPanel(
        condition = "input.menu == 'app'",
        below = "about",
        fluidRow(
          column(
            width = 12,
            box(
              title = "Selecione",
              width = 12,
              background = "orange",
              "Escolha os valores abaixo."
            ),
            selectInput(inputId = 'modelo',
                        label = 'Medidas de efeito',
                        choices = c('Proporção'='df_prop',
                                    'Diferença de médias'='df_medp',
                                    'Correlação'='df_corr',
                                    'Resposta dicotômica'='df_dich'
                        ),
                        width = 200,
                        selected = NULL,
                        multiple = FALSE
            ),
            numericInput(inputId = "alpha", label = "Nível de significância",
                         value = 0.05, min = 0.00, max = 1,
                         width = 200, step = 0.01),
            box(width = 9,
              actionButton("escolher_modelo", "Definir modelo"),
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
      ),
      
    # tab about
    tabItem(tabName = "about",
            h2("Projeto: Meta analyzer"),
            p("Orientadora: Prof. Dra. Camila Bertini Martins"),
            p("Alunos: Alexandre Hild Aono (92169) & Ricardo Manhães Savii (92482)")
    )
  )
))