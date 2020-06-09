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
  dashboardHeader(
    title = "Meta Analyzer"
  ),
  
  dashboardSidebar(

    absolutePanel(
      width="230px",
      fixed=TRUE,

      sidebarMenu(
        id="menu",
        radioButtons(
          inputId="language",
          label="",
          choices=c("English"="en", "Português"="pt"),
          selected="en"
        ),

        menuItem(
          "App", 
          tabName="app", 
          icon=icon("home")
        ),
  
        fluidRow(
          column(
            width = 12,
            uiOutput("test"),
          )
        ),

        menuItem(
          "Source code",
          icon = icon("file-code-o"),
          href = "https://github.com/ricoms/shiny_meta_analyzer"
        )
      )
    )
  ),
  
  dashboardBody(
    tags$head(includeCSS('www/style.css')),
    tabItems(
      # First tab content
      tabItem(
        tabName="app",
        uiOutput("desc"),
        uiOutput("painel"),
        uiOutput("results"),
        fluidRow()
      )
    )
  )
)
