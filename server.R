## server.app.R ##

library(ggplot2)
library(grid)
library(shiny)
library(rhandsontable)
library(meta)

source('data_examples_app.R', local=TRUE)

server <- function(input, output) {
  
  # definições feitas pelo usuário na aplicação
  header <- reactive({
    input$header
  })
  sep <- reactive({
    input$sep
  })
  quote <- reactive({
    input$quote
  })
  alpha <- reactive({
    input$alpha
  })
  
  # values será o espelho da rHandsonTable no programa
  values <- reactiveValues(data = df_prop)
  
  # permite a edição direto na tabela rHandsonTable
  observe ({
    if(!is.null(input$hot))
      values$data <- hot_to_r(input$hot)
  })
  
  # permite a importação à partir de um arquivo
  observeEvent (input$botao_arquivo, {
    inFile <- input$arquivo
    if (is.null(inFile))
      return(NULL)
    values$data <- read.table(inFile$datapath, header = header(), sep = sep(), quote = quote())
  })
  
  # permite a importação de um data.frame previamente criado
  observeEvent (input$escolher_modelo, {
    values$data <- get(input$modelo)
  })
  
  # dados é a variável final que será levada até o plot
  dados <- reactiveValues(data = df_prop)
  
  # dados só é atualizado quando botão plot (Submeter) é apertado
  observeEvent(input$plot, {
    dados$data <- values$data
  })
  
  # modelo é a variável que manterá atualizado o modelo escolhido
  modelo <- reactiveValues(data = "NULL")
  
  # define o modelo a ser analisado
  observeEvent(input$plot, {
    modelo$data <- input$modelo
  })
  
  # Representa/renderiza a rHandsonTable
  output$hot <- renderRHandsontable({
    rhandsontable(values$data, rowHeaders = NULL) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE, allowRowEdit=TRUE) %>%
      hot_cols(columnSorting = TRUE, allowInvalid = TRUE)
  })
  
  # Permite realizar o download da tabela visualizada
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste('data-', Sys.Date(), '.csv', sep='') 
    },
    content = function(file) {
      write.csv(values$data, file, row.names = FALSE)
    }
  )

  ################################################
  # Cálculo do objeto meta
  ################################################
  meta <- reactive({
    source('define_meta_app.R', local=TRUE)
    if (modelo$data %in% names(compute_meta)) {
      meta <- do.call(compute_meta[[modelo$data]], arg_meta[[modelo$data]])
    } else {
      NULL
    }
  })
    
  ################################################
  # Renderização do forest plot
  ################################################
  plotForest <- function(){
    meta <- meta()
     if (!is.null(meta)) {
       if (1-pchisq(meta$Q, meta$df.Q) > alpha()) { # if p-value > alpha só apresenta fixed effect model
         forest(meta, studlab = paste(dados$data$Estudos),
                comb.random=FALSE, comb.fixed=TRUE)
       } else {
         forest(meta, studlab = paste(dados$data$Estudos),
                comb.random=TRUE, comb.fixed=FALSE)
       }
     } else {
       frame()
     }
  }
  output$forest <- renderPlot ({
    plotForest()
  })
  # Permite realizar o download da figura Forest
  output$downloadForest <- downloadHandler(
    filename <- function() {
      paste('forest_', Sys.Date(), '.pdf', sep='')
    },
    content <- function(FILE=NULL) {
      pdf(file=FILE)
      plotForest()
      dev.off()
    }
  )

  ################################################
  # Renderização do funnel plot
  ################################################
  plotFunnel <- function(){
    meta <- meta()
     if (!is.null(meta)) {
       if (1-pchisq(meta$Q, meta$df.Q) > alpha()) { # if p-value > alpha só apresenta fixed effect model
         funnel.meta(meta, studlab = paste(dados$data$Estudos),
                comb.random=FALSE, comb.fixed=TRUE)
       } else {
         funnel.meta(meta, studlab = paste(dados$data$Estudos),
                comb.random=TRUE, comb.fixed=FALSE)
       }
     } else {
       frame()
     }
  }
  output$funnel <- renderPlot ({
    plotFunnel()
  })
  # Permite realizar o download da figura Forest
  output$downloadFunnel <- downloadHandler(
    filename <- function() {
      paste('funnel_', Sys.Date(), '.pdf', sep='')
    },
    content <- function(FILE=NULL) {
      pdf(file=FILE)
      plotForest()
      dev.off()
    }
  )
  
  ################################################
  # Teste de assimetria egger
  ################################################
  output$eggerTest <- renderPrint ({
    meta <- meta()
     if (!is.null(meta)) {
       return(list(metabias(meta, k.min=3),
                   cor.test()
                   ))
     } else {
       frame()
     }
  })
  
  # ################################################
  # # Teste de assimetria begg
  # ################################################
  # output$beggTest <- renderPrint ({
  #    if (!is.null(meta())) {
  #      return(metabias(meta, k.min=3))
  #    } else {
  #      frame()
  #    }
  # })
}