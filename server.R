## server.app.R ##

library(ggplot2)
library(grid)
library(shiny)
library(rhandsontable)
library(meta)

source('data_examples_app.R', local=TRUE)

server <- function(input, output) {
  
  #Abas sendo criadas
  output$desc <- renderUI({
    if(input$escolher_modelo){
      box(title = "Meta-analise", width = 12, solidHeader = TRUE, status = "primary",
          "Selecione o tipo de entrada para construir a sua analise."
      )
    }
  })
  
  output$painel <- renderUI({
    if(input$escolher_modelo){
      tabBox(
        title = "Entrada e Config",
        id = "ttabs",
        width = 12,
        tabPanel("Manual Input",
                 wellPanel(
                   rHandsontableOutput("hot"),
                   tags$hr(),
                   tags$head(
                     tags$style(HTML('#plot{background-color:orange}'))
                   ),
                   actionButton("plot", "Gerar resultados", width = "150px"),
                   downloadButton('downloadData', 'Salvar Tabela')
                 )
        ),
        tabPanel("Import",
                 fluidRow(
                   column(
                     4,
                     checkboxInput(inputId = "header", label = "Cabeçalho", TRUE),
                     radioButtons(inputId = "sep", label = "Separador",
                                  choices = c('virgula'=',', 'ponto e virgula'=';','tabulação'='\t'), selected = ","),
                     radioButtons(inputId = "quote", label = "Citação",
                                  choices = c('sem'='', 'aspas duplas'='"','aspas simples'="'"),selected = '"')
                   ),
                   column(
                     8,
                     fileInput("arquivo", "Escolher arquivo .csv ou .txt",
                               accept=c('text/csv','text/comma-separated-values',
                                        'text/tab-separated-values','text/plain','.csv','.tsv','.txt')),
                     actionButton("botao_arquivo", "Importar arquivo"),
                     tags$hr(),
                     tags$p("Escreva suas entradas em um arquivo .csv ou .txt."),
                     tags$p("Cada linha de seu arquivo será considerada como uma observação multivariada.")
                   )
                   
                 )#endfluidrow
                 
        ),#endtabpanel
        tabPanel("Configuração Avançada",
                 fluidRow(
                   column(6,
                          conditionalPanel(
                            condition = "input.modelo == 'df_prop'",
                            p(h3('Configurações para modelo de proporção:')),
                            selectInput(inputId = 'smprop',
                                        label = 'Medidas do modelo de proporção',
                                        choices = c('Logit'='PLOGIT',
                                                    'Log'='PLN',
                                                    'Freeman-Tukey Double arcsine'='PFT',
                                                    'Arcsine' = 'PAS',
                                                    'Raw, i.e. untransformed, proportions'='PRAW'),
                                        selected = "PLOGIT",
                                        width = 300,
                                        multiple = FALSE
                            ),
                            p(h6('A transformação Logit é utilizado como padrão no pacote metafor.')),
                            
                            selectInput(inputId = 'ciprop',
                                        label = 'Método de cálculo do intervalo de confiança',
                                        choices = c('exact binomial (Clopper-Pearson)'='CP',
                                                    'Wilson Score'='WS',
                                                    'Wilson Score interval with continuity correction'='WSCC',
                                                    'Agresti-Coull' = 'AC',
                                                    'Simple approximation'='SA',
                                                    'Simple approximation interval with continuity correction' = 'SACC',
                                                    'Normal approximation interval based on summary measure, i.e. defined by argument sm' = 'NAsm'),
                                        selected = "CP",
                                        width = 300,
                                        multiple = FALSE
                            ),
                            p(h6('O método Clopper-Pearson é utilizado como padrão no pacote metafor.'))  
                          ),
                          
                          conditionalPanel( # diferenca de medias
                            condition = "input.modelo == 'df_med1'",
                            p(h3('Configurações para modelo de diferença entre médias:')),
                            selectInput(inputId = 'smmean',
                                        label = 'Medidas do modelo de diferença entre médias',
                                        choices = c('mean difference'='MD',
                                                    'standardised mean difference'='SMD'),
                                        selected = "MD",
                                        width = 300,
                                        multiple = FALSE
                            ),
                            p(h6('Não há medida utilizada como padrão no pacote metafor.'))
                          ),
                          
                          conditionalPanel( # diferenca de media padronizada
                            condition = "input.modelo == 'df_medp'",
                            p(h3('Configurações para modelo de diferença na média padronizada:')),
                            selectInput(inputId = 'smmean',
                                        label = 'Medidas do modelo de diferença entre médias',
                                        choices = c('mean difference'='MD',
                                                    'standardised mean difference'='SMD'),
                                        selected = "MD",
                                        width = 300,
                                        multiple = FALSE
                            ),
                            p(h6('Não há medida utilizada como padrão no pacote metafor.')),
                            selectInput(inputId = 'smdmean',
                                        label = 'Método de cálculo da diferença na média padronizada',
                                        choices = c('Hedges\' g (1981)'='Hedges',
                                                    'Cohen\'s d (1988)'='Cohen',
                                                    'Glass\' delta (1976)'='Glass'),
                                        selected = "Hedges",
                                        width = 300,
                                        multiple = FALSE
                            ),
                            p(h6('O método de Hedges é utilizado como padrão no pacote metafor.'))
                          ),
                          
                          conditionalPanel( # Correlacao
                            condition = "input.modelo == 'df_corr'",
                            p(h3('Configurações para modelo de correlação:')),
                            selectInput(inputId = 'smcor',
                                        label = 'Medidas do modelo de correlação',
                                        choices = c('Raw correlation coefficient'='COR',
                                                    'Fisher\'s z transformation of correlations'='ZCOR'),
                                        selected = "ZCOR",
                                        width = 300,
                                        multiple = FALSE
                            ),
                            p(h6('A medida de Fisher é utilizado como padrão no pacote metafor.'))
                          ),
                          
                          conditionalPanel( # Respostas dicotomicas
                            condition = "input.modelo == 'df_dich'",
                            p(h3('Configurações para modelo dicotômico:')),
                            selectInput("dichotomousoptions", strong("Seleção de medida"),
                                        c("log relative risk" = "RR",
                                          "log odds ratio" = "OR",
                                          "risk difference" = "RD",
                                          "arcsine square-root transformed risk difference (Rücker et al., 2009)." = "AS",
                                          "log odds ratio estimated with Peto’s method (Yusuf et al., 1985)." = "PETO",
                                          "probit transformed risk difference as an estimate of the standardized mean difference." = "PBIT",
                                          "transformed odds ratio as an estimate of the standardized mean difference (normal distributions)." = "OR2DN",
                                          "transformed odds ratio as an estimate of the standardized mean difference (logistic distributions)." = "OR2DL"
                                        ), selected = "OR"),
                            p(h6('logs odds ratio is the default option and is the one you should use for the example provided in the Input Examples tab.'))
                          )
                   ),
                   column(6,
                          p(h3('Configurações Efeito Randômico:')),
                          selectInput(inputId = 'measure',
                                      label = 'Estimador do Modelo de Efeito Randômico',
                                      choices = c('DerSimonian-Laird estimate (1986)' = 'DL',
                                                  'Restricted maximum-likelihood'='REML',
                                                  'Maximum-likelihood'='ML',
                                                  'Hunter_Schmidt'='HS',
                                                  'Sidik-Jonkman'='SJ',
                                                  'Hedges'='HE',
                                                  'Empirical Bayes'='EB',
                                                  'Paule-Mandel method (1982)'='PM'),
                                      selected = "DL",
                                      width = 300,
                                      multiple = FALSE
                          ),
                          p(h6('DerSimonian-Laird é o estimador padrão do pacote metafor.')),
                          
                          checkboxInput("khadjust", label = "Knapp & Hartung Adjustment", value = FALSE),
                          p(h6('O ajuste Knapp & Hartung como padrão não é utilizado no pacote metafor.'))
                   )
                 )
        )
      )#endtabbox
    }
  })
  output$results <- renderUI({
    tabBox(
      title = "Resultados",
      id = "tresults",
      width = 12,
      tabPanel("Forest Plot",
               wellPanel(
                 plotOutput("forest", height = 400, width = 800),
                 downloadButton('downloadForest', 'Save forest as pdf')
               )
      ),
      tabPanel("Funnel Plot",
               wellPanel(
                 plotOutput("funnel", height = 400, width = 600),
                 downloadButton('downloadFunnel', 'Salvar funnel como pdf')
               )
      )
      # tabPanel("Teste de assimetria",
      #          wellPanel(
      #            verbatimTextOutput("beggTest")
      #          )
      # )
      # tabPanel("Boot Density",
      #          wellPanel(
      #            plotOutput("dens"),
      #            downloadButton('downloadDensity', 'Save plot as pdf')
      #          )
      # )
    )
  })
  
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
  
  # ################################################
  # # Teste de assimetria egger
  # ################################################
  # output$eggerTest <- renderPrint ({
  #   meta <- meta()
  #    if (!is.null(meta)) {
  #      return(list(metabias(meta, k.min=3),
  #                  cor.test()
  #                  ))
  #    } else {
  #      frame()
  #    }
  # })
  
  ################################################
  # Teste de assimetria begg
  ################################################
  output$beggTest <- renderPrint ({
     if (!is.null(meta())) {
       return(metabias(meta(), k.min=3))
     } else {
       frame()
     }
  })
}