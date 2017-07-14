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
      box(title = "About", width = 12, solidHeader = TRUE, status = "primary",
          HTML("<h3>Authors</h3>
                <p>Ricardo Manhães Savii (<a href = 'http://lattes.cnpq.br/7614391299549728'>lattes</a>),</p>
                <p>Alexandre Hild Aono (<a href = 'http://lattes.cnpq.br/5745062922235619'>lattes</a>), </p>
                <p>Profa Dra. Camila Bertini Martins (<a href = 'http://lattes.cnpq.br/3770708843269785'>lattes</a>)</p><br>"),
          HTML("<p>Select below the type of input to build your analysis, for any doubt or problem, please, reach us at</p>"),
          HTML("<p><a href = 'https://groups.google.com/d/forum/meta-analyzer-unifesp'>meta-analyzer-unifesp.groups.google</a></p>")
          )
  })
  
  output$painel <- renderUI({
    if(input$escolher_modelo){
      tabBox(
        title = "Input and Configuration", id = "ttabs", width = 12,
        
        tabPanel("Manual Input",
                 fluidRow(
                      column(6,
                        rHandsontableOutput("hot")
                      ),
                      column(6,
                        HTML("<h4>Standard headers</h4>"),
                        HTML("<p><b>Studies</b>: identification of different studies to be compared.</p>"),
                        # prop
                        conditionalPanel(
                          condition = "input.modelo == 'df_prop'",
                          HTML("<p><b>events</b>: number of positive events.</p>"),
                          HTML("<p><b>n</b>: the total number of cases included.</p>")
                        ),
                        # medp
                        conditionalPanel(
                          condition = "input.modelo == 'df_medp'",
                          HTML("<p><b>n</b>: the total number of cases included.</p>"),
                          HTML("<p><b>mean</b>: mean of group.</p>"),
                          HTML("<p><b>sd</b>: standard deviation of group.</p>"),
                          HTML("<p><b>#.e</b>: measure of experimental group.</p>"),
                          HTML("<p><b>#.c</b>: measure of control group</p>")
                        ),
                        # corr
                        conditionalPanel(
                          condition = "input.modelo == 'df_corr'",
                          HTML("<p><b>n</b>: the total number of cases included.</p>"),
                          HTML("<p><b>r</b>: correlation coefficient reported.</p>")
                        ),
                        # dich
                        conditionalPanel(
                          condition = "input.modelo == 'df_dich'",
                          HTML("<p><b>event</b>: number of positive events.</p>"),
                          HTML("<p><b>n</b>: the total number of cases included.</p>"),
                          HTML("<p><b>#.e</b>: measure of experimental group.</p>"),
                          HTML("<p><b>#.c</b>: measure of control group</p>")
                        )
                      ),
                      column(12,
                        tags$hr(),
                        tags$head(
                          tags$style(HTML('#plot{background-color:orange}'))
                        ),
                        actionButton("plot", "Generate results", width = "150px"),
                        downloadButton('downloadData', 'Save Data')
                      ),
                      tags$br() 
                    )#endfluidrow
        ),#endtabpanel
        
        tabPanel("Import File",
                 fluidRow(
                   column(4,
                     checkboxInput(inputId = "header", label = "Header", TRUE),
                     radioButtons(inputId = "sep", label = "Separator",
                                  choices = c('comma ,'=',', 'semicolon ;'=';','tab'='\t'), selected = ","),
                     radioButtons(inputId = "quote", label = "Citation",
                                  choices = c('none'='', 'double quotes " "'='"',"single quotation marks ' '"="'"),selected = '"')
                   ),
                   column(8,
                     fileInput("arquivo", "Choose .csv or .txt file",
                               accept=c('text/csv','text/comma-separated-values',
                                        'text/tab-separated-values','text/plain','.csv','.tsv','.txt')),
                     actionButton("botao_arquivo", "Import file"),
                     tags$hr(),
                     tags$p("Write your input into a .csv or .txt file following the template for the chosen Effect Size."),
                     tags$p("The template is the column headers visible at Manual Input tab. Each line in your file will be considered as a multivariate."),
                     tags$p("After a successful import your data should be visible in the table atManual input tab.")
                   )
                   
                 )#endfluidrow
        ),#endtabpanel
        
        tabPanel("Advanced settings",
                 fluidRow(
                   column(6,
                          conditionalPanel(
                            condition = "input.modelo == 'df_prop'",
                            p(h3('Settings for proportion model:')),
                            selectInput(inputId = 'smprop',
                                        label = 'Proportion model measures',
                                        choices = c('Logit'='PLOGIT',
                                                    'Log'='PLN',
                                                    'Freeman-Tukey Double arcsine'='PFT',
                                                    'Arcsine' = 'PAS',
                                                    'Raw, i.e. untransformed, proportions'='PRAW'),
                                        selected = "PLOGIT",
                                        width = 300,
                                        multiple = FALSE
                            ),
                            p(h6('Logit transformation is used as standard in the metafor package.')),
                            
                            selectInput(inputId = 'ciprop',
                                        label = 'Method of calculating the confidence interval',
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
                            p(h6('The Clopper-Pearson method is used as default in the metafor package.'))  
                          ),
                          
                          # conditionalPanel( # diferenca de medias
                          #   condition = "input.modelo == 'df_med1'",
                          #   p(h3('Settings for mean differences model:')),
                          #   selectInput(inputId = 'smmean',
                          #               label = 'Mean differences measures',
                          #               choices = c('mean difference'='MD',
                          #                           'standardized mean difference'='SMD'),
                          #               selected = "MD",
                          #               width = 300,
                          #               multiple = FALSE
                          #   ),
                          #   p(h6('There is no metric used as default in the metafor package.'))
                          # ),
                          
                          conditionalPanel( # diferenca de media padronizada
                            condition = "input.modelo == 'df_medp'",
                            p(h3('Settings for mean differences model:')),
                            selectInput(inputId = 'smmean',
                                        label = 'Mean differences measures',
                                        choices = c('mean difference'='MD',
                                                    'standardized mean difference'='SMD'),
                                        selected = "MD", width = 300, multiple = FALSE
                            ),
                            p(h6('There is no metric used as default in the metafor package.')),
                            selectInput(inputId = 'smdmean',
                                        label = 'Method of computing the difference for the standardized mean',
                                        choices = c('Hedges\' g (1981)'='Hedges',
                                                    'Cohen\'s d (1988)'='Cohen',
                                                    'Glass\' delta (1976)'='Glass'),
                                        selected = "Hedges", width = 300, multiple = FALSE
                            ),
                            p(h6('The Hedges method is used as default in the metafor package.'))
                          ),
                          
                          conditionalPanel( # Correlacao
                            condition = "input.modelo == 'df_corr'",
                            p(h3('Settings for the correlation model:')),
                            selectInput(inputId = 'smcor',
                                        label = 'Correlation measures',
                                        choices = c('Raw correlation coefficient'='COR',
                                                    'Fisher\'s z transformation of correlations'='ZCOR'),
                                        selected = "ZCOR", width = 300, multiple = FALSE
                            ),
                            p(h6('The Fisher measure is used as default in the metafor package.'))
                          ),
                          
                          conditionalPanel( # Respostas dicotomicas
                            condition = "input.modelo == 'df_dich'",
                            p(h3('Settings for dichotomous model:')),
                            selectInput("dichotomousoptions", strong("Dichotomous measures"),
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
                          p(h3('Settings for random effect:')),
                          selectInput(inputId = 'measure',
                                      label = 'Random Effect Model Estimator',
                                      choices = c('DerSimonian-Laird estimate (1986)' = 'DL',
                                                  'Restricted maximum-likelihood'='REML',
                                                  'Maximum-likelihood'='ML',
                                                  'Hunter_Schmidt'='HS',
                                                  'Sidik-Jonkman'='SJ',
                                                  'Hedges'='HE',
                                                  'Empirical Bayes'='EB',
                                                  'Paule-Mandel method (1982)'='PM'),
                                      selected = "DL", width = 300, multiple = FALSE
                          ),
                          p(h6('DerSimonian-Laird is the default estimator in the metafor package.')),
                          
                          checkboxInput("khadjust", label = "Knapp & Hartung Adjustment", value = FALSE),
                          p(h6('The Knapp & Hartung setting is not used as default in the metafor package.'))
                   )
                 )
        )
      )#endtabbox
    }
  })
  output$results <- renderUI({
    tabBox(
      title = "Results",
      id = "results",
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
         forest(meta, studlab = paste(dados$data$Studies),
                comb.random=FALSE, comb.fixed=TRUE)
       } else {
         forest(meta, studlab = paste(dados$data$Studies),
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
         funnel.meta(meta, studlab = paste(dados$data$Studies),
                comb.random=FALSE, comb.fixed=TRUE)
       } else {
         funnel.meta(meta, studlab = paste(dados$data$Studies),
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