## server.app.R ##

library(ggplot2)
library(grid)
library(shiny)
library(rhandsontable)
library(meta)

source('data_examples_app.R', local=TRUE)

load("translation.bin") # contains the dictionary, parsed as a double list

server <- function(input, output) {

  ################################################
  #Translation function
  ################################################
  tr <- function(text){ # translates text into current language
    sapply(text,function(s) translation[[s]][[input$language]], USE.NAMES=FALSE)
  }
  
  ################################################
  #Dashboard
  ################################################
  output$test <- renderUI({
    options <- c('df_prop','df_medp','df_corr','df_dich')
    names(options) <- c(tr("Proportion"),tr("Mean Differences"),tr("Correlation"),tr("Dichotomous Models"))
    
    fluidRow(
      column(width=12,
             box(title = tr("Model configuration"), width = 12, background = "orange",
                 
                 
                 selectInput(inputId = 'modelo',
                             label = tr('Effect Size'),
                             choices = options,
                             selected = NULL, multiple = FALSE
                 ),
                 numericInput(inputId = "alpha", label = tr("Level of significance"),
                              value = 0.05, min = 0.00, max = 1,
                              step = 0.01
                 ),
                 
                 checkboxInput(inputId = "random_comb", label = tr("Random"), TRUE),
                 checkboxInput(inputId = "fixed_comb", label = tr("Fixed"), TRUE)
                 
             ),
             
             box(width = 10,
                 actionButton("escolher_modelo", tr("Define model")),
                 background = "orange"
             )
      )
    )
  })
  
  
  ################################################
  #Abas sendo criadas
  ################################################
  output$desc <- renderUI({
      box(title = tr("About"), width = 12, solidHeader = TRUE, status = "primary",
          HTML(tr("<h3>Authors</h3>")),
          HTML("<p>Ricardo Manhães Savii (<a href = 'http://lattes.cnpq.br/7614391299549728'>lattes</a>),</p>
                <p>Alexandre Hild Aono (<a href = 'http://lattes.cnpq.br/5745062922235619'>lattes</a>), </p>"),
          HTML(tr("<p>Professor Ph.D. Camila Bertini Martins (<a href = 'http://lattes.cnpq.br/3770708843269785'>lattes</a>)</p><br>")),
          HTML(tr("<p>Select below the type of input to build your analysis, for any doubt or problem, please, reach us at</p>")),
          HTML("<p><a href = 'https://groups.google.com/d/forum/meta-analyzer-unifesp'>meta-analyzer-unifesp.groups.google</a></p>")
          )
  })
  
  output$painel <- renderUI({
    separator <- c(",",";","\t")
    names(separator) <- c(tr("comma"),tr("semicolon"),tr("tab"))
    citation <- c("",'"',"'")
    names(citation) <- c(tr("none"),tr('double quotes " "'),tr("single quotation marks ' '"))
    
    if(TRUE){
      tabBox(
        title = tr("Input and Configuration"), id = "ttabs", width = 12,
        
        tabPanel(tr("Manual Input"),
                 fluidRow(
                      column(6,
                        rHandsontableOutput("hot")
                      ),
                      column(6,
                        HTML(tr("<h4>Standard headers</h4>")),
                        HTML(tr("<p><b>Studies</b>: identification of different studies to be compared.</p>")),
                        HTML(gen_help_header())
                      ),
                      column(12,
                        tags$hr(),
                        tags$head(
                          tags$style(HTML('#plot{background-color:orange}'))
                        ),
                        actionButton("plot", tr("Generate results"), width = "150px"),
                        downloadButton('downloadData', tr("Save Data"))
                      ),
                      tags$br() 
                    )#endfluidrow
        ),#endtabpanel
        
        tabPanel(tr("Import File"),
                 fluidRow(
                   column(4,
                     checkboxInput(inputId = "header", label = tr("Header"), TRUE),
                     radioButtons(inputId = "sep", label = tr("Separator"),
                                  choices = separator,selected = ","),
                     radioButtons(inputId = "quote", label = tr("Citation"),
                                  choices = citation, selected = '"')
                   ),
                   column(8,
                     fileInput("arquivo", tr("Choose .csv or .txt file"),
                               accept=c('text/csv','text/comma-separated-values',
                                        'text/tab-separated-values','text/plain','.csv','.tsv','.txt')),
                     actionButton("botao_arquivo", tr("Import file")),
                     tags$hr(),
                     tags$p(tr("Write your input into a .csv or .txt file following the template for the chosen Effect Size.")),
                     tags$p(tr("The template is the column headers visible at Manual Input tab. Each line in your file will be considered as a multivariate.")),
                     tags$p(tr("After a successful import your data should be visible in the table atManual input tab."))
                   )
                   
                 )#endfluidrow
        ),#endtabpanel
        
        tabPanel(tr("Advanced settings"),
                 fluidRow(
                   column(6,
                          conditionalPanel(
                            condition = "input.modelo == 'df_prop'",
                            p(h3(tr("Settings for proportion model:"))),
                            selectInput(inputId = 'smprop',
                                        label = tr('Proportion model measures'),
                                        choices = c('Logit'='PLOGIT',
                                                    'Log'='PLN',
                                                    'Freeman-Tukey Double arcsine'='PFT',
                                                    'Arcsine' = 'PAS',
                                                    'Raw, i.e. untransformed, proportions'='PRAW'),
                                        selected = "PLOGIT",
                                        width = 300,
                                        multiple = FALSE
                            ),
                            p(h6(tr("Logit transformation is used as standard in the metafor package."))),
                            
                            selectInput(inputId = 'ciprop',
                                        label = tr("Method of calculating the confidence interval"),
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
                            p(h6(tr("The Clopper-Pearson method is used as default in the metafor package.")))  
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
                            p(h3(tr("Settings for mean differences model:"))),
                            selectInput(inputId = 'smmean',
                                        label = 'Mean differences measures',
                                        choices = c('mean difference'='MD',
                                                    'standardized mean difference'='SMD'),
                                        selected = "MD", width = 300, multiple = FALSE
                            ),
                            p(h6(tr("There is no metric used as default in the metafor package."))),
                            selectInput(inputId = 'smdmean',
                                        label = tr('Method of computing the difference for the standardized mean'),
                                        choices = c('Hedges\' g (1981)'='Hedges',
                                                    'Cohen\'s d (1988)'='Cohen',
                                                    'Glass\' delta (1976)'='Glass'),
                                        selected = "Hedges", width = 300, multiple = FALSE
                            ),
                            p(h6(tr("The Hedges method is used as default in the metafor package.")))
                          ),
                          
                          conditionalPanel( # Correlacao
                            condition = "input.modelo == 'df_corr'",
                            p(h3(tr("Settings for the correlation model:"))),
                            selectInput(inputId = 'smcor',
                                        label = tr("Correlation measures"),
                                        choices = c('Raw correlation coefficient'='COR',
                                                    'Fisher\'s z transformation of correlations'='ZCOR'),
                                        selected = "ZCOR", width = 300, multiple = FALSE
                            ),
                            p(h6(tr("The Fisher measure is used as default in the metafor package.")))
                          ),
                          
                          conditionalPanel( # Respostas dicotomicas
                            condition = "input.modelo == 'df_dich'",
                            p(h3(tr("Settings for dichotomous model:"))),
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
                            p(h6(tr("logs odds ratio is the default option and is the one you should use for the example provided in the Input Examples tab.")))
                          )
                   ),
                   column(6,
                          p(h3(tr("Settings for random effect:"))),
                          selectInput(inputId = 'measure',
                                      label = tr("Random Effect Model Estimator"),
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
                          p(h6(tr("DerSimonian-Laird is the default estimator in the metafor package."))),
                          
                          checkboxInput("khadjust", label = "Knapp & Hartung Adjustment", value = FALSE),
                          p(h6(tr("The Knapp & Hartung setting is not used as default in the metafor package.")))
                   )
                 )
        )
      )#endtabbox
    }
  })
  output$results <- renderUI({
    tabBox(
      title = tr("Results"),
      id = "results",
      width = 12,
      tabPanel(tr("Forest Plot"),
               wellPanel(
                 plotOutput("forest", height = 400, width = 800),
                 downloadButton('downloadForest', tr("Save forest as pdf"))
               )
      ),
      tabPanel(tr("Funnel Plot"),
               wellPanel(
                 plotOutput("funnel", height = 400, width = 600),
                 downloadButton('downloadFunnel', tr("Save funnel as pdf"))
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
  
  ################################################
  # definições feitas pelo usuário na aplicação
  ################################################
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

  
  ################################################
  # React para conteúdo da tabela
  ################################################
  ## values será o espelho da rHandsonTable no programa
  values <- reactiveValues(data = df_prop)
  
  ## permite a edição direto na tabela rHandsonTable
  observe ({
    if(!is.null(input$hot))
      values$data <- hot_to_r(input$hot)
  })
  
  ## permite a importação à partir de um arquivo
  observeEvent (input$botao_arquivo, {
    inFile <- input$arquivo
    if (is.null(inFile))
      return(NULL)
    values$data <- read.table(inFile$datapath, header = header(), sep = sep(), quote = quote())
  })
  
  ## permite a importação de um data.frame previamente criado
  observeEvent (input$escolher_modelo, {
    values$data <- get(input$modelo)
  })
  
  
  ################################################
  # React para texto explicativo dos cabeçalhos das tabelas
  ################################################
  helper_header <- reactiveValues(modelo = 'df_prop')
  
  observeEvent (input$escolher_modelo, {
    helper_header$modelo <- input$modelo
  })
  
  gen_help_header <- function() {
    if (helper_header$modelo == 'df_prop'){
      return(cbind(tr("<p><b>events</b>: number of positive events.</p>"),
        tr("<p><b>n</b>: the total number of cases included.</p>")))
      
    } else if (helper_header$modelo == 'df_medp'){
      return(cbind(tr("<p><b>n</b>: the total number of cases included.</p>"),
       tr("<p><b>mean</b>: mean of group.</p>"),
       tr("<p><b>sd</b>: standard deviation of group.</p>"),
       tr("<p><b>#.e</b>: measure of experimental group.</p>"),
       tr("<p><b>#.c</b>: measure of control group</p>")))
      
    } else if (helper_header$modelo == 'df_corr'){
      return(cbind(tr("<p><b>n</b>: the total number of cases included.</p>"),
       tr("<p><b>r</b>: correlation coefficient reported.</p>")))
      
    } else if (helper_header$modelo == 'df_dich'){
      return(cbind(tr("<p><b>event</b>: number of positive events.</p>"),
       tr("<p><b>n</b>: the total number of cases included.</p>"),
       tr("<p><b>#.e</b>: measure of experimental group.</p>"),
       tr("<p><b>#.c</b>: measure of control group</p>")))
    }
  }
  
  ################################################
  # dados é a variável final que será levada até o plot
  ################################################
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
       forest(meta, studlab = paste(dados$data$Studies),
              comb.random=input$random_comb,
              comb.fixed=input$fixed_comb)
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
      funnel.meta(meta, studlab = paste(dados$data$Studies),
                  comb.random=input$random_comb,
                  comb.fixed=input$fixed_comb)
    } else {
      frame()
    }
  }
  # Deprecated: above is correct now
  # plotFunnel <- function(){
  #   meta <- meta()
  #    if (!is.null(meta)) {
  #      if (1-pchisq(meta$Q, meta$df.Q) > alpha()) { # if p-value > alpha só apresenta fixed effect model
  #        funnel.meta(meta, studlab = paste(dados$data$Studies),
  #               comb.random=FALSE, comb.fixed=TRUE)
  #      } else {
  #        funnel.meta(meta, studlab = paste(dados$data$Studies),
  #               comb.random=TRUE, comb.fixed=FALSE)
  #      }
  #    } else {
  #      frame()
  #    }
  # }
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