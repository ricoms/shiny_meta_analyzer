## ui.app.R ##
library(shiny)
library(shinydashboard)
library(rhandsontable)

ui <- dashboardPage(skin = "black",
  # tags$head(includeScript("google-analytics.js")),
  ## cadastrar o endereço web na conta do google analytics,
  ## e salvar a chave de tracking em arquivo com nome google-analytics.js
  ## na pasta do projeto e sem '<script></script>'
  
  dashboardHeader(title = "Metadata analyzer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("App", tabName = "app", icon = icon("home")),
      #menuItem("Configurações Avançadas", tabName = "config", icon = icon("dashboard")),
      menuItem("About", tabName = "about", icon = icon("glyphicon glyphicon-info-sign", lib= "glyphicon")),
      menuItem("Source code", icon = icon("file-code-o"),
               href = "https://github.com")
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href="custom.css"),
      tags$script(src="script.js", type="text/javascript")
    ),
    tabItems(
      # First tab content
      tabItem(tabName = "app",

        # Painel principal
        fluidRow(
          box(title = "Configurações iniciais", status = "primary", collapsible = TRUE, width = 12,
              fluidRow(
                column(6,
                       p("Para iniciar escolha um effect-size modelo ou submeta um arquivo pré-formatado."),
                       selectInput(inputId = 'modelo',
                                   label = 'Effect sizes',
                                   choices = c('Proporção'='df_prop',
                                               'Diferença de médias'='df_med1',
                                               'Diferença de médias padronizadas'='df_medp',
                                               # 'Diferença de médias2'='df_med2',
                                               'Correlação'='df_corr',
                                               'Riscos relativos'='df_RR',
                                               "Risk difference" = "df_RD",
                                               "Odds ratio" = "df_OR"
                                               ),
                                   width = 300,
                                   selected = NULL,
                                   multiple = FALSE
                       ),
                       checkboxInput(inputId = 'subir_arquivo',
                                     label = 'Importar arquivo?'
                       ),
                       actionButton("escolher_modelo", "Definir modelo")
                ),
                
                
                column(6,
                       numericInput(inputId = 'alpha',
                                    label = "significância",
                                    value = 0.05,
                                    min = 0.00,
                                    max = 1,
                                    width = 100,
                                    step = 0.01)
                )
              ),
              tags$hr(),
              
              # Painel da rHandsonTable
              fluidRow(
                column(12,
                    wellPanel(
                      rHandsontableOutput("hot") 
                    )
                )
              ),
              tags$hr(),
              fluidRow(
                column(4,
                       actionButton("plot", "Gerar resultados")
                  
                ),
                column(8,
                       downloadButton('downloadData', 'Salvar Tabela')
                )
              )
            )
          ),
        
        # Painel para importação de arquivo
        conditionalPanel(
          condition = "input.subir_arquivo == true",
          fluidRow(
            box(title = "Entrada por arquivo", status = "warning", collapsible = TRUE, width = 12,
                fluidRow(
                  column(4,
                         checkboxInput(inputId = 'header',
                                       label = 'Cabeçalho',
                                       TRUE),
                         radioButtons(inputId = 'sep',
                                      label = 'Separador',
                                      choices = c('virgula'=',',
                                                  'ponto e virgula'=';',
                                                  'tabulação'='\t'),
                                      selected = ','),
                         radioButtons(inputId = 'quote',
                                      label = 'Citaçao',
                                      choices = c('sem'='',
                                                  'aspas duplas'='"',
                                                  'aspas simples'="'"),
                                      selected = '"')
                  ),
                  column(8,
                         fileInput('arquivo', 'Escolher arquivo .csv ou .txt',
                                   multiple = FALSE,
                                   accept = c('text/csv',
                                              'text/comma-separated-values',
                                              'text/tab-separated-values',
                                              'text/plain',
                                              '.csv',
                                              '.tsv',
                                              '.txt'
                                   )
                         ),
                         actionButton("botao_arquivo", "Importar arquivo"),
                         tags$hr(),
                         tags$p("Escreva suas entradas em um arquivo .csv ou .txt."),
                         tags$p("seus dados devem possuir exatamente os mesmos cabeçalhos aos dados de exemplo para o modelo escolhido"),
                         p('Para ter um exemplo de dados, selecione um dos exemplos prontos no painel de configurações iniciais.'),
                         tags$p(tags$b("Importante:"), "siga corretamente as orientações acima e configurações desta aba,
                      configurações incorretas impedirão a leitura dos dados.")
                  )
                )
            )
          )
        ),
        
        #plot do resultado final  
        fluidRow(
          box(title = "Forest plot", status = "success", collapsible = TRUE, width = 12, align = "center",
              plotOutput("forest"),
              tags$hr(),
              column(width = 4,
                     downloadButton('downloadForest', 'Salvar forest como pdf')
                     )
              ),
          box(title = "Funnel plot", status = "success", collapsible = TRUE, width = 12, align = "center",
              plotOutput("funnel", height = 400, width = 600),
              tags$hr(),
              column(width = 4,
                     downloadButton('downloadFunnel', 'Salvar funnel como pdf')
                     )
              ),
          box(title = "Testes de assimetria", status = "success", collapsible = TRUE, width = 12,
              verbatimTextOutput("eggerTest")
              #, verbatimTextOutput("beggTest")
          )
        )
      ),
      
      # tab de configurações avançadas
      # tabItem(tabName = "config",
      #         column(6,
      #                p(h3('Configurações Efeito Randômico:')),
      #                selectInput(inputId = 'measure',
      #                            label = 'Estimador do Modelo de Efeito Randômico',
      #                            choices = c('DerSimonian-Laird estimate (1986)' = 'DL',
      #                                        'Restricted maximum-likelihood'='REML',
      #                                        'Maximum-likelihood'='ML',
      #                                        'Hunter_Schmidt'='HS',
      #                                        'Sidik-Jonkman'='SJ',
      #                                        'Hedges'='HE',
      #                                        'Empirical Bayes'='EB',
      #                                        'Paule-Mandel method (1982)'='PM'),
      #                            selected = "DL",
      #                            width = 300,
      #                            multiple = FALSE
      #                ),
      #                p(h6('DerSimonian-Laird é o estimador padrão do pacote metafor.')),
      #                
      #                checkboxInput("khadjust", label = "Knapp & Hartung Adjustment", value = FALSE),
      #                p(h6('O ajuste Knapp & Hartung como padrão não é utilizado no pacote metafor.')),
      #                tags$hr(),
      #                
      #                p(h3('Configurações para modelo de correlação:')),
      #                selectInput(inputId = 'smcor',
      #                            label = 'Medidas do modelo de correlação',
      #                            choices = c('Raw correlation coefficient'='COR',
      #                                        'Fisher\'s z transformation of correlations'='ZCOR'),
      #                            selected = "ZCOR",
      #                            width = 300,
      #                            multiple = FALSE
      #                ),
      #                p(h6('A medida de Fisher é utilizado como padrão no pacote metafor.')),
      #                tags$hr(),
      #                
      #                p(h3('Configurações para modelo dicotômico:')),
      #                selectInput("dichotomousoptions", strong("Seleção de medida"),
      #                             c("log relative risk" = "RR",
      #                               "log odds ratio" = "OR",
      #                               "risk difference" = "RD",
      #                               "arcsine square-root transformed risk difference (Rücker et al., 2009)." = "AS",
      #                               "log odds ratio estimated with Peto’s method (Yusuf et al., 1985)." = "PETO",
      #                               "probit transformed risk difference as an estimate of the standardized mean difference." = "PBIT",
      #                               "transformed odds ratio as an estimate of the standardized mean difference (normal distributions)." = "OR2DN",
      #                               "transformed odds ratio as an estimate of the standardized mean difference (logistic distributions)." = "OR2DL"
      #                             ), selected = "OR"),
      #                p(h6('logs odds ratio is the default option and is the one you should use for the example provided in the Input Examples tab.'))
      #         ),
      #         column(6,
      #                p(h3('Configurações para modelo de proporção:')),
      #                selectInput(inputId = 'smprop',
      #                            label = 'Medidas do modelo de proporção',
      #                            choices = c('Logit'='PLOGIT',
      #                                        'Log'='PLN',
      #                                        'Freeman-Tukey Double arcsine'='PFT',
      #                                        'Arcsine' = 'PAS',
      #                                        'Raw, i.e. untransformed, proportions'='PRAW'),
      #                            selected = "PLOGIT",
      #                            width = 300,
      #                            multiple = FALSE
      #                ),
      #                p(h6('A transformação Logit é utilizado como padrão no pacote metafor.')),
      #                
      #                selectInput(inputId = 'ciprop',
      #                            label = 'Método de cálculo do intervalo de confiança',
      #                            choices = c('exact binomial (Clopper-Pearson)'='CP',
      #                                        'Wilson Score'='WS',
      #                                        'Wilson Score interval with continuity correction'='WSCC',
      #                                        'Agresti-Coull' = 'AC',
      #                                        'Simple approximation'='SA',
      #                                        'Simple approximation interval with continuity correction' = 'SACC',
      #                                        'Normal approximation interval based on summary measure, i.e. defined by argument sm' = 'NAsm'),
      #                            selected = "CP",
      #                            width = 300,
      #                            multiple = FALSE
      #                ),
      #                p(h6('O método Clopper-Pearson é utilizado como padrão no pacote metafor.')),
      #                tags$hr(),
      #                
      #                p(h3('Configurações para modelo de diferença de médias:')),
      #                selectInput(inputId = 'smmean',
      #                            label = 'Medidas do modelo de diferença de médias',
      #                            choices = c('mean difference'='MD',
      #                                        'standardised mean difference'='SMD'),
      #                            selected = "MD",
      #                            width = 300,
      #                            multiple = FALSE
      #                ),
      #                p(h6('Não há medida utilizada como padrão no pacote metafor.')),
      #                
      #                selectInput(inputId = 'smdmean',
      #                            label = 'Método de cálculo da diferença na média padronizada',
      #                            choices = c('Hedges\' g (1981)'='Hedges',
      #                                        'Cohen\'s d (1988)'='Cohen',
      #                                        'Glass\' delta (1976)'='Glass'),
      #                            selected = "Hedges",
      #                            width = 300,
      #                            multiple = FALSE
      #                ),
      #                p(h6('O método de Hedges é utilizado como padrão no pacote metafor.'))
      #         )
      #         
      # ),
      
      # tab about
      tabItem(tabName = "about",
              h2("Projeto: Meta analyzer"),
              p("Orientadora: Prof. Dra. Camila Bertini Martins"),
              p("Aluno: Ricardo Manhães Savii")
      )
    )
  )
)