## define_meta_app.R ##

################################################
# Funções utilizadas para os effect-sizes
################################################
compute_meta = list(
  
  df_prop = metaprop,

  df_medp = metacont,

  df_corr = metacor,

  df_dich = metabin
)

################################################
# Argumentos utilizados para as funções dos effect-sizes
################################################
arg_meta = reactiveValues(

  df_prop = list(
    event=dados$data$events,
    n=dados$data$n,
    sm=input$smprop,
    method.ci=input$ciprop,
    method.tau=input$measure
  ),

  df_medp = list(
    data=dados$data,
    n.e=dados$data$n.e, 
    mean.e=dados$data$mean.e, 
    sd.e=dados$data$sd.e,
    n.c=dados$data$n.c, 
    mean.c=dados$data$mean.c, 
    sd.c=dados$data$sd.c,
    sm=input$smmean, 
    method.smd=input$smdmean, 
    method.tau=input$measure
  ),

  df_corr = list(
    cor=dados$data$r,
    n=dados$data$n,
    sm=input$smcor,
    method.tau=input$measure
  ),

  df_dich = list(
    event.e=dados$data$event.e, 
    n.e=dados$data$n.e,
    event.c=dados$data$event.c,
    n.c=dados$data$n.c,
    sm=input$dichotomousoptions,
    method="MH", 
    backtransf=TRUE
  )
)