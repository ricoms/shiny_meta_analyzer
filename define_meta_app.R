## define_meta_app.R ##

################################################
# Funções utilizadas para os effect-sizes
################################################
compute_meta = list(
  
  df_prop = metaprop,

  df_med1 = metacont,

  df_medp = metacont,

  df_corr = metacor,

  df_RR = metabin,

  df_RD = metabin,
  
  df_OR = metabin
)

################################################
# Argumentos utilizados para as funções dos effect-sizes
################################################
arg_meta = reactiveValues(
  df_prop = list(event = dados$data$eventos, n = dados$data$n,
                  sm = "PLOGIT", method.ci = "CP", method.tau = "DL"),

  df_med1 = list(data = dados$data,
                  n.e = dados$data$n.e, mean.e = dados$data$mean.e, sd.e = dados$data$sd.e,
                  n.c = dados$data$n.c, mean.c = dados$data$mean.c, sd.c = dados$data$sd.c,
                  sm = "SM", method.tau = "DL"),

  df_medp = list(data = dados$data,
                  n.e = dados$data$n.e, mean.e = dados$data$mean.e, sd.e = dados$data$sd.e,
                  n.c = dados$data$n.c, mean.c = dados$data$mean.c, sd.c = dados$data$sd.c,
                  sm = "SMD", method.smd = "Cohen", method.tau = "DL"),

  df_corr = list(cor = dados$data$r, n = dados$data$N,
                  sm = "ZCOR", method.tau = "DL"),

  df_RR = list(event.e = dados$data$event.e, n.e = dados$data$n.e,
                event.c = dados$data$event.c, n.c = dados$data$n.c,
                sm = 'RR', method = "MH", backtransf = TRUE),

  df_RD = list(event.e = dados$data$event.e, n.e = dados$data$n.e,
                event.c = dados$data$event.c, n.c = dados$data$n.c,
                sm = 'RD', method = "MH", backtransf = TRUE),
  
  df_OR = list(event.e = dados$data$event.e, n.e = dados$data$n.e,
               event.c = dados$data$event.c, n.c = dados$data$n.c,
               sm = 'OR', method = "MH", backtransf = TRUE)
)