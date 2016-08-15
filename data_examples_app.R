## data_examples_app.R ##

################################################
# Modelo de proporção
################################################
df_prop = data.frame(Estudos = c("Porcu et al. 2002",
                               "Ferrari & Dalacorte 2007",
                               "Maues et al. 2007",
                               "Mendes-Chiloff et al. 2008"),
                     eventos = c(17, 23, 6, 106),
                     n = c(30, 50, 30, 189))

################################################
# Modelo de médias 1
################################################
df_med1 = data.frame(Estudos = c("Porcu et al. 2002",
                               "Ferrari & Dalacorte 2007",
                               "Maues et al. 2007",
                               "Mendes-Chiloff et al. 2008"),
                     n.e = c(30, 23, 83, 21),
                     mean.e = c(51.57, 75.09, 30.08, 2.95),
                     sd.e = c(16.5, 23.01, 14.29, 1.28),
                     n.c = c(30, 24, 81, 21),
                     mean.c = c(72.97, 81.63, 35.38, 3.48),
                     sd.c = c(13.23, 14.42, 16.13, 0.68))

################################################
# Modelo de médias padronizadas
################################################
df_medp = data.frame(Estudos = c("Porcu et al. 2002",
                               "Ferrari & Dalacorte 2007",
                               "Maues et al. 2007",
                               "Mendes-Chiloff et al. 2008"),
                     n.e = c(30, 23, 83, 21),
                     mean.e = c(51.57, 75.09, 30.08, 2.95),
                     sd.e = c(16.5, 23.01, 14.29, 1.28),
                     n.c = c(30, 24, 81, 21),
                     mean.c = c(72.97, 81.63, 35.38, 3.48),
                     sd.c = c(13.23, 14.42, 16.13, 0.68))

################################################
# Modelo de correlação
################################################
df_corr = data.frame(Estudos = c("Porcu et al. 2002",
                               "Ferrari & Dalacorte 2007",
                               "Maues et al. 2007",
                               "Mendes-Chiloff et al. 2008"),
                     N = c(351, 163, 328, 1718),
                     r = c(0.01, 0.4, 0.162, 0.225))

################################################
# Modelos dicotômicos RR
################################################
df_RR = data.frame(Estudos = c("Rockliff 1966",
                               "Dalessio 1966",
                               "Sturman 1969",
                               "Killian 1968"),
                   event.e = c(0, 0, 15, 8),
                   n.e = c(9, 10, 54, 27),
                   event.c = c(8, 10, 51, 27),
                   n.c = c(9, 10, 54, 27))

################################################
# Modelos dicotômicos RD
################################################
df_RD = data.frame(Estudos = c("Rockliff 1966",
                               "Dalessio 1966",
                               "Sturman 1969",
                               "Killian 1968"),
                   event.e = c(0, 0, 15, 8),
                   n.e = c(9, 10, 54, 27),
                   event.c = c(8, 10, 51, 27),
                   n.c = c(9, 10, 54, 27))

################################################
# Modelos dicotômicos OR
################################################
df_OR = data.frame(Estudos = c("Emslie et al., 1997",
                               "Emslie et al., 2002",
                               "Tads Study, 2004"),
                   event.e = c(27, 71, 66),
                   n.e = c(48, 109, 109),
                   event.c = c(16, 54, 39),
                   n.c = c(48, 101, 112))