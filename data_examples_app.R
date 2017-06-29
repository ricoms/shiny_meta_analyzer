## data_examples_app.R ##

################################################
# Modelo de proporção
################################################
df_prop = data.frame(Estudos = c("Estudo 1",
                               "Estudo 2",
                               "Estudo 3",
                               "Estudo 4"),
                     eventos = c(17, 23, 6, 106),
                     n = c(30, 50, 30, 189))

################################################
# Modelo de médias padronizadas
################################################
df_medp = data.frame(Estudos = c("Estudo 1",
                               "Estudo 2",
                               "Estudo 3",
                               "Estudo 4"),
                     n.e = c(30, 23, 83, 21),
                     mean.e = c(51.57, 75.09, 30.08, 2.95),
                     sd.e = c(16.5, 23.01, 14.29, 1.28),
                     n.c = c(30, 24, 81, 21),
                     mean.c = c(72.97, 81.63, 35.38, 3.48),
                     sd.c = c(13.23, 14.42, 16.13, 0.68))

################################################
# Modelo de correlação
################################################
df_corr = data.frame(Estudos = c("Estudo 1",
                               "Estudo 2",
                               "Estudo 3",
                               "Estudo 4"),
                     N = c(351, 163, 328, 1718),
                     r = c(0.01, 0.4, 0.162, 0.225))

################################################
# Modelos dicotômicos RR, RD ou OR
################################################
df_dich = data.frame(Estudos = c("Estudo 1",
                               "Estudo 2",
                               "Estudo 3",
                               "Estudo 4"),
                   event.e = c(0, 0, 15, 8),
                   n.e = c(9, 10, 54, 27),
                   event.c = c(8, 10, 51, 27),
                   n.c = c(9, 10, 54, 27))
