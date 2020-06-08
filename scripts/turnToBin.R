library(plyr)
translationContent <- read.delim("dictionary.csv", header = TRUE, sep = "\t", as.is = TRUE) 
translation <- dlply(translationContent ,.(key), function(s) key = as.list(s))

save(translation, file = "translation.bin")