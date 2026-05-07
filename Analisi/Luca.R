rm(list=ls());gc();

load("Analisi/dati_modificati.Rdata")

# modelli biologici ----

# install.packages("MicrobialGrowth")

colnames(dati)
library(MicrobialGrowth)
g <- MicrobialGrowth(example_data$time, example_data$y1,
                     model = "gompertz")
g