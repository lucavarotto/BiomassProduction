rm(list=ls());gc();

load("Analisi/dati_modificati.Rdata")

# modelli biologici ----

# install.packages("MicrobialGrowth")

colnames(dati)
library(MicrobialGrowth)
g <- MicrobialGrowth(x, y,
                     model = "gompertz")
g


# regressione gompertz ----

library(nls)

colnames(dati)
