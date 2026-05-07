rm(list=ls());gc();

load("dati_modificati.Rdata")

# modelli a effetti casuali ----

colnames(dati)
library(lme4)
m0 <- lm(OD~ tempo + (P + I + D)^2, data=dati)
m0
AIC(m0)

table(dati$condizione_sperimentale)
m1 <- lmer(OD ~ tempo + (tempo|condizione_sperimentale),
           data=dati, control=lmerControl(autoscale = TRUE))
m1
AIC(m1)

# modelli biologici ----

install.packages("MicrobialGrowth")

g <- MicrobialGrowth(example_data$time, example_data$y1, model = "gompertz")
g