rm(list=ls());gc();

load("Analisi/dati_modificati.Rdata")

# bilanciamento disegno ----

library(ggplot2)
library(dplyr)

conteggi <- dati %>%
  group_by(I, D, P) %>%
  summarise(n_biomasse = n_distinct(id_biomassa), .groups = 'drop')

ggplot(conteggi, aes(x = as.factor(I), y = as.factor(D))) +
  geom_tile(aes(fill = n_biomasse), color = "white") +
  # Aggiungiamo il numero scritto dentro ogni cella
  geom_text(aes(label = n_biomasse), color = "white", fontface = "bold") +
  facet_wrap(~ P, labeller = label_both) +
  scale_fill_viridis_c(option = "magma", end=0.8) +
  labs(
    title = "Verifica Bilanciamento del Disegno Sperimentale",
    subtitle = "Conteggio del numero di biomasse per combinazione di I, D e P",
    x = "Intensità Luminosa (I)",
    y = "Durata Esposizione (D)",
    fill = "N. Repliche"
  ) +
  theme_minimal()

# modelli biologici ----

# install.packages("MicrobialGrowth")

colnames(dati)
library(MicrobialGrowth)
#g <- MicrobialGrowth(x, y, model = "gompertz")
#g


# regressione gompertz con effetti casuali ----

# Asym rappresenta la capacità portante o il valore massimo teorico

# b2 (Parametro di posizione / Latenza
# Controlla lo spostamento della curva lungo l'asse X (il tempo).
# Un valore di b2 più alto "sposta" la curva verso destra,
# indicando che la crescita inizia più tardi.
# b2 è legato al valore della variabile y quando il tempo t = 0

# b3 rappresenta la velocità o il tasso di crescita intrinseco.
# Se b3 è alto, la curva sarà molto ripida (crescita esplosiva); se b3 è basso, la curva salirà molto dolcemente.

dati_m_gompertz <- na.omit(dati)
dati_m_gompertz <- dati_m_gompertz |> mutate(
  I = I - 280,
  D = D - 18,
  P = P - 25.5
)

library(nlme)

modello_gompertz <- nlme(
  model = OD ~ SSgompertz(tempo, Asym, b2, b3),
  data = dati_m_gompertz,
  fixed = list(
    Asym ~ (I+D)^2+P,
    b2 ~ (I+D)^2+P,           
    b3 ~ (I+D)^2+P     
  ),
  random = Asym ~ 1 | id_biomassa,
  start = c(
    Asym = c(14, rep(0, 4)), 
    b2 = c(5.5, rep(0, 4)),     
    b3 = c(0.75, rep(0, 4))),
  control = nlmeControl(maxIter = 350, # Il numero massimo di iterazioni durante la
                        # fase di ottimizzazione della stima di massima verosimiglianza
                        pnlsMaxIter = 25, # il modello tiene fissi
                        # gli effetti casuali e cerca di aggiornare
                        # i parametri fissi per minimizzare l'errore.
                        msMaxIter = 200, # si riferisce all'ottimizzatore numerico
                        msMaxEval = 500, # Aumentiamo il numero di valutazioni della funzione obiettivo
                        niterEM = 50, # importante per trovare buoni punti di partenza per le varianze
                        opt = "nlminb")
)
modello_gompertz2 <- update(modello_gompertz,
                            random = b2 ~ 1 | id_biomassa)
modello_gompertz3 <- update(modello_gompertz,
                            random = b3 ~ 1 | id_biomassa)
modello_gompertz4 <- update(modello_gompertz,
                            random = Asym + b3 ~ 1 | id_biomassa)
modello_gompertz5 <- update(modello_gompertz,
                            random = list(id_biomassa = pdDiag(Asym + b2 ~ 1)))
AIC(modello_gompertz, modello_gompertz2, modello_gompertz3,
    modello_gompertz4)

?MuMIn::dredge()

summary(modello_gompertz)

dati_m_gompertz$predicted <- predict(modello_gompertz)
library(ggforce)
x11()
ggplot(dati_m_gompertz, aes(x = tempo, color = as.factor(condizione_sperimentale))) +
  geom_point(aes(y = OD), alpha = 0.6) +
  geom_line(aes(y = OD, group = id_biomassa), linetype = "dashed", alpha = 0.4) +
  geom_line(aes(y = predicted, group = id_biomassa), linewidth = 1.2)+
  facet_wrap_paginate(~ id_biomassa, scales = "free_y", ncol = 3, nrow = 3, page = 6) +
  scale_color_viridis_d(option = "plasma", guide = "none") +
  labs(
    title = "Confronto Dati Reali vs Stima Gompertz",
    subtitle = "I punti indicano le osservazioni reali; le linee continue rappresentano il modello NLME",
    x = "Tempo (giorni)",
    y = "Optical Density (OD)"
  ) +
  theme_minimal()

# regressione gompertz solo fisso ----

modello_gompertz_fisso <- gnls(
  model = OD ~ SSgompertz(tempo, Asym, b2, b3),
  data = dati_m_gompertz,
  params = list(
    Asym ~ (I + D)^2+P, # Interazione per l'asintoto
    b2 ~ (I + D)^2+P, # Interazione per la latenza
    b3 ~ (I + D)^2+P # Interazione per la rapidità
  ),
  start = c(
    Asym = c(3, rep(0, 4)),  
    b2   = c(1.5, rep(0, 4)),               
    b3   = c(0.8, rep(0, 4))
  )
)

AIC(modello_gompertz, modello_gompertz_fisso)

anova(modello_gompertz_fisso)

summary(modello_gompertz_fisso)
coef(modello_gompertz_fisso) |> round(4)

dati_m_gompertz$predicted_fisso <- predict(modello_gompertz)

library(ggforce)
ggplot(dati_m_gompertz, aes(x = tempo, color = as.factor(condizione_sperimentale))) +
  geom_point(aes(y = OD), alpha = 0.6) +
  geom_line(aes(y = OD, group = id_biomassa), linetype = "dashed", alpha = 0.4) +
  geom_line(aes(y = predicted_fisso, group = id_biomassa), linewidth = 1.2)+
  facet_wrap_paginate(~ id_biomassa, scales = "free_y", ncol = 3, nrow = 3, page = 2) +
  scale_color_viridis_d(option = "plasma", guide = "none") +
  labs(
    title = "Confronto Dati Reali vs Stima Gompertz",
    subtitle = "I punti indicano le osservazioni reali; le linee continue rappresentano il modello NLME",
    x = "Tempo (giorni)",
    y = "Optical Density (OD)"
  ) + theme_minimal()

matrix(coef(modello_gompertz_fisso), nrow=3, byrow=T) %*%
  t(as.matrix(model.matrix(~(I + D)^2+P, data=dati[1,])))


# modello parametrico sugli incrementi ----

dati_lme <- na.omit(dati_incrementi)
dati_lme <- dati_incrementi |> mutate(
  I = I - 280,
  D = D - 18,
  P = P - 25.5
)
dati_lme$tempo <- dati_lme$tempo + 1

library(lme4)
colnames(dati_lme)
m0 <- lme(incremento_OD ~ tempo + (I + D)^2+P + tempo:I + tempo:D + tempo:P,
          random = ~ 1 + tempo | as.factor(id_biomassa), data=dati_lme,
          control = lmeControl(maxIter = 1000, # Il numero massimo di iterazioni durante la
                               # fase di ottimizzazione della stima di massima verosimiglianza
                               msMaxIter = 100, # si riferisce all'ottimizzatore numerico
                               niterEM = 50,
                               msMaxEval = 400,
                               opt = "optim")
)
m1 <- lme(incremento_OD ~ tempo + I((log(tempo))^2) + (I + D)^2+P + I((log(tempo))^2):I + I((log(tempo))^2):D + I((log(tempo))^2):P,
          random = ~ 1 + tempo | as.factor(id_biomassa), data=dati_lme,
          control = lmeControl(maxIter = 1000, # Il numero massimo di iterazioni durante la
                               # fase di ottimizzazione della stima di massima verosimiglianza
                               msMaxIter = 100, # si riferisce all'ottimizzatore numerico
                               niterEM = 50,
                               msMaxEval = 400,
                               opt = "optim")
)
m2 <- lme(incremento_OD ~ tempo + I(tempo^2) + I(tempo^3) + (I + D)^2+P +
            I(tempo):I + I(tempo):D + I(tempo):P +
            I(tempo^2):I + I(tempo^2):D + I(tempo^2):P +
            I(tempo^3):I + I(tempo^3):D + I(tempo^3):P,
          random = ~ 1 + tempo | as.factor(id_biomassa), data=dati_lme,
          control = lmeControl(maxIter = 1000, # Il numero massimo di iterazioni durante la
                               # fase di ottimizzazione della stima di massima verosimiglianza
                               msMaxIter = 100, # si riferisce all'ottimizzatore numerico
                               niterEM = 50,
                               msMaxEval = 400,
                               opt = "optim")
)
summary(m2)
AIC(m0, m1, m2)

library(ggforce)
library(ggplot2)

dati_lme$predicted <- fitted(m2)
ggplot(dati_lme, aes(x = tempo, color = as.factor(condizione_sperimentale))) +
  geom_point(aes(y = incremento_OD), alpha = 0.6) +
  geom_line(aes(y = incremento_OD, group = id_biomassa), linetype = "dashed", alpha = 0.4) +
  geom_line(aes(y = predicted, group = id_biomassa), linewidth = 1.2) +
  facet_wrap_paginate(~ id_biomassa, scales = "free_y", ncol = 3, nrow = 3, page = 3) +
  scale_color_viridis_d(option = "plasma", guide = "none") +
  labs(
    title = "Confronto incrementi veri vs stimati",
    subtitle = "Punti: Osservazioni reali | Linea continua: Predizione specifica per biomassa",
    x = "Tempo (giorni)",
    y = "Optical Density (OD)"
  ) + 
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))


N0 <- 0.01
dati_lme <- dati_lme %>%
  group_by(id_biomassa) %>%
  arrange(tempo) %>%
  mutate(
    # Somma cumulata degli incrementi predetti + il valore iniziale reale N0
    predicted_OD_cum = N0 + cumsum(predicted)
  ) %>%
  ungroup()

ggplot(dati_lme, aes(x = tempo, color = as.factor(condizione_sperimentale))) +
  geom_point(aes(y = OD), alpha = 0.6) +
  geom_line(aes(y = OD, group = id_biomassa), linetype = "dashed", alpha = 0.4) +
  geom_line(aes(y = predicted_OD_cum, group = id_biomassa), linewidth = 1.2) +
  facet_wrap_paginate(~ id_biomassa, scales = "free_y", ncol = 3, nrow = 3, page = 5) +
  scale_color_viridis_d(option = "plasma", guide = "none") +
  labs(
    title = "Ricostruzione della Curva di Crescita (da modello LME su incrementi)",
    subtitle = "Punti: OD osservata | Linea continua: Somma cumulata degli incrementi stimati",
    x = "Tempo (giorni)",
    y = "Optical Density (OD Totale)"
  ) + 
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))

# GAM incrementi ----

library(mgcv)

dati_gam <- na.omit(dati_incrementi)
dati_gam <- dati_incrementi |> mutate(
  I = I - 280,
  D = D - 18,
  P = P - 25.5
)
dati_gam$Gruppo_ID <- interaction(dati_gam$I, dati_gam$D)
dati_gam$id_biomassa <- as.factor(dati_gam$id_biomassa)

modello_gam <- gam(
  incremento_OD ~ 
    # Effetti principali delle covariate lineari
    I + D + P + (I + D)^2 + 
    # s(tempo) crea la spline. 'by' permette alla curva di cambiare per ogni combinazione di I e D
    s(tempo, by = Gruppo_ID, k = 5) + 
    # Interazione liscia con P (opzionale, se pensi che P cambi la forma della curva)
    s(tempo, by = P, k = 5) + 
    # EFFETTO CASUALE: l'intercetta casuale per ogni biomassa (equivalente a (1|ID) nei modelli misti)
    s(id_biomassa, bs = "re"), 
  
  data = dati_gam,
  method = "REML" # Consigliato per stime accurate degli effetti casuali e dello smoothing
)

summary(modello_gam)

## plot degli effetti stimati ----

x11()
plot(modello_gam, 
     pages = 1,           # Mette tutto in una pagina
     shade = TRUE,        # Aggiunge l'intervallo di confidenza ombreggiato
     residuals = TRUE,    # Mostra i punti dei residui parziali
     pch = 1, cex = 0.5, 
     main = "Effetti Smooth del Tempo")

## altro plot degli effetti stimati ----

library(gratia)
x11()
draw(modello_gam)

## plot fitted vs osservati ----

library(tidymv)
library(ggplot2)
library(dplyr)

dati_gam <- dati_gam %>%
  mutate(Valori_Stimati = predict(modello_gam, newdata = .))

dati_gam %>%
  ggplot(aes(x = tempo)) +
  # Punti che indicano i valori realmente osservati
  geom_point(aes(y = incremento_OD, color = condizione_sperimentale), alpha = 0.6, size = 2) +
  # Linea che indica il trend stimato dal modello GAM
  geom_line(aes(y = Valori_Stimati, group = id_biomassa), color = "black", linewidth = 0.8) +
  facet_wrap_paginate(~ id_biomassa, scales = "free_y", ncol = 3, nrow = 3, page = 1) +
  theme_minimal() +
  labs(
    title = "Confronto Osservati vs Stimati (GAM)",
    subtitle = "I punti rappresentano i dati reali, la linea nera è la stima del modello",
    x = "Settimana (t)", y = "Delta OD", color = "Disegno (I_D)"
  ) +
  theme(
    strip.background = element_rect(fill = "gray95", color = NA),
    strip.text = element_text(face = "bold")
  )

dati_cumulati <- dati_gam %>%
  arrange(id_biomassa, tempo) %>%
  group_by(id_biomassa) %>%
  mutate(
    OD_Cumulato_Osservato = cumsum(incremento_OD),
    OD_Cumulato_Stimato   = cumsum(Valori_Stimati)
  ) %>%
  ungroup()

dati_cumulati %>%
  ggplot(aes(x = tempo)) +
  geom_point(aes(y = OD_Cumulato_Osservato, color = condizione_sperimentale), alpha = 0.6, size = 2) +
  geom_line(aes(y = OD_Cumulato_Stimato, group = id_biomassa), color = "black", linewidth = 0.8) +
  facet_wrap_paginate(~ id_biomassa, scales = "free_y", ncol = 3, nrow = 3, page = 1) +
  theme_minimal() +
  labs(
    title = "Confronto Osservati vs Stimati su Scala CUMULATA (Biomassa Totale)",
    subtitle = "I punti indicano il cumulato reale, la linea nera è l'integrale della stima GAM",
    x = "Settimana (t)", 
    y = "OD Cumulato (Biomassa)", 
    color = "Disegno (I_D)"
  ) +
  theme(
    strip.background = element_rect(fill = "gray95", color = NA),
    strip.text = element_text(face = "bold")
  )
