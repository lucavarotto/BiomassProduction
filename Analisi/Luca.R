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
# Controlla lo spostamento della curva lungo l'asse X (il tempo). Un valore di b2 più alto "sposta" la curva verso destra, indicando che la crescita inizia più tardi.

# b3 rappresenta la velocità o il tasso di crescita intrinseco.
# Se b3 è alto, la curva sarà molto ripida (crescita esplosiva); se b3 è basso, la curva salirà molto dolcemente.

dati_m_gompertz <- na.omit(dati)
dati_m_gompertz <- dati_m_gompertz |> mutate(
  I = I - 280,
  D = D - 18,
  P = P - 25.5
)

library(nlme)

# Modello con interazioni complete sui parametri Asym e b3
modello_gompertz <- nlme(
  model = OD ~ SSgompertz(tempo, Asym, b2, b3),
  data = dati_m_gompertz,
  fixed = list(
    Asym ~ (I+D)^2+P,
    b2 ~ (I+D)^2+P,           
    b3 ~ (I+D)^2+P     
  ),
  random = Asym ~ 1 | id_biomassa,
  # Nota: i valori di start devono ora includere gli zeri per i nuovi termini di interazione
  # In un modello I*D*P ci sono 8 coefficienti (intercetta + 3 principali + 3 doppie + 1 tripla)
  start = c(
    Asym = c(5, rep(0, 4)), 
    b2 = c(2, rep(0, 4)),     
    b3 = c(0.8, rep(0, 4)) )
)

summary(modello_gompertz)

dati_m_gompertz$predicted <- predict(modello_gompertz)

# Selezioniamo alcuni gruppi (id_biomassa) rappresentativi per non affollare il grafico
set.seed(123) # Per riproducibilità
ids_da_mostrare <- 1:53

dati_plot <- dati_m_gompertz %>%
  filter(id_biomassa %in% ids_da_mostrare)

library(ggforce)
ggplot(dati_plot, aes(x = tempo, color = as.factor(condizione_sperimentale))) +
  geom_point(aes(y = OD), alpha = 0.6) +
  geom_line(aes(y = OD, group = id_biomassa), linetype = "dashed", alpha = 0.4) +
  geom_line(aes(y = predicted, group = id_biomassa), linewidth = 1.2)+
  # SOSTITUIAMO facet_wrap con facet_wrap_paginate
  facet_wrap_paginate(~ id_biomassa, scales = "free_y", ncol = 4, nrow = 3, page = 4) +
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
    Asym ~ (I + D + P)^2,  # Interazione per l'asintoto
    b2 ~ (I + D + P)^2,    # Interazione per la latenza (richiesta)
    b3 ~ I+D+P     # Interazione per la rapidità
  ),
  # Dobbiamo aggiornare gli start: ora abbiamo 8 parametri per OGNI componente
  # (Intercetta, I, D, P, I:D, I:P, D:P, I:D:P) x 3 parametri della Gompertz = 24 coefficienti
  start = c(
    Asym = c(3, rep(0, 6)),  
    b2   = c(1.5, rep(0, 6)),               
    b3   = c(0.8, rep(0, 3))
  )
)

anova(modello_gompertz_fisso)

summary(modello_gompertz_fisso)
coef(modello_gompertz_fisso) |> round(4)

dati_m_gompertz$predicted_fisso <- predict(modello_gompertz)

# Selezioniamo alcuni gruppi (id_biomassa) rappresentativi per non affollare il grafico
set.seed(123) # Per riproducibilità
ids_da_mostrare <- 1:53

dati_plot <- dati_m_gompertz %>%
  filter(id_biomassa %in% ids_da_mostrare)

library(ggforce)
ggplot(dati_plot, aes(x = tempo, color = as.factor(condizione_sperimentale))) +
  geom_point(aes(y = OD), alpha = 0.6) +
  geom_line(aes(y = OD, group = id_biomassa), linetype = "dashed", alpha = 0.4) +
  geom_line(aes(y = predicted_fisso, group = id_biomassa), linewidth = 1.2)+
  # SOSTITUIAMO facet_wrap con facet_wrap_paginate
  facet_wrap_paginate(~ id_biomassa, scales = "free_y", ncol = 4, nrow = 3, page = 1) +
  scale_color_viridis_d(option = "plasma", guide = "none") +
  labs(
    title = "Confronto Dati Reali vs Stima Gompertz",
    subtitle = "I punti indicano le osservazioni reali; le linee continue rappresentano il modello NLME",
    x = "Tempo (giorni)",
    y = "Optical Density (OD)"
  ) +
  theme_minimal()

matrix(coef(modello_gompertz_fisso), nrow=3, byrow=T) %*%
  t(as.matrix(model.matrix(~(I + D)^2+P, data=dati[1,])))
