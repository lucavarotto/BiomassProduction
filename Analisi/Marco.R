rm(list=ls());gc();

load("Analisi/dati_modificati.Rdata")

library(readxl)
library(dplyr)
library(ggplot2)
library(MuMIn)
library(nlme)

# # Caricamento e pulizia
# dati <- readxl::read_xlsx("Analisi/nostoc.xlsx", skip = 2)
# # Rinominiamo le colonne (basandoci sulla struttura del file)
# colnames(dati) <- c("row_id", "I", "D", "P", "tempo", "OD")
# 
# # Trasformiamo OD in numerico (gestisce eventuali "-" o caratteri non validi)
# dati$OD <- as.numeric(dati$OD)
# 
# # Creiamo l'id_biomassa univoco per le 53 biomasse
# dati$id_biomassa <- rep(1:53, each = 7)
# 
# # Creiamo l'indicatore di gruppo (combinazione fattori)
# dati$condizione_sperimentale <- paste(dati$I,
#                                       dati$D,
#                                       dati$P,
#                                       sep = "_")
# 
# table(dati$condizione_sperimentale)/7
# 
# # Analisi esplorativa ----
# 
## Media globale ----

ggplot(dati, aes(x = tempo, y = OD)) +
  geom_line(aes(group = id_biomassa), alpha = 0.15, na.rm = TRUE) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.2, na.rm = TRUE) +
  scale_color_viridis_d(option = "plasma", end = 0.9) +
  labs(
    title = "Impatto del Consumo Energetico (I x D) sulla Crescita",
    subtitle = "Le linee spesse indicano la media per ogni livello di intensità luminosa totale",
    x = "Tempo (giorni)",
    y = "Resa di Biomassa (OD)",
    color = "Consumo (I*D)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

## Marginale ----

library(ggplot2)

ggplot(dati, aes(x = tempo, y = OD, color = as.factor(I), group = id_biomassa)) +
  geom_line(alpha = 0.7, na.rm = TRUE) +
  geom_point(alpha = 0.5, na.rm = TRUE) + # Opzionale: aggiunge i punti per chiarezza
  labs(
    title = "Traiettorie di crescita per il fattore I",
    subtitle = "Ogni linea rappresenta una delle 53 biomasse",
    x = "Tempo (giorni)",
    y = "Optical Density (OD)",
    color = "Fattore I"
  ) + theme_minimal()
table(dati$I)/7

ggplot(dati, aes(x = tempo, y = OD, color = as.factor(D), group = id_biomassa)) +
  geom_line(alpha = 0.7, na.rm = TRUE) +
  geom_point(alpha = 0.5, na.rm = TRUE) + # Opzionale: aggiunge i punti per chiarezza
  labs(
    title = "Traiettorie di crescita per il fattore D",
    subtitle = "Ogni linea rappresenta una delle 53 biomasse",
    x = "Tempo (giorni)",
    y = "Optical Density (OD)",
    color = "Fattore D"
  ) + theme_minimal()
table(dati$D)/7

ggplot(dati, aes(x = tempo, y = OD, color = as.factor(P), group = id_biomassa)) +
  geom_line(alpha = 0.7, na.rm = TRUE) +
  geom_point(alpha = 0.5, na.rm = TRUE) + # Opzionale: aggiunge i punti per chiarezza
  labs(
    title = "Traiettorie di crescita per il fattore I",
    subtitle = "Ogni linea rappresenta una delle 53 biomasse",
    x = "Tempo (giorni)",
    y = "Optical Density (OD)",
    color = "Fattore P"
  ) + theme_minimal()
table(dati$P)/7

## Congiunta ----

ggplot(dati, aes(x = tempo, y = OD, color = condizione_sperimentale, group = id_biomassa)) +
  geom_line(alpha = 0.7, na.rm = TRUE) +
  geom_point(alpha = 0.5, na.rm = TRUE) + # Opzionale: aggiunge i punti per chiarezza
  labs(
    title = "Traiettorie di crescita per combinazione di fattori",
    subtitle = "Ogni linea rappresenta una delle 53 biomasse",
    x = "Tempo (giorni)",
    y = "Optical Density (OD)",
    color = "Fattori (I_D_P)"
  ) +
  theme_minimal()

ggplot(dati, aes(x = tempo, y = OD, color = condizione_sperimentale)) +
  geom_line(aes(group = id_biomassa), alpha = 0.2, na.rm = TRUE) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.5, na.rm = TRUE) +
  stat_summary(fun = mean, geom = "point", size = 2, na.rm = TRUE) +
  labs(
    title = "Traiettorie medie per condizione sperimentale",
    subtitle = "Le linee spesse rappresentano la media di gruppo; le linee chiare le singole biomasse",
    x = "Tempo (giorni)",
    y = "Optical Density (OD)",
    color = "Fattori (I_D_P)"
  ) +
  theme_minimal()

## Condizionate ----

dati$condizione_sperimentale2 <- paste(dati$I, dati$D, sep = "_")

ggplot(dati, aes(x = tempo, y = OD, color = condizione_sperimentale2)) +
  geom_line(aes(group = id_biomassa), alpha = 0.2, na.rm = TRUE) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.5, na.rm = TRUE) +
  stat_summary(fun = mean, geom = "point", size = 2, na.rm = TRUE) +
  labs(
    title = "Traiettorie medie per condizione sperimentale",
    subtitle = "Le linee spesse rappresentano la media di gruppo; le linee chiare le singole biomasse",
    x = "Tempo (giorni)",
    y = "Optical Density (OD)",
    color = "Fattori (I_D_P)"
  ) + theme_minimal()

ggplot(dati, aes(x = tempo, y = OD, color = condizione_sperimentale2)) +
  geom_line(aes(group = id_biomassa), alpha = 0.2, na.rm = TRUE) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.5, na.rm = TRUE) +
  stat_summary(fun = mean, geom = "point", size = 2, na.rm = TRUE) +
  facet_wrap(~I)+
  labs(
    x = "Tempo (giorni)",
    y = "Optical Density (OD)",
    color = "Fattori (I_D_P)"
  ) +
  theme_minimal()

ggplot(dati, aes(x = tempo, y = OD, color = condizione_sperimentale2)) +
  geom_line(aes(group = id_biomassa), alpha = 0.2, na.rm = TRUE) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.5, na.rm = TRUE) +
  stat_summary(fun = mean, geom = "point", size = 2, na.rm = TRUE) +
  facet_wrap(~P)+
  labs(
    x = "Tempo (giorni)",
    y = "Optical Density (OD)",
    color = "Fattori (I_D_P)"
  ) +
  theme_minimal()

## Esplorazione outliers ----

ggplot(subset(dati, condizione_sperimentale=="470_18_25.5"), aes(x = tempo, y = OD, color = condizione_sperimentale, group = id_biomassa)) +
  geom_line(alpha = 0.7, na.rm = TRUE) +
  geom_point(alpha = 0.5, na.rm = TRUE) + # Opzionale: aggiunge i punti per chiarezza
  labs(
    title = "Traiettorie di crescita per combinazione di fattori",
    subtitle = "Ogni linea rappresenta una delle 53 biomasse",
    x = "Tempo (giorni)",
    y = "Optical Density (OD)",
    color = "Fattori (I_D_P)"
  ) +
  theme_minimal()

## Esplorazione NA ----

ggplot(subset(dati, id_biomassa %in% dati$id_biomassa[is.na(dati$OD)]), aes(x = tempo, y = OD, color = condizione_sperimentale, group = id_biomassa)) +
  geom_line(alpha = 0.7, na.rm = TRUE) +
  geom_point(alpha = 0.5, na.rm = TRUE) + # Opzionale: aggiunge i punti per chiarezza
  labs(
    title = "Traiettorie di crescita per combinazione di fattori",
    subtitle = "Ogni linea rappresenta una delle 53 biomasse",
    x = "Tempo (giorni)",
    y = "Optical Density (OD)",
    color = "Fattori (I_D_P)"
  ) +
  ylim(0,max(dati$OD, na.rm=T))+
  theme_minimal()

ggplot(dati, aes(x = tempo, y = OD, color = id_biomassa %in% dati$id_biomassa[is.na(dati$OD)], group = id_biomassa)) +
  geom_line(alpha = 0.7, na.rm = TRUE) +
  geom_point(alpha = 0.5, na.rm = TRUE) + # Opzionale: aggiunge i punti per chiarezza
  labs(
    title = "Traiettorie di crescita per combinazione di fattori",
    subtitle = "Ogni linea rappresenta una delle 53 biomasse",
    x = "Tempo (giorni)",
    y = "Optical Density (OD)",
    color = "Fattori (I_D_P)"
  ) +
  ylim(0,max(dati$OD, na.rm=T))+
  theme_minimal()

ggplot(subset(dati, I==90 & D==24 & (P == 1 | P==25.5) ), aes(x = tempo, y = OD, color = condizione_sperimentale, group = id_biomassa)) +
  geom_line(alpha = 0.7, na.rm = TRUE) +
  geom_point(alpha = 0.5, na.rm = TRUE) + # Opzionale: aggiunge i punti per chiarezza
  labs(
    title = "Traiettorie di crescita per combinazione di fattori",
    subtitle = "Ogni linea rappresenta una delle 53 biomasse",
    x = "Tempo (giorni)",
    y = "Optical Density (OD)",
    color = "Fattori (I_D_P)"
  ) +
  theme_minimal()






library(viridis)
dati$consumo <- as.factor(dati$I * dati$D)

ggplot(dati, aes(x = tempo, y = OD, color = consumo)) +
  geom_line(aes(group = id_biomassa), alpha = 0.15, na.rm = TRUE) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.2, na.rm = TRUE) +
  scale_color_viridis_d(option = "plasma", end = 0.9) +
  labs(
    title = "Impatto del Consumo Energetico (I x D) sulla Crescita",
    subtitle = "Le linee spesse indicano la media per ogni livello di intensità luminosa totale",
    x = "Tempo (giorni)",
    y = "Resa di Biomassa (OD)",
    color = "Consumo (I*D)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

# Dati incrementali: differenze ----

library(dplyr)

dati_incrementi <- dati %>%
  group_by(id_biomassa) %>%
  arrange(tempo) %>%
  mutate(
    incremento_OD = OD - lag(OD),
    intervallo = paste0(lag(tempo), "-", tempo)
  ) |>
  filter(!is.na(incremento_OD)) |> # Rimuoviamo il tempo 0 (che non ha incremento)
  ungroup()

# Visualizzazione della struttura del nuovo dataset (6 righe per biomassa)
# str(dati_incrementi)

# 2. Grafico degli incrementi con palette Plasma
ggplot(dati_incrementi, aes(x = tempo, y = incremento_OD, color = consumo)) +
  # Linee sottili per le singole biomasse
  geom_line(aes(group = id_biomassa), alpha = 0.15, na.rm = TRUE) +
  
  # Linea spessa della media per livello di consumo
  stat_summary(fun = mean, geom = "line", size = 1.2, na.rm = TRUE) +
  
  # Palette Plasma (da viridis)
  scale_color_viridis_d(option = "plasma") +
  
  labs(
    title = "Velocità di crescita: Incrementi giornalieri di OD",
    subtitle = "L'incremento indica quanto la biomassa aumenta tra un giorno e il successivo",
    x = "Giorno (t)",
    y = "Delta OD (OD[t] - OD[t-1])",
    color = "Consumo (I*D)"
  ) +
  theme_minimal()


# Dati incrementali: rapporti ----

dati_incrementi_rapporti <- dati %>%
  group_by(id_biomassa) %>%
  arrange(tempo) %>%
  mutate(
    incremento_OD = OD / lag(OD),
    intervallo = paste0(lag(tempo), "/", tempo)
  ) |>
  filter(!is.na(incremento_OD)) |> # Rimuoviamo il tempo 0 (che non ha incremento)
  ungroup()

# Visualizzazione della struttura del nuovo dataset (6 righe per biomassa)
# str(dati_incrementi)

# 2. Grafico degli incrementi con palette Plasma
ggplot(dati_incrementi_rapporti, aes(x = tempo, y = incremento_OD, color = consumo)) +
  # Linee sottili per le singole biomasse
  geom_line(aes(group = id_biomassa), alpha = 0.15, na.rm = TRUE) +
  
  # Linea spessa della media per livello di consumo
  stat_summary(fun = mean, geom = "line", size = 1.2, na.rm = TRUE) +
  
  # Palette Plasma (da viridis)
  scale_color_viridis_d(option = "plasma") +
  
  labs(
    title = "Velocità di crescita: Incrementi giornalieri di OD",
    subtitle = "L'incremento indica quanto la biomassa aumenta tra un giorno e il successivo",
    x = "Giorno (t)",
    y = "Delta OD (OD[t] - OD[t-1])",
    color = "Consumo (I*D)"
  ) +
  theme_minimal()

# aggiungo alcune linee di commenti per testare il successo del commit + push

dati_m_gompertz <- na.omit(dati)
dati_m_gompertz <- dati_m_gompertz |> mutate(
  I = I - 280,
  D = D - 18,
  P = P - 25.5
)

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
  facet_wrap_paginate(~ id_biomassa, scales = "free_y", ncol = 4, nrow = 3, page = 2) +
  scale_color_viridis_d(option = "plasma", guide = "none") +
  labs(
    title = "Confronto Dati Reali vs Stima Gompertz",
    subtitle = "I punti indicano le osservazioni reali; le linee continue rappresentano il modello NLME",
    x = "Tempo (giorni)",
    y = "Optical Density (OD)"
  ) +
  theme_minimal()

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

modello_gompertz %>% AIC
# modello_gompertz_fisso %>% AIC
# dati_m_gompertz$predicted_fisso <- predict(modello_gompertz)
dati_m_gompertz$predicted <- predict(modello_gompertz)

set.seed(123) # Per riproducibilità
ids_da_mostrare <- 1:53

dati_plot <- dati_m_gompertz %>%
  filter(id_biomassa %in% ids_da_mostrare)

library(ggforce)
x11()
ggplot(dati_plot, aes(x = tempo, color = as.factor(condizione_sperimentale))) +
  geom_point(aes(y = OD), alpha = 0.6) +
  geom_line(aes(y = OD, group = id_biomassa), linetype = "dashed", alpha = 0.4) +
  geom_line(aes(y = predicted, group = id_biomassa), linewidth = 1.2)+
  facet_wrap_paginate(~ id_biomassa, scales = "free_y", ncol = 3, nrow = 3, page = 1) +
  scale_color_viridis_d(option = "plasma", guide = "none") +
  labs(
    title = "Confronto Dati Reali vs Stima Gompertz",
    subtitle = "I punti indicano le osservazioni reali; le linee continue rappresentano il modello NLME",
    x = "Tempo (giorni)",
    y = "Optical Density (OD)"
  ) +
  theme_minimal()

summary(modello_gompertz)
# Controlla i residui per unità
plot(modello_gompertz)

# Vedi quali unità hanno residui grandi
residuals(modello_gompertz, type = "normalized") |> 
  abs() |> 
  sort(decreasing = TRUE) |> 
  head(10)

# Controlla i BLUP stimati
random.effects(modello_gompertz)
# Visualizza le curve delle unità problematiche
dati_m_gompertz |> 
  filter(id_biomassa %in% c(45, 36, 37, 52)) |> 
  ggplot(aes(x = tempo, y = OD, color = as.factor(id_biomassa))) +
  geom_point() +
  geom_line(aes(y = predict(modello_gompertz)[
    dati_m_gompertz$id_biomassa %in% c(45, 36, 37, 52)
  ])) +
  facet_wrap(~ id_biomassa, scales = "free_y")

# previsione delle nuove curve

# Valori osservati di I, D, P nel disegno sperimentale
library(marginaleffects)
pred_curve <- predictions(
  modello_gompertz,
  newdata = datagrid(
    tempo = 0:6,  # i 7 punti osservati
    I = unique(dati_m_gompertz$I),
    D = unique(dati_m_gompertz$D),
    P = unique(dati_m_gompertz$P),
    id_biomassa = dati_m_gompertz$id_biomassa[1]
  ),
  re.form = NA
)

pred_curve |> 
  as_tibble() |>
  select(I, D, P, tempo, estimate, conf.low, conf.high) |>
  group_by(I, D, P) |>
  group_split()

# previsione delle nuove curve, vedendo se all'aggiunta di nuove settimane si raggiunge plateau
pred_curve_10s <- predictions(
  modello_gompertz,
  newdata = datagrid(
    tempo = 0:9,  # i 7 punti osservati
    I = unique(dati_m_gompertz$I),
    D = unique(dati_m_gompertz$D),
    P = unique(dati_m_gompertz$P),
    id_biomassa = dati_m_gompertz$id_biomassa[1]
  ),
  re.form = NA
)

pred_curve_10s |> 
  as_tibble() |>
  select(I, D, P, tempo, estimate, conf.low, conf.high) |>
  group_by(I, D, P) |>
  group_split()

# guardiamo comparisons
library(marginaleffects)

# Effetto di I tenendo D e P ai loro valori unici
comp_I <- comparisons(
  modello_gompertz,
  newdata = datagrid(
    tempo = Inf,
    I = unique(dati_m_gompertz$I),
    D = unique(dati_m_gompertz$D),
    P = unique(dati_m_gompertz$P),
    id_biomassa = dati_m_gompertz$id_biomassa[1]
  ),
  variables = "I",
  re.form = NA
)

comp_I |> as_tibble() |> select(I, D, P, contrast, estimate, conf.low, conf.high, p.value) |> print(n = 27)

library(marginaleffects)

# Effetto di D tenendo I e P ai loro valori unici
comp_D <- comparisons(
  modello_gompertz,
  newdata = datagrid(
    tempo = Inf,
    I = unique(dati_m_gompertz$I),
    D = unique(dati_m_gompertz$D),
    P = unique(dati_m_gompertz$P),
    id_biomassa = dati_m_gompertz$id_biomassa[1]
  ),
  variables = "D",
  re.form = NA
)

comp_D |> as_tibble() |> select(I, D, P, contrast, estimate, conf.low, conf.high, p.value)|> print(n = 27)

library(marginaleffects)

# Effetto di P tenendo D e I ai loro valori unici
comp_P <- comparisons(
  modello_gompertz,
  newdata = datagrid(
    tempo = Inf,
    I = unique(dati_m_gompertz$I),
    D = unique(dati_m_gompertz$D),
    P = unique(dati_m_gompertz$P),
    id_biomassa = dati_m_gompertz$id_biomassa[1]
  ),
  variables = "P",
  re.form = NA
)

comp_P |> as_tibble() |> select(I, D, P, contrast, estimate, conf.low, conf.high, p.value) |> print(n = 27)

# avg comparisons
avg_comparisons(
  modello_gompertz,
  newdata = datagrid(
    tempo = Inf,
    I = unique(dati_m_gompertz$I),
    D = unique(dati_m_gompertz$D),
    P = unique(dati_m_gompertz$P),
    id_biomassa = dati_m_gompertz$id_biomassa[1]
  ),
  variables = c("I", "D", "P"),
  re.form = NA
)

View(dati)

# previsione su griglia di valori out of sample
pred_curve_nuove <- predictions(
  modello_gompertz,
  newdata = datagrid(
    tempo = 0:6,  # i 7 punti osservati
    I = seq(570,670,by = 10) - 280,
    D = seq(21,27,by=1) - 18,
    P = seq(60,75,by=3) - 25.5,
    id_biomassa = dati_m_gompertz$id_biomassa[1]
  ),
  re.form = NA
)

pred_curve_nuove |> 
  as_tibble() |>
  select(I, D, P, tempo, estimate, conf.low, conf.high) |>
  group_by(I, D, P) |>
  slice_tail(n = 1) |>
  ungroup() |>
  arrange(desc(estimate))
