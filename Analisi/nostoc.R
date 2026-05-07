rm(list=ls());gc();

library(readxl)
library(dplyr)
library(ggplot2)

# Caricamento e pulizia
dati <- readxl::read_xlsx("Analisi/nostoc.xlsx", skip = 2)
# Rinominiamo le colonne (basandoci sulla struttura del file)
colnames(dati) <- c("row_id", "I", "D", "P", "tempo", "OD")

# Trasformiamo OD in numerico (gestisce eventuali "-" o caratteri non validi)
dati$OD <- as.numeric(dati$OD)

# Creiamo l'id_biomassa univoco per le 53 biomasse
dati$id_biomassa <- rep(1:53, each = 7)

# Creiamo l'indicatore di gruppo (combinazione fattori)
dati$condizione_sperimentale <- paste(dati$I,
                                      dati$D,
                                      dati$P,
                                      sep = "_")

table(dati$condizione_sperimentale)/7

# Analisi esplorativa ----

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

save(dati, dati_incrementi, dati_incrementi_rapporti,
     file = "dati_modificati.Rdata")
