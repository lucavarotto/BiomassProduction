library(ggplot2)

load("Analisi/dati_modificati.Rdata")


# Esplorative -------------------------------------------------------------

## Tutte le curve
# png(file = "Grafici/tutte_curve.png", width=1200, height=1200)
ggplot(dati, aes(x = tempo, y = OD)) +
  geom_line(aes(group = id_biomassa), na.rm = TRUE) +
  scale_color_viridis_d(option = "plasma", end = 0.9) +
  labs(
    title = "",
    subtitle = "",
    x = "Tempo (giorni)",
    y = "Resa di Biomassa (OD)",
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )
# dev.off()

## Curve medie
ggplot(dati, aes(x = tempo, y = OD, color = as.factor(condizione_sperimentale))) +
  # 1. Curve individuali sullo sfondo: forziamo il group su id_biomassa e il colore grigio
  geom_line(aes(group = id_biomassa), color = "gray85", alpha = 0.5, na.rm = TRUE) +
  geom_point(aes(group = id_biomassa), color = "gray85", alpha = 0.3, size = 1, na.rm = TRUE) +
  
  # 2. Linee delle MEDIE per ogni condizione sperimentale (spesse e colorate)
  stat_summary(fun = mean, geom = "line", linewidth = 1.2, na.rm = TRUE) +
  stat_summary(fun = mean, geom = "point", size = 2.5, na.rm = TRUE) +
  
  # Estetica del grafico
  labs(
    title = "Evoluzione della Densità Ottica: Profili Medi per Condizione",
    subtitle = "Le linee grigie sullo sfondo rappresentano le singole repliche biologiche",
    x = "Tempo (giorni)",
    y = "Optical Density (OD)",
    color = "Condizione Sperimentale"
  ) + 
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

## Per condizione sperimentale
ggplot(dati, aes(x = tempo, y = OD, color = as.factor(condizione_sperimentale), group = id_biomassa)) +
  geom_line(alpha = 0.7, na.rm = TRUE) +
  geom_point(alpha = 0.5, na.rm = TRUE) + # Opzionale: aggiunge i punti per chiarezza
  labs(
    title = "",
    x = "Tempo (giorni)",
    y = "Optical Density (OD)",
    color = "Condizione Sperimentale"
  ) + theme_minimal()
    

## Condizionate a I
ggplot(dati, aes(x = tempo, y = OD, color = as.factor(I), group = id_biomassa)) +
  geom_line(alpha = 0.7, na.rm = TRUE) +
  geom_point(alpha = 0.5, na.rm = TRUE) + # Opzionale: aggiunge i punti per chiarezza
  labs(
    title = "",
    x = "Tempo (giorni)",
    y = "Optical Density (OD)",
    color = "Fattore I"
  ) + theme_minimal()


## Condizionatw rispetto a D
ggplot(dati, aes(x = tempo, y = OD, color = as.factor(D), group = id_biomassa)) +
  geom_line(alpha = 0.7, na.rm = TRUE) +
  geom_point(alpha = 0.5, na.rm = TRUE) + # Opzionale: aggiunge i punti per chiarezza
  labs(
    title = "",
    x = "Tempo (giorni)",
    y = "Optical Density (OD)",
    color = "Fattore D"
  ) + theme_minimal()

## Condizionate rispetto a P
ggplot(dati, aes(x = tempo, y = OD, color = as.factor(P), group = id_biomassa)) +
  geom_line(alpha = 0.7, na.rm = TRUE) +
  geom_point(alpha = 0.5, na.rm = TRUE) + # Opzionale: aggiunge i punti per chiarezza
  labs(
    title = "",
    x = "Tempo (giorni)",
    y = "Optical Density (OD)",
    color = "Fattore P"
  ) + theme_minimal()

## Marginale I
ggplot(dati, aes(x = tempo, y = OD, color = as.factor(I), group = id_biomassa)) +
  geom_line(alpha = 0.7, na.rm = TRUE) +
  geom_point(alpha = 0.5, na.rm = TRUE) + # Opzionale: aggiunge i punti per chiarezza
  labs(
    title = "",
    x = "Tempo (giorni)",
    y = "Optical Density (OD)",
    color = "Fattore I"
  ) + theme_minimal()

## Marginale per D
ggplot(dati, aes(x = tempo, y = OD, color = as.factor(D), group = id_biomassa)) +
  geom_line(alpha = 0.7, na.rm = TRUE) +
  geom_point(alpha = 0.5, na.rm = TRUE) + # Opzionale: aggiunge i punti per chiarezza
  labs(
    title = "",
    x = "Tempo (giorni)",
    y = "Optical Density (OD)",
    color = "Fattore D"
  ) + theme_minimal()

## Marginale per P
ggplot(dati, aes(x = tempo, y = OD, color = as.factor(P), group = id_biomassa)) +
  geom_line(alpha = 0.7, na.rm = TRUE) +
  geom_point(alpha = 0.5, na.rm = TRUE) + # Opzionale: aggiunge i punti per chiarezza
  labs(
    title = "",
    x = "Tempo (giorni)",
    y = "Optical Density (OD)",
    color = "Fattore P"
  ) + theme_minimal()


## Dati mancanti
ggplot(subset(dati, id_biomassa %in% dati$id_biomassa[is.na(dati$OD)]), aes(x = tempo, y = OD, color = condizione_sperimentale, group = id_biomassa)) +
  geom_line(alpha = 0.7, na.rm = TRUE) +
  geom_point(alpha = 0.5, na.rm = TRUE) + # Opzionale: aggiunge i punti per chiarezza
  labs(
    title = "Dati mancanti",
    x = "Tempo (giorni)",
    y = "Optical Density (OD)",
    color = "Condizione sperimentale"
  ) +
  ylim(0,max(dati$OD, na.rm=T))+
  theme_minimal()

ggplot(dati, aes(x = tempo, y = OD, color = id_biomassa %in% dati$id_biomassa[is.na(dati$OD)], group = id_biomassa)) +
  geom_line(alpha = 0.7, na.rm = TRUE) +
  geom_point(alpha = 0.5, na.rm = TRUE) + # Opzionale: aggiunge i punti per chiarezza
  labs(
    title = "",
    x = "Tempo (giorni)",
    y = "Optical Density (OD)",
    color = "NA"
  ) +
  ylim(0,max(dati$OD, na.rm=T))+
  theme_minimal()

ggplot(subset(dati, I==90 & D==24 & (P == 1 | P==25.5) ), aes(x = tempo, y = OD, color = condizione_sperimentale, group = id_biomassa)) +
  geom_line(alpha = 0.7, na.rm = TRUE) +
  geom_point(alpha = 0.5, na.rm = TRUE) + # Opzionale: aggiunge i punti per chiarezza
  labs(
    title = "",
    x = "Tempo (giorni)",
    y = "Optical Density (OD)",
    color = "Condizione sperimentale"
  ) +
  theme_minimal()


# Grafico incrementi ------------------------------------------------------

# Differenze per consumo
ggplot(dati_incrementi, aes(x = tempo, y = incremento_OD, color = consumo)) +
  geom_line(aes(group = id_biomassa), alpha = 0.15, na.rm = TRUE) +
  stat_summary(fun = mean, geom = "line", size = 1.2, na.rm = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +
  scale_color_viridis_d(option = "plasma") +
  labs(
    title = "Velocità di crescita: Incrementi giornalieri di OD",
    subtitle = "L'incremento indica quanto la biomassa aumenta tra un giorno e il successivo",
    x = "Giorno (t)",
    y = "Delta OD (OD[t] - OD[t-1])",
    color = "Consumo (I*D)"
  ) +
  theme_minimal()

# Differenze per combinazione di condizioni sperimentali
ggplot(dati_incrementi, aes(x = tempo, y = incremento_OD, color = condizione_sperimentale)) +
  geom_line(aes(group = id_biomassa), alpha = 0.15, na.rm = TRUE) +
  stat_summary(fun = mean, geom = "line", size = 1.2, na.rm = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +
  scale_color_viridis_d(option = "plasma") +
  labs(
    title = "Velocità di crescita: Incrementi giornalieri di OD",
    subtitle = "L'incremento indica quanto la biomassa aumenta tra un giorno e il successivo",
    x = "Settimana (t)",
    y = "Delta OD (OD[t] - OD[t-1])",
    color = "Condizioni Sperimentali"
  ) +
  theme_minimal()

# Rapporti per comb. condiz. sperimentali
ggplot(dati_incrementi_rapporti, aes(x = tempo, y = incremento_OD, color = condizione_sperimentale)) +
  geom_line(aes(group = id_biomassa), alpha = 0.15, na.rm = TRUE) +
  stat_summary(fun = mean, geom = "line", size = 1.2, na.rm = TRUE) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", size = 1) +
  scale_color_viridis_d(option = "plasma") +
  labs(
    title = "Velocità di crescita: Incrementi giornalieri di OD",
    subtitle = "L'incremento indica quanto la biomassa aumenta tra un giorno e il successivo",
    x = "Settimana (t)",
    y = "Delta OD (OD[t] - OD[t-1])",
    color = "Condizioni Sperimentali"
  ) +
  theme_minimal()


# Grafico tridimensionale con predict della gompertz

# Caricamento librerie necessarie
library(plotly)
library(dplyr)

# In questo caso fisso il tempo (per esempio a 7, poi andrebbe trovato il t ottimale in cui fermarsi)
# e fisso anche il fosforo, quindi faccio variare solo I e D

# Creiamo una griglia densa per I, D e fissiamo P (o creiamo più slice)
i_seq <- seq(90, 470, length.out = 50)
d_seq <- seq(12, 24, length.out = 50)
p_valore_fisso <- 25.5  # Esempio: visualizziamo la sezione centrale di P

# 1. Creiamo la griglia con i valori CENTRATI (come il modello si aspetta)
grid <- expand.grid(
  I = seq(90 - 280, 470 - 280, length.out = 50), # Range: -190 a +190
  D = seq(12 - 18, 24 - 18, length.out = 50),    # Range: -6 a +6
  P = 0,                               # Fissato a 0 (che è il vecchio 25.5)
  tempo = max(dati_m_gompertz$tempo)
)

# 2. Predizione
grid$Risposta_Predetta <- predict(modello_gompertz, newdata = grid, level = 0)

# 3. Trasformazione per il grafico (Matrice Z)
z_matrix <- matrix(grid$Risposta_Predetta, nrow = 50, ncol = 50)

# 4. GRAFICO: Usiamo i valori REALI per gli assi X e Y per leggibilità
plot_ly(x = ~seq(90, 470, length.out = 50), # Valori originali per l'asse X
        y = ~seq(12, 24, length.out = 50),  # Valori originali per l'asse Y
        z = ~z_matrix) %>%
  add_surface() %>%
  layout(
    scene = list(
      xaxis = list(title = "Intensità Luminosa (I) REALE"),
      yaxis = list(title = "Durata (D) REALE"),
      zaxis = list(title = "Risposta (OD)")
    ),
    title = paste0("Superficie di Risposta P = ", p_valore_fisso)
  )

# -------------------------------------------------------------------------
# Al variare di P

# Parametri fissi
tempo_target <- max(dati_m_gompertz$tempo)
p_levels <- c(1, 25.5, 50)
i_seq_real <- seq(90, 470, length.out = 40) # Diminuito leggermente per fluidità
d_seq_real <- seq(12, 24, length.out = 40)

# 1. Funzione per generare i dati di ogni superficie
get_surface_data <- function(val_p) {
  grid_p <- expand.grid(
    I = i_seq_real - 280,
    D = d_seq_real - 18,
    P = val_p - 25.5,
    tempo = tempo_target
  )
  grid_p$Risposta_Predetta <- predict(modello_gompertz, newdata = grid_p, level = 0)
  return(matrix(grid_p$Risposta_Predetta, nrow = 40, ncol = 40))
}

# 2. Creazione del grafico con scene multiple
plot_ly() %>%
  # Superficie 1 (P = 1)
  add_surface(z = ~get_surface_data(1), x = ~i_seq_real, y = ~d_seq_real, 
              scene = "scene1", name = "P=1", colorscale = "Viridis", showscale = FALSE) %>%
  # Superficie 2 (P = 25.5)
  add_surface(z = ~get_surface_data(25.5), x = ~i_seq_real, y = ~d_seq_real, 
              scene = "scene2", name = "P=25.5", colorscale = "Viridis", showscale = FALSE) %>%
  # Superficie 3 (P = 50)
  add_surface(z = ~get_surface_data(50), x = ~i_seq_real, y = ~d_seq_real, 
              scene = "scene3", name = "P=50", colorscale = "Viridis", showscale = TRUE) %>%
  layout(
    title = "Confronto Superfici di Risposta al variare di P",
    # Configurazione spaziale delle tre scene
    grid = list(rows = 1, columns = 3, pattern = 'independent'),
    scene = list(domain = list(column = 0), xaxis = list(title = "I"), yaxis = list(title = "D"), zaxis = list(title = "OD"), title = "P=1"),
    scene2 = list(domain = list(column = 1), xaxis = list(title = "I"), yaxis = list(title = "D"), zaxis = list(title = "OD"), title = "P=25.5"),
    scene3 = list(domain = list(column = 2), xaxis = list(title = "I"), yaxis = list(title = "D"), zaxis = list(title = "OD"), title = "P=50")
  )