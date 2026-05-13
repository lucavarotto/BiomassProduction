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
