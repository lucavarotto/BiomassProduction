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

dati_m_gompertz2 <- dati_m_gompertz |> mutate(
  I = (I - 280)/190,
  D = (D - 18)/6,
  P = (P - 25.5)/24.5
)

library(nlme)

modello_gompertz <- nlme(
  model = OD ~ SSgompertz(tempo, Asym, b2, b3),
  data = dati_m_gompertz2,
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
  control = nlmeControl(msMaxIter = 200, # si riferisce all'ottimizzatore numerico
                        maxIter = 500, # Il numero massimo di iterazioni durante la
                        # fase di ottimizzazione della stima di massima verosimiglianza
                        pnlsMaxIter = 25 # il modello tiene fissi
                        # gli effetti casuali e cerca di aggiornare
                        # i parametri fissi per minimizzare l'errore.
  )
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

dati_m_gompertz2$predicted <- predict(modello_gompertz)
library(ggforce)
#x11()
ggplot(dati_m_gompertz2, aes(x = tempo, color = as.factor(condizione_sperimentale))) +
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
  data = dati_m_gompertz2,
  params = list(
    Asym ~ (I + D)^2+P, # Interazione per l'asintoto
    b2 ~ (I + D)^2+P, # Interazione per la latenza
    b3 ~ (I + D)^2+P # Interazione per la rapidità
  ),
  start = c(
    Asym = c(3, rep(0, 4)),  
    b2   = c(1, rep(0, 4)),               
    b3   = c(0.8, rep(0, 4))
  )
)

AIC(modello_gompertz, modello_gompertz_fisso)

anova(modello_gompertz_fisso)

summary(modello_gompertz_fisso)
coef(modello_gompertz_fisso) |> round(4)

dati_m_gompertz2$predicted_fisso <- predict(modello_gompertz)

library(ggforce)
ggplot(dati_m_gompertz2, aes(x = tempo, color = as.factor(condizione_sperimentale))) +
  geom_point(aes(y = OD), alpha = 0.6) +
  geom_line(aes(y = OD, group = id_biomassa), linetype = "dashed", alpha = 0.4) +
  geom_line(aes(y = predicted_fisso, group = id_biomassa), linewidth = 1.2)+
  facet_wrap_paginate(~ id_biomassa, scales = "free_y", ncol = 4, nrow = 3, page = 1) +
  scale_color_viridis_d(option = "plasma", guide = "none") +
  labs(
    title = "Confronto Dati Reali vs Stima Gompertz",
    subtitle = "I punti indicano le osservazioni reali; le linee continue rappresentano il modello NLME",
    x = "Tempo (giorni)",
    y = "Optical Density (OD)"
  ) + theme_minimal()

matrix(coef(modello_gompertz_fisso), nrow=3, byrow=T) %*%
  t(as.matrix(model.matrix(~(I + D)^2+P, data=dati[1,])))

# aggiungiamo termine quadratico al modello fisso
gomp_quad <- gnls(
  model = OD ~ SSgompertz(tempo, Asym, b2, b3),
  data = dati_m_gompertz2,
  params = list(
    Asym ~ (I + D)^2+P + I(I^2) + I(D^2) + I(P^2), # Interazione per l'asintoto
    b2 ~ (I + D)^2+P + I(I^2) + I(D^2) + I(P^2), # Interazione per la latenza
    b3 ~ (I + D)^2+P + I(I^2) + I(D^2) + I(P^2) # Interazione per la rapidità
  ),
  start = c(
    Asym = c(3, rep(0, 7)),  
    b2   = c(1.5, rep(0, 7)),               
    b3   = c(0.8, rep(0, 7))
  )
)

dati_m_gompertz2$pred_quad <- predict(gomp_quad)

library(ggforce)
ggplot(dati_m_gompertz2, aes(x = tempo, color = as.factor(condizione_sperimentale))) +
  geom_point(aes(y = OD), alpha = 0.6) +
  geom_line(aes(y = OD, group = id_biomassa), linetype = "dashed", alpha = 0.4) +
  geom_line(aes(y = predicted_fisso, group = id_biomassa), linewidth = 1.2)+
  facet_wrap_paginate(~ id_biomassa, scales = "free_y", ncol = 4, nrow = 3, page = 5) +
  scale_color_viridis_d(option = "plasma", guide = "none") +
  labs(
    title = "Confronto Dati Reali vs Stima Gompertz",
    subtitle = "I punti indicano le osservazioni reali; le linee continue rappresentano il modello NLME",
    x = "Tempo (giorni)",
    y = "Optical Density (OD)"
  ) + theme_minimal()

ggplot(dati_m_gompertz2, aes(x = tempo, color = as.factor(condizione_sperimentale))) +
  geom_point(aes(y = OD), alpha = 0.6) +
  geom_line(aes(y = OD, group = id_biomassa), linetype = "dashed", alpha = 0.4) +
  geom_line(aes(y = pred_quad, group = id_biomassa), linewidth = 1.2)+
  facet_wrap_paginate(~ id_biomassa, scales = "free_y", ncol = 4, nrow = 3, page = 5) +
  scale_color_viridis_d(option = "plasma", guide = "none") +
  labs(
    title = "Confronto Dati Reali vs Stima Gompertz",
    subtitle = "I punti indicano le osservazioni reali; le linee continue rappresentano il modello NLME",
    x = "Tempo (giorni)",
    y = "Optical Density (OD)"
  ) + theme_minimal()

AIC(modello_gompertz_fisso, gomp_quad)
BIC(modello_gompertz_fisso, gomp_quad)

anova(gomp_quad)

#tolto I:D su b3
gomp_quad2 <- gnls(
  model = OD ~ SSgompertz(tempo, Asym, b2, b3),
  data = dati_m_gompertz2,
  params = list(
    Asym ~ (I + D)^2+P + I(I^2) + I(D^2) + I(P^2), # Interazione per l'asintoto
    b2 ~ (I + D)^2+P + I(I^2) + I(D^2) + I(P^2), # Interazione per la latenza
    b3 ~ I + D + P + I(I^2) + I(D^2) + I(P^2) # Interazione per la rapidità
  ),
  start = c(
    Asym = c(3, rep(0, 7)),  
    b2   = c(1.5, rep(0, 7)),               
    b3   = c(0.8, rep(0, 6))
  )
)

AIC(gomp_quad, gomp_quad2)
BIC(gomp_quad, gomp_quad2)

anova(gomp_quad2)

#tolto I2 su b3 --> no
gomp_quad3 <- gnls(
  model = OD ~ SSgompertz(tempo, Asym, b2, b3),
  data = dati_m_gompertz2,
  params = list(
    Asym ~ (I + D)^2+P + I(I^2) + I(D^2) + I(P^2), # Interazione per l'asintoto
    b2 ~ (I + D)^2+P + I(I^2) + I(D^2) + I(P^2), # Interazione per la latenza
    b3 ~ I +  D + P + I(D^2) + I(P^2) # Interazione per la rapidità
  ),
  start = c(
    Asym = c(3, rep(0, 7)),  
    b2   = c(1.5, rep(0, 7)),               
    b3   = c(0.8, rep(0, 5))
  )
)

AIC(gomp_quad2, gomp_quad3)
BIC(gomp_quad2, gomp_quad3)

anova(gomp_quad2)

#teniamo tutto

# effetti casuali per ID ####
#su asintoto
gomp_quad_re <- update(modello_gompertz,
                            random = Asym ~ 1 | id_biomassa)
dati_m_gompertz2$pred_re <- predict(gomp_quad_re)

library(ggforce)
ggplot(dati_m_gompertz2, aes(x = tempo, color = as.factor(condizione_sperimentale))) +
  geom_point(aes(y = OD), alpha = 0.6) +
  geom_line(aes(y = OD, group = id_biomassa), linetype = "dashed", alpha = 0.4) +
  geom_line(aes(y = pred_quad, group = id_biomassa), linewidth = 1.2)+
  facet_wrap_paginate(~ id_biomassa, scales = "free_y", ncol = 4, nrow = 3, page = 5) +
  scale_color_viridis_d(option = "plasma", guide = "none") +
  labs(
    title = "Confronto Dati Reali vs Stima Gompertz",
    subtitle = "I punti indicano le osservazioni reali; le linee continue rappresentano il modello NLME",
    x = "Tempo (giorni)",
    y = "Optical Density (OD)"
  ) + theme_minimal()

ggplot(dati_m_gompertz2, aes(x = tempo, color = as.factor(condizione_sperimentale))) +
  geom_point(aes(y = OD), alpha = 0.6) +
  geom_line(aes(y = OD, group = id_biomassa), linetype = "dashed", alpha = 0.4) +
  geom_line(aes(y = pred_re, group = id_biomassa), linewidth = 1.2)+
  facet_wrap_paginate(~ id_biomassa, scales = "free_y", ncol = 4, nrow = 3, page = 5) +
  scale_color_viridis_d(option = "plasma", guide = "none") +
  labs(
    title = "Confronto Dati Reali vs Stima Gompertz",
    subtitle = "I punti indicano le osservazioni reali; le linee continue rappresentano il modello NLME",
    x = "Tempo (giorni)",
    y = "Optical Density (OD)"
  ) + theme_minimal()

AIC(gomp_quad, gomp_quad_re)
BIC(gomp_quad, gomp_quad_re)

#re su b3
gomp_quad_re_b <- update(modello_gompertz,
                       random = b3 ~ 1 | id_biomassa)
dati_m_gompertz2$pred_re_b <- predict(gomp_quad_re_b)
ggplot(dati_m_gompertz2, aes(x = tempo, color = as.factor(condizione_sperimentale))) +
  geom_point(aes(y = OD), alpha = 0.6) +
  geom_line(aes(y = OD, group = id_biomassa), linetype = "dashed", alpha = 0.4) +
  geom_line(aes(y = pred_re, group = id_biomassa), linewidth = 0.6)+
  geom_line(aes(y = pred_re_b, group = id_biomassa), linewidth = 1.2)+
  facet_wrap_paginate(~ id_biomassa, scales = "free_y", ncol = 4, nrow = 3, page = 1) +
  scale_color_viridis_d(option = "plasma", guide = "none") +
  labs(
    title = "Confronto Dati Reali vs Stima Gompertz",
    subtitle = "I punti indicano le osservazioni reali; le linee continue rappresentano il modello NLME",
    x = "Tempo (giorni)",
    y = "Optical Density (OD)"
  ) + theme_minimal()


AIC(gomp_quad_re, gomp_quad_re_b)
BIC(gomp_quad_re, gomp_quad_re_b)
#meglio su asintoto

#asintoto e b3
gomp_quad_re_b <- update(modello_gompertz,
                         random = Asym + b3 ~ 1 | id_biomassa)
dati_m_gompertz2$pred_re_b <- predict(gomp_quad_re_b)
ggplot(dati_m_gompertz2, aes(x = tempo, color = as.factor(condizione_sperimentale))) +
  geom_point(aes(y = OD), alpha = 0.6) +
  geom_line(aes(y = OD, group = id_biomassa), linetype = "dashed", alpha = 0.4) +
  geom_line(aes(y = pred_re, group = id_biomassa), linewidth = 0.6)+
  geom_line(aes(y = pred_re_b, group = id_biomassa), linewidth = 1.2)+
  facet_wrap_paginate(~ id_biomassa, scales = "free_y", ncol = 4, nrow = 3, page = 3) +
  scale_color_viridis_d(option = "plasma", guide = "none") +
  labs(
    title = "Confronto Dati Reali vs Stima Gompertz",
    subtitle = "I punti indicano le osservazioni reali; le linee continue rappresentano il modello NLME",
    x = "Tempo (giorni)",
    y = "Optical Density (OD)"
  ) + theme_minimal()

AIC(gomp_quad_re, gomp_quad_re_b)
BIC(gomp_quad_re, gomp_quad_re_b)

dati_m_gompertz2$res_gomp_quad_re <- residuals(gomp_quad_re)
ggplot(dati_m_gompertz2, aes(x = tempo, color = as.factor(condizione_sperimentale))) +
  #geom_point(aes(y = OD), alpha = 0.6) +
  #geom_line(aes(y = OD, group = id_biomassa), linetype = "dashed", alpha = 0.4) +
  #geom_line(aes(y = pred_re, group = id_biomassa), linewidth = 0.6)+
  geom_line(aes(y = res_gomp_quad_re, group = id_biomassa), linewidth = 1.2)+
  facet_wrap_paginate(~ id_biomassa, scales = "free_y", ncol = 4, nrow = 3, page = 5) +
  scale_color_viridis_d(option = "plasma", guide = "none") +
  labs(
    title = "Confronto Dati Reali vs Stima Gompertz",
    subtitle = "I punti indicano le osservazioni reali; le linee continue rappresentano il modello NLME",
    x = "Tempo (giorni)",
    y = "Optical Density (OD)"
  ) + theme_minimal()

ggplot(dati_m_gompertz2, aes(x = tempo, y = res_gomp_quad_re, color = condizione_sperimentale)) +
  geom_line(aes(group = id_biomassa), alpha = 0.5, na.rm = TRUE) +
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

#correlazione seriale
library(nlme)

gomp_quad_re_ar1 <- nlme(
  model = OD ~ SSgompertz(tempo, Asym, b2, b3),
  data = dati_m_gompertz2,
  
  fixed = list(
    Asym ~ (I + D)^2 + P + I(I^2) + I(D^2) + I(P^2),
    b2   ~ (I + D)^2 + P + I(I^2) + I(D^2) + I(P^2),
    b3   ~ (I + D)^2 + P + I(I^2) + I(D^2) + I(P^2)
  ),
  
  random = Asym ~ 1 | id_biomassa,
  
  correlation = corAR1(form = ~ tempo | id_biomassa),
  
  start = c(
    Asym = c(3, rep(0, 7)),
    b2   = c(1.5, rep(0, 7)),
    b3   = c(0.8, rep(0, 7))
  )
)

dati_m_gompertz2$pred_re_ar1 <- predict(gomp_quad_re_ar1)
ggplot(dati_m_gompertz2, aes(x = tempo, color = as.factor(condizione_sperimentale))) +
  geom_point(aes(y = OD), alpha = 0.6) +
  geom_line(aes(y = OD, group = id_biomassa), linetype = "dashed", alpha = 0.4) +
  geom_line(aes(y = pred_re, group = id_biomassa), linewidth = 0.6)+
  geom_line(aes(y = pred_re_ar1, group = id_biomassa), linewidth = 1.2)+
  facet_wrap_paginate(~ id_biomassa, scales = "free_y", ncol = 4, nrow = 3, page = 5) +
  scale_color_viridis_d(option = "plasma", guide = "none") +
  labs(
    title = "Confronto Dati Reali vs Stima Gompertz",
    subtitle = "I punti indicano le osservazioni reali; le linee continue rappresentano il modello NLME",
    x = "Tempo (giorni)",
    y = "Optical Density (OD)"
  ) + theme_minimal()

AIC(gomp_quad_re, gomp_quad_re_ar1)
BIC(gomp_quad_re, gomp_quad_re_ar1)

dati_m_gompertz2$res_gomp_quad_re_ar1 <- residuals(gomp_quad_re_ar1)
ggplot(dati_m_gompertz2, aes(x = tempo, color = as.factor(condizione_sperimentale))) +
  #geom_point(aes(y = OD), alpha = 0.6) +
  #geom_line(aes(y = OD, group = id_biomassa), linetype = "dashed", alpha = 0.4) +
  #geom_line(aes(y = pred_re, group = id_biomassa), linewidth = 0.6)+
  geom_line(aes(y = res_gomp_quad_re_ar1, group = id_biomassa), linewidth = 1.2)+
  facet_wrap_paginate(~ id_biomassa, scales = "free_y", ncol = 4, nrow = 3, page = 1) +
  scale_color_viridis_d(option = "plasma", guide = "none") +
  labs(
    title = "Confronto Dati Reali vs Stima Gompertz",
    subtitle = "I punti indicano le osservazioni reali; le linee continue rappresentano il modello NLME",
    x = "Tempo (giorni)",
    y = "Optical Density (OD)"
  ) + theme_minimal()

ggplot(dati_m_gompertz2, aes(x = tempo, y = res_gomp_quad_re_ar1, color = condizione_sperimentale)) +
  geom_line(aes(group = id_biomassa), alpha = 0.5, na.rm = TRUE) +
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
#Nota: basta AR1 non CAR1 perchè i punti del tempo sono equispaziati

#eteroschedasticità
dati_m_gompertz2$tempo_safe <- dati_m_gompertz2$tempo + 0.01

gomp_quad_re_w <- nlme(
  model = OD ~ SSgompertz(tempo_safe, Asym, b2, b3), # Usiamo il tempo traslato
  data = dati_m_gompertz2,
  fixed = list(
    Asym ~ (I + D)^2 + P + I(I^2) + I(D^2) + I(P^2),
    b2   ~ (I + D)^2 + P + I(I^2) + I(D^2) + I(P^2),
    b3   ~ (I + D)^2 + P + I(I^2) + I(D^2) + I(P^2)
  ),
  random = Asym ~ 1 | id_biomassa,
  weights = varPower(form = ~ tempo_safe), # Ora varPower è al sicuro da zeri
  start = fixef(gomp_quad_re_ar1),
  control = nlmeControl(maxIter = 100, msMaxIter = 200, minScale = 1e-5)
)

dati_m_gompertz2$pred_re_w <- predict(gomp_quad_re_w)
ggplot(dati_m_gompertz2, aes(x = tempo, color = as.factor(condizione_sperimentale))) +
  geom_point(aes(y = OD), alpha = 0.6) +
  geom_line(aes(y = OD, group = id_biomassa), linetype = "dashed", alpha = 0.4) +
  geom_line(aes(y = pred_re, group = id_biomassa), linewidth = 0.6)+
  geom_line(aes(y = pred_re_w, group = id_biomassa), linewidth = 1.2)+
  facet_wrap_paginate(~ id_biomassa, scales = "free_y", ncol = 4, nrow = 3, page = 5) +
  scale_color_viridis_d(option = "plasma", guide = "none") +
  labs(
    title = "Confronto Dati Reali vs Stima Gompertz",
    subtitle = "I punti indicano le osservazioni reali; le linee continue rappresentano il modello NLME",
    x = "Tempo (giorni)",
    y = "Optical Density (OD)"
  ) + theme_minimal()

AIC(gomp_quad_re_ar1, gomp_quad_re_w)
BIC(gomp_quad_re_ar1, gomp_quad_re_w)
anova(gomp_quad_re_ar1, gomp_quad_re_w)

dati_m_gompertz2$res_gomp_quad_re_w <- residuals(gomp_quad_re_w)
ggplot(dati_m_gompertz2, aes(x = tempo, color = as.factor(condizione_sperimentale))) +
  #geom_point(aes(y = OD), alpha = 0.6) +
  #geom_line(aes(y = OD, group = id_biomassa), linetype = "dashed", alpha = 0.4) +
  #geom_line(aes(y = pred_re, group = id_biomassa), linewidth = 0.6)+
  geom_line(aes(y = res_gomp_quad_re_w, group = id_biomassa), linewidth = 1.2)+
  facet_wrap_paginate(~ id_biomassa, scales = "free_y", ncol = 4, nrow = 3, page = 1) +
  scale_color_viridis_d(option = "plasma", guide = "none") +
  labs(
    title = "Confronto Dati Reali vs Stima Gompertz",
    subtitle = "I punti indicano le osservazioni reali; le linee continue rappresentano il modello NLME",
    x = "Tempo (giorni)",
    y = "Optical Density (OD)"
  ) + theme_minimal()

ggplot(dati_m_gompertz2, aes(x = tempo, y = res_gomp_quad_re_w, color = condizione_sperimentale)) +
  geom_line(aes(group = id_biomassa), alpha = 0.5, na.rm = TRUE) +
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

#Nota: non converge!!

#semplifichiamo predittori
gomp_re_w <- nlme(
  model = OD ~ SSgompertz(tempo_safe, Asym, b2, b3), # Usiamo il tempo traslato
  data = dati_m_gompertz2,
  fixed = list(
    Asym ~ (I + D)^2 + P,
    b2   ~ (I + D)^2 + P,
    b3   ~ (I + D)^2 + P
  ),
  random = Asym ~ 1 | id_biomassa,
  weights = varPower(form = ~ tempo_safe), # Ora varPower è al sicuro da zeri
  start = c(
    Asym = c(3, rep(0, 4)),
    b2   = c(1.5, rep(0, 4)),
    b3   = c(0.8, rep(0, 4))
  ),
  control = nlmeControl(maxIter = 100, msMaxIter = 200, minScale = 1e-5)
)

