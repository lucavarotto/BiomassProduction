dati2 <- dati
dati2$I <- (dati2$I - 280)/190
dati2$D <- (dati2$D - 18)/6
dati2$P <- (dati2$P - 25.5)/24.5

#modello nullo ####
gompertz0 <- gnls(
  model = OD ~ SSgompertz(tempo, Asym, b2, b3),
  data = dati_m_gompertz2,
  params = list(
    Asym ~ 1, # Interazione per l'asintoto
    b2 ~ 1, # Interazione per la latenza
    b3 ~ 1 # Interazione per la rapidità
  ),
  start = c(
    Asym = 3,  
    b2   = 1.5,               
    b3   = 0.8
  )
)

previsioni <- data.frame(
  gompertz0 = predict(gompertz0),
  tempo = dati$tempo[-c(84,105)],
  id_biomassa = dati$id_biomassa[-c(84,105)],
  condizione_sperimentale = dati$condizione_sperimentale[-c(84,105)]
)

residui <- data.frame(
  gompertz0 = residuals(gompertz0),
  tempo = dati$tempo[-c(84,105)],
  id_biomassa = dati$id_biomassa[-c(84,105)],
  condizione_sperimentale = dati$condizione_sperimentale[-c(84,105)]
)

crit_info <- data.frame(
  modello = "Nullo",
  AIC = AIC(gompertz0),
  BIC = BIC(gompertz0)
)

# nullo con AR1 ####
gompertz0_ar1<- gnls(
  model = OD ~ SSgompertz(tempo, Asym, b2, b3),
  data = dati_m_gompertz2,
  params = list(
    Asym ~ 1, # Interazione per l'asintoto
    b2 ~ 1, # Interazione per la latenza
    b3 ~ 1 # Interazione per la rapidità
  ),
  correlation = corAR1(form = ~ tempo | id_biomassa),
  start = c(
    Asym = 3,  
    b2   = 1.5,               
    b3   = 0.8
  )
)

previsioni$gompertz0_ar1 <- predict(gompertz0_ar1)
residui$gomp_cov <- residuals(gompertz0_ar1)
crit_info <- bind_rows(crit_info,
                       data.frame(modello = "Nullo ocn AR1", 
                                  AIC = AIC(gompertz0_ar1), 
                                  BIC = BIC(gompertz0_ar1)))

# Covariate singole e interazione ####
gomp_cov <- gnls(
  model = OD ~ SSgompertz(tempo, Asym, b2, b3),
  data = dati_m_gompertz2,
  params = list(
    Asym ~ (I + D + P)^2 , # Interazione per l'asintoto
    b2 ~ (I + D + P)^2 , # Interazione per la latenza
    b3 ~ (I + D + P)^2  # Interazione per la rapidità
  ),
  start = c(
    Asym = c(3, rep(0, 6)),  
    b2   = c(1.5, rep(0, 6)),               
    b3   = c(0.8, rep(0, 6))
  )
)

summary(gomp_cov)

previsioni$gomp_cov <- predict(gomp_cov)
residui$gomp_cov <- residuals(gomp_cov)
crit_info <- bind_rows(crit_info,
                       data.frame(modello = "Covariate 1 ordine", 
                                     AIC = AIC(gomp_cov), 
                                     BIC = BIC(gomp_cov)))
#Covariate e interazione con AR1
gomp_cov_ar1 <- gnls(
  model = OD ~ SSgompertz(tempo, Asym, b2, b3),
  data = dati_m_gompertz2,
  params = list(
    Asym ~ (I + D + P)^2 , # Interazione per l'asintoto
    b2 ~ (I + D + P)^2 , # Interazione per la latenza
    b3 ~ (I + D + P)^2  # Interazione per la rapidità
  ),
  correlation = corAR1(form = ~ tempo | id_biomassa),
  start = c(
    Asym = c(3, rep(0, 6)),  
    b2   = c(1.5, rep(0, 6)),               
    b3   = c(0.8, rep(0, 6))
  )
)

previsioni$gomp_cov_ar1 <- predict(gomp_cov_ar1)
residui$gomp_cov_ar1 <- residuals(gomp_cov_ar1)
crit_info <- bind_rows(crit_info,
                       data.frame(modello = "Covariate 1 ordine e AR1", 
                                  AIC = AIC(gomp_cov_ar1), 
                                  BIC = BIC(gomp_cov_ar1)))

#Covariate al quadrato ####
gomp_quad <- gnls(
  model = OD ~ SSgompertz(tempo, Asym, b2, b3),
  data = dati_m_gompertz2,
  params = list(
    Asym ~ (I + D + P)^2 + I(I^2) + I(P^2) + I(D^2), # Interazione per l'asintoto
    b2 ~ (I + D + P)^2 + I(I^2) + I(P^2) + I(D^2), # Interazione per la latenza
    b3 ~ (I + D + P)^2 + I(I^2) + I(P^2) + I(D^2) # Interazione per la rapidità
  ),
  start = c(
    Asym = c(3, rep(0, 9)),  
    b2   = c(1.5, rep(0, 9)),               
    b3   = c(0.8, rep(0, 9))
  )
)

summary(gomp_cov)

previsioni$gomp_quad <- predict(gomp_quad)
residui$gomp_quad <- residuals(gomp_quad)
crit_info <- bind_rows(crit_info,
                       data.frame(modello = "Covariate al quadrato", 
                                  AIC = AIC(gomp_quad), 
                                  BIC = BIC(gomp_quad)))

#Covariate al quadrato AR1####
gomp_quad_ar1 <- gnls(
  model = OD ~ SSgompertz(tempo, Asym, b2, b3),
  data = dati_m_gompertz2,
  params = list(
    Asym ~ (I + D + P)^2 + I(I^2) + I(P^2) + I(D^2), # Interazione per l'asintoto
    b2 ~ (I + D + P)^2 + I(I^2) + I(P^2) + I(D^2), # Interazione per la latenza
    b3 ~ (I + D + P)^2 + I(I^2) + I(P^2) + I(D^2) # Interazione per la rapidità
  ),
  correlation = corAR1(form = ~ tempo | id_biomassa),
  start = c(
    Asym = c(3, rep(0, 9)),  
    b2   = c(1.5, rep(0, 9)),               
    b3   = c(0.8, rep(0, 9))
  )
)

summary(gomp_quad_ar1)

previsioni$gomp_quad_ar1 <- predict(gomp_quad_ar1)
residui$gomp_quad_ar1 <- residuals(gomp_quad_ar1)
crit_info <- bind_rows(crit_info,
                       data.frame(modello = "Covariate al quadrato e AR1", 
                                  AIC = AIC(gomp_quad_ar1), 
                                  BIC = BIC(gomp_quad_ar1)))

previsioni$OD <- dati$OD[-c(84, 105)]
ggplot(previsioni, aes(x = tempo, color = as.factor(condizione_sperimentale))) +
  geom_point(aes(y = OD), alpha = 0.6) +
  geom_line(aes(y = OD, group = id_biomassa), linetype = "dashed", alpha = 0.4) +
  geom_line(aes(y = gomp_quad_ar1, group = id_biomassa), linewidth = 1.2)+
  facet_wrap_paginate(~ id_biomassa, scales = "free_y", ncol = 4, nrow = 3, page = 1) +
  scale_color_viridis_d(option = "plasma", guide = "none") +
  labs(
    title = "Confronto Dati Reali vs Stima Gompertz",
    subtitle = "I punti indicano le osservazioni reali; le linee continue rappresentano il modello NLME",
    x = "Tempo (giorni)",
    y = "Optical Density (OD)"
  ) + theme_minimal()

ggplot(residui, aes(x = tempo, y = gomp_quad_ar1, color = condizione_sperimentale)) +
  geom_line(aes(group = id_biomassa), alpha = 0.2, na.rm = TRUE) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.5, na.rm = TRUE) +
  stat_summary(fun = mean, geom = "point", size = 2, na.rm = TRUE) +
  labs(
    title = "Traiettorie medie per condizione sperimentale",
    subtitle = "Le linee spesse rappresentano la media di gruppo; le linee chiare le singole biomasse",
    x = "Tempo (giorni)",
    y = "Residui",
    color = "Fattori (I_D_P)"
  ) +
  theme_minimal()

#Step ####
anova(gomp_quad_ar1)
gomp_quad_ar1b <- gnls(
  model = OD ~ SSgompertz(tempo, Asym, b2, b3),
  data = dati_m_gompertz2,
  params = list(
    Asym ~ (I + D + P)^2 + I(I^2) + I(P^2) + I(D^2), # Interazione per l'asintoto
    b2 ~ (I + D + P)^2 + I(I^2) + I(P^2) + I(D^2), # Interazione per la latenza
    b3 ~ (I + D + P)^2 + I(I^2) + I(D^2) # Interazione per la rapidità
  ),
  correlation = corAR1(form = ~ tempo | id_biomassa),
  start = c(
    Asym = c(3, rep(0, 9)),  
    b2   = c(1.5, rep(0, 9)),               
    b3   = c(0.8, rep(0, 8))
  )
)

AIC(gomp_quad_ar1, gomp_quad_ar1b)
BIC(gomp_quad_ar1, gomp_quad_ar1b)

anova(gomp_quad_ar1b)
gomp_quad_ar1c <- gnls(
  model = OD ~ SSgompertz(tempo, Asym, b2, b3),
  data = dati_m_gompertz2,
  params = list(
    Asym ~ (I + D + P)^2 + I(I^2) + I(P^2) + I(D^2), # Interazione per l'asintoto
    b2 ~ (I + D + P)^2 + I(I^2) + I(P^2) + I(D^2), # Interazione per la latenza
    b3 ~ (I + D)^2 + P + D:P + I(I^2) + I(D^2) # Interazione per la rapidità
  ),
  correlation = corAR1(form = ~ tempo | id_biomassa),
  start = c(
    Asym = c(3, rep(0, 9)),  
    b2   = c(1.5, rep(0, 9)),               
    b3   = c(0.8, rep(0, 7))
  )
)

AIC(gomp_quad_ar1b, gomp_quad_ar1c)
BIC(gomp_quad_ar1b, gomp_quad_ar1c)

#si ferma toglieno solo DP^2 in b3, differenza in termini di IC piccolissima, tengo tutto dappertutto per coerenza
