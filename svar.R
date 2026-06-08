#nota ----

if(FALSE){
  "
  
  Todas las series son estacionarias (I(0)). Se estima un VAR en niveles
  
  
  "
}

# cargar librerias ----
options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               dplyr,
               tidyr, 
               urca, 
               vars,
               ggplot2,
               tseries,
               readr)

# importar datos si no existen en el entorno ----
if (!exists("df_bananox")) {
  if (file.exists("dataset_spillover_transformado.csv")) {
    df_bananox <- readr::read_csv("dataset_spillover_transformado.csv") %>%
      dplyr::mutate(fecha = as.Date(fecha))
  } else {
    source("johansen_o_diff.R")  # este script debe crear df_bananox en el entorno global
  }
}

#rezago optimo para el var ----

#este orden es el orden Cholesky
endog_var <- df_bananox[, c("dlog_remesas", 
                            "d_tasa_pasiva", 
                            #"d_liquidez", 
                            "d_ratio_titulos", 
                            "roa")]

exog_var <- df_bananox[, c("d2018", "dcovid", "dboom_remesas")] 

# exog_var_full <- cbind(
#   model.matrix(~ mes - 1, data = df_bananox)[, -1],  # 11 dummies mensuales
#   df_bananox$d2018,
#   df_bananox$dcovid,
#   df_bananox$dboom_remesas
# )
# 
# colnames(exog_var_full) <- c(paste0("mes", 2:12), "d2018", "dcovid", "dboom_remesas")
# exog_var_full <- as.matrix(exog_var_full)


#esto guardo el bucle siguiente
tabla_rezagos <- data.frame()

#bucle para estimar de 1 a 12 rezagos y extraer el Log-Likelihood de cada uno
for (i in 1:12) {
  #estimar el modelo temporalmente
  modelo_tmp <- VAR(endog_var, p = i, type = "const", exogen = exog_var)
  
  #extraer el Log-Likelihood del sistema de ecuaciones
  ll <- as.numeric(stats::logLik(modelo_tmp))
  
  #extraer los criterios de informacion (AIC, BIC, HQ y SC)
  aic_val <- stats::AIC(modelo_tmp)
  bic_val <- stats::BIC(modelo_tmp)
  
  #guardo
  tabla_rezagos <- rbind(tabla_rezagos, data.frame(
    Rezago = i,
    Log_Likelihood = ll,
    AIC = aic_val,
    BIC_SC = bic_val 
  ))
}

print("tabla comparativa")
print(tabla_rezagos)

print(paste("max Log-Likelihood", 
            tabla_rezagos$Rezago[which.max(tabla_rezagos$Log_Likelihood)]))
print(paste("min AIC)", 
            tabla_rezagos$Rezago[which.min(tabla_rezagos$AIC)]))
print(paste("min BIC/Schwarz", 
            tabla_rezagos$Rezago[which.min(tabla_rezagos$BIC_SC)]))

# modelo var ----

var_mod <- VAR(endog_var,
               p = 4, 
               type = "const", 
               exogen = exog_var)

# Log-Likelihood del VAR
cat("Log-Likelihood:", as.numeric(logLik(var_mod)), "\n")

# summary(var_mod)
# print(var_mod)


#confirmar residuos 

print("raices (estabilidad)")
raices <- roots(var_mod)
raices_comp <- roots(var_mod, modulus = FALSE)
plot(raices_comp, unit_circle = TRUE, main = "Raíces inversas del VAR(3)")

print("test portmanteau (autocorrelacion)")

#p-value debe ser > 0.05 los residuos son ruido blanco.

serial_test <- serial.test(var_mod, lags.pt = 12, type = "BG")
print(serial_test)

#test de normalidad
norm_test <- normality.test(var_mod, multivariate.only = TRUE)
print(norm_test)

cat("h_0: normalidad en los residuos")

#test de heterocedasticidad
arch_test <- arch.test(var_mod, lags.multi = 12, multivariate.only = TRUE)
print(arch_test)

cat("h_0: no efectos arch en los residuos")

#establidad estructural cusm (ve si el modelo es estable a lo largo de la muestra)

plot(stability(var_mod))

# (una por ecuación)
#test granger

causality(var_mod, cause = "dlog_remesas")

#irf ----

#irf Cholesky

set.seed(54973997) #si sos trans llama a este numero
irf_chol <- irf(var_mod, 
                impulse = "dlog_remesas", 
                #response = c("roa", "d_liquidez", "d_ratio_titulos", "d_tasa_pasiva"),
                response = c("roa", "d_tasa_pasiva", "d_ratio_titulos"),
                #response = c("roa", "d_tasa_pasiva"),
                n.ahead = 24, 
                ortho = TRUE, 
                boot = TRUE, 
                runs = 1000)

plot(irf_chol)

#tabla para el doc

resp_names <- colnames(irf_chol$irf$dlog_remesas)


irf_data <- bind_rows(lapply(resp_names, function(resp) {
  tibble(
    horizon = 0:(nrow(irf_chol$irf$dlog_remesas) - 1),  
    respuesta = resp,
    impulso   = "dlog_remesas",
    puntual   = irf_chol$irf$dlog_remesas[, resp],
    inferior  = irf_chol$Lower$dlog_remesas[, resp],
    superior  = irf_chol$Upper$dlog_remesas[, resp]
  )
}))

print(head(irf_data, 20))

#imprimetodo
print(irf_data, n = Inf)

# Guardar CSV
readr::write_csv(irf_data, "irf_spillover_remesas.csv")

#irf generalizadas (Pesaran‑Shin)

irf_general <- irf(var_mod, 
               impulse = "dlog_remesas", 
               response = c("roa", "d_tasa_pasiva", "d_ratio_titulos"),
               n.ahead = 24, 
               ortho = FALSE, 
               boot = TRUE, 
               runs = 1000)

plot(irf_general)

#tabla para el doc

resp_names_general <- colnames(irf_general$irf$dlog_remesas)


irf_data_general <- bind_rows(lapply(resp_names_general, function(resp) {
  tibble(
    horizon = 0:(nrow(irf_general$irf$dlog_remesas) - 1),
    respuesta = resp,
    impulso   = "dlog_remesas",
    puntual   = irf_general$irf$dlog_remesas[, resp],
    inferior  = irf_general$Lower$dlog_remesas[, resp],
    superior  = irf_general$Upper$dlog_remesas[, resp]
  )
}))

print(head(irf_data_general, 20))

print(irf_data_general, n = Inf)

# Guardar CSV
readr::write_csv(irf_data_general, "irf_spillover_remesas_generales.csv")

#fevd ----

fevd_mod <- fevd(var_mod, n.ahead = 24)
plot(fevd_mod)

fevd_roa <- fevd_mod$dlog_remesas[, "roa"]
plot(0:(length(fevd_roa)-1), fevd_roa, type = "l", xlab = "Horizonte", 
     ylab = "% varianza del ROA explicada por remesas")

fevd_mod$dlog_remesa

#varianza de roa

fevd_mod$roa

