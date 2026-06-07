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
                            "d_liquidez", 
                            "d_apalancamiento", 
                            "roa")]

exog_var <- df_bananox[, c("d2018", "dcovid")]

tabla_rezagos <- data.frame()

#bucle para estimar de 1 a 24 rezagos y extraer el Log-Likelihood de cada uno
for (i in 1:24) {
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
               p = p, 
               type = "const", 
               exogen = exog_var)


#confirmar residuos 

print("raices (estabilidad)").
roots(var_mod)  

print("test portmanteau (autocorrelacion)")
#p-value debe ser > 0.05 los residuos son ruido blanco.
serial_test <- serial.test(var_mod, lags.pt = 12, type = "PT.asymptotic")
print(serial_test)

#irf

set.seed(54973997) #si sos trans llama a este numero
irf_chol <- irf(var_mod, 
                impulse = "dlog_remesas", 
                response = c("roa", "d_liquidez", "d_apalancamiento", "d_tasa_pasiva"),
                n.ahead = 24, 
                ortho = TRUE, 
                boot = TRUE, 
                runs = 1000)

plot(irf_chol)

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

# Guardar CSV
readr::write_csv(irf_data, "irf_spillover_remesas.csv")



#fevd

fevd_mod <- fevd(var_mod, n.ahead = 24)
plot(fevd_mod)

fevd_roa <- fevd_mod$dlog_remesas[, "roa"]
plot(0:(length(fevd_roa)-1), fevd_roa, type = "l", xlab = "Horizonte", 
     ylab = "% varianza del ROA explicada por remesas")

fevd_mod$dlog_remesa

#test granger

causality(var_mod, cause = "dlog_remesas")



