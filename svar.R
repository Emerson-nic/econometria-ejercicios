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
               urca, 
               vars,
               ggplot2,
               tseries)

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

lag_sel <- VARselect(endog_var, lag.max = 12, type = "const", exogen = exog_var)

p <- lag_sel$selection["AIC(n)"]

# modelo var ----

var_mod <- VAR(endog_var,
               p = p, 
               type = "const", 
               exogen = exog_var)

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

#test granger

causality(var_mod, cause = "dlog_remesas")

