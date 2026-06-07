#notas ----

if(FALSE){
  "
  roa, liquidez son % (estos van de 0 a 1), por cada 100 pesos de activo 
  que se tiene el sistema bancario,cuantos pesos de utilidad neta genero
  en el mes. Ejemplo: roa = 0.0014 significa que por cada 100 pesos 
  de activos se generaron 0.14 pesos de utilidad mensual.
  
  En liquidez por cada 100 pesos de pasivo (obligaciones con el publico y otros),
  cuantos pesos en efectivo y equivalentes de alta liquidez tiene el banco
  para cubrir esos compromisos. Ejemplo: liquidez = 0.23 significa que 
  por cada 100 pesos de pasivo hay 23 pesos de activos líquidos inmediatos.
  
  
  apalancamiento es un ratio financiero
  
  por cada 100 pesos de patrimonio (capital contable),
  cuantos pesos de pasivo tiene el sistema bancario. Ejemplo: 
  apalancamiento = 9.5 significa que por cada 100 pesos de capital
  hay 950 pesos de deuda, es decir, el sistema está fuertemente apalancado.
  
  tasa_pasiva es un %
  
  es el costo promedio ponderado que pagan los bancos por los depositos
  del publico. Ejemplo: tasa_pasiva = 6.88 significa que por cada 100 pesos 
  depositados durante un año, el banco paga aproximadamente 6.88 pesos 
  de intereses.
  
  Nota metodologica:
  roa, liquidez y tasa_pasiva son porcentajes/ratios: no se transforman con logaritmos.
  apalancamiento es un ratio financiero, tampoco admite log-diferencias.
  Solo flujo_remesas (variable nominal positiva) se hace log_diff12 
  flujo_remesas se interpreta como tasa de variacion anual
  
  
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
if (!exists("dataset_banano")) {
  if (file.exists("dataset_spillover_remesas_clean.csv")) {
    dataset_banano <- readr::read_csv("dataset_spillover_remesas_clean.csv") %>%
      dplyr::mutate(fecha = as.Date(fecha))
  } else {
    source("limpieza_spillover.R")
  }
}

#transformacion ahora si dog ----

df_banano <- dataset_banano %>%
  dplyr::mutate(
    log_remesas = log(flujo_remesas),
    dlog_remesas = log_remesas - lag(log_remesas, 12)
  ) %>%
  dplyr::select(fecha, roa, liquidez, apalancamiento, tasa_pasiva, 
                dlog_remesas, d2018, dcovid) %>%
  tidyr::drop_na()

# graficos de linea por variable ----

dataset_banano_graficos <- df_banano %>%
  dplyr::select(-d2018,
                -dcovid)

dataset_banano_graficos %>%
  tidyr::pivot_longer(cols = -fecha, names_to = "variable", values_to = "valor") %>%
  ggplot2::ggplot(aes(x = fecha, y = valor)) +
  ggplot2::geom_line(color = "steelblue", linewidth = 0.6) +
  ggplot2::facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  ggplot2::labs(x = "Fecha", y = NULL) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(strip.text = element_text(face = "bold"))

#guardar plot
ggplot2::ggsave("grafico_variables.pdf", width = 8, height = 6)

# test Johansen ----

variables_endogenas <- df_banano %>%
  dplyr::select(roa, 
                liquidez, 
                apalancamiento, 
                dlog_remesas, 
                tasa_pasiva)

variables_exogenas <- df_banano %>%
  dplyr::select(d2018, 
                dcovid)

lag_select <- VARselect(variables_endogenas, 
                        lag.max = 12, 
                        type = "const")

p_lag <- lag_select$selection["AIC(n)"]

johansen_test <- ca.jo(
  variables_endogenas,
  type = "trace",
  ecdet = "const",
  K = p_lag,
  dumvar = as.matrix(variables_exogenas)
)

summary(johansen_test)

# test de estacionariedad ----

#funcion de estacionariedad
test_estacionariedad <- function(data, variables,
                                 adf_alternative = "stationary",
                                 kpss_null = "Level") {
  resultados <- data.frame(variable = variables,
                           adf_pvalue = NA_real_,
                           kpss_pvalue = NA_real_)
  for (i in seq_along(variables)) {
    var <- variables[i]
    serie <- na.omit(data[[var]])
    adf_res <- tseries::adf.test(serie, alternative = adf_alternative)
    kpss_res <- tseries::kpss.test(serie, null = kpss_null)
    resultados$adf_pvalue[i] <- adf_res$p.value
    resultados$kpss_pvalue[i] <- kpss_res$p.value
  }
  return(resultados)
}

#aplicar funcion
vars_a_testear <- c("roa", "liquidez", "apalancamiento", "dlog_remesas", "tasa_pasiva")
resultados_pval <- test_estacionariedad(df_banano, vars_a_testear)
print(resultados_pval)

#resultados contradictorios lo mejor es diferenciar solo roa es estacionario
# liquidez
# apalacamiento
# tasa_pasivas
# seran diff i(2)

#2diff ----

#nuevo dataset
df_bananox <- dataset_banano %>%
  dplyr::mutate(
    log_remesas = log(flujo_remesas),
    dlog_remesas = log_remesas - lag(log_remesas, 1),
    d_liquidez = liquidez - lag(liquidez, 1),
    d_apalancamiento = apalancamiento - lag(apalancamiento, 1),
    d_tasa_pasiva = tasa_pasiva - lag(tasa_pasiva, 1)
  ) %>%
  dplyr::select(fecha, roa, d_liquidez, d_apalancamiento, d_tasa_pasiva, 
                dlog_remesas, d2018, dcovid) %>%
  tidyr::drop_na()

print(head(df_bananox, 24))  

#aplicar test de estacionariedad

vars_a_testear1 <- c("roa", "d_liquidez", "d_apalancamiento", 
                     "dlog_remesas", "d_tasa_pasiva")

resultados_pval1 <- test_estacionariedad(df_bananox, vars_a_testear1)
print(resultados_pval1)

#super estacionarios

readr::write_csv(df_bananox, "dataset_spillover_transformado.csv")

# graficos de linea por variable trans ----

df_bananox_graficos <- df_bananox %>%
  dplyr::select(-d2018,
                -dcovid)

df_bananox_graficos %>%
  tidyr::pivot_longer(cols = -fecha, names_to = "variable", values_to = "valor") %>%
  ggplot2::ggplot(aes(x = fecha, y = valor)) +
  ggplot2::geom_line(color = "steelblue", linewidth = 0.6) +
  ggplot2::facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  ggplot2::labs(x = "Fecha", y = NULL) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(strip.text = element_text(face = "bold"))

#guardar plot
ggplot2::ggsave("grafico_variables_tranformada.pdf", width = 8, height = 6)
