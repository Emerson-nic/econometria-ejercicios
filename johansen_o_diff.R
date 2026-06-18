#notas ----

if(FALSE){
  "
  Este archivo es johansen_o_diff.R
  
  roa, liquidez son % (estos van de 0 a 1), por cada 100 pesos de activo 
  que se tiene el sistema bancario,cuantos pesos de utilidad neta genero
  en el mes. Ejemplo: roa = 0.0014 significa que por cada 100 pesos 
  de activos se generaron 0.14 pesos de utilidad mensual.
  
  En liquidez por cada 100 pesos de pasivo (obligaciones con el publico y otros),
  cuantos pesos en efectivo y equivalentes de alta liquidez tiene el banco
  para cubrir esos compromisos. Ejemplo: liquidez = 0.23 significa que 
  por cada 100 pesos de pasivo hay 23 pesos de activos líquidos inmediatos.
  
  
  ratio_titulos es un ratio financiero
  
  por cada 100 pesos de activo total del sistema bancario,
  cuantos pesos estan invertidos en titulos de deuda (bonos, certificados, reportos).
  Ejemplo: ratio_titulos = 0.15 significa que por cada 100 pesos de activo
  hay 15 pesos colocados en instrumentos de renta fija.
  
  tasa_pasiva es un %
  
  es el costo promedio ponderado que pagan los bancos por los depositos
  del publico. Ejemplo: tasa_pasiva = 6.88 significa que por cada 100 pesos 
  depositados durante un año, el banco paga aproximadamente 6.88 pesos 
  de intereses.
  
  Nota metodologica:
  roa, liquidez y tasa_pasiva son porcentajes/ratios: no se transforman con logaritmos.
  ratio de titulos es un ratio financiero, tampoco admite log-diferencias.
  Solo flujo_remesas (variable nominal positiva) se hace log_diff12 
  flujo_remesas se interpreta como tasa de variacion anual
  
  Nota metodologica: 
  el svar me da mucha autocorrelacion (recuerdese que se esta tratando con
  tasas de crecimiento intermensual). La recomendacion para tratar mejor esto
  es que deestacionalizar las serie con X-13ARIMA, las tasas de varianza
  intermensual no elimina el componente estacional mensual. 
  
  aun asi con la correcion anterio el svar sigui con autocorrelacion
  se decidio usar en todas las variables menos roa
  
  se agrega el credito bancario como variable pero se trabaja en diff
  
  "
}

# cargar librerias ----
options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, 
               urca, 
               vars,
               ggplot2,
               tseries,
               strucchange,
               seasonal,
               usethis #para .Renviron igual que .env de python
               )

# importar datos si no existen en el entorno ----
if (!exists("dataset_banano")) {
  if (file.exists("dataset_spillover_remesas_clean.csv")) {
    dataset_banano <- readr::read_csv("dataset_spillover_remesas_clean.csv") %>%
      dplyr::mutate(fecha = as.Date(fecha))
  } else {
    source("limpieza_spillover.R")
  }
}

# graficos en niveles de variable ----

dataset_banano_graficos0 <- dataset_banano %>%
  dplyr::select(-d2018,
                -dcovid,
                -d2008)

dataset_banano_graficos0 %>%
  tidyr::pivot_longer(cols = -fecha, names_to = "variable", values_to = "valor") %>%
  ggplot2::ggplot(aes(x = fecha, y = valor)) +
  ggplot2::geom_line(color = "steelblue", linewidth = 0.6) +
  ggplot2::facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  ggplot2::labs(x = "Fecha", y = NULL) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(strip.text = element_text(face = "bold"))

dataset_banano_graficos0 %>%
  ggplot(aes(x = fecha, y = flujo_remesas)) +  
  geom_line() 
# +
# labs(title = "Identificar boom en Remesas")


#transformacion ahora si dog ----

df_banano <- dataset_banano %>%
#   dplyr::mutate(
#     log_remesas = log(flujo_remesas),
#     dlog_remesas = log_remesas - lag(log_remesas, 12)
#   ) %>%
   dplyr::select(fecha, roa, liquidez, ratio_titulos, tasa_pasiva, credito_privado,
                 flujo_remesas, d2018, dcovid, imae ) %>% #dlog_remesas incluir si se activa log_remesas
   tidyr::drop_na()

# este transfomacion se hace mas adelante

# graficos de linea por variable ----

dataset_banano_graficos <- df_banano %>%
  dplyr::select(-d2018,
                -dcovid)

dataset_banano_graficos %>%
  tidyr::pivot_longer(cols = -fecha, names_to = "variable", values_to = "valor") %>%
  dplyr::mutate(
    variable = dplyr::case_when(
      variable == "roa"  ~ "Rentabilidad Bancaria (ROA)",
      variable == "liquidez" ~ "Índice de Liquidez",
      variable == "ratio_titulos" ~ "Inversión en Títulos Valores",
      variable == "tasa_pasiva" ~ "Tasa de Interés Pasiva",
      variable == "flujo_remesas" ~ "Flujo de Remesas",
      variable == "imae" ~ "IMAE",
      variable == "credito_privado" ~ "Credito Privado",
      TRUE ~ variable
    )
  ) %>%
  dplyr::mutate(
    variable = factor(variable, levels = c(
      "Flujo de Remesas", 
      "IMAE",
      "Índice de Liquidez", 
      "Inversión en Títulos Valores",
      "Tasa de Interés Pasiva", 
      "Rentabilidad Bancaria (ROA)",
      "Credito Privado"
    ))
  ) %>%
  ggplot2::ggplot(aes(x = fecha, y = valor)) +
  ggplot2::geom_line(color = "steelblue", linewidth = 0.6) +
  ggplot2::facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  ggplot2::labs(x = NULL, y = NULL) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(
    strip.text = element_text(face = "bold", size = 11),
    panel.spacing = unit(1, "lines")
  )

#guardar plot
ggplot2::ggsave("grafico_variables.pdf", width = 8, height = 6)

# test Johansen ----

df_banano007 <- dataset_banano %>%
     dplyr::mutate(
       log_remesas = log(flujo_remesas),
       dlog_remesas = log_remesas - lag(log_remesas, 12),
       log_credito = log(credito_privado),
       dlog_credito = log_credito - lag(log_credito, 12)
     ) %>%
  dplyr::select(fecha, roa, liquidez, ratio_titulos, tasa_pasiva, 
                dlog_remesas, d2018, dcovid, imae, dlog_credito) %>% 
  tidyr::drop_na()

variables_endogenas <- df_banano007 %>%
  dplyr::select(roa, 
                liquidez, 
                ratio_titulos, 
                dlog_remesas, 
                tasa_pasiva)

variables_exogenas <- df_banano007 %>%
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

cat("no hay cointegracion no cumple el supuestos de que
    las variables sean i(1) en niveles (roa es i(0))")

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
vars_a_testear <- c("roa", "liquidez", "ratio_titulos", "flujo_remesas", 
                    "tasa_pasiva", "imae", "credito_privado") 

resultados_pval <- test_estacionariedad(dataset_banano, vars_a_testear)
print(resultados_pval)

#resultados contradictorios lo mejor es diferenciar solo roa es estacionario
# liquidez
# ratio_titulos
# tasa_pasivas


# diff interanual ----

#hacer la frecuencia mensual 
remesas_ts <- ts(dataset_banano$flujo_remesas, 
                 start = c(2008, 1), 
                 frequency = 12)

titulos_ts <- ts(dataset_banano$ratio_titulos, 
                 start = c(2008, 1), 
                 frequency = 12)

tasa_ts <- ts(dataset_banano$tasa_pasiva, 
              start = c(2008, 1), 
              frequency = 12)

liquidez_ts <- ts(dataset_banano$liquidez, 
                  start = c(2008, 1), 
                  frequency = 12)

credito_ts <- ts(dataset_banano$credito_privado, 
                  start = c(2008, 1), 
                  frequency = 12)

imae_ts <- ts(dataset_banano$imae, 
              start = c(2008, 1), 
              frequency = 12)

# se extra la serie desestacionalizada usando X-13ARIMA
remesas_sa <- seasonal::final(seasonal::seas(remesas_ts))
liquidez_sa <- seasonal::final(seasonal::seas(liquidez_ts))
titulos_sa <- seasonal::final(seasonal::seas(titulos_ts))
tasa_sa <- seasonal::final(seasonal::seas(tasa_ts))
credito_sa <- seasonal::final(seasonal::seas(credito_ts))
imae_sa <- seasonal::final(seasonal::seas(imae_ts))

df_bananox00 <- dataset_banano %>%
  dplyr:: mutate(
    flujo_remesas_sa = as.numeric(remesas_sa), 
    liquidez_sa      = as.numeric(liquidez_sa),
    ratio_titulos_sa = as.numeric(titulos_sa),
    tasa_pasiva_sa   = as.numeric(tasa_sa),
    imae_sa = as.numeric(imae_sa),
    credito_sa = as.numeric(credito_sa)
  )

vars_a_testear00 <- c("flujo_remesas_sa", 
                      "liquidez_sa",  
                      "ratio_titulos_sa",
                      "tasa_pasiva_sa",
                      "imae_sa",
                      "credito_sa")

resultados_pval00 <- test_estacionariedad(df_bananox00, vars_a_testear00)
print(resultados_pval00)

#nuevo dataset
#intermesual el diff
df_bananox <- dataset_banano %>%
  dplyr::mutate(
    flujo_remesas_sa = as.numeric(remesas_sa), 
    liquidez_sa = as.numeric(liquidez_sa),
    ratio_titulos_sa = as.numeric(titulos_sa),
    tasa_pasiva_sa = as.numeric(tasa_sa),
    imae_sa = as.numeric(imae_sa),
    credito_sa = as.numeric(credito_sa),
    log_remesas_sa  = log(flujo_remesas_sa),
    dlog_remesas = log_remesas_sa - lag(log_remesas_sa, 1),
    log_credito_sa  = log(credito_sa),
    dlog_credito = log_credito_sa - lag(log_credito_sa, 1),
    log_imae_sa = log(imae_sa),
    dlog_imae = log_imae_sa - lag(log_imae_sa, 1),
    d_liquidez = liquidez_sa - lag(liquidez_sa, 1),
    d_ratio_titulos = ratio_titulos_sa - lag(ratio_titulos_sa, 1),
    d_tasa_pasiva = tasa_pasiva_sa - lag(tasa_pasiva_sa, 1)
    
  ) %>%
  dplyr::select(fecha, roa, d_liquidez, d_ratio_titulos, d_tasa_pasiva, dlog_imae,
                dlog_remesas, dlog_credito, d2018, dcovid) %>%
  tidyr::drop_na()

print(head(df_bananox, 24))

#aplicar test de estacionariedad

vars_a_testear1 <- c("roa", "d_liquidez", "d_ratio_titulos", 
                     "dlog_remesas", "d_tasa_pasiva", "dlog_imae",
                     "dlog_credito")

resultados_pval1 <- test_estacionariedad(df_bananox, vars_a_testear1)
print(resultados_pval1)

#super estacionarios

# graficos de linea por variable trans ----

df_bananox_graficos <- df_bananox %>%
  dplyr::select(-d2018,
                -dcovid)

df_bananox_graficos %>%
  tidyr::pivot_longer(cols = -fecha, names_to = "variable", values_to = "valor") %>%
  dplyr::mutate(
    variable = dplyr::case_when(
      variable == "roa"  ~ "Rentabilidad Bancaria (ROA)",
      variable == "d_liquidez"  ~ "Var. Índice de Liquidez",
      variable == "d_ratio_titulos" ~ "Var. Inversión en Títulos",
      variable == "d_tasa_pasiva"  ~ "Var. Tasa de Interés Pasiva",
      variable == "dlog_remesas"  ~ "Crecimiento de Remesas (dlog)",
      variable == "dlog_imae"  ~ "Crecimiento IMAE (dlog)",
      variable == "dlog_credito" ~ "Crecimiento de Credito Privado (dlog)",
      TRUE  ~ variable
    )
  ) %>%
  dplyr::mutate(
    variable = factor(variable, levels = c(
      "Crecimiento de Remesas (dlog)", 
      "Crecimiento IMAE (dlog)",
      "Var. Índice de Liquidez", 
      "Var. Inversión en Títulos",
      "Var. Tasa de Interés Pasiva", 
      "Rentabilidad Bancaria (ROA)",
      "Crecimiento de Credito Privado (dlog)"
    ))
  ) %>%
  ggplot2::ggplot(aes(x = fecha, y = valor)) +
  ggplot2::geom_line(color = "steelblue", linewidth = 0.6) +
  ggplot2::facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  ggplot2::labs(x = NULL, y = NULL) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(
    strip.text = element_text(face = "bold", size = 11),
    panel.spacing = unit(1, "lines")
  )

#guardar plot
ggplot2::ggsave("grafico_variables_transformadas.pdf", width = 8, height = 6)


#identificar choques estructurales ----

#identificar el boom de remesa

#Ajuste de un modelo solo con intercepto (media)

#df_banano_esta la variable en niveles
#dlog_remesass la diferencia de log_remesa el quiebre 

bp_dlog <- breakpoints(dlog_remesas ~ 1, data = df_bananox, h = 0.15, breaks = 4)
summary(bp_dlog)
plot(bp_dlog)
breakdates(bp_dlog)

cat("se escogio 4 quiebres posibles donde el crierio BIC
    favorecio 1 quiebres ~ -900.3270")
cat("el RSS son las siglas de Residual Sum of Squares 
    (Suma de Cuadrados de los Residuos) mide la variabilidad no
    explicada pero se prefiere el bic por que penaliza la 
    complejidad añadida osae los paremetros (5)")

#miremos fechas del quiebre

df_bananox$fecha[147]
cat("el quiebre dice que es el 2020-04-01 es el boom de las remesas
    para la serie desestacionalizada")


#agregar dummy

fecha_boom <- as.Date("2020-04-01")
df_bananox <- df_bananox %>%
  mutate(
    dboom_remesas_covid = if_else(fecha >= as.Date("2020-04-01"), 1, 0),
    mes = factor(month(fecha)),
  ) %>%
  dplyr::select(-dcovid)

# guardar dataset

readr::write_csv(df_bananox, "dataset_spillover_transformado.csv")

if(FALSE){
  "
  
  En el corto plazo no hay evidencia de nada se rechaza la hipotesis de que
  hay efecto spillover hacia el roa pero habra efecto spillover en el
  credito privado causado por las remesa, en el largo plazo deberia cumplirse
  talvez haya potencia estadistica insuficiente si el vecm no cuentra efecto
  
  variables para ver si se puede hacer vecm
  
  log(remesas) #millones de dolares
  log(imae)
  tasa_pasiva
  log(credito_privado) #millones de cordobas
  
  ver archivo vecm
  
  "
}

# bloque cointegrado ----

#variables a ocupar

dataset_vecm <- dataset_banano %>%
  dplyr::mutate(log_remesa = log(flujo_remesas),
                log_imae = log(imae),
                log_credito = log(credito_privado)
  ) %>%
  dplyr::inner_join(df_bananox %>% 
                      dplyr::select(fecha, d2018, dboom_remesas_covid), by = "fecha") %>% 
  # d2018 se llama d2018.y o d2018.x porque dplyr ve 2 variables que se llaman iguales 
  # asi que hace eso para que no se llamew iguales pero ambas son la misma cosa
  tidyr::drop_na()

readr::write_csv(dataset_vecm, "dataset_vecm.csv")