#nota ----

if(FALSE){
  "
  Este archivo se llama svar_ratio_titulos
  
  Este archivo es el mismo que svar_liquidez pero con otro enfoque 
  en las variables, se usa ratio de titulos
  
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
               readr, 
               usethis #para .Renviron igual que .env de python
)

# importar datos si no existen en el entorno ----
if (!exists("df_bananox")) {
  if (file.exists("dataset_spillover_transformado.csv")) {
    df_bananox <- readr::read_csv("dataset_spillover_transformado.csv") %>%
      dplyr::mutate(fecha = as.Date(fecha))
  } else {
    source("johansen_o_diff.R")  # este script debe crear df_bananox en el entorno global
  }
}

#nombres para los graficos----
nombres_formales <- c(
  dlog_remesas    = "Crecimiento de Remesas (dlog)",
  dlog_imae       = "Crecimiento IMAE (dlog)",
  d_liquidez      = "Var. Índice de Liquidez",
  d_ratio_titulos = "Var. Inversión en Títulos",
  d_tasa_pasiva   = "Var. Tasa de Interés Pasiva",
  roa             = "Rentabilidad Bancaria (ROA)"
)

#rezago optimo para el var ----

#este orden es el orden Cholesky
endog_var <- df_bananox[, c("dlog_remesas", 
                            "d_tasa_pasiva", 
                            #"d_liquidez", 
                            "d_ratio_titulos", 
                            "roa")]

exog_var <- df_bananox[, c("d2018", "dboom_remesas_covid", "dlog_imae")] 


#esto guardo el bucle siguiente
tabla_rezagos <- data.frame()

#bucle para estimar de 1 a 12 rezagos y extraer el Log-Likelihood de cada uno ----
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
               p = 3, 
               type = "const", 
               exogen = exog_var)

# Log-Likelihood del VAR
cat("Log-Likelihood:", as.numeric(logLik(var_mod)), "\n")


# summary(var_mod)
# print(var_mod)


#confirmar residuos 

print("raices (estabilidad)")
raices_comp <- roots(var_mod, modulus = FALSE)
plot(raices_comp, unit_circle = TRUE, main = "Raíces inversas del VAR(3)")

#mejor los graficos de raices

png("raices_var_titulos.png", width = 2000, height = 2000, res = 300, bg = "transparent") #exportar

plot(raices_comp, type = "p", pch = 20, col = "red", 
     xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1),
     xlab = "Parte Real", ylab = "Parte Imaginaria") 
abline(h = 0, v = 0, lty = 2, col = "gray")
symbols(x = 0, y = 0, circles = 1, inches = FALSE, add = TRUE, fg = "blue", lwd = 1)

dev.off() #esto es lo que lo guarda con la funcion png


print("test portmanteau (autocorrelacion)")

#p-value debe ser > 0.05 los residuos son ruido blanco.

serial_test_1 <- serial.test(var_mod, lags.bg = 6, type = "BG")
print(serial_test_1)

serial_test_2 <- serial.test(var_mod, lags.bg = 12, type = "BG")
print(serial_test_2)

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

#guardar cusm pra grafico

estabil_mod <- stability(var_mod)

# grafico del cusm

pdf("cusm_titulos.pdf", width = 9, height = 7)

par(mfrow = c(2, 2)) #2x2 los graficos

for (nom in names(estabil_mod$stability)) {
  
  #buscar nombre
  titulo_formal <- if (nom %in% names(nombres_formales)) nombres_formales[nom] else nom
  plot(estabil_mod$stability[[nom]], 
       main = paste("Estabilidad CUSUM:", titulo_formal),
       xlab = "Eje de Tiempo (Muestra)", 
       ylab = "Fluctuación Acumulada")
}

dev.off()
par(mfrow = c(1, 1)) #1x1 los graficos

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
                #response = c("roa", "d_liquidez", "d_tasa_pasiva"),
                #response = c("roa", "d_tasa_pasiva"),
                n.ahead = 12, 
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
readr::write_csv(irf_data, "irf_spillover_remesas_titulos.csv")

#graficos para el dog ortoganales

irf_grafico_df <- irf_data %>%
  dplyr::mutate(
    respuesta_formal = dplyr::case_when(
      respuesta == "roa"             ~ "Rentabilidad Bancaria (ROA)",
      respuesta == "d_liquidez"      ~ "Var. Índice de Liquidez",
      respuesta == "d_ratio_titulos" ~ "Var. Inversión en Títulos",
      respuesta == "d_tasa_pasiva"   ~ "Var. Tasa de Interés Pasiva",
      TRUE                           ~ respuesta
    )
  ) %>%
  dplyr::mutate(
    respuesta_formal = factor(respuesta_formal, levels = c(
      "Var. Índice de Liquidez", 
      "Var. Tasa de Interés Pasiva",
      "Var. Inversión en Títulos",
      "Rentabilidad Bancaria (ROA)"
    ))
  )

grafico_irf <- ggplot2::ggplot(irf_grafico_df, aes(x = horizon)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6) +
  ggplot2::geom_ribbon(aes(ymin = inferior, ymax = superior), fill = "gray80", alpha = 0.6) +
  ggplot2::geom_line(aes(y = puntual), color = "steelblue", linewidth = 0.8) +
  ggplot2::facet_wrap(~ respuesta_formal, scales = "free_y", ncol = 1) +
  ggplot2::scale_x_continuous(breaks = seq(0, 12, by = 2)) +
  ggplot2::labs(
    #title = "Funciones Impulso-Respuesta (IRF) Ortogonales",
    #subtitle = "Choque exógeno en el Crecimiento de Remesas (dlog) - Bandas al 95%",
    x = "Horizonte Temporal (Meses)",
    y = "Respuesta de la Variable"
  ) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(
    strip.text = element_text(face = "bold", size = 11),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "gray30", size = 10),
    panel.spacing = unit(1, "lines")
  )

print(grafico_irf)

ggplot2::ggsave("03_graficos_irf_remesas_titulos.pdf", plot = grafico_irf, width = 7, height = 8)

#irf generalizadas (Pesaran‑Shin)

irf_general <- irf(var_mod, 
                   impulse = "dlog_remesas", 
                   #response = c("roa", "d_liquidez", "d_ratio_titulos", "d_tasa_pasiva"),
                   response = c("roa", "d_tasa_pasiva", "d_ratio_titulos"),
                   #response = c("roa", "d_liquidez", "d_tasa_pasiva"),
                   n.ahead = 12, 
                   ortho = FALSE, 
                   boot = TRUE, 
                   runs = 1000)

plot(irf_general)

#tabla para el doc

resp_names_general <- colnames(irf_general$irf$dlog_remesas)

irf_data_general <- bind_rows(lapply(resp_names_general, function(resp) {
  tibble(
    Horizonte = 0:(nrow(irf_general$irf$dlog_remesas) - 1),
    Impulso   = "Crecimiento de Remesas (dlog)",
    Respuesta = resp,
    Estimacion_Central = irf_general$irf$dlog_remesas[, resp],
    Limite_Inferior    = irf_general$Lower$dlog_remesas[, resp],
    Limite_Superior    = irf_general$Upper$dlog_remesas[, resp]
  )
})) %>%
  dplyr::mutate(
    Respuesta = dplyr::case_when(
      Respuesta == "roa"             ~ "Rentabilidad Bancaria (ROA)",
      Respuesta == "d_liquidez"      ~ "Var. Índice de Liquidez",
      Respuesta == "d_ratio_titulos" ~ "Var. Inversión en Títulos",
      Respuesta == "d_tasa_pasiva"   ~ "Var. Tasa de Interés Pasiva",
      TRUE                           ~ Respuesta
    )
  )

print(head(irf_data_general, 20))

print(irf_data_general, n = Inf)

# Guardar CSV
readr::write_csv(irf_data_general, "irf_spillover_remesas_generales_titulos.csv")

#graficos generales Pesaran‑Shin

irf_grafico_general_df <- irf_data_general %>%
  dplyr::mutate(
    Respuesta = factor(Respuesta, levels = c(
      "Var. Índice de Liquidez", 
      "Var. Tasa de Interés Pasiva",
      "Var. Inversión en Títulos",
      "Rentabilidad Bancaria (ROA)"
    ))
  )

grafico_irf_general <- ggplot2::ggplot(irf_grafico_general_df, aes(x = Horizonte)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6) +
  ggplot2::geom_ribbon(aes(ymin = Limite_Inferior, ymax = Limite_Superior), fill = "gray80", alpha = 0.6) +
  ggplot2::geom_line(aes(y = Estimacion_Central), color = "darkred", linewidth = 0.8) +
  ggplot2::facet_wrap(~ Respuesta, scales = "free_y", ncol = 1) +
  ggplot2::scale_x_continuous(breaks = seq(0, 12, by = 2)) +
  ggplot2::labs(
    #title = "Funciones Impulso-Respuesta Generalizadas (Pesaran-Shin)",
    #subtitle = "Choque exógeno en el Crecimiento de Remesas (dlog) - Bandas al 95%",
    x = "Horizonte Temporal (Meses)",
    y = "Respuesta de la Variable"
  ) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(
    strip.text = element_text(face = "bold", size = 11),
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "gray30", size = 10),
    panel.spacing = unit(1, "lines")
  )

print(grafico_irf_general)

ggplot2::ggsave("04_graficos_irf_generales_pesaran_titulos.pdf", plot = grafico_irf_general, width = 7, height = 8)

#fevd ----

fevd_mod <- fevd(var_mod, n.ahead = 12)
plot(fevd_mod)

fevd_roa <- fevd_mod$dlog_remesas[, "roa"]
plot(0:(length(fevd_roa)-1), fevd_roa, type = "l", xlab = "Horizonte", 
     ylab = "% varianza del ROA explicada por remesas")

fevd_mod$dlog_remesa

#varianza de roa

matriz_roa <- fevd_mod$roa

fevd_roa_por_remesas <- matriz_roa[, "dlog_remesas"] * 100

horizonte <- 1:12

plot(horizonte, fevd_roa_por_remesas, 
     type = "l", 
     col = "steelblue", 
     lwd = 2,
     xlab = "Horizonte Temporal (Meses)", 
     ylab = "% varianza del ROA explicada por remesas",
     main = "Impacto de las Remesas sobre el ROA (FEVD)")

tabla_fevd_roa <- data.frame(
  Mes = horizonte,
  Explicado_por_Remesas = matriz_roa[, "dlog_remesas"] * 100,
  Explicado_por_Titulos = matriz_roa[, "d_ratio_titulos"] * 100,
  Explicado_por_Tasa = matriz_roa[, "d_tasa_pasiva"] * 100,
  Explicado_por_Propio_ROA = matriz_roa[, "roa"] * 100
)

print("Descomposición de Varianza del ROA (%):")
print(round(tabla_fevd_roa, 2))

readr::write_csv(tabla_fevd_roa, "fevd_roa_desglosado_titulos.csv")

#grafico roa fevd

tabla_fevd_long <- tabla_fevd_roa %>%
  tidyr::pivot_longer(
    cols = -Mes, 
    names_to = "Fuente_Choque", 
    values_to = "Porcentaje"
  ) %>%
  dplyr::mutate(
    Fuente_Choque = dplyr::case_when(
      Fuente_Choque == "Explicado_por_Remesas"       ~ "Choque: Remesas",
      Fuente_Choque == "Explicado_por_Liquidez"      ~ "Choque: Liquidez", 
      Fuente_Choque == "Explicado_por_Tasa"          ~ "Choque: Tasa Pasiva",
      Fuente_Choque == "Explicado_por_Titulos"       ~ "Choque: Inversion en Titulos",
      Fuente_Choque == "Explicado_por_Propio_ROA"    ~ "Choque Inercial: ROA",
      TRUE                                           ~ Fuente_Choque
    )
  ) %>%
  dplyr::mutate(
    Fuente_Choque = factor(Fuente_Choque, levels = c(
      "Choque Inercial: ROA",
      "Choque: Tasa Pasiva",
      "Choque: Inversion en Titulos",
      "Choque: Liquidez",
      "Choque: Remesas"
    ) %>% intersect(unique(Fuente_Choque)))
  )

grafico_fevd_roa <- ggplot2::ggplot(tabla_fevd_long, aes(x = Mes, y = Porcentaje, fill = Fuente_Choque)) +
  ggplot2::geom_col(width = 0.75, color = "white", linewidth = 0.2) +
  ggplot2::scale_x_continuous(breaks = 1:12) +
  ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 100.05)) +
  ggplot2::scale_fill_brewer(palette = "Set2") +
  ggplot2::labs(
    #title = "Descomposicion de Varianza de la Rentabilidad Bancaria (ROA)",
    #subtitle = "Contribucion porcentual de cada choque estructural en un horizonte de 12 meses",
    x = "Horizonte Temporal (Meses)",
    y = "Porcentaje de Varianza Explicada (%)",
    fill = "Origen del Choque:"
  ) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(color = "gray30", size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank() 
  )

print(grafico_fevd_roa)

ggplot2::ggsave("05_grafico_fevd_roa_formal_titulos.pdf", plot = grafico_fevd_roa, width = 7.5, height = 5)
