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
  
  nota: las girf generales las persan estan malas son navi eso no es una prueba 
  de robutez me paso hacer girf solo para vecm los demas son no significativo 
  asi que no vale la pena
  
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
if (!exists("dataset_vecm")) {
  if (file.exists("Csv de dataset y resultados/dataset_vecm.csv")) {
    dataset_vecm <- readr::read_csv("Csv de dataset y resultados/dataset_vecm.csv") %>%
      dplyr::mutate(fecha = as.Date(fecha))
  } else {
    source("Scripts/johansen_o_diff.R")
  }
}

# bloque cointegrado ----

#variables a ocupar

cointegrax <- dataset_vecm %>%
  dplyr::select(log_remesa, 
                log_imae, 
                tasa_pasiva,
                log_credito ) %>%
  
  as.data.frame()

cointegrax_dummy <- dataset_vecm %>%
  dplyr::select(dboom_remesas_covid, d2018.x) %>%
  as.matrix() # ca.jo prefiere data.frames puros o matrices

#test jhansen

vecm_test_trace <- urca::ca.jo(cointegrax,
                               type = "trace", 
                               ecdet = "const", 
                               K = 3, # razago optimo del var(liquidez, titulos)
                               season = 12, #genera dummys estacionales dentro del test
                               dumvar = cointegrax_dummy) #quiebres estructurales
summary(vecm_test_trace)

vecm_test_eigen <- urca::ca.jo(cointegrax,
                               type = "eigen", 
                               ecdet = "const", 
                               K = 3, # razago optimo del var(liquidez, titulos)
                               season = 12, #genera dummys estacionales dentro del test
                               dumvar = cointegrax_dummy) #quiebres estructurales
summary(vecm_test_eigen)

if(FALSE){
  "
  
  ambos test prueban que hay 2 vectores de cointegracion pero quienes
  el primero parace ser la ec. remesa, pero para verificar que ec usar
  se hara un vecm formal con r=2 para ver sus velocidades de ajuste
  es decir se buscan quienes corrigen el desequilibrio de largo plazo
  
  "
}


# preuba de ajustes ----

# vemc formal con r = 2
modelo_vecm_restringido <- urca::cajorls(vecm_test_trace, r = 2)

summary(modelo_vecm_restringido$rlm)

# sin el rlm imprime
#       Length Class     Mode   
# rlm    12     mlm      list   
# beta   10    -none-   numeric

if(FALSE){
  "
  
  el estimate es alpha osea velocidades de ajustes  
  
  El sd1 (diciembre) al sd11 (octubre) son dummys estacionales creada con el test jhansen
  (seasonal = 12)
  
  los sd1 y las variables menos ect son coeficiente Gamma 1% de algo causa tanto
  
  el ECT son los terminos de correcion de errores, jhansen nos dio 2 vectores
  de correcion quienes son? para eso se buscan los los ect significativos y 
  tambien la correccion de error de alpha debe ser negativa
  
  
 credito como el estimado (alpha) es ect2 = -0.017414 esta variable tiende a la
 cointegracion uno de 2 amortiguadores del sistema
 
 el imae tiene ect1 y ect2 positivos este no hace correciones de errores mas bien
 los acelera, osea cuando ocurre un choque en el imae, acelera tiene comportamiento
 divergente de corto plazo
 
 las remesas son debilmente exogenas porque ect1 y ect2 no son significativos
 es decir alpha de las remesas son igual a 0
 
 las r dicen de forma no formal que son ecuaciones maestras que amarran 
 variable
 
 las vectores de cointegracion son credito y imae pero creidto son los terminos
 de correccion de errores y el imae intensica estos errores
  "
}

# seleccion de rezago ----

# seleccion de rezagos optimos (VARselect) ----

# Se ejecuta la selección sobre las variables en niveles
seleccion_rezagos <- vars::VARselect(y = cointegrax, 
                                     lag.max = 10, 
                                     type = "const",
                                     season = 12, 
                                     exogen = cointegrax_dummy)

print(seleccion_rezagos$selection)

if(FALSE){
  "
  VARselect indica que HQ, SC y FPE da p=2 por parsimonia se usa p=2
  "
}

#print(round(seleccion_rezagos$criteria, 4))


# jhansen k=2 ----

# test Johansen con el rezago 2
vecm_test_trace_k2 <- urca::ca.jo(cointegrax,
                                  type = "trace", 
                                  ecdet = "const", 
                                  K = 2, 
                                  season = 12, 
                                  dumvar = cointegrax_dummy) 


summary(vecm_test_trace_k2)



vecm_test_eigen_k2 <- urca::ca.jo(cointegrax,
                               type = "eigen", 
                               ecdet = "const", 
                               K = 2, 
                               season = 12,
                               dumvar = cointegrax_dummy) 
summary(vecm_test_eigen_k2)

#prueba de alpha ver quien es el termino de correcciones de errores

modelo_vecm_restringido_k2 <- urca::cajorls(vecm_test_trace_k2, r = 2)

summary(modelo_vecm_restringido_k2$rlm)

if(FALSE){
  "
  
  las conclusiones son iguales que el anterior if(FALSE)
  solo que con este p=2 la tasa_pasiva tiene ect1 significativo y postivo
  es decir la variable se aleja del equilibrio
  
  "
}

#modelo vecm ----

modelo_largo_plazo <- vars::vec2var(vecm_test_trace_k2, r = 2)
summary(modelo_largo_plazo)

nombres_formales <- c(
  log_remesa = "Remesas",
  log_credito = "Crédito Privado",
  log_imae = "IMAE",
  tasa_pasiva = "Tasa Pasiva"
)

#test de residuos ----

#raices inversa
cat("la ser un vecm de 4 variables con r=2, debe existir 2 raices en el cirtulo unitario tocandose")

A1 <- modelo_largo_plazo$A[[1]]
A2 <- modelo_largo_plazo$A[[2]]
m <- ncol(A1)

matriz_companera <- rbind(
  cbind(A1, A2),
  cbind(diag(m), matrix(0, m, m))
)

valores_propios <- eigen(matriz_companera)$values
raices_comp <- complex(real = Re(valores_propios), imaginary = Im(valores_propios))

png("Graficos/05_raices_vecm.png", width = 2000, height = 2000, res = 300, bg = "transparent")
plot(raices_comp, type = "p", pch = 20, col = "darkred", 
     xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1),
     xlab = "Parte Real", ylab = "Parte Imaginaria") 
abline(h = 0, v = 0, lty = 2, col = "gray")
symbols(x = 0, y = 0, circles = 1, inches = FALSE, add = TRUE, fg = "navyblue", lwd = 1.5)
dev.off()


#autocorrelacion portmanteau ----
serial_test_vecm <- vars::serial.test(modelo_largo_plazo, lags.pt = 12, type = "PT.asymptotic")
print(serial_test_vecm)

#normalidad multivariada
norm_test_vecm <- vars::normality.test(modelo_largo_plazo, multivariate.only = TRUE)
print(norm_test_vecm)


#test heterocedasticidad
arch_test_vecm <- vars::arch.test(modelo_largo_plazo, lags.multi = 12, multivariate.only = TRUE)
print(arch_test_vecm)


# estabilidad estructural cusum (CUSUM) 
#no hay funcion para eso pero me no creo que haya si

#test ganger

#objeto summary.mlm anidado
resumen_rlm <- summary(modelo_vecm_restringido_k2$rlm)

#se coeficiente exacto (Equivalente al Test de Granger) para la ecuación del credito
resultado_granger_credito <- resumen_rlm$`Response log_credito.d`$coefficients["log_remesa.dl1", , drop = FALSE]

print("test de Granger corto plazo remesa a credito")
print(resultado_granger_credito)

#imae
resultado_granger_imae <- resumen_rlm$`Response log_imae.d`$coefficients["log_remesa.dl1", , drop = FALSE]

print("test de Granger corto plazo remesa a imae")
print(resultado_granger_imae)

#irf ----
set.seed(54973997) # si sos trans llama 
irf_vecm <- vars::irf(modelo_largo_plazo, 
                      impulse = "log_remesa", 
                      response = c("log_credito", "log_imae", "tasa_pasiva"),
                      n.ahead = 36,
                      ortho = TRUE,
                      boot = TRUE,
                      runs = 1000)

plot(irf_vecm, 
     main = "VECM IRF: Respuesta del Crédito ante Choque en Remesas",
     ylab = "log_credito",
     xlab = "Horizonte Temporal (Meses)")

#tablas para el doc

resp_names <- colnames(irf_vecm$irf$log_remesa)

irf_data_vecm <- bind_rows(lapply(resp_names, function(resp) {
  tibble(
    horizon   = 0:(nrow(irf_vecm$irf$log_remesa) - 1),  
    respuesta = resp,
    impulso   = "log_remesa",
    puntual   = irf_vecm$irf$log_remesa[, resp],
    inferior  = irf_vecm$Lower$log_remesa[, resp],
    superior  = irf_vecm$Upper$log_remesa[, resp]
  )
})) %>%
  dplyr::mutate(
    respuesta_formal = dplyr::case_when(
      respuesta == "log_credito" ~ "credito",
      respuesta == "log_imae"    ~ "imae",
      respuesta == "tasa_pasiva" ~ "tasa pasiva",
      TRUE ~ respuesta
    )
  )

cat('estos irf son de largo plazo')

# imprimir todo
print(irf_data_vecm, n = Inf)

readr::write_csv(irf_data_vecm, "Csv de dataset y resultados/irf_spillover_vecm_remesas_orto.csv")

#irf de imae a prestamos

irf_vecm_imae <- vars::irf(modelo_largo_plazo, 
                      impulse = "log_imae", 
                      response = c("log_credito"),
                      n.ahead = 36,
                      ortho = TRUE,
                      boot = TRUE,
                      runs = 1000)

plot(irf_vecm_imae, 
     main = "VECM IRF: Respuesta del Crédito ante Choque en imae",
     ylab = "log_imae",
     xlab = "Horizonte Temporal (Meses)")

#tablas para el doc

resp_names_imae <- colnames(irf_vecm_imae$irf$log_imae)

irf_data_vecm_imae <- bind_rows(lapply(resp_names_imae, function(resp) {
  tibble(
    horizon   = 0:(nrow(irf_vecm_imae$irf$log_imae) - 1),  
    respuesta = resp,
    impulso   = "log_imae",
    puntual   = irf_vecm_imae$irf$log_imae[, resp],
    inferior  = irf_vecm_imae$Lower$log_imae[, resp],
    superior  = irf_vecm_imae$Upper$log_imae[, resp]
  )
})) %>%
  dplyr::mutate(
    respuesta_formal = dplyr::case_when(
      respuesta == "log_credito" ~ "credito",
      respuesta == "log_imae"    ~ "imae",
      respuesta == "tasa_pasiva" ~ "tasa pasiva",
      TRUE ~ respuesta
    )
  )

# imprimir todo
print(irf_data_vecm_imae, n = Inf)

readr::write_csv(irf_data_vecm_imae, "Csv de dataset y resultados/irf_spillover_vecm_imae_orto.csv")

#irf generales imae Pesaran‑Shin ----

irf_vecm_general <- vars::irf(modelo_largo_plazo, 
                      impulse = "log_remesa", 
                      response = c("log_credito", "log_imae", "tasa_pasiva"),
                      n.ahead = 36,
                      ortho = FALSE,
                      boot = TRUE,
                      runs = 1000)

plot(irf_vecm_general, 
     main = "VECM IRF: Respuesta del Crédito ante Choque en Remesas",
     ylab = "log_credito",
     xlab = "Horizonte Temporal (Meses)")

#tablas para el doc

resp_names_general <- colnames(irf_vecm_general$irf$log_remesa)

irf_data_vecm_general <- bind_rows(lapply(resp_names, function(resp) {
  tibble(
    horizon   = 0:(nrow(irf_vecm_general$irf$log_remesa) - 1),  
    respuesta = resp,
    impulso   = "log_remesa",
    puntual   = irf_vecm_general$irf$log_remesa[, resp],
    inferior  = irf_vecm_general$Lower$log_remesa[, resp],
    superior  = irf_vecm_general$Upper$log_remesa[, resp]
  )
})) %>%
  dplyr::mutate(
    respuesta_formal = dplyr::case_when(
      respuesta == "log_credito" ~ "credito",
      respuesta == "log_imae"    ~ "imae",
      respuesta == "tasa_pasiva" ~ "tasa pasiva",
      TRUE ~ respuesta
    )
  )

# imprimir todo
print(irf_data_vecm_general, n = Inf)

readr::write_csv(irf_data_vecm_general, "Csv de dataset y resultados/irf_spillover_vecm_remesas_gen.csv")

# graficos pesaran-shin y ortogolanes cholesky de los itf anteriores ----

#ortogonales

grafico_irf_remesas_orto <- ggplot2::ggplot(irf_data_vecm, aes(x = horizon)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6) +
  ggplot2::geom_ribbon(aes(ymin = inferior, ymax = superior), fill = "gray80", alpha = 0.6) +
  ggplot2::geom_line(aes(y = puntual), color = "steelblue", linewidth = 0.8) +
  ggplot2::facet_wrap(~ respuesta_formal, scales = "free_y", ncol = 1) +
  ggplot2::scale_x_continuous(breaks = seq(0, 36, by = 6)) +
  ggplot2::labs(
    x = "Horizonte Temporal (Meses)",
    y = "Respuesta de la Variable"
  ) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(
    strip.text = element_text(face = "bold", size = 11),
    plot.title = element_text(face = "bold", size = 12),
    panel.spacing = unit(1, "lines")
  )
ggplot2::ggsave("Graficos/06_irf_orto_remesas.pdf", plot = grafico_irf_remesas_orto, width = 7, height = 8)

grafico_irf_imae_orto <- ggplot2::ggplot(irf_data_vecm_imae, aes(x = horizon)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6) +
  ggplot2::geom_ribbon(aes(ymin = inferior, ymax = superior), fill = "gray80", alpha = 0.6) +
  ggplot2::geom_line(aes(y = puntual), color = "steelblue", linewidth = 0.8) +
  ggplot2::facet_wrap(~ respuesta_formal, scales = "free_y", ncol = 1) +
  ggplot2::scale_x_continuous(breaks = seq(0, 36, by = 6)) +
  ggplot2::labs(
    x = "Horizonte Temporal (Meses)",
    y = "Respuesta del Crédito"
  ) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(
    strip.text = element_text(face = "bold", size = 11),
    plot.title = element_text(face = "bold", size = 12)
  )
ggplot2::ggsave("Graficos/07_irf_orto_imae_credito.pdf", plot = grafico_irf_imae_orto, width = 7, height = 4)


#pesaran-shin 

grafico_irf_remesas_gen <- ggplot2::ggplot(irf_data_vecm_general, aes(x = horizon)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6) +
  ggplot2::geom_ribbon(aes(ymin = inferior, ymax = superior), fill = "gray80", alpha = 0.6) +
  ggplot2::geom_line(aes(y = puntual), color = "darkred", linewidth = 0.8) +
  ggplot2::facet_wrap(~ respuesta_formal, scales = "free_y", ncol = 1) +
  ggplot2::scale_x_continuous(breaks = seq(0, 36, by = 6)) +
  ggplot2::labs(
    x = "Horizonte Temporal (Meses)",
    y = "Respuesta de la Variable"
  ) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(
    strip.text = element_text(face = "bold", size = 11),
    plot.title = element_text(face = "bold", size = 12),
    panel.spacing = unit(1, "lines")
  )
ggplot2::ggsave("Graficos/08_irf_gen_remesas.pdf", plot = grafico_irf_remesas_gen, width = 7, height = 8)

# irf de tasa pasiva a credito ----

set.seed(54973997)
irf_vecm_tasa <- vars::irf(modelo_largo_plazo, 
                           impulse = "tasa_pasiva", 
                           response = c("log_credito"),
                           n.ahead = 36,
                           ortho = TRUE,
                           boot = TRUE,
                           runs = 1000)

resp_names_tasa <- colnames(irf_vecm_tasa$irf$tasa_pasiva)

irf_data_vecm_tasa <- dplyr::bind_rows(lapply(resp_names_tasa, function(resp) {
  tibble(
    horizon   = 0:(nrow(irf_vecm_tasa$irf$tasa_pasiva) - 1),  
    respuesta = resp,
    impulso   = "tasa_pasiva",
    puntual   = irf_vecm_tasa$irf$tasa_pasiva[, resp],
    inferior  = irf_vecm_tasa$Lower$tasa_pasiva[, resp],
    superior  = irf_vecm_tasa$Upper$tasa_pasiva[, resp]
  )
})) %>%
  dplyr::mutate(respuesta_formal = "credito")

print(irf_data_vecm_tasa, n = Inf)
readr::write_csv(irf_data_vecm_tasa, "Csv de dataset y resultados/irf_spillover_vecm_tasa_orto.csv")

grafico_irf_tasa_orto <- ggplot2::ggplot(irf_data_vecm_tasa, aes(x = horizon)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6) +
  ggplot2::geom_ribbon(aes(ymin = inferior, ymax = superior), fill = "gray80", alpha = 0.6) +
  ggplot2::geom_line(aes(y = puntual), color = "steelblue", linewidth = 0.8) +
  ggplot2::facet_wrap(~ respuesta_formal, scales = "free_y", ncol = 1) +
  ggplot2::scale_x_continuous(breaks = seq(0, 36, by = 6)) +
  ggplot2::labs(x = "Horizonte Temporal (Meses)", y = "Respuesta del Crédito") +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(strip.text = element_text(face = "bold", size = 11))
ggplot2::ggsave("Graficos/10_irf_orto_tasa_credito.pdf", plot = grafico_irf_tasa_orto, width = 7, height = 4)


#pesaran-shin
irf_vecm_tasa_gen <- vars::irf(modelo_largo_plazo, 
                               impulse = "tasa_pasiva", 
                               response = c("log_credito"),
                               n.ahead = 36,
                               ortho = FALSE,
                               boot = TRUE,
                               runs = 1000)

resp_names_tasa_gen <- colnames(irf_vecm_tasa_gen$irf$tasa_pasiva)

irf_data_vecm_tasa_gen <- dplyr::bind_rows(lapply(resp_names_tasa_gen, function(resp) {
  tibble(
    horizon   = 0:(nrow(irf_vecm_tasa_gen$irf$tasa_pasiva) - 1),  
    respuesta = resp,
    impulso   = "tasa_pasiva",
    puntual   = irf_vecm_tasa_gen$irf$tasa_pasiva[, resp],
    inferior  = irf_vecm_tasa_gen$Lower$tasa_pasiva[, resp],
    superior  = irf_vecm_tasa_gen$Upper$tasa_pasiva[, resp]
  )
})) %>%
  dplyr::mutate(respuesta_formal = "credito")

print(irf_data_vecm_tasa_gen, n = Inf)
readr::write_csv(irf_data_vecm_tasa_gen, "Csv de dataset y resultados/irf_spillover_vecm_tasa_gen.csv")

grafico_irf_tasa_gen <- ggplot2::ggplot(irf_data_vecm_tasa_gen, aes(x = horizon)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6) +
  ggplot2::geom_ribbon(aes(ymin = inferior, ymax = superior), fill = "gray80", alpha = 0.6) +
  ggplot2::geom_line(aes(y = puntual), color = "darkred", linewidth = 0.8) +
  ggplot2::facet_wrap(~ respuesta_formal, scales = "free_y", ncol = 1) +
  ggplot2::scale_x_continuous(breaks = seq(0, 36, by = 6)) +
  ggplot2::labs(x = "Horizonte Temporal (Meses)", y = "Respuesta del Crédito") +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(strip.text = element_text(face = "bold", size = 11))
ggplot2::ggsave("Graficos/11_irf_gen_tasa_credito.pdf", plot = grafico_irf_tasa_gen, width = 7, height = 4)


#irf de imae a prestamos general

irf_vecm_imae_general <- vars::irf(modelo_largo_plazo, 
                           impulse = "log_imae", 
                           response = c("log_credito"),
                           n.ahead = 36,
                           ortho = FALSE,
                           boot = TRUE,
                           runs = 1000)

plot(irf_vecm_imae_general, 
     main = "VECM IRF: Respuesta del Crédito ante Choque en imae",
     ylab = "log_imae",
     xlab = "Horizonte Temporal (Meses)")

#tablas para el doc

resp_names_imae_general <- colnames(irf_vecm_imae_general$irf$log_imae)

irf_data_vecm_imae_general <- bind_rows(lapply(resp_names_imae_general, function(resp) {
  tibble(
    horizon   = 0:(nrow(irf_vecm_imae_general$irf$log_imae) - 1),  
    respuesta = resp,
    impulso   = "log_imae",
    puntual   = irf_vecm_imae_general$irf$log_imae[, resp],
    inferior  = irf_vecm_imae_general$Lower$log_imae[, resp],
    superior  = irf_vecm_imae_general$Upper$log_imae[, resp]
  )
})) %>%
  dplyr::mutate(
    respuesta_formal = dplyr::case_when(
      respuesta == "log_credito" ~ "credito",
      respuesta == "log_imae"    ~ "imae",
      respuesta == "tasa_pasiva" ~ "tasa pasiva",
      TRUE ~ respuesta
    )
  )

# imprimir todo
print(irf_data_vecm_imae_general, n = Inf)

readr::write_csv(irf_data_vecm_imae_general, "Csv de dataset y resultados/irf_spillover_vecm_imae_gen.csv")

grafico_irf_imae_gen <- ggplot2::ggplot(irf_data_vecm_imae_general, aes(x = horizon)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6) +
  ggplot2::geom_ribbon(aes(ymin = inferior, ymax = superior), fill = "gray80", alpha = 0.6) +
  ggplot2::geom_line(aes(y = puntual), color = "darkred", linewidth = 0.8) +
  ggplot2::facet_wrap(~ respuesta_formal, scales = "free_y", ncol = 1) +
  ggplot2::scale_x_continuous(breaks = seq(0, 36, by = 6)) +
  ggplot2::labs(
    x = "Horizonte Temporal (Meses)",
    y = "Respuesta del Crédito"
  ) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(
    strip.text = element_text(face = "bold", size = 11),
    plot.title = element_text(face = "bold", size = 12)
  )
ggplot2::ggsave("Graficos/09_irf_gen_imae_credito.pdf", plot = grafico_irf_imae_gen, width = 7, height = 4)



# fev ----

fevd_vecm <- vars::fevd(modelo_largo_plazo, n.ahead = 36)

obtener_fevd_limpia <- function(fevd_objeto) {
  fevd_lista <- lapply(names(fevd_objeto), function(v) {
    mat <- fevd_objeto[[v]]
    data.frame(
      horizon = 1:nrow(mat),
      variable_respondiente = v,
      log_remesa = round(as.numeric(mat[, "log_remesa"]) * 100, 2),
      log_credito = round(as.numeric(mat[, "log_credito"]) * 100, 2),
      log_imae = round(as.numeric(mat[, "log_imae"]) * 100, 2),
      tasa_pasiva = round(as.numeric(mat[, "tasa_pasiva"]) * 100, 2),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, fevd_lista)
}

fevd_data_vecm <- obtener_fevd_limpia(fevd_vecm)

print(as.data.frame(fevd_data_vecm))
readr::write_csv(fevd_data_vecm, "Csv de dataset y resultados/fevd_vecm_largo_plazo.csv")

#grafico

tabla_fevd_credito_long <- fevd_data_vecm %>%
  dplyr::filter(variable_respondiente == "log_credito") %>%
  dplyr::select(-variable_respondiente) %>%
  tidyr::pivot_longer(
    cols = c(log_remesa, log_credito, log_imae, tasa_pasiva), 
    names_to = "Fuente_Choque", 
    values_to = "Porcentaje"
  ) %>%
  dplyr::mutate(
    Fuente_Choque = dplyr::case_when(
      Fuente_Choque == "log_credito"  ~ "Choque Inercial: Crédito",
      Fuente_Choque == "tasa_pasiva"  ~ "Choque: Tasa Pasiva",
      Fuente_Choque == "log_imae"     ~ "Choque: IMAE",
      Fuente_Choque == "log_remesa"   ~ "Choque: Remesas",
      TRUE                            ~ Fuente_Choque
    )
  ) %>%
  dplyr::mutate(
    Fuente_Choque = factor(Fuente_Choque, levels = c(
      "Choque Inercial: Crédito",
      "Choque: Tasa Pasiva",
      "Choque: IMAE",
      "Choque: Remesas"
    ))
  )

grafico_fevd_credito <- ggplot2::ggplot(tabla_fevd_credito_long, ggplot2::aes(x = horizon, y = Porcentaje, fill = Fuente_Choque)) +
  ggplot2::geom_col(width = 0.8, color = "white", linewidth = 0.1) +
  ggplot2::scale_x_continuous(breaks = seq(1, 36, by = 3)) +
  ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 100.05)) +
  ggplot2::scale_fill_brewer(palette = "Set2") +
  ggplot2::labs(
    x = "Horizonte Temporal (Meses)",
    y = "Porcentaje de Varianza Explicada (%)",
    fill = "Origen del Choque:"
  ) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 12),
    plot.subtitle = ggplot2::element_text(color = "gray30", size = 10),
    legend.position = "bottom",
    legend.title = ggplot2::element_text(face = "bold", size = 9),
    legend.text = ggplot2::element_text(size = 9),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank() 
  )

print(grafico_fevd_credito)

ggplot2::ggsave("Graficos/12_grafico_fevd_credito_formal.pdf", plot = grafico_fevd_credito, width = 7.5, height = 5)


# girf manuales ----

#algun dia....