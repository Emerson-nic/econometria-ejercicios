# cargar librerias ----
options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, 
               urca, 
               vars,
               ggplot2)

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


# graficos de linea por variable ----

dataset_banano_graficos <- dataset_banano %>%
  dplyr::select(-d2008,
                -d2018,
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
endog <- dataset_banano %>%
  dplyr::select(roa, liquidez, apalancamiento, flujo_remesas, tasa_pasiva)

#rezago optimo 
lag_seleccion <- vars::VARselect(endog, lag.max = 12, type = "const")
p_optimo <- lag_seleccion$selection["AIC(n)"]

johansen_test <- urca::ca.jo(endog, type = "trace", ecdet = "const", K = p_optimo)

# mostrar resultados clave
summary(johansen_test)
