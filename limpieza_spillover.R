#descripcion ----

if (FALSE) {
  "Esto actua como un bloque de comentarios.
  las variables: 
  ROA: Resultado del Periodo / Activo Total
  LIQUIDEZ: (Disponibilidades + Inversiones a Corto Plazo) / Pasivo Total
  APALANCAMIENTO: Pasivo Total / Patrimonio
  estan sacadas de la base de datos de siboif exactamete de la hoja
  SISTEMA_BANCARIO
  
  Esta dataset busca saber si existe el fenomeno spillover por remesas,
  este archivo solo es limpieza de dataset
  "
}

#cargar librerias ---- 
options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               readxl,
               lubridate,
               janitor
               )

# funcion para pivotar el siboif (de formato contabilidad a timeseries) ----
procesar_siboif <- function(file_path, sheet_name) {
  #lee el archivo saltando los encabezados iniciales
  df_raw <- read_excel(file_path, sheet = sheet_name, skip = 9) %>% 
    clean_names() 
  
  #transponer y limpiar fechas
  df_clean <- df_raw %>%
    rename(cuenta = 1) %>% 
    filter(!is.na(cuenta)) %>% 
    pivot_longer(cols = -cuenta, names_to = "fecha_texto", values_to = "valor") %>%
    mutate(
      fecha_texto = str_remove(fecha_texto, "^x"),
      fecha = dmy(fecha_texto),
      fecha = floor_date(fecha, "month") 
    ) %>%
    drop_na(fecha, valor) %>% 
    select(fecha, cuenta, valor) %>%
    pivot_wider(names_from = cuenta, values_from = valor) %>%
    clean_names()
  
  return(df_clean)
}

#aplicar la funcion anterior ----

bg <- procesar_siboif("ib_balance_general_0.xlsx", "SISTEMA BANCARIO")
er <- procesar_siboif("ib_estado_resultados_0.xlsx", "SISTEMA BANCARIO")

siboif_ts <- bg %>%
  full_join(er, by = "fecha") %>%
  mutate(across(everything(), ~replace_na(., 0))) %>%
  mutate(
    roa = resultado_del_ejercicio / activo,
    liquidez = (disponibilidades + inversiones_a_corto_plazo) / pasivo,
    apalancamiento = pasivo / patrimonio
  ) %>%
  #seleccionar solo las variables de interés para el modelo 
  select(fecha, roa, liquidez, apalancamiento)

#limpieza de las remesas ----

remesas_ts <- read_excel("remesas.xls", sheet = "1a.2.1.04", skip = 4) %>%
  rename(anio = 1) %>%
  select(-Total) %>% 
  filter(!is.na(anio)) %>%
  pivot_longer(cols = -anio, names_to = "mes", values_to = "flujo_remesas") %>%
  mutate(
    mes_num = match(mes, c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                           "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")),
    fecha = make_date(anio, mes_num, 1)
  ) %>%
  select(fecha, flujo_remesas) %>%
  drop_na()

#liempieza en tasa de interes ----

tasas_raw <- read_excel("activaspasivas ponderadas_2021_2025.xlsx", sheet = "Tasas Pasivas")

tasa_pasiva_fila <- tasas_raw %>%
  filter(str_detect(tolower(names(.)[1]), "total|córdobas|dólares")) %>%
  slice(1) 

valores_tasa <- as.numeric(tasa_pasiva_fila[-1])
valores_tasa <- valores_tasa[!is.na(valores_tasa)] 

tasas_ts <- tibble(
  fecha = seq(as.Date("2020-01-01"), by = "month", length.out = length(valores_tasa)),
  tasa_pasiva = valores_tasa
)

#unificar

dataset_banano <- list(siboif_ts, remesas_ts, tasas_ts) %>%
  reduce(left_join, by = "fecha") %>%
  arrange(fecha) %>%
  drop_na() 

glimpse(dataset_banano)

write_csv(dataset_banano, "dataset_spillover_remesas_clean.csv")