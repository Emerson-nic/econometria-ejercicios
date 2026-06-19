#descripcion ----

if (FALSE) {
  "Esto actua como un bloque de comentarios.
  las variables: 
  ROA: Resultado del Periodo / Activo Total
  LIQUIDEZ: (Disponibilidades + Inversiones a Corto Plazo) / Pasivo Total
  Ratio de Titulos:  inversion titulos / activo
  estan sacadas de la base de datos de siboif exactamete de la hoja
  SISTEMA_BANCARIO
  
  en tasas pasiva se obtiene del secmca Tasa de interés pasiva 
  nominal en ME (Moneda Extranjera) ~ (es un proxy) ya que la nueva metodologia del bcn
  pondera con referencia 2019-2023 entonces las observacionesque ofrece son
  a partir de enero de 2020
  
  Esta dataset busca saber si existe el fenomeno spillover por remesas,
  este archivo solo es limpieza de dataset
  
  remesas en millones de dolares 
  
  En nicaragua el mercado bursatil es de renta fija por lo que 
  
  nota metodologia: se incluye el imae para capturar autocorrelacion
  sera tratada como variable exogena
  
  "
}

#cargar librerias ---- 
options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               dplyr,
               tidyr,
               stringr,
               readxl,
               lubridate,
               janitor,
               usethis #para .Renviron igual que .env de python
               )

# importar datos si no existen en el entorno ----
if (!exists("df_credito")) {
  if (file.exists("df_credito.csv")) {
    df_credito <- readr::read_csv("df_credito.csv") %>%
      dplyr::mutate(fecha = as.Date(fecha))
  } else {
    source("datos_json.R")
  }
}

# funcion para pivotar el siboif (de formato contabilidad a timeseries) ----
procesar_siboif <- function(file_path, sheet_name) {
  #lee el archivo saltando los encabezados iniciales
  df_raw <- read_excel(file_path, sheet = sheet_name, skip = 9) %>%
    clean_names()
  
  #transponer y limpiar fechas
  df_clean <- df_raw %>%
    dplyr::rename(cuenta = 1) %>% 
    dplyr::filter(!is.na(cuenta)) %>% 
    tidyr::pivot_longer(cols = -cuenta, names_to = "fecha_texto", values_to = "valor") %>%
    dplyr::mutate(
      fecha_texto = stringr::str_remove(fecha_texto, "^x"),
      fecha = dmy(fecha_texto),
      fecha = floor_date(fecha, "month") 
    ) %>%
    tidyr::drop_na(fecha, valor) %>% 
    dplyr::distinct(fecha, cuenta, .keep_all = TRUE) %>%
    dplyr::select(fecha, cuenta, valor) %>%
    tidyr::pivot_wider(names_from = cuenta, values_from = valor) %>%
    clean_names()
  
  return(df_clean)
}

#aplicar la funcion anterior ----

bg <- procesar_siboif("ib_balance_general_0.xlsx", "SISTEMA_BANCARIO")
bg_18 <- procesar_siboif("ib_balance_general.xlsx", "SISTEMA_BANCARIO")
er <- procesar_siboif("ib_estado_resultados_0.xlsx", "SISTEMA_BANCARIO")
er_18 <- procesar_siboif("ib_estado_resultados.xlsx", "SISTEMA_BANCARIO")

print("nombres en Balance General (bg):")
print(names(bg))
print("nombres en Estado de Resultados (er):")
print(names(er))
print("dataset Balance General (bg):")
print(head(bg, 12))
print("dataset en Estado de Resultados (er):")
print(head(er, 12))


print("nombres en Balance General (bg_18):")
print(names(bg_18))
print("nombres en Estado de Resultados (er_18):")
print(names(er_18))
print("dataset Balance General (bg_18):")
print(head(bg_18, 12))
print("dataset en Estado de Resultados (er_18):")
print(head(er_18, 12))

# series apartir del 2019
siboif_ts <- bg %>%
  dplyr::full_join(er, by = "fecha") %>%
  dplyr::mutate(across(everything(), ~tidyr::replace_na(., 0))) %>%
  dplyr::mutate(
    roa = resultado_del_ejercicio.y / activo,
    liquidez = (efectivo_y_equivalentes_de_efectivo + 
                  inversiones_a_valor_razonable_con_cambios_en_resultados) / pasivo,
    ratio_titulos = (inversiones_a_valor_razonable_con_cambios_en_resultados +
                       inversiones_a_valor_razonable_con_cambios_en_otro_resultado_integral +
                       inversiones_a_costo_amortizado_neto) / activo
  ) %>%
  #seleccionar solo las variables de interes para el modelo 
  dplyr::select(fecha, roa, liquidez, ratio_titulos)

#serie hasta 2018

siboif_18_ts <- bg_18 %>%
  dplyr::full_join(er_18, by = "fecha") %>%
  dplyr::mutate(across(everything(), ~tidyr::replace_na(., 0))) %>%
  dplyr::mutate(
    roa = resultados_del_periodo.y / activo, 
    liquidez = (disponibilidades + inversiones_al_valor_razonable_con_cambios_en_resultados) / pasivo,
    ratio_titulos = inversiones_en_valores_neto / activo
  ) %>%
  dplyr::select(fecha, roa, liquidez, ratio_titulos)

print(head(siboif_18_ts$roa, 12))

#empalme dog
siboif_completo_ts <- dplyr::bind_rows(siboif_18_ts, siboif_ts) %>%
  dplyr::arrange(fecha) %>%
  dplyr::distinct(fecha, .keep_all = TRUE)

#limpieza de las remesas ----

remesas_raw <- readxl::read_excel("remesas.xls", sheet = "1a.2.1.04", skip = 0) 
print("nombres en remesas")
print(names(remesas_raw))
print(head(remesas_raw, 10))

remesas_ts <- readxl::read_excel("remesas.xls", 
                                 sheet = "1a.2.1.04", 
                                 skip = 5, 
                                 col_names = FALSE) %>%

  setNames(c("anio", "Ene", "Feb", "Mar", "Abr", "May", "Jun", 
             "Jul", "Ago", "Sep", "Oct", "Nov", "Dic", "extra")) %>%
  dplyr::select(-extra) %>%
  dplyr::mutate(anio = as.numeric(anio)) %>%
  dplyr::filter(!is.na(anio)) %>%
  tidyr::pivot_longer(cols = -anio, names_to = "mes", values_to = "flujo_remesas") %>%
  dplyr::mutate(
    flujo_remesas = as.numeric(flujo_remesas),
    mes_num = match(mes, c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                           "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")),
    fecha = make_date(anio, mes_num, 1)
  ) %>%
  dplyr::select(fecha, flujo_remesas) %>%
  tidyr::drop_na()

#liempieza en tasa de interes ----

tasas_raw <- suppressMessages(readxl::read_excel("Tasas de interés en moneda extranjera.xls", 
                                                 sheet = "Datos",  
                                                 skip = 0))
print("nombres en tasas")
print(names(tasas_raw))
print(head(tasas_raw, 10))


tasas_raw <- suppressMessages(readxl::read_excel("Tasas de interés en moneda extranjera.xls", 
                                                 sheet = "Datos",
                                                 skip = 7,
                                                 col_names = FALSE)) 
print("nombres en tasas")
print(names(tasas_raw))
print(head(tasas_raw, 10))

tasas_ts <- tasas_raw %>%
  dplyr::select(1, 2) %>%
  dplyr::rename(tiempo_raw = 1, tasa_pasiva = 2) %>%
  dplyr::mutate(tasa_pasiva = as.numeric(tasa_pasiva)) %>%
  tidyr::drop_na(tasa_pasiva) %>%
  dplyr::mutate(
    fecha = seq(from = as.Date("1996-01-01"), by = "month", length.out = dplyr::n())
  ) %>%
  dplyr::select(fecha, tasa_pasiva)

#imae (Serie Desestacionalizada) ----

imae_raw <- readxl::read_excel("Cuadros_de_salida_IMAE.xlsx", 
                               sheet = "IMAE", 
                               skip = 30, 
                               col_names = FALSE) %>%
  setNames(c("anio", "mes", 
             "orig_m", "orig_ia","orig_acum", "orig_pa", "_", 
             "sa_m", "sa_ia", "sa_acum" , "sa_pa", "__", 
             "tc_m", "tc_ia", "tc_acum" , "tc_pa", "___"))

print(names(imae_raw))
head(imae_raw, 20)

#selecionar la serie desestacionalizada
imae_ts_df <- imae_raw %>%
  dplyr::select(orig_m) %>%
  dplyr::mutate(
    imae = as.numeric(orig_m) / 100, #se divede 100 para que quede en decimal 
    # y sea compatible con las otras series del vare
    fecha = seq(from = as.Date("2006-01-01"), by = "month", length.out = dplyr::n())
  ) %>%
  dplyr::select(fecha, imae) %>%
  tidyr::drop_na()


print("nombres imae:")
print(names(imae_ts_df))

print("imae")
print(head(imae_ts_df, 12))

#nuevas variables para otro var----

if(FALSE){
  "
  
  Este var tendira estas variables:
  remesas
  Tasa de crecimiento del saldo de cartera de crédito comercial o de consumo
  La Proxy de Demanda (Ciclo y Riesgo):
  1. IMAE
  2. El Índice de Morosidad o Cartera Vencida
  ROA
  
  en esta caso se busca indice de morosidad o cartera vencida
  y cartera de credito o similar
  
  buscar las variables nuevas 
  lo hare con la api en otro archivo 
  el api de siboif no me corre mejor hago un json de FyU OSD MN y ME
  del semca
  
  fyu = fuentes y usos
  osd = otras sociedades de deposito
  mn y me = moneda nacional y extranjeras
  
  "
}


#unificar ----

dataset_banano <- list(siboif_completo_ts, remesas_ts, tasas_ts, imae_ts_df, df_credito) %>%
  purrr::reduce(dplyr::left_join, by = "fecha") %>%
  dplyr::arrange(fecha) %>%
  tidyr::drop_na() 

#agregar dummys

dataset_banano <- dataset_banano %>%
  dplyr::mutate(
    d2008  = dplyr::if_else(lubridate::year(fecha) == 2008, 1, 0),
    d2018  = dplyr::if_else(lubridate::year(fecha) == 2018, 1, 0),
    dcovid = dplyr::if_else(fecha >= as.Date("2020-03-01") & fecha <= as.Date("2021-12-01"), 1, 0)
  )

dplyr::glimpse(dataset_banano)

readr::write_csv(dataset_banano, "dataset_spillover_remesas_clean.csv")


