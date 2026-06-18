#descripcion ----

if (FALSE) {
  "
  
  obtuve datos en jhonson, lo hago porque quiero ver
  si puedo obtener datos historicos de credito, la siboif en junio del 2026 
  solo esta disponible datos mensuaes del 2025 al 2026, obs insificientes
  para un svar 
  
  el credito esta expresado en millones de cordobas
  
  "
}

#cargar librerias ---- 
options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               httr,
               jsonlite,
               dplyr,
               tidyr,
               usethis #para .Renviron igual que .env de python
               )

#leer json ----
datos_crudos <- fromJSON("FyU_OSD_MN_y_ME.json")




#variable credito ----
df_completo <- bind_rows(datos_crudos, .id = "fecha_texto")

#nombre 
meses_es <- c("Enero" = "01", "Febrero" = "02", "Marzo" = "03", "Abril" = "04", 
              "Mayo" = "05", "Junio" = "06", "Julio" = "07", "Agosto" = "08", 
              "Septiembre" = "09", "Octubre" = "10", "Noviembre" = "11", "Diciembre" = "12")

print(names(df_completo))

#limpieza y tranformar credito ----
df_credito <- df_completo %>%
  select(
    fecha_texto,
    empresas = `6. Otras sociedades no financieras`,
    hogares = `7. Hogares e ISFLSH`
  ) %>%
  separate(fecha_texto, into = c("mes_texto", "anio"), sep = " - ") %>%
  mutate(
    mes_texto = str_trim(mes_texto),
    anio = str_trim(anio),
    mes_num = meses_es[mes_texto],
    
    fecha = as.Date(paste(anio, mes_num, "01", sep = "-")),
    
    credito_privado = empresas + hogares
  ) %>%
  select(fecha, credito_privado) %>%
  arrange(fecha)

#datos para el svar ---

print(head(df_credito))

#exportar para juntarlo a limpieza_spillover

readr::write_csv(df_credito, "df_credito.csv")

