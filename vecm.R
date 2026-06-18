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
  if (file.exists("dataset_vecm.csv")) {
    dataset_vecm <- readr::read_csv("dataset_vecm.csv") %>%
      dplyr::mutate(fecha = as.Date(fecha))
  } else {
    source("johansen_o_diff.R")
  }
}

# bloque cointegrado ----

#variables a ocupar

cointegrax <- dataset_vecm %>%
  dplyr::select(log_remesa, log_credito, log_imae, tasa_pasiva) %>%
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

#print(round(seleccion_rezagos$criteria, 4))
