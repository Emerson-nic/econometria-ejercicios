# instalar y cargar ----
options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  urca, tseries, dplyr, vars, readxl, tidyr
)

data <- read_excel("data.xlsx")

# preparacion de datos 
series <- c("itcer",
            "ipi_eeuu",
            "iti",
            "transable",
            "remesas",
            "no_transable")

data_cointegracion <- data %>%
  dplyr::select(itcer, ipi_eeuu, iti, transable, remesas) %>%
  na.omit()

#Johenson test sin correcion de quiebres ====

#especificaciones

#rezagos
lag_select <- VARselect(data_cointegracion, lag.max = 8, type = "const")
p_lag <- lag_select$selection["AIC(n)"]

#test de Johansen
#'ecdet = const' asume una constante en la relación de cointegracion
johansen_test <- ca.jo(data_cointegracion, 
                       type = "trace", 
                       ecdet = "const", 
                       K = p_lag)
#resultados
summary(johansen_test)

#johnsen test con amortiguadores estructurales ----

#dummys
data_con_dummies <- data %>% 
  mutate(
    dummy_2008 = ifelse(año == 2008 & mes %in% c("III", "IV"), 1, 0),
    dummy_2018 = ifelse(año == 2018 & mes %in% c("II", "III", "IV"), 1, 0),
    dummy_2020 = ifelse(año == 2020, 1, 0)
  )

#combinar al dataset

variables_endogenas <- data_con_dummies %>%
  dplyr::select(itcer, ipi_eeuu, iti, transable, remesas)

variables_exogenas <- data_con_dummies %>%
  dplyr::select( dummy_2008,
                 dummy_2018,
                 dummy_2020)

#especificar de nuevo

lag_select_nuevo <- VARselect(variables_endogenas, lag.max = 8, type = "const", 
                              exogen = variables_exogenas)

p_lag_nuevo <- lag_select_nuevo$selection["AIC(n)"]

#johansen test

johansen_test1 <- ca.jo(variables_endogenas, 
                        type = "trace", 
                        ecdet = "const", 
                        K = p_lag_nuevo,
                        dumvar = variables_exogenas)
#resultados
summary(johansen_test1)