# Efecto Spillover de las Remesas sobre la Rentabilidad del Sistema Bancario Nicaragüense (2019-2025)

Este repositorio contiene la base de datos mensual construida, los scripts de procesamiento y el código de estimación econométrica para replicar de forma íntegra el artículo de investigación titulado: *"Efecto Spillover de las Remesas sobre la Rentabilidad del Sistema Bancario Nicaragüense (2019-2025)"*.

------------------------------------------------------------------------

## Resumen

*Nicaragua recibe remesas familiares equivalentes a más del 25% de su Producto Interno Bruto (PIB), una inyección masiva de liquidez que es captada casi en su totalidad por el sistema bancario. Este estudio evalúa empíricamente si dicha afluencia exógena genera un efecto derrame (spillover) sobre la intermediación crediticia y la rentabilidad de la banca comercial formal.*

*A través de un enfoque dual que combina Vectores Autorregresivos Estructurales (SVAR) para el corto plazo y un Modelo de Corrección de Errores Vectorial (VECM) para el equilibrio asintótico de largo plazo, descubrimos que la banca nicaragüense opera bajo el paradigma del Banco Perezoso (Lazy Bank Hypothesis).*

### Resultados

- Impacto Nulo en el Corto Plazo: Un choque positivo en la tasa de crecimiento de las remesas no produce una expansión crediticia ni altera la rentabilidad sobre activos (ROA_t). Las instituciones financieras esterilizan pasivamente la liquidez internacional en sus hojas de balance.
- El Canal de Transmisión Indirecto: El modelo VECM devela que las remesas no financian la producción de forma directa. En su lugar, el flujo de divisas dinamiza el consumo de los hogares y la actividad real (IMAE_t). Es esta reactivación económica la que, de forma rezagada, arrastra a los bancos a expandir su oferta de crédito.
- Sensibilidad al Costo de Fondeo: Las decisiones de colocación están gobernadas por la rígida administración de los costos internos (tasa pasiva) y la percepción del riesgo del ciclo económico, no por la holgura de liquidez externa.

------------------------------------------------------------------------

## Arquitectura Metodológica

El diseño econométrico se divide en dos fases:

### 1. Dinámica Transitoria (SVAR)

Se estimaron tres modelos estructurales mediante una descomposición recursiva de Cholesky, asumiendo la exogeneidad contemporánea de las remesas: 

* Canal de Crédito: $y_t^C = (g_t^r, \Delta i_t^p, g_t^c, ROA_t)'$ 
* Canal de Liquidez: $y_t^L = (g_t^r, \Delta i_t^p, \Delta L_t, ROA_t)'$ 
* Canal de Títulos: $y_t^T = (g_t^r, \Delta i_t^p, \Delta T_t, ROA_t)'$

### 2. Dinámica Estructural (VECM)

Para auditar la transmisión a largo plazo, se modeló el espacio de cointegración de Johansen con las variables macro-financieras en niveles ($K=2$, $r=2$): 

* Sistema asintótico: $\ln(\text{remesas}_t) \rightarrow \ln(\text{IMAE}_t) \rightarrow i_t^p \rightarrow \ln(\text{crédito}_t)$

Controles Exógenos: Ambos enfoques incorporan variables indicadoras para aislar quiebres estructurales severos validados mediante el algoritmo de Bai y Perron (crisis sociopolítica de 2018 y el boom de remesas post-pandemia en 2020).
---
## Estructura del Repositorio

* `/`: Archivos principales del manuscrito académico en formato reproducible (`.qmd`, `.pdf`).
* `grafico_variables.pdf` y `grafico_variables_transformadas.pdf`: Visualización de las series originales y transformadas estacionarias I(0).
* `raices_var_credito.png` y `05_raices_vecm.png`: Gráficos de diagnóstico matemático y estabilidad de los sistemas matriciales.
* `06_irf_orto_remesas.pdf` y `07_irf_orto_imae_credito.pdf`: Funciones de Impulso-Respuesta (IRF) ortogonales del modelo VECM.
* `05_grafico_fevd_credito_formal.pdf`: Descomposición de varianza del error de predicción (FEVD) extendida a 36 meses.

*(Nota: Todos los outputs gráficos y tabulares se generan directamente desde el script de replicación).*
---

## Entorno de Desarrollo y Replicabilidad

Todo el flujo de trabajo, desde el suavizado estacional hasta la modelación estructural y visualización, fue desarrollado en RStudio.

### Stack de Librerías en R:

- Manipulación de Datos: `tidyverse`, `readxl`, `janitor`, `lubridate`, `usethis`.
- Econometría y Series de Tiempo: `vars` (SVAR y VECM), `urca` y `tseries` (Pruebas estocásticas de Johansen, ADF, KPSS), `strucchange` (Quiebres estructurales).
- Filtrado Estacional: `seasonal` (Algoritmo X-13ARIMA-SEATS).
- Visualización: `ggplot2`.

### Instrucciones para replicar:
 
Clona este repositorio abriendo tu terminal y ejecutando: ```git clone [https://github.com/Emerson-nic/econometria-spillover.git](https://github.com/Emerson-nic/econometria-spillover.git)```
