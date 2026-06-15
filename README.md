# Efecto Spillover de las Remesas sobre la Rentabilidad del Sistema Bancario Nicaragüense (2019-2025)

Este repositorio contiene la base de datos mensual construida, los scripts de procesamiento y el código de estimación econométrica para replicar de forma íntegra el artículo de investigación titulado: *"Efecto Spillover de las Remesas sobre la Rentabilidad del Sistema Bancario Nicaragüense (2019-2025)"*.

---

## Resumen

*Este estudio evalúa empíricamente si los flujos de remesas familiares hacia Nicaragua (los cuales superan el 25% del PIB nacional) generan un efecto derrame (spillover) sobre el desempeño operativo y la rentabilidad de la banca comercial formal.*

*A través de un enfoque de Vectores Autorregresivos Estructurales (SVAR) con frecuencia mensual e identificación recursiva de Cholesky, se modelaron dos canales de transmisión macrofinanciera: el Canal de Liquidez y el Canal de Títulos.*

### Resultados

- Impacto Nulo sobre el ROA: Un choque positivo y unitario en la tasa de crecimiento de las remesas ($g_t^r$) no produce un impacto estadísticamente significativo sobre la rentabilidad sobre activos ($ROA_t$) en ningún horizonte temporal (las bandas de confianza al 95% calculadas por *bootstrap* siempre contienen el cero).
- Confirmación de la Hipótesis del *Lazy Bank*: La descomposición de varianza del error de predicción ($FEVD$) revela que las remesas explican menos del 0.3% de la variabilidad del ROA. En su lugar, el exceso de liquidez exógena es absorbido pasivamente por las instituciones, acumulándose de forma defensiva en activos disponibles ($\Delta L_t$) o títulos valores de bajo riesgo ($\Delta T_t$).
- Ausencia de Causalidad de Granger: Los test de causalidad arrojan $p$-valores superiores a 0.87, confirmando que la trayectoria pasada de las remesas carece por completo de poder predictivo sobre la rentabilidad del sistema.

---

## Metodología

Los modelos estructurales comparten una descomposición recursiva bajo la siguiente jerarquía de transmisión:

1.  Modelo 1 (Canal de Liquidez): $y_t^L = (g_t^r, \Delta i_t^p, \Delta L_t, ROA_t)'$

2.  Modelo 2 (Canal de Títulos): $y_t^T = (g_t^r, \Delta i_t^p, \Delta T_t, ROA_t)'$

Donde $g_t^r$ representa la tasa de crecimiento continuo de las remesas desestacionalizadas, $\Delta i_t^p$ es la variación de la tasa de interés pasiva ponderada, $\Delta L_t$ es la variación del índice de liquidez, $\Delta T_t$ es el ratio de inversión en títulos y $ROA_t$ es la rentabilidad sobre activos.

### Controles Exógenos y Robustez

El bloque exógeno incorpora variables indicadoras para capturar quiebres estructurales validados formalmente mediante el test de Bai y Perron (Crisis sociopolítica 2018 y el *boom* de remesas post-pandemia en abril de 2020), además del crecimiento del IMAE ($g_t^y$) para aislar el efecto del ciclo económico real. La robustez del modelo se validó mediante Funciones de Impulso-Respuesta Generalizadas (GIRF) de Pesaran-Shin (1998) presentadas en el Anexo del documento.

---

## Estructura del Repositorio

- `/`: Archivos principales del manuscrito académico en formato reproducible (`.qmd`, `.pdf`).
- `grafico_variables.pdf` y `grafico_variables_transformadas.pdf`: Visualización de las series originales y transformadas estacionarias $I(0)$.
- `raices_var.png` y `cusm.pdf`: Gráficos de diagnóstico (estabilidad matemática de las raíces del polinomio característico y test CUSUM de estabilidad de parámetros).
- `03_graficos_irf_remesas.pdf` y `03_graficos_irf_remesas_titulos.pdf`: Funciones de impulso-respuesta estructurales.
- `04_graficos_irf_generales_pesaran.pdf`: Gráficos de robustez correspondientes a las GIRF.
- `05_grafico_fevd_roa_formal.pdf`: Descomposición de varianza visual del error de predicción.

---

## Entorno de Desarrollo y Software

Todo el flujo de trabajo desde la manipulación de datos hasta la modelación y visualización se desarrolló en RStudio con R.

### Paquetes Principales Utilizados:

- Ecosistema de Datos: `tidyverse`, `readxl`, `janitor`, `lubridate`.
- Análisis de Series de Tiempo y Econometría: `vars` (Estimación SVAR y diagnósticos), `urca` y `tseries` (Pruebas ADF y KPSS), `strucchange` (Test de quiebres estructurales de Bai y Perron) y `seasonal` (Algoritmo de desestacionalización X-13ARIMA-SEATS).
- Visualización: `ggplot2`.

---

## Instrucciones para la Replicabilidad

Si deseas replicar las estimaciones o auditar las rutinas de código:

1.  Tener instalado RStudio u otro enterno de desarrollo.
2.  Clone este repositorio en tu entorno local: `git clone https://github.com/Emerson-nic/econometria-spillover.git`
