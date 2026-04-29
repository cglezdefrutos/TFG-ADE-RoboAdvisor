# ==============================================================================
# SCRIPT 01: EXTRACCIÓN, TRANSFORMACIÓN Y CARGA (ETL) DE DATOS FINANCIEROS
# Objetivo: Descargar precios de ETFs, limpiarlos y calcular retornos diarios.
# ==============================================================================

# 1. Cargar librerías necesarias
library(quantmod)
library(PerformanceAnalytics)

# 2. Definir el Universo de Inversión
activos_riesgo <- c("SPY", "VGK", "EEM", "AGG", "GLD")
ticker_rf <- "BIL"
tickers_totales <- c(activos_riesgo, ticker_rf)

# 3. Fase de extracción: Conexión con Yahoo Finance
cat("Descargando datos de Yahoo Finance...\n")
fecha_inicio <- "2014-01-01"
getSymbols(tickers_totales, src = "yahoo", from = fecha_inicio, auto.assign = TRUE)

# 4. Fase de transformación
# Extraemos solo la columna "Adjusted Close" de cada ETF y fusionamos en una lista
precios_lista <- list()
for (ticker in tickers_totales) {
  precios_lista[[ticker]] <- Ad(get(ticker))
}

# Unimos todas las columnas en una sola matriz alineada por fechas (objeto xts)
precios_matriz <- do.call(merge, precios_lista)

# Renombramos las columnas para que queden limpias (ej: "SPY" en vez de "SPY.Adjusted")
colnames(precios_matriz) <- tickers_totales

# Limpieza de los datos (Manejo de NAs)
# na.omit() elimina los días donde falta la cotización de algún activo.
precios_limpios <- na.omit(precios_matriz)

# Calculamos los retornos logarítmicos diarios
cat("Calculando retornos logarítmicos...\n")
retornos_diarios <- Return.calculate(precios_limpios, method = "log")

# Omitimos el primer día de retorno ya que siempre es NA (no hay día anterior para comparar)
retornos_diarios <- na.omit(retornos_diarios)

# 5. Fase de carga
# Guardamos los datos en formato .rds
ruta_guardado <- "data/processed/retornos_etfs.rds"
saveRDS(retornos_diarios, file = ruta_guardado)
cat("¡Proceso ETL Completado! Datos limpios guardados en:", ruta_guardado, "\n")

# 6. Matriz de correlaciones entre los ETFs
cat("\nCalculando matriz de correlaciones de Pearson...\n")
matriz_correlacion <- cor(retornos_diarios[, activos_riesgo])
print(round(matriz_correlacion, 2))

# Generamos un gráfico avanzado de correlaciones (función de la librería PerformanceAnalytics)
cat("\nGenerando gráfico de correlaciones...\n")
chart.Correlation(retornos_diarios[, activos_riesgo], histogram = TRUE, pch = 19)
