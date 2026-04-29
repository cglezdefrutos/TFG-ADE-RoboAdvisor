# ==============================================================================
# SCRIPT 01: EXTRACCIÓN, TRANSFORMACIÓN Y CARGA (ETL) DE DATOS FINANCIEROS
# Objetivo: Descargar precios de ETFs, limpiarlos y calcular retornos diarios.
# ==============================================================================

# 1. Cargar librerías necesarias
library(quantmod)
library(PerformanceAnalytics)

# 2. Definir el Universo de Inversión (Los 5 ETFs justificados)
activos_riesgo <- c("SPY", "VGK", "EEM", "AGG", "GLD")
ticker_rf <- "BIL"
tickers_totales <- c(activos_riesgo, ticker_rf)

# 3. EXTRACCIÓN: Conexión con Yahoo Finance (12 años de histórico)
cat("Descargando datos de Yahoo Finance...\n")
fecha_inicio <- "2014-01-01"
getSymbols(tickers_totales, src = "yahoo", from = fecha_inicio, auto.assign = TRUE)

# 4. TRANSFORMACIÓN 1: Extraer solo la columna "Adjusted Close" y fusionar
precios_lista <- list()
for (ticker in tickers_totales) {
  precios_lista[[ticker]] <- Ad(get(ticker))
}

# Unimos todas las columnas en una sola matriz alineada por fechas (objeto xts)
precios_matriz <- do.call(merge, precios_lista)

# Renombramos las columnas para que queden limpias (ej: "SPY" en vez de "SPY.Adjusted")
colnames(precios_matriz) <- tickers_totales

# 5. LIMPIEZA DE DATOS (Manejo de NAs)
# Los mercados cierran en festivos diferentes (ej. Europa vs EEUU).
# na.omit() elimina los días donde falta la cotización de algún activo.
precios_limpios <- na.omit(precios_matriz)

# 6. TRANSFORMACIÓN 2: Calcular retornos logarítmicos diarios
cat("Calculando retornos logarítmicos...\n")
retornos_diarios <- Return.calculate(precios_limpios, method = "log")

# El primer día de retorno siempre es NA (porque no hay día anterior para comparar)
retornos_diarios <- na.omit(retornos_diarios)

# 7. CARGA (Load): Guardar los datos procesados
# Guardamos en formato .rds (Formato nativo de R, muy rápido de leer para Shiny)
ruta_guardado <- "data/processed/retornos_etfs.rds"
saveRDS(retornos_diarios, file = ruta_guardado)

cat("¡ETL Completado! Datos limpios guardados en:", ruta_guardado, "\n")

# 8. VALIDACIÓN: Matriz de Correlaciones
# Objetivo: Demostrar matemáticamente la descorrelación del universo de activos.
cat("\nCalculando matriz de correlaciones de Pearson...\n")

# Calculamos la matriz matemática
matriz_correlacion <- cor(retornos_diarios[, activos_riesgo])

# La imprimimos por consola redondeada a 2 decimales
print(round(matriz_correlacion, 2))

# Generamos un gráfico avanzado de correlaciones (se abrirá en el panel Plots)
# Esta función es nativa de la librería PerformanceAnalytics que ya hemos cargado
cat("\nGenerando gráfico de correlaciones...\n")
chart.Correlation(retornos_diarios[, activos_riesgo], histogram = TRUE, pch = 19)
