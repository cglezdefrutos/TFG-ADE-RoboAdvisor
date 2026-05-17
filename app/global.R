# ==============================================================================
# ARCHIVO: global.R
# Objetivo: Cargar dependencias, datos estáticos y portfolio base. Este script
#           se ejecuta una única vez al iniciar el servidor de la aplicación.
# ==============================================================================

# 1. Cargamos las librerías del backend y frontend
library(shiny)
library(bslib)
library(plotly)
library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog)
library(PerformanceAnalytics)

# 2. Cargamos los datos del script 01 y los separamos
ruta_datos <- "../data/processed/retornos_etfs.rds"
if(!file.exists(ruta_datos)) {
  stop("¡Error crítico de inicio! No se encuentra el archivo retornos_etfs.rds.")
}
retornos_totales <- readRDS(ruta_datos)
activos_riesgo <- c("SPY", "VGK", "EEM", "AGG", "GLD")
retornos_riesgo <- retornos_totales[, activos_riesgo]
retornos_rf <- retornos_totales$BIL

# 3. Inicializamos el portfolio base con esos activos
portafolio_base <- portfolio.spec(assets = activos_riesgo)

# 4. Introducimos las restricciones del modelo
# Restricción Presupuestaria: Invertir el 100% del capital (Suma de pesos = 1)
portafolio_base <- add.constraint(portfolio = portafolio_base,
                                  type = "full_investment")

# Restricción de No Negatividad: Solo posiciones largas (Pesos >= 0)
portafolio_base <- add.constraint(portfolio = portafolio_base,
                                  type = "long_only")

# Restricciones de Caja (Box Constraints): Forzar diversificación
portafolio_base <- add.constraint(portfolio = portafolio_base,
                                  type = "box",
                                  min = 0.05,
                                  max = 0.80)
