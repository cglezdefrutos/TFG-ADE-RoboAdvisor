# ==============================================================================
# ARCHIVO: global.R
# Objetivo: Cargar dependencias, datos estáticos y reglas matemáticas base.
# Este script se ejecuta una única vez al iniciar el servidor de la aplicación.
# ==============================================================================

# 1. Cargar librerías del backend (Matemáticas) y frontend (Visualización)
library(shiny)               # Framework web nativo de R
library(bslib)               # Temas de diseño modernos (Bootstrap 5)
library(plotly)              # Gráficos interactivos HTML (Hover, zoom)
library(PortfolioAnalytics)  # Motor de Markowitz
library(ROI)                 # Framework de optimización
library(ROI.plugin.quadprog) # Solver de programación cuadrática
library(PerformanceAnalytics)# Cálculo de VaR, Sharpe y Beta

# 2. Carga de Datos (Capa de Persistencia)
# Carga de Datos del Hito 1
ruta_datos <- "../data/processed/retornos_etfs.rds"
if(!file.exists(ruta_datos)) {
  stop("¡Error crítico de inicio! No se encuentra el archivo retornos_etfs.rds.")
}
retornos_totales <- readRDS(ruta_datos)

# 3. Excluimos el activo libre de riesgo del resto
activos_riesgo <- c("SPY", "VGK", "EEM", "AGG", "GLD")
retornos_riesgo <- retornos_totales[, activos_riesgo]
retornos_rf <- retornos_totales$BIL

# 4. Inicialización del Motor de Optimización Estático
portafolio_base <- portfolio.spec(assets = activos_riesgo)

# Restricción Presupuestaria: Inversión total (Suma pesos = 100%)
portafolio_base <- add.constraint(portfolio = portafolio_base,
                                  type = "full_investment")

# Restricción Operativa: No ventas en corto (Pesos >= 0)
portafolio_base <- add.constraint(portfolio = portafolio_base,
                                  type = "long_only")

# Restricciones de Caja: Evitar soluciones de esquina (Min 5%, Max 80%)
portafolio_base <- add.constraint(portfolio = portafolio_base,
                                  type = "box",
                                  min = 0.05,
                                  max = 0.80)
