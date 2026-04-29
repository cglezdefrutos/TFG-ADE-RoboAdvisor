# ==============================================================================
# SCRIPT 02: OPTIMIZACIÓN DE CARTERA (EL MOTOR DE MARKOWITZ)
# Objetivo: Encontrar los pesos óptimos para minimizar el riesgo (Varianza).
# ==============================================================================

# 1. Cargar librerías necesarias
library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog) # Motor matemático para optimización convexa

# 2. Cargar los datos limpios del Hito 1 (Capa de Persistencia)
ruta_datos <- "data/processed/retornos_etfs.rds"
if(!file.exists(ruta_datos)) {
  stop("¡Error! No se encuentra el archivo .rds. Ejecuta primero 01_etl_yahoo.R")
}

retornos_totales <- readRDS(ruta_datos)
activos_riesgo <- c("SPY", "VGK", "EEM", "AGG", "GLD")
retornos_riesgo <- retornos_totales[, activos_riesgo]

cat("Datos cargados correctamente. Activos:", paste(activos_riesgo, collapse=", "), "\n\n")

# 3. Inicializar la cartera
# Creamos un objeto PortfolioSpec base con los nombres de nuestros 5 ETFs
portafolio_base <- portfolio.spec(assets = activos_riesgo)

# 4. Añadir Restricciones (Las reglas del juego)
# 4.1. Restricción Presupuestaria: Invertir el 100% del capital (Suma de pesos = 1)
portafolio_base <- add.constraint(portfolio = portafolio_base,
                                  type = "full_investment")

# 4.2. Restricción de No Negatividad: Solo posiciones largas (Pesos >= 0)
portafolio_base <- add.constraint(portfolio = portafolio_base,
                                  type = "long_only")

# 4.3. Restricciones de Caja (Box Constraints): Forzar diversificación
portafolio_base <- add.constraint(portfolio = portafolio_base,
                                  type = "box",
                                  min = 0.05,
                                  max = 0.80)

# 5. Añadir Objetivo Matemático
# Le decimos al algoritmo que su misión es buscar la varianza más baja posible
portafolio_min_var <- add.objective(portfolio = portafolio_base,
                                    type = "risk",
                                    name = "var")

# 6. Ejecutar el Optimizador (El Músculo Matemático)
cat("Ejecutando motor de Programación Cuadrática (ROI)...\n")
optimizacion <- optimize.portfolio(R = retornos_riesgo,
                                   portfolio = portafolio_min_var,
                                   optimize_method = "ROI",
                                   trace = TRUE)

# 7. Resultados
cat("\n=== PESOS ÓPTIMOS (CARTERA DE MÍNIMA VARIANZA) ===\n")
pesos_optimos <- extractWeights(optimizacion)
print(round(pesos_optimos * 100, 2)) # Multiplicado por 100 para verlo en %

# 8. La Función del Robo Advisor (El Entregable del Hito 2)
cat("\nCreando la función principal del Robo-Advisor...\n")

generar_cartera_optima <- function(nivel_riesgo) {
  # Validamos que el nivel de riesgo esté entre 1 y 10
  if(nivel_riesgo < 1 || nivel_riesgo > 10) stop("El riesgo debe ser entre 1 y 10")

  # 1. Transformación del nivel cualitativo a Aversión al Riesgo (Lambda)
  lambda_base <- (11 - nivel_riesgo) * 10
  lambda_taylor <- lambda_base * 0.5

  # Añadimos el doble objetivo a nuestra cartera base con restricciones
  port_obj <- add.objective(portfolio = portafolio_base, type = "return", name = "mean")
  port_obj <- add.objective(portfolio = port_obj, type = "risk", name = "var", risk_aversion = lambda_taylor)

  # Ejecutamos el motor de optimización
  opt <- optimize.portfolio(R = retornos_riesgo,
                            portfolio = port_obj,
                            optimize_method = "ROI")

  # Extraemos pesos
  pesos <- extractWeights(opt)
  return(round(pesos * 100, 2))
}

# --- PRUEBAS DE PERFILES DE INVERSIÓN ---
cat("\nSimulando Cliente 1: Perfil CONSERVADOR\n")
print(generar_cartera_optima(nivel_riesgo =1))
print(generar_cartera_optima(nivel_riesgo =2))
print(generar_cartera_optima(nivel_riesgo =3))


cat("\nSimulando Cliente 2: Perfil MODERADO\n")
print(generar_cartera_optima(nivel_riesgo =4))
print(generar_cartera_optima(nivel_riesgo =5))
print(generar_cartera_optima(nivel_riesgo =6))
print(generar_cartera_optima(nivel_riesgo =7))

cat("\nSimulando Cliente 3: Perfil AGRESIVO\n")
print(generar_cartera_optima(nivel_riesgo = 8))
print(generar_cartera_optima(nivel_riesgo = 9))
print(generar_cartera_optima(nivel_riesgo = 10))
