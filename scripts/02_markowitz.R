# ==============================================================================
# SCRIPT 02: OPTIMIZACIÓN DE CARTERA (EL MOTOR DE MARKOWITZ)
# Objetivo: Encontrar los pesos óptimos para minimizar el riesgo.
# ==============================================================================

# 1. Cargar librerías necesarias
library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog)

# 2. Cargar los datos limpios del script 01
ruta_datos <- "data/processed/retornos_etfs.rds"
if(!file.exists(ruta_datos)) {
  stop("¡Error! No se encuentra el archivo .rds. Ejecuta primero 01_etl_yahoo.R")
}

retornos_totales <- readRDS(ruta_datos)
activos_riesgo <- c("SPY", "VGK", "EEM", "AGG", "GLD")
retornos_riesgo <- retornos_totales[, activos_riesgo]
cat("Datos cargados correctamente. Activos:", paste(activos_riesgo, collapse=", "), "\n\n")

# 3. Inicializamos la cartera: Creamos un objeto PortfolioSpec base con los nombres de nuestros 5 ETFs
portafolio_base <- portfolio.spec(assets = activos_riesgo)

# 4. Añadimos las restricciones del modelo
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

# 5. Añadimos el objetivo matemático: Buscar la varianza más baja posible
portafolio_min_var <- add.objective(portfolio = portafolio_base,
                                    type = "risk",
                                    name = "var")

# 6. Ejecutamos el optimizador para el portafolio construido
cat("Ejecutando motor de Programación Cuadrática (ROI)...\n")
optimizacion <- optimize.portfolio(R = retornos_riesgo,
                                   portfolio = portafolio_min_var,
                                   optimize_method = "ROI",
                                   trace = TRUE)

# 7. Mostramos el resultado
cat("\n=== PESOS ÓPTIMOS (CARTERA DE MÍNIMA VARIANZA) ===\n")
pesos_optimos <- extractWeights(optimizacion)
print(round(pesos_optimos * 100, 2))

# 8. Creamos la función para realizar las pruebas de los perfiles
cat("\nCreando la función principal del Robo-Advisor...\n")
generar_cartera_optima <- function(nivel_riesgo) {
  if(nivel_riesgo < 1 || nivel_riesgo > 10) stop("El riesgo debe ser entre 1 y 10")

  # Transformamos el nivel de riesgo cualitativo a la aversión al riesgo (Lambda)
  lambda <- (11 - nivel_riesgo) * 10
  lambda_ponderada <- lambda * 0.5

  # Añadimos el doble objetivo a nuestra cartera base con restricciones
  port_obj <- add.objective(portfolio = portafolio_base, type = "return", name = "mean")
  port_obj <- add.objective(portfolio = port_obj, type = "risk", name = "var", risk_aversion = lambda_ponderada)

  # Ejecutamos el motor de optimización
  opt <- optimize.portfolio(R = retornos_riesgo,
                            portfolio = port_obj,
                            optimize_method = "ROI")

  # Extraemos y devolvemos los pesos óptimos
  pesos <- extractWeights(opt)
  return(round(pesos * 100, 2))
}

# 9. Pruebas de perfiles de inversión
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
