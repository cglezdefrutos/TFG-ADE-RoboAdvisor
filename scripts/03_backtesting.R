# ==============================================================================
# SCRIPT 03: BACKTESTING Y EVALUACIÓN DE DESEMPEÑO
# Objetivo: Simular la rentabilidad histórica de las carteras y calcular
#           las métricas de riesgo y desempeño del Marco Teórico.
# ==============================================================================

# 1. Cargar librerías
library(PerformanceAnalytics)
library(tidyverse)
library(tidyquant)
library(gridExtra)

# 2. Cargar los datos limpios del script 01 y separarlos
ruta_datos <- "data/processed/retornos_etfs.rds"
if(!file.exists(ruta_datos)) {
  stop("¡Error! No se encuentra el archivo .rds. Ejecuta primero 01_etl_yahoo.R")
}

retornos_totales <- readRDS(ruta_datos)
activos_riesgo <- c("SPY", "VGK", "EEM", "AGG", "GLD")
retornos_activos <- retornos_totales[, activos_riesgo]
retornos_rf <- retornos_totales$BIL
retornos_mercado <- retornos_totales$SPY

# 3. Simulación Histórica (backtesting) de las carteras para los 3 perfiles
pesos_conservador <- c(0.05, 0.05, 0.05, 0.80, 0.05)
pesos_moderado <- c(0.0555, 0.05, 0.05, 0.7666, 0.0780)
pesos_agresivo <- c(0.3076, 0.05, 0.05, 0.2397, 0.3527)

retornos_conservador <- Return.portfolio(R = retornos_activos, weights = pesos_conservador)
retornos_moderado    <- Return.portfolio(R = retornos_activos, weights = pesos_moderado)
retornos_agresivo    <- Return.portfolio(R = retornos_activos, weights = pesos_agresivo)

# Unimos las tres carteras en un solo objeto para poder compararlas
carteras_simuladas <- merge(retornos_conservador, retornos_moderado, retornos_agresivo)
colnames(carteras_simuladas) <- c("Conservador", "Moderado", "Agresivo")


# 4. Cálculo de métricas
cat("\n=== MÉTRICAS DE RIESGO Y DESEMPEÑO (Anualizadas) ===\n")

rentabilidad_anual <- Return.annualized(carteras_simuladas)
var_95 <- VaR(carteras_simuladas, p = 0.95, method = "historical")

nombres_carteras <- colnames(carteras_simuladas)
sharpe_anual <- numeric(3)
beta_capm <- numeric(3)
alfa_jensen <- numeric(3)

# Evaluamos las carteras una por una para poder utilizar BIL como Rf con
# precisión diaria en vez de realizar su media
for(i in 1:3) {
  cartera_actual <- carteras_simuladas[, i]
  sharpe_anual[i] <- SharpeRatio.annualized(cartera_actual, Rf = retornos_rf)
  beta_capm[i]    <- CAPM.beta(Ra = cartera_actual, Rb = retornos_mercado)
  alfa_jensen[i]  <- CAPM.jensenAlpha(Ra = cartera_actual, Rb = retornos_mercado, Rf = retornos_rf)
}

# Consolidamos los datos y los imprimimos en una tabla
tabla_resultados <- rbind(
  Rentabilidad_Anual_Pct = as.numeric(round(rentabilidad_anual * 100, 2)),
  VaR_95_Diario_Pct      = as.numeric(round(var_95 * 100, 2)),
  Ratio_Sharpe           = as.numeric(round(sharpe_anual, 2)),
  Beta_Mercado           = as.numeric(round(beta_capm, 2)),
  Alfa_Jensen_Pct        = as.numeric(round(alfa_jensen * 100, 4))
)
colnames(tabla_resultados) <- c("Conservador", "Moderado", "Agresivo")
print(tabla_resultados)

# 5. Creamos el dashboard visual
cat("\nGenerando dashboard...\n")

# Transformación a formato largo y cálculo de Drawdowns
df_base <- data.frame(Fecha = index(carteras_simuladas), coredata(carteras_simuladas)) %>%
  pivot_longer(-Fecha, names_to = "Perfil", values_to = "Retorno")

df_completo <- df_base %>%
  group_by(Perfil) %>%
  mutate(
    Crecimiento = 10000 * cumprod(1 + Retorno),
    Max_Hist = cummax(Crecimiento),
    Drawdown = (Crecimiento - Max_Hist) / Max_Hist
  ) %>%
  ungroup()

# Creamos los paneles individuales con ggplot2
colores_perfil <- c("Agresivo" = "#D55E00", "Moderado" = "#E69F00", "Conservador" = "#0072B2")
plot_1 <- ggplot(df_completo, aes(x = Fecha, y = Crecimiento, color = Perfil)) +
  geom_line(size = 0.55) +
  scale_color_manual(values = colores_perfil) +
  labs(title = "Evolución Histórica de la Inversión (Base 10.000€)", y = "Euros (€)", x = NULL) +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank(), plot.title = element_text(size = 14, face = "bold"))

plot_2 <- ggplot(df_completo, aes(x = Fecha, y = Retorno, fill = Perfil)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Perfil, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = colores_perfil) +
  labs(title = "Distribución de Retornos Diarios", y = "Retorno %", x = NULL) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(legend.position = "none", strip.background = element_blank())

plot_3 <- ggplot(df_completo, aes(x = Fecha, y = Drawdown, fill = Perfil)) +
  geom_area(alpha = 0.7) +
  facet_wrap(~Perfil, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = colores_perfil) +
  labs(title = "Evolución de Drawdowns", y = "Pérdida %", x = NULL) +
  scale_y_continuous(labels = scales::percent, limits = c(-0.25, 0)) +
  theme_minimal() +
  theme(legend.position = "none", strip.background = element_blank())

# Combinamos los paneles en un solo dashboard y lo guardamos
dashboard_definitivo <- grid.arrange(plot_1, plot_2, plot_3, heights = c(1, 1.2, 1.2))
ruta_guardado <- "results/dashboard_backtesting_mifid2.png"
if(!dir.exists("results")) dir.create("results")
ggsave(filename = ruta_guardado, plot = dashboard_definitivo, width = 12, height = 12, dpi = 300, bg = "white")
cat("¡Dashboard generado y guardado exitosamente en la carpeta 'results'!\n")
