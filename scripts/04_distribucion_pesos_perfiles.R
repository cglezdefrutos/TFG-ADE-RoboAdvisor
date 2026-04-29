# ==============================================================================
# SCRIPT DEFECTIVO: GENERADOR DE GRÁFICO COMBINADO PARA LA MEMORIA (ggplot2)
# ==============================================================================

# 1. Cargar librerías necesarias
library(tidyverse)
library(gridExtra)

# 2. Definir los datos y ATAR LOS COLORES EXACTOS DE LA APP WEB
activos <- c("SPY", "VGK", "EEM", "AGG", "GLD")

# Usamos un vector "nombrado" para que ggplot no se líe alfabéticamente
colores <- c("SPY" = '#1f77b4',  # Azul
             "VGK" = '#ff7f0e',  # Naranja
             "EEM" = '#2ca02c',  # Verde
             "AGG" = '#d62728',  # Rojo
             "GLD" = '#ffd700')  # Amarillo

crear_df_pesos <- function(pesos, perfil) {
  data.frame(
    Activo = activos,
    Peso = pesos,
    Perfil = perfil
  ) %>%
    mutate(
      label = paste0(Activo, "\n", round(Peso, 2), "%"),
      Peso_num = Peso / 100
    )
}

# Pesos exactos del Script 02
df_cons <- crear_df_pesos(c(5.00, 5.00, 5.00, 80.00, 5.00), "Perfil Conservador (Riesgo 2/10)")
df_mod  <- crear_df_pesos(c(5.55, 5.00, 5.00, 76.66, 7.80), "Perfil Moderado (Riesgo 7/10)")
df_agr  <- crear_df_pesos(c(30.76, 5.00, 5.00, 23.97, 35.27), "Perfil Agresivo (Riesgo 10/10)")

# Combinamos en un solo data frame
df_total <- rbind(df_cons, df_mod, df_agr)

# 3. Definir la función de trazado profesional con ggplot2
plot_profesional <- function(df_perfil) {
  ggplot(df_perfil, aes(x = "", y = Peso_num, fill = Activo)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar("y", start = 0) +
    # Dejamos SOLO el porcentaje dentro del quesito, con tamaño de letra ajustado
    geom_text(aes(label = paste0(round(Peso, 2), "%")),
              position = position_stack(vjust = 0.5),
              color = "white", size = 4, fontface = "bold") +
    scale_fill_manual(values = colores) +
    theme_void() +
    theme(
      legend.position = "bottom", # Movemos los nombres a la leyenda
      legend.title = element_blank(), # Quitamos la palabra "Activo" de la leyenda
      legend.text = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.margin = margin(t = 20, r = 10, b = 20, l = 10)
    ) +
    labs(title = df_perfil$Perfil[1])
}

# Generamos los tres gráficos individuales
p1 <- plot_profesional(df_cons)
p2 <- plot_profesional(df_mod)
p3 <- plot_profesional(df_agr)

# 4. Combinar en un único lienzo
p_final <- grid.arrange(p1, p2, p3, ncol = 3)

# 5. GUARDAR CON ALTA RESOLUCIÓN PARA EL TFG (300 DPI)
cat("\nGuardando gráfico final en alta resolución...\n")
ruta_guardado <- "results/pesos_perfiles_inversion.png"
if(!dir.exists("results")) dir.create("results")

ggsave(filename = ruta_guardado, plot = p_final, width = 16, height = 7, dpi = 300, bg = "white")
cat("¡Gráfico guardado en:", ruta_guardado, "!\n")
