# ==============================================================================
# ARCHIVO: server.R
# Objetivo: Procesar los inputs del test, ejecutar el algoritmo de optimización
#           y devolver los gráficos y métricas a la interfaz.
# ==============================================================================

server <- function(input, output, session) {

  # 1. LÓGICA DE NEGOCIO Y REACCIÓN A EVENTOS

  # 1.1. Calculamos el nivel de riesgo (1-10) según las respuestas al test.
  # Este bloque se ejecuta cuando el usuario hace clic en el botón "Optimizar Cartera"
  nivel_riesgo <- eventReactive(input$calc, {
    # Sumamos los puntos de las 3 preguntas
    puntos <- sum(as.numeric(input$q1), as.numeric(input$q2), as.numeric(input$q3),
                  as.numeric(input$q4), as.numeric(input$q5), as.numeric(input$q6),
                  as.numeric(input$q7), as.numeric(input$q8), as.numeric(input$q9),
                  as.numeric(input$q10))

    # Normalizamos: de [10-100] a [1-10]
    nota <- round((puntos - 10) / (100 - 10) * 9 + 1)
    return(nota)
  })

  # Observador independiente para cambiar de pestaña
  observeEvent(input$calc, {
    nav_select("tabs_main", "panel_resultados")
  })

  # 1.2. Ejecutamos el motor de Markowitz
  pesos_cartera <- reactive({
    # Calculamos la aversión al riesgo
    riesgo <- nivel_riesgo()
    lambda <- ((11 - riesgo) * 10) * 0.5

    # Añadimos los objetivos al "portafolio_base" (cargado en global.R)
    port_obj <- add.objective(portfolio = portafolio_base, type = "return", name = "mean")
    port_obj <- add.objective(portfolio = port_obj, type = "risk", name = "var", risk_aversion = lambda)

    # Ejecutamos el solver matemático y devolvemos los pesos
    opt <- optimize.portfolio(R = retornos_riesgo, portfolio = port_obj, optimize_method = "ROI")
    pesos <- extractWeights(opt)
    return(round(pesos, 4))
  })

  # 1.3. Realizamos la simulación histórica (Backtesting)
  backtest_resultados <- reactive({
    pesos <- pesos_cartera()

    # Simulamos el comportamiento diario de la cartera con esos pesos
    retornos_cartera <- Return.portfolio(R = retornos_riesgo, weights = pesos)

    # Calculamos métricas exigidas
    rent_anual <- Return.annualized(retornos_cartera)
    var_95 <- VaR(retornos_cartera, p = 0.95, method = "historical")

    # Simulamos el crecimiento de un capital inicial de 10.000€
    crecimiento <- cumprod(1 + retornos_cartera) * 10000

    # Devolvemos una lista con todo lo calculado
    list(rentabilidad = rent_anual, var = var_95, serie_crecimiento = crecimiento)
  })

  # 2. RENDERIZADO (ENVIAMOS LOS RESULTADOS AL FRONTEND -> ui.R)

  # 2.1. Completamos las tarjetas superiores
  output$perfil_txt <- renderText({
    riesgo <- nivel_riesgo()
    if(riesgo <= 3) return(paste("Conservador (Riesgo", riesgo, "/ 10)"))
    if(riesgo <= 7) return(paste("Moderado (Riesgo", riesgo, "/ 10)"))
    return(paste("Agresivo (Riesgo", riesgo, "/ 10)"))
  })

  output$rent_txt <- renderText({
    res <- backtest_resultados()
    paste0(round(as.numeric(res$rentabilidad) * 100, 2), " %")
  })

  output$var_txt <- renderText({
    res <- backtest_resultados()
    paste0(round(as.numeric(res$var) * 100, 2), " %")
  })

  # 2.2. Generamos el gráfico del quesito de activos (distribución de pesos) con plotly
  output$plot_quesito <- renderPlotly({
    pesos <- pesos_cartera()
    activos <- names(pesos)
    plot_ly(labels = ~activos, values = ~pesos, type = 'pie',
            textinfo = 'label+percent', hoverinfo = 'text',
            text = ~paste(activos, ": ", round(pesos*100, 2), "%"),
            marker = list(colors = c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#ffd700'))) %>%
      layout(showlegend = FALSE, margin = list(t=0, b=0, l=0, r=0))
  })

  # 2.3. Generamos el gráfico de evolución del dinero (Backtesting)
  output$plot_evolucion <- renderPlotly({
    crecimiento <- backtest_resultados()$serie_crecimiento
    fechas <- index(crecimiento)
    valores <- as.numeric(crecimiento)

    plot_ly(x = ~fechas, y = ~valores, type = 'scatter', mode = 'lines',
            line = list(color = '#2c3e50', width = 2)) %>%
      layout(xaxis = list(title = "Año"),
             yaxis = list(title = "Capital (Base 10.000€)", tickformat = ".0f"),
             hovermode = "x unified") # Muestra el valor exacto al pasar el ratón
  })
}
