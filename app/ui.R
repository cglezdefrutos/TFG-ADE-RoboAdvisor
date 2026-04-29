# ==============================================================================
# ARCHIVO: ui.R
# Objetivo: Mostrar la interfaz del Robo-Advisor al usuario cuando ejecute la
#           la aplicación con los datos correspondientes
# ==============================================================================

ui <- page_fillable(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = "Robo-Advisor TFG",
  div(
    class = "d-flex align-items-center p-3 mb-3 bg-light rounded shadow-sm",
    img(src = "logo_ucm.png", height = "70px", class = "me-4"),
    div(
      h2("Robo-Advisor Financiero", class = "mb-0 text-primary", style = "font-weight: bold;"),
      p("Trabajo de Fin de Grado - Universidad Complutense de Madrid", class = "text-muted mb-0", style = "font-size: 1.1rem;")
    )
  ),

  navset_tab(
    id = "tabs_main", # ID para poder cambiar de pestaña desde el servidor

    # PESTAÑA 1: CUESTIONARIO
    nav_panel(
      title = "1. Test de Idoneidad (MiFID II)",
      value = "panel_test",

      card(
        card_header(class = "bg-warning", "Advertencia Importante"),
        p("De acuerdo con la normativa MiFID II, este Robo-Advisor tiene la responsabilidad de llevar a cabo una evaluación de idoneidad. ",
          strong("El objetivo es permitir que la empresa actúe en su mejor interés"),
          " adaptando la cartera a su situación financiera, conocimientos y objetivos de riesgo.")
      ),

      layout_column_wrap(
        width = 1/2, # Dos columnas de preguntas
        # BLOQUE A: CONOCIMIENTOS Y EXPERIENCIA
        card(
          card_header("A. Conocimientos y Experiencia"),
          selectInput("q1", "1. En los últimos 3 años, ¿cuántas operaciones de compra/venta ha realizado en productos de inversión (Acciones, ETFs, Fondos)?",
                      choices = list("Nunca (0 operaciones)" = 1, "Ocasionalmente (1 a 5)" = 4, "Frecuentemente (6 a 15)" = 7, "Muy frecuentemente / Aportaciones periódicas" = 10)),
          selectInput("q2", "2. ¿Cuál de las siguientes afirmaciones sobre riesgo y rentabilidad considera correcta?",
                      choices = list("No entiendo bien la relación" = 1, "La renta fija garantiza que nunca perderé dinero" = 4, "Para obtener rentabilidad a largo plazo hay que asumir fluctuaciones a corto" = 7, "A mayor plazo el riesgo se mitiga y la diversificación reduce el riesgo no sistemático" = 10))
        ),

        # BLOQUE B: SITUACIÓN FINANCIERA
        card(
          card_header("B. Situación Financiera y Capacidad de Asumir Pérdidas"),
          selectInput("q3", "3. ¿Qué porcentaje de sus ingresos netos mensuales consigue ahorrar tras gastos fijos y deudas?",
                      choices = list("Menos del 5% / Nada" = 1, "Entre 5% y 15%" = 4, "Entre 15% y 30%" = 7, "Más del 30%" = 10)),
          selectInput("q4", "4. Si sus ingresos cesaran hoy, ¿cuántos meses podría mantener su nivel de vida con sus ahorros actuales?",
                      choices = list("Menos de 3 meses" = 1, "Entre 3 y 6 meses" = 4, "Entre 6 y 12 meses" = 7, "Más de 12 meses" = 10)),
          selectInput("q5", "5. ¿Qué porcentaje de su patrimonio líquido total representa la cantidad que va a invertir aquí?",
                      choices = list("Más del 50% (Casi todo mi ahorro)" = 1, "Entre 25% y 50%" = 4, "Entre 10% y 25%" = 7, "Menos del 10%" = 10))
        ),

        # BLOQUE C: OBJETIVOS Y ESTRÉS (I)
        card(
          card_header("C. Objetivos de Inversión y Tolerancia al Riesgo"),
          selectInput("q6", "6. ¿Durante cuánto tiempo tiene previsto mantener esta inversión?",
                      choices = list("Menos de 2 años" = 1, "Entre 2 y 5 años" = 4, "Entre 5 y 10 años" = 7, "Más de 10 años" = 10)),
          selectInput("q7", "7. ¿Considera probable necesitar retirar parte de la inversión por un imprevisto?",
                      choices = list("Muy probable (Liquidez inmediata)" = 1, "Bastante probable" = 4, "Poco probable" = 7, "Imposible (Tengo fondo de emergencia separado)" = 10)),
          selectInput("q8", "8. ¿Cuál es el objetivo principal que busca con esta cartera?",
                      choices = list("Preservar capital / Evitar cualquier pérdida" = 1, "Proteger de la inflación con riesgo bajo" = 4, "Crecimiento moderado equilibrado" = 7, "Maximizar rentabilidad con alta volatilidad" = 10))
        ),

        # BLOQUE D: OBJETIVOS Y ESTRÉS (II)
        card(
          card_header("D. Reacción ante Escenarios de Estrés"),
          selectInput("q9", "9. Si invierte 10.000€ y su cartera cae a 8.500€ (-15%) en un mes, ¿qué haría?",
                      choices = list("Vendería inmediatamente (incluso al -5%)" = 1, "Vendería al llegar al -15%" = 4, "Mantendría esperando recuperación" = 7, "Aprovecharía para invertir más" = 10)),
          selectInput("q10", "10. ¿Qué escenario anual de rentabilidad/riesgo le hace sentir más cómodo?",
                      choices = list("+2% Rentabilidad / -1% Caída máxima" = 1, "+5% / -8% Caída" = 4, "+8% / -15% Caída" = 7, "+12% / -25% Caída" = 10))
        )
      ),

      card(
        div(class = "d-flex justify-content-center",
            actionButton("calc", "Finalizar Test y Generar Cartera", class = "btn-primary btn-lg"))
      )
    ),

    # PESTAÑA 2: RESULTADOS
    nav_panel(
      title = "2. Mi Cartera Optimizada",
      value = "panel_resultados",

      layout_column_wrap(
        width = 1/3,
        value_box(title = "Perfil Calculado", value = textOutput("perfil_txt"), showcase = bsicons::bs_icon("person-vcard")),
        value_box(title = "Rentabilidad Esperada", value = textOutput("rent_txt"), showcase = bsicons::bs_icon("graph-up"), theme = "success"),
        value_box(title = "Riesgo Máximo (VaR)", value = textOutput("var_txt"), showcase = bsicons::bs_icon("shield-exclamation"), theme = "danger")
      ),
      layout_column_wrap(
        width = 1/2,
        card(card_header("Composición de la Cartera (Markowitz)"), plotlyOutput("plot_quesito")),
        card(card_header("Simulación Histórica (Crecimiento de 10.000€)"), plotlyOutput("plot_evolucion"))
      )
    )
  )
)
